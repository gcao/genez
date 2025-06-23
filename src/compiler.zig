const std = @import("std");
const ast = @import("frontend/ast.zig");
const bytecode = @import("backend/bytecode.zig");
const debug_output = @import("core/debug_output.zig");
const module = @import("core/module.zig");

// Stage transformation modules (direct imports, no interfaces)
const ast_to_hir = @import("transforms/ast_to_hir.zig");
const hir_to_mir = @import("transforms/hir_to_mir.zig");
const mir_to_lir = @import("transforms/mir_to_lir.zig");
const lir_to_bytecode = @import("transforms/lir_to_bytecode.zig");
const mir_to_bytecode = @import("transforms/mir_to_bytecode.zig");
const hir_typechecker = @import("core/hir_typechecker.zig");
const macro_expander = @import("transforms/macro_expander.zig");

pub const CompilerOptions = struct {
    debug_mode: bool = false,
    optimize: bool = false,
    type_check: bool = true,
};

pub const CompilationContext = struct {
    allocator: std.mem.Allocator,
    options: CompilerOptions,
    module_loader: *module.ModuleLoader,
    module_registry: ?*@import("core/module_registry.zig").ModuleRegistry,
    current_file: ?[]const u8,

    pub fn init(allocator: std.mem.Allocator, options: CompilerOptions) !CompilationContext {
        const loader = try module.ModuleLoader.init(allocator);
        errdefer loader.deinit();
        
        const registry = try allocator.create(@import("core/module_registry.zig").ModuleRegistry);
        errdefer allocator.destroy(registry);
        registry.* = @import("core/module_registry.zig").ModuleRegistry.init(allocator);
        
        return .{
            .allocator = allocator,
            .options = options,
            .module_loader = loader,
            .module_registry = registry,
            .current_file = null,
        };
    }
    
    pub fn initWithFile(allocator: std.mem.Allocator, options: CompilerOptions, filename: ?[]const u8) !CompilationContext {
        var ctx = try init(allocator, options);
        ctx.current_file = filename;
        return ctx;
    }
    
    pub fn deinit(self: *CompilationContext) void {
        self.module_loader.deinit();
        // Note: module_registry ownership may be transferred to CompiledResult
        // Only clean up if we still own it
        if (self.module_registry) |registry| {
            registry.deinit();
            self.allocator.destroy(registry);
        }
    }
};

pub fn compile(ctx: CompilationContext, nodes: []ast.AstNode) !mir_to_bytecode.ConversionResult {
    // Initialize debug output
    const debug = debug_output.DebugOutput.init(std.io.getStdOut().writer(), ctx.options.debug_mode);
    
    // Display AST
    try debug.writeAST(nodes, "AST");

    // Macro expansion
    debug.writeMessage("\n=== Macro Expansion ===\n", .{});
    const expanded_nodes = try macro_expander.expandMacros(ctx.allocator, nodes);
    defer ctx.allocator.free(expanded_nodes);
    
    // Display expanded AST if macros were expanded
    if (ctx.options.debug_mode) {
        try debug.writeAST(expanded_nodes, "Expanded AST");
    }

    // AST -> HIR
    debug.writeMessage("\n=== AST to HIR ===\n", .{});
    var hir_prog = try ast_to_hir.convert(ctx.allocator, expanded_nodes);
    defer hir_prog.deinit();

    // Display HIR
    try debug.writeHIR(hir_prog, "HIR");

    // Process imports - load required modules
    if (hir_prog.imports.items.len > 0) {
        debug.writeMessage("\n=== Processing Imports ===\n", .{});
        
        // Create a module resolver
        var resolver = try @import("core/module_resolver.zig").ModuleResolver.init(ctx.allocator);
        defer resolver.deinit();
        
        for (hir_prog.imports.items) |import| {
            debug.writeMessage("Import: {s}", .{import.module_path});
            if (import.alias) |alias| {
                debug.writeMessage(" as {s}", .{alias});
            }
            debug.writeMessage("\n", .{});
            
            // Try to resolve the module
            var resolved = resolver.resolve(import.module_path, ctx.current_file) catch |err| {
                debug.writeMessage("  Failed to resolve: {any}\n", .{err});
                continue;
            };
            defer resolved.deinit(ctx.allocator);
            
            debug.writeMessage("  Resolved to: {s}\n", .{resolved.absolute_path});
            
            // Check if module is already being loaded (circular import detection)
            if (resolver.isLoaded(resolved.module_id)) {
                debug.writeMessage("  Module already loaded\n", .{});
                continue;
            }
            
            // Mark module as being loaded
            try resolver.beginLoading(resolved.module_id);
            defer resolver.endLoading(resolved.module_id);
            
            // Read the module file
            const module_source = std.fs.cwd().readFileAlloc(ctx.allocator, resolved.absolute_path, std.math.maxInt(usize)) catch |err| {
                debug.writeMessage("  Failed to read module file: {any}\n", .{err});
                continue;
            };
            defer ctx.allocator.free(module_source);
            
            debug.writeMessage("  Module source loaded ({d} bytes)\n", .{module_source.len});
            
            // Parse the module
            const parser = @import("frontend/parser.zig");
            const module_parse_result = parser.parseGeneSourceWithFilename(ctx.allocator, module_source, resolved.absolute_path) catch |err| {
                debug.writeMessage("  Failed to parse module: {any}\n", .{err});
                continue;
            };
            defer {
                module_parse_result.arena.deinit();
                ctx.allocator.destroy(module_parse_result.arena);
            }
            
            debug.writeMessage("  Module parsed successfully\n", .{});
            
            // Recursively compile the module (this will handle its imports too)
            var module_ctx = try CompilationContext.init(ctx.allocator, ctx.options);
            defer module_ctx.deinit();
            
            var module_result = compile(module_ctx, module_parse_result.nodes) catch |err| {
                debug.writeMessage("  Failed to compile module: {any}\n", .{err});
                continue;
            };
            defer module_result.deinit();
            
            debug.writeMessage("  Module compiled successfully\n", .{});
            
            // Create a CompiledModule and register it
            const module_registry = @import("core/module_registry.zig");
            const compiled_module = try module_registry.CompiledModule.init(
                ctx.allocator,
                import.module_path,
                resolved.absolute_path
            );
            errdefer compiled_module.deinit();
            
            // Add all functions from the module to the compiled module
            // We need to clone functions since module_result will be cleaned up
            for (module_result.created_functions.items) |func| {
                const func_copy = try ctx.allocator.create(bytecode.Function);
                func_copy.* = try func.clone(ctx.allocator);
                try compiled_module.addFunction(func_copy);
            }
            
            // Also add the main function if it has a meaningful name
            if (!std.mem.eql(u8, module_result.main_func.name, "main")) {
                const main_func_copy = try ctx.allocator.create(bytecode.Function);
                main_func_copy.* = try module_result.main_func.clone(ctx.allocator);
                try compiled_module.addFunction(main_func_copy);
            }
            
            // Register the module
            if (ctx.module_registry) |registry| {
                try registry.registerModule(compiled_module);
            } else {
                // If no registry, we have to clean up the module
                compiled_module.deinit();
            }
            
            debug.writeMessage("  Module registered with {d} functions\n", .{compiled_module.functions.items.len});
        }
    }

    // Type checking (if enabled)
    if (ctx.options.type_check) {
        debug.writeMessage("\n=== Type Checking ===\n", .{});
        var type_checker = hir_typechecker.HIRTypeChecker.init(ctx.allocator);
        defer type_checker.deinit();
        
        type_checker.checkProgram(hir_prog) catch |err| {
            if (type_checker.hasErrors()) {
                type_checker.printErrors();
            }
            return err;
        };
        
        if (type_checker.hasErrors()) {
            type_checker.printErrors();
            return error.TypeCheckFailed;
        }
        
        debug.writeMessage("Type checking passed\n", .{});
    }

    // HIR -> MIR
    debug.writeMessage("\n=== HIR to MIR ===\n", .{});
    var mir_prog = try hir_to_mir.convert(ctx.allocator, hir_prog);
    defer mir_prog.deinit();

    // Display MIR
    try debug.writeMIR(mir_prog, "MIR");

    // MIR -> Bytecode (bypassing LIR for now until function handling is fixed)
    debug.writeMessage("\n=== MIR to Bytecode ===\n", .{});
    const conversion_result = try mir_to_bytecode.convert(ctx.allocator, &mir_prog);

    // Display Bytecode
    try debug.writeBytecode(conversion_result.main_func, "Bytecode");

    return conversion_result;
}
