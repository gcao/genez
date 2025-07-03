const std = @import("std");
const ast = @import("frontend/ast.zig");
const bytecode = @import("backend/bytecode.zig");
const debug_output = @import("core/debug_output.zig");
const module_resolver = @import("core/module_resolver.zig");
const module_registry = @import("core/module_registry.zig");
const types = @import("core/types.zig");

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
    module_resolver: *module_resolver.ModuleResolver,
    module_registry: ?*module_registry.ModuleRegistry,
    current_file: ?[]const u8,

    pub fn init(allocator: std.mem.Allocator, options: CompilerOptions) !CompilationContext {
        const resolver = try allocator.create(module_resolver.ModuleResolver);
        errdefer allocator.destroy(resolver);
        resolver.* = try module_resolver.ModuleResolver.init(allocator);
        errdefer resolver.deinit();
        
        const registry = try allocator.create(module_registry.ModuleRegistry);
        errdefer allocator.destroy(registry);
        registry.* = module_registry.ModuleRegistry.init(allocator);
        
        return .{
            .allocator = allocator,
            .options = options,
            .module_resolver = resolver,
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
        self.module_resolver.deinit();
        self.allocator.destroy(self.module_resolver);
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
        
        // Use the module resolver from context
        const resolver = ctx.module_resolver;
        
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
            // Use the module ID from import (or alias if provided)
            const module_id = import.alias orelse blk: {
                // Extract module name from path (e.g., "./math" -> "math", "utils/helpers" -> "utils/helpers")
                var path = import.module_path;
                if (std.mem.startsWith(u8, path, "./")) {
                    path = path[2..];
                } else if (std.mem.startsWith(u8, path, "../")) {
                    // For parent imports, use the full path as ID
                    break :blk import.module_path;
                }
                // Remove .gene extension if present
                if (std.mem.endsWith(u8, path, ".gene")) {
                    break :blk path[0..path.len - 5];
                }
                break :blk path;
            };
            
            const compiled_module = try module_registry.CompiledModule.init(
                ctx.allocator,
                module_id,
                resolved.absolute_path
            );
            errdefer compiled_module.deinit();
            
            debug.writeMessage("  Created CompiledModule with ID: {s}\n", .{module_id});
            
            // Add all functions from the module to the compiled module
            // We need to clone functions since module_result will be cleaned up
            for (module_result.created_functions.items) |func| {
                const func_copy = try ctx.allocator.create(bytecode.Function);
                func_copy.* = try func.clone(ctx.allocator);
                try compiled_module.addFunction(func_copy);
            }
            
            // Check if the module has properties (first expression is a map)
            // Parse the source again to get the AST since we need property info
            const module_ast = parser.parseGeneSourceWithFilename(ctx.allocator, module_source, resolved.absolute_path) catch |err| {
                debug.writeMessage("  Failed to re-parse module for properties: {any}\n", .{err});
                return err;
            };
            defer {
                module_ast.arena.deinit();
                ctx.allocator.destroy(module_ast.arena);
            }
            
            // If the first node is a map, it contains module properties
            if (module_ast.nodes.len > 0) {
                if (module_ast.nodes[0] == .Expression) {
                    if (module_ast.nodes[0].Expression == .MapLiteral) {
                        const map_literal = module_ast.nodes[0].Expression.MapLiteral;
                        debug.writeMessage("  Processing {} module properties\n", .{map_literal.entries.len});
                        
                        // Add each property to the module namespace
                        for (map_literal.entries) |entry| {
                            // Get property name
                            const key_str = switch (entry.key.*.Literal.value) {
                                .String => |s| s,
                                else => continue, // Skip non-string keys
                            };
                            
                            // Convert AST value to runtime value
                            const value = switch (entry.value.*) {
                                .Literal => |lit| switch (lit.value) {
                                    .Int => |i| types.Value{ .Int = i },
                                    .Float => |f| types.Value{ .Float = f },
                                    .String => |s| types.Value{ .String = try ctx.allocator.dupe(u8, s) },
                                    .Bool => |b| types.Value{ .Bool = b },
                                    .Nil => types.Value{ .Nil = {} },
                                    else => continue, // Skip complex values for now
                                },
                                else => continue, // Skip non-literal values
                            };
                            
                            // Add to module namespace
                            try compiled_module.namespace.define(key_str, value);
                            debug.writeMessage("    Added property: {s} = {any}\n", .{key_str, value});
                        }
                    }
                }
            }
            
            // Execute the module's main function to initialize properties
            // This will evaluate top-level expressions like property definitions
            if (module_result.main_func.instructions.items.len > 1) { // More than just Return
                debug.writeMessage("  Executing module initialization...\n", .{});
                
                // Create a temporary VM to execute the module's init code
                var init_vm = @import("backend/vm.zig").VM.init(ctx.allocator, std.io.getStdOut().writer());
                defer init_vm.deinit();
                
                // Set the module registry so the init code can access other modules if needed
                if (ctx.module_registry) |registry| {
                    init_vm.setModuleRegistry(registry);
                }
                
                // Execute the init function
                init_vm.execute(&module_result.main_func) catch |err| {
                    debug.writeMessage("  Module initialization failed: {any}\n", .{err});
                };
                
                // Extract variables from the VM and add exportable ones to the module namespace
                var var_iter = init_vm.getVariables();
                while (var_iter.next()) |entry| {
                    const name = entry.key_ptr.*;
                    const value = entry.value_ptr.*;
                    
                    // Skip built-in operators and functions
                    if (std.mem.eql(u8, name, "print") or 
                        std.mem.eql(u8, name, "println") or
                        std.mem.eql(u8, name, "+") or
                        std.mem.eql(u8, name, "-") or
                        std.mem.eql(u8, name, "*") or
                        std.mem.eql(u8, name, "/") or
                        std.mem.eql(u8, name, "%") or
                        std.mem.eql(u8, name, "**") or
                        std.mem.eql(u8, name, "==") or
                        std.mem.eql(u8, name, "!=") or
                        std.mem.eql(u8, name, "<") or
                        std.mem.eql(u8, name, ">") or
                        std.mem.eql(u8, name, "<=") or
                        std.mem.eql(u8, name, ">=") or
                        std.mem.eql(u8, name, "&&") or
                        std.mem.eql(u8, name, "||") or
                        std.mem.eql(u8, name, "!") or
                        std.mem.startsWith(u8, name, "Class") or
                        std.mem.startsWith(u8, name, "Any") or
                        std.mem.startsWith(u8, name, "Number") or
                        std.mem.startsWith(u8, name, "Int") or
                        std.mem.startsWith(u8, name, "Float") or
                        std.mem.startsWith(u8, name, "String") or
                        std.mem.startsWith(u8, name, "Bool") or
                        std.mem.startsWith(u8, name, "Nil") or
                        std.mem.startsWith(u8, name, "Symbol") or
                        std.mem.startsWith(u8, name, "Fn") or
                        std.mem.startsWith(u8, name, "BuiltinFn") or
                        std.mem.startsWith(u8, name, "Macro") or
                        std.mem.startsWith(u8, name, "Array") or
                        std.mem.startsWith(u8, name, "Map") or
                        std.mem.startsWith(u8, name, "gc_") or
                        std.mem.startsWith(u8, name, "len") or
                        std.mem.startsWith(u8, name, "type")) {
                        continue;
                    }
                    
                    // Export based on naming conventions:
                    // - Variables starting with uppercase are exported
                    // - All functions are exported
                    // - All classes are exported
                    const should_export = blk: {
                        if (name.len > 0 and std.ascii.isUpper(name[0])) {
                            // Uppercase variable
                            break :blk true;
                        }
                        
                        switch (value) {
                            .Function => break :blk true,
                            .Class => break :blk true,
                            else => break :blk false,
                        }
                    };
                    
                    if (should_export) {
                        // Clone the value for the module namespace
                        const cloned_value = try value.clone(ctx.allocator);
                        try compiled_module.namespace.define(name, cloned_value);
                        debug.writeMessage("    Exported {s}: {any}\n", .{name, std.meta.activeTag(value)});
                    }
                }
                
                debug.writeMessage("  Module initialization completed\n", .{});
            }
            
            // Register the module
            if (ctx.module_registry) |registry| {
                try registry.registerModule(compiled_module);
                debug.writeMessage("DEBUG: Registered module {s} in compiler\n", .{compiled_module.id});
            } else {
                debug.writeMessage("WARNING: No module registry available, module {s} not registered\n", .{compiled_module.id});
                // If no registry, we have to clean up the module
                compiled_module.deinit();
            }
            
            debug.writeMessage("  Module registered with {d} functions\n", .{compiled_module.functions.items.len});
            
            // Handle selective imports - create individual bindings for each imported item
            if (import.items) |items| {
                debug.writeMessage("  Processing selective imports ({d} items)\n", .{items.len});
                
                // We need to add the imported items to the main function's scope
                // This will be done during MIR generation when we process the import statement
                // For now, we just validate that the items exist in the module
                
                for (items) |item| {
                    // Check if the item exists in the module's namespace
                    if (compiled_module.namespace.lookup(item.name)) |_| {
                        debug.writeMessage("    Found item: {s}", .{item.name});
                        if (item.alias) |alias| {
                            debug.writeMessage(" (aliased as {s})", .{alias});
                        }
                        debug.writeMessage("\n", .{});
                    } else {
                        // Check if it's a function
                        var found = false;
                        for (compiled_module.functions.items) |func| {
                            if (std.mem.eql(u8, func.name, item.name)) {
                                found = true;
                                debug.writeMessage("    Found function: {s}", .{item.name});
                                if (item.alias) |alias| {
                                    debug.writeMessage(" (aliased as {s})", .{alias});
                                }
                                debug.writeMessage("\n", .{});
                                break;
                            }
                        }
                        
                        if (!found) {
                            debug.writeMessage("    WARNING: Item '{s}' not found in module\n", .{item.name});
                            // Don't error out - let it fail at runtime if the item is actually used
                        }
                    }
                }
            }
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
