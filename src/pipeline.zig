const std = @import("std");
const ast = @import("frontend/ast.zig");
const parser = @import("frontend/parser.zig");
const bytecode = @import("backend/bytecode.zig");
const compiler = @import("compiler.zig");
const lir_to_bytecode = @import("transforms/lir_to_bytecode.zig");
const mir_to_bytecode = @import("transforms/mir_to_bytecode.zig");

/// Result of a compilation pipeline run
pub const CompiledResult = struct {
    /// The main bytecode function
    main_func: bytecode.Function,
    /// Created functions that need cleanup
    created_functions: std.ArrayList(*bytecode.Function),
    /// Arena allocator used for parsing (caller must clean up)
    parse_arena: *std.heap.ArenaAllocator,
    /// Allocator used for the compilation
    allocator: std.mem.Allocator,
    /// Module registry containing all loaded modules
    module_registry: ?*@import("core/module_registry.zig").ModuleRegistry,
    /// FFI functions from HIR
    ffi_functions: std.ArrayList(*@import("ir/hir.zig").HIR.FFIFunction),

    pub fn deinit(self: *CompiledResult) void {
        // Clean up created functions
        // Note: The functions themselves are allocated in the arena, so we don't destroy them
        for (self.created_functions.items) |func| {
            func.deinit();
        }
        self.created_functions.deinit();

        // Clean up main function
        self.main_func.deinit();

        // Clean up module registry if present (must be done before arena cleanup)
        if (self.module_registry) |registry| {
            registry.deinit();
            // Note: registry itself is allocated in the arena, so don't destroy it
        }
        
        // FFI functions are allocated in the arena, so we don't free them here
        // Just clean up the list itself
        self.ffi_functions.deinit();
        
        // Clean up parse arena - this will free all arena-allocated memory
        self.parse_arena.deinit();
        self.allocator.destroy(self.parse_arena);
    }
};

/// Compile Gene source code into bytecode
pub fn compileSource(allocator: std.mem.Allocator, source: []const u8, options: compiler.CompilerOptions) !CompiledResult {
    return compileSourceWithFilename(allocator, source, null, options);
}

/// Compile Gene source code with filename information
pub fn compileSourceWithFilename(allocator: std.mem.Allocator, source: []const u8, filename: ?[]const u8, options: compiler.CompilerOptions) !CompiledResult {
    // Parse source into AST with filename
    const parse_result = try parser.parseGeneSourceWithFilename(allocator, source, filename);
    const nodes = parse_result.nodes;

    // Compile nodes to bytecode
    // Use the arena allocator for compilation to ensure consistent memory management
    const arena_allocator = parse_result.arena.allocator();
    var ctx = try compiler.CompilationContext.initWithFile(arena_allocator, options, filename);
    // Don't defer deinit here, ownership transfers to CompiledResult
    const conversion_result = try compiler.compile(ctx, nodes);
    
    // Simply transfer ownership of FFI functions from conversion_result
    // The FFI functions are allocated in the arena, but we'll take ownership
    const ffi_functions_copy = conversion_result.ffi_functions;
    
    // Transfer ownership of module_registry BEFORE deinit
    const registry = ctx.module_registry;
    ctx.module_registry = null; // Prevent double-free
    
    // Store the main function and created functions before deinit
    const main_func = conversion_result.main_func;
    const created_functions = conversion_result.created_functions;
    
    ctx.deinit();

    return CompiledResult{
        .main_func = main_func,
        .created_functions = created_functions,
        .parse_arena = parse_result.arena,
        .allocator = allocator,
        .module_registry = registry,
        .ffi_functions = ffi_functions_copy,
    };
}

/// Compile AST nodes into bytecode (useful for testing or when AST is already available)
/// Note: This function does not properly clean up created_functions - use compileSource instead
pub fn compileNodes(allocator: std.mem.Allocator, nodes: []ast.AstNode, options: compiler.CompilerOptions) !bytecode.Function {
    var ctx = try compiler.CompilationContext.init(allocator, options);
    defer ctx.deinit();
    var conversion_result = try compiler.compile(ctx, nodes);
    defer conversion_result.deinit();

    // Clone the main function to return it
    var main_func = bytecode.Function.init(allocator);
    main_func.name = try allocator.dupe(u8, conversion_result.main_func.name);
    main_func.param_count = conversion_result.main_func.param_count;

    // Copy instructions
    for (conversion_result.main_func.instructions.items) |instr| {
        var new_instr = instr;
        if (instr.immediate) |operand| {
            new_instr.immediate = try operand.clone(allocator);
        }
        try main_func.instructions.append(new_instr);
    }

    return main_func;
}

/// Read and compile a Gene source file
pub fn compileFile(allocator: std.mem.Allocator, path: []const u8, options: compiler.CompilerOptions) !CompiledResult {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    const source = try file.readToEndAlloc(allocator, std.math.maxInt(usize));
    defer allocator.free(source);

    return try compileSourceWithFilename(allocator, source, path, options);
}
