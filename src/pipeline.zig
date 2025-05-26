const std = @import("std");
const ast = @import("frontend/ast.zig");
const parser = @import("frontend/parser.zig");
const bytecode = @import("backend/bytecode.zig");
const compiler = @import("compiler.zig");

/// Result of a compilation pipeline run
pub const CompiledResult = struct {
    /// The main bytecode function
    main_func: bytecode.Function,
    /// Arena allocator used for parsing (caller must clean up)
    parse_arena: *std.heap.ArenaAllocator,
    
    pub fn deinit(self: *CompiledResult, allocator: std.mem.Allocator) void {
        self.main_func.deinit();
        self.parse_arena.deinit();
        allocator.destroy(self.parse_arena);
    }
};

/// Compile Gene source code into bytecode
pub fn compileSource(allocator: std.mem.Allocator, source: []const u8, options: compiler.CompilerOptions) !CompiledResult {
    // Parse source into AST
    const parse_result = try parser.parseGeneSource(allocator, source);
    
    // Get the parsed nodes
    const nodes = parser.getLastParseNodes() orelse {
        parse_result.arena.deinit();
        allocator.destroy(parse_result.arena);
        return error.NoNodesReturned;
    };
    
    // Compile nodes to bytecode
    const ctx = compiler.CompilationContext.init(allocator, options);
    const conversion_result = try compiler.compile(ctx, nodes);
    
    return CompiledResult{
        .main_func = conversion_result.main_func,
        .parse_arena = parse_result.arena,
    };
}

/// Compile AST nodes into bytecode (useful for testing or when AST is already available)
pub fn compileNodes(allocator: std.mem.Allocator, nodes: []ast.AstNode, options: compiler.CompilerOptions) !bytecode.Function {
    const ctx = compiler.CompilationContext.init(allocator, options);
    const conversion_result = try compiler.compile(ctx, nodes);
    return conversion_result.main_func;
}

/// Read and compile a Gene source file
pub fn compileFile(allocator: std.mem.Allocator, path: []const u8, options: compiler.CompilerOptions) !CompiledResult {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();
    
    const source = try file.readToEndAlloc(allocator, std.math.maxInt(usize));
    defer allocator.free(source);
    
    return try compileSource(allocator, source, options);
}