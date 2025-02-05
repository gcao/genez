const std = @import("std");
const ast = @import("ast.zig");
const bytecode = @import("bytecode.zig");
const ast_to_hir = @import("ast_to_hir.zig");
const hir_to_mir = @import("hir_to_mir.zig");
const mir_to_bytecode = @import("mir_to_bytecode.zig");

pub const CompilerOptions = struct {
    debug_mode: bool = false,
    optimize: bool = false,
};

pub const CompilationContext = struct {
    allocator: std.mem.Allocator,
    options: CompilerOptions,

    pub fn init(allocator: std.mem.Allocator, options: CompilerOptions) CompilationContext {
        return .{
            .allocator = allocator,
            .options = options,
        };
    }
};

pub fn compile(ctx: CompilationContext, nodes: []ast.AstNode) !bytecode.Function {
    if (ctx.options.debug_mode) {
        std.debug.print("\n=== AST to HIR ===\n", .{});
    }
    // AST -> HIR
    var hir_prog = try ast_to_hir.convert(ctx.allocator, nodes);
    defer hir_prog.deinit();

    if (ctx.options.debug_mode) {
        std.debug.print("\n=== HIR to MIR ===\n", .{});
    }
    // HIR -> MIR
    var mir_prog = try hir_to_mir.convert(ctx.allocator, hir_prog);
    defer mir_prog.deinit();

    if (ctx.options.debug_mode) {
        std.debug.print("\n=== MIR to Bytecode ===\n", .{});
    }
    // MIR -> Bytecode
    return try mir_to_bytecode.convert(ctx.allocator, &mir_prog);
}