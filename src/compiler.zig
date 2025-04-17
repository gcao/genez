const std = @import("std");
const ast = @import("ast.zig");
const bytecode = @import("bytecode.zig");

// Stage transformation interfaces
const ast_to_hir = @import("ast_to_hir_interface.zig");
const hir_to_mir = @import("hir_to_mir_interface.zig");
const mir_to_bytecode = @import("mir_to_bytecode_interface.zig");

// Stage-specific serialization modules
const ast_serialize = @import("ast_serialize.zig");
const hir_serialize = @import("hir_serialize.zig");
const mir_serialize = @import("mir_serialize.zig");
const bytecode_serialize = @import("bytecode_serialize.zig");

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
    // Display AST
    if (ctx.options.debug_mode) {
        std.debug.print("\n=== AST ===\n", .{});
        for (nodes) |node| {
            try ast_serialize.serializeNode(std.io.getStdOut().writer(), node, 0);
            std.debug.print("\n", .{});
        }
    }

    // AST -> HIR
    if (ctx.options.debug_mode) {
        std.debug.print("\n=== AST to HIR ===\n", .{});
    }
    var hir_prog = try ast_to_hir.convert(ctx.allocator, nodes);
    defer hir_prog.deinit();

    // Display HIR
    if (ctx.options.debug_mode) {
        std.debug.print("\n=== HIR ===\n", .{});
        try hir_serialize.serializeModule(std.io.getStdOut().writer(), hir_prog, 0);
        std.debug.print("\n", .{});
    }

    // HIR -> MIR
    if (ctx.options.debug_mode) {
        std.debug.print("\n=== HIR to MIR ===\n", .{});
    }
    var mir_prog = try hir_to_mir.convert(ctx.allocator, hir_prog);
    defer mir_prog.deinit();

    // Display MIR
    if (ctx.options.debug_mode) {
        std.debug.print("\n=== MIR ===\n", .{});
        try mir_serialize.serializeModule(std.io.getStdOut().writer(), mir_prog, 0);
        std.debug.print("\n", .{});
    }

    // MIR -> Bytecode
    if (ctx.options.debug_mode) {
        std.debug.print("\n=== MIR to Bytecode ===\n", .{});
    }
    const func = try mir_to_bytecode.convert(ctx.allocator, &mir_prog);

    // Display Bytecode
    if (ctx.options.debug_mode) {
        std.debug.print("\n=== Bytecode ===\n", .{});
        try bytecode_serialize.serializeFunction(std.io.getStdOut().writer(), func, 0);
        std.debug.print("\n", .{});
    }

    // std.debug.print("[DEBUG_TRACE] Exiting compiler.compile\n", .{}); // TRACE 5
    return func;
}
