const std = @import("std");
const ast = @import("ast.zig");
const hir = @import("hir.zig");
const mir = @import("mir.zig");
const bytecode = @import("bytecode.zig");

pub fn compileAst(allocator: *std.mem.Allocator, nodes: []ast.AstNode) !bytecode.Function {
    // AST -> HIR
    var hir_prog = try astToHir(allocator.*, nodes);
    defer hir_prog.deinit();

    // HIR -> MIR
    var mir_prog = try mir.hirToMir(allocator.*, hir_prog);
    defer mir_prog.deinit();

    // MIR -> Bytecode
    return try mirToBytecode(allocator.*, mir_prog);
}

fn astToHir(allocator: std.mem.Allocator, nodes: []ast.AstNode) !hir.HIR {
    var hir_prog = hir.HIR.init(allocator);
    errdefer hir_prog.deinit();

    // Create main function for top-level code
    var main_func = hir.HIR.Function.init(allocator);
    main_func.name = "main";

    // Convert each AST node to HIR statements
    for (nodes) |node| {
        try lowerNode(&main_func.body, node);
    }

    try hir_prog.functions.append(main_func);
    return hir_prog;
}

fn lowerNode(statements: *std.ArrayList(hir.HIR.Statement), node: ast.AstNode) !void {
    switch (node) {
        .Expression => |expr| {
            const hir_expr = try lowerExpression(statements.allocator, expr);
            try statements.append(.{ .Expression = hir_expr });
        },
        else => {},
    }
}

fn lowerExpression(allocator: std.mem.Allocator, expr: ast.Expression) !hir.HIR.Expression {
    return switch (expr) {
        .Literal => |lit| switch (lit.value) {
            .Int => |val| .{ .literal = .{ .int = val } },
            .String => |val| .{ .literal = .{ .string = try allocator.dupe(u8, val) } },
            .Bool => |val| .{ .literal = .{ .bool = val } },
        },
        .Variable => |var_expr| .{ .variable = .{ .name = try allocator.dupe(u8, var_expr.name) } },
        .BinaryOp => |bin_op| .{
            .binary_op = .{
                .op = switch (bin_op.op) {
                    .Add => .add,
                },
                .left = try allocator.create(hir.HIR.Expression),
                .right = try allocator.create(hir.HIR.Expression),
            },
        },
        else => error.Unimplemented,
    };
}

fn mirToBytecode(allocator: *std.mem.Allocator, mir_prog: mir.MIR) !bytecode.Function {
    var instructions = std.ArrayList(bytecode.Instruction).init(allocator.*);
    errdefer instructions.deinit();

    // Convert MIR blocks to bytecode instructions
    for (mir_prog.functions.items) |func| {
        for (func.blocks.items) |block| {
            for (block.instructions.items) |instr| {
                try lowerInstruction(&instructions, instr);
            }
        }
    }

    return bytecode.Function{
        .instructions = try instructions.toOwnedSlice(),
    };
}

fn lowerInstruction(instructions: *std.ArrayList(bytecode.Instruction), instr: mir.MIR.Instruction) !void {
    switch (instr) {
        .LoadInt => |val| try instructions.append(.{ .code = .{ .LoadInt = .{ .value = val } } }),
        .LoadString => |val| try instructions.append(.{ .code = .{ .LoadString = .{ .value = val } } }),
        .LoadBool => |val| try instructions.append(.{ .code = .{ .LoadBool = .{ .value = val } } }),
        .Add => try instructions.append(.{ .code = .Add }),
        .Print => try instructions.append(.{ .code = .Print }),
        .Return => try instructions.append(.{ .code = .Return }),
        else => {},
    }
}