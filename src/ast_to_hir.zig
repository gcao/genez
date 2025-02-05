const std = @import("std");
const ast = @import("ast.zig");
const hir = @import("hir.zig");

pub fn convert(allocator: std.mem.Allocator, nodes: []const ast.AstNode) !hir.HIR {
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
    }
}

fn lowerExpression(allocator: std.mem.Allocator, expr: ast.Expression) !hir.HIR.Expression {
    return switch (expr) {
        .Literal => |lit| switch (lit.value) {
            .Int => |val| .{ .literal = .{ .int = val } },
            .String => |val| .{ .literal = .{ .string = try allocator.dupe(u8, val) } },
            .Bool => |val| .{ .literal = .{ .bool = val } },
            .Float => |val| .{ .literal = .{ .float = val } },
            .Nil => .{ .literal = .{ .nil = {} } },
            .Symbol => |val| .{ .literal = .{ .symbol = try allocator.dupe(u8, val) } },
            .Array => |val| .{ .literal = .{ .array = try allocator.dupe(ast.Value, val) } },
            .Map => |val| {
                var new_map = std.StringHashMap(ast.Value).init(allocator);
                var it = val.iterator();
                while (it.next()) |entry| {
                    try new_map.put(try allocator.dupe(u8, entry.key_ptr.*), entry.value_ptr.*);
                }
                return .{ .literal = .{ .map = new_map } };
            },
        },
        .Variable => |var_expr| .{ .variable = .{ .name = try allocator.dupe(u8, var_expr.name) } },
        .BinaryOp => |bin_op| {
            const left = try allocator.create(hir.HIR.Expression);
            left.* = try lowerExpression(allocator, bin_op.left.*);

            const right = try allocator.create(hir.HIR.Expression);
            right.* = try lowerExpression(allocator, bin_op.right.*);

            return .{
                .binary_op = .{
                    .op = switch (bin_op.op) {
                        .Add => .add,
                    },
                    .left = left,
                    .right = right,
                },
            };
        },
    };
}
