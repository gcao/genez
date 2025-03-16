const std = @import("std");
const AstNode = @import("ast.zig").AstNode;
const hir = @import("hir.zig");

pub fn convert(allocator: std.mem.Allocator, nodes: []const AstNode) !hir.HIR {
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

fn lowerNode(statements: *std.ArrayList(hir.HIR.Statement), node: AstNode) !void {
    switch (node.tag) {
        .Expression => |expr| {
            const hir_expr = try lowerExpression(statements.allocator, expr);
            try statements.append(.{ .Expression = hir_expr });
        },
    }
}

fn lowerExpression(allocator: std.mem.Allocator, expr: AstNode.Expression) !hir.HIR.Expression {
    return switch (expr) {
        .Literal => |lit| switch (lit.value) {
            .Int => |val| .{ .literal = .{ .int = val } },
            .String => |val| .{ .literal = .{ .string = try allocator.dupe(u8, val) } },
            .Bool => |val| .{ .literal = .{ .bool = val } },
            .Float => |val| .{ .literal = .{ .float = val } },
            .Nil => .{ .literal = .{ .nil = {} } },
            .Symbol => |val| .{ .literal = .{ .symbol = try allocator.dupe(u8, val) } },
            .Array => |val| .{ .literal = .{ .array = try allocator.dupe(AstNode.Value, val) } },
            .Map => |val| {
                var new_map = std.StringHashMap(AstNode.Value).init(allocator);
                var it = val.iterator();
                while (it.next()) |entry| {
                    try new_map.put(try allocator.dupe(u8, entry.key_ptr.*), entry.value_ptr.*);
                }
                return .{ .literal = .{ .map = new_map } };
            },
        },
        .Variable => |var_expr| .{ .variable = .{ .name = try allocator.dupe(u8, var_expr.name) } },
        .BinaryOp => |bin_op| {
            var left = try lowerExpression(allocator, bin_op.left.*);
            errdefer left.deinit(allocator);

            var right = try lowerExpression(allocator, bin_op.right.*);
            errdefer right.deinit(allocator);

            const left_ptr = try allocator.create(hir.HIR.Expression);
            errdefer allocator.destroy(left_ptr);
            left_ptr.* = left;

            const right_ptr = try allocator.create(hir.HIR.Expression);
            errdefer allocator.destroy(right_ptr);
            right_ptr.* = right;

            return hir.HIR.Expression{
                .binary_op = .{
                    .op = switch (bin_op.op) {
                        .add => .add,
                        .sub => .sub,
                        .mul => .mul,
                        .div => .div,
                        .lt => .lt,
                        .gt => .gt,
                        .eq => .eq,
                    },
                    .left = left_ptr,
                    .right = right_ptr,
                },
            };
        },
        .IfExpr => |if_expr| {
            var cond = try lowerExpression(allocator, if_expr.condition.*);
            errdefer cond.deinit(allocator);

            var then_branch = try lowerExpression(allocator, if_expr.then_branch.*);
            errdefer then_branch.deinit(allocator);

            var else_branch = try lowerExpression(allocator, if_expr.else_branch.*);
            errdefer else_branch.deinit(allocator);

            const cond_ptr = try allocator.create(hir.HIR.Expression);
            errdefer allocator.destroy(cond_ptr);
            cond_ptr.* = cond;

            const then_ptr = try allocator.create(hir.HIR.Expression);
            errdefer allocator.destroy(then_ptr);
            then_ptr.* = then_branch;

            const else_ptr = try allocator.create(hir.HIR.Expression);
            errdefer allocator.destroy(else_ptr);
            else_ptr.* = else_branch;

            return hir.HIR.Expression{
                .if_expr = .{
                    .condition = cond_ptr,
                    .then_branch = then_ptr,
                    .else_branch = else_ptr,
                },
            };
        },
        .FuncDef => |func_def| {
            var params = std.ArrayList(hir.HIR.FuncParam).init(allocator);
            errdefer {
                for (params.items) |*param| {
                    param.deinit(allocator);
                }
                params.deinit();
            }

            for (func_def.params.items) |param| {
                try params.append(.{
                    .name = try allocator.dupe(u8, param.name),
                    .param_type = .int, // Default to int for now
                });
            }

            var body = try lowerExpression(allocator, func_def.body.*);
            errdefer body.deinit(allocator);

            const body_ptr = try allocator.create(hir.HIR.Expression);
            errdefer allocator.destroy(body_ptr);
            body_ptr.* = body;

            return hir.HIR.Expression{
                .func_def = .{
                    .name = try allocator.dupe(u8, func_def.name),
                    .params = params,
                    .return_type = .int, // Default to int for now
                    .body = body_ptr,
                },
            };
        },
        .FuncCall => |func_call| {
            var func = try lowerExpression(allocator, func_call.func.*);
            errdefer func.deinit(allocator);

            var args = std.ArrayList(*hir.HIR.Expression).init(allocator);
            errdefer {
                for (args.items) |arg| {
                    arg.deinit(allocator);
                    allocator.destroy(arg);
                }
                args.deinit();
            }

            for (func_call.args.items) |arg| {
                var arg_expr = try lowerExpression(allocator, arg.*);
                errdefer arg_expr.deinit(allocator);

                const arg_ptr = try allocator.create(hir.HIR.Expression);
                errdefer allocator.destroy(arg_ptr);
                arg_ptr.* = arg_expr;

                try args.append(arg_ptr);
            }

            const func_ptr = try allocator.create(hir.HIR.Expression);
            errdefer allocator.destroy(func_ptr);
            func_ptr.* = func;

            return hir.HIR.Expression{
                .func_call = .{
                    .func = func_ptr,
                    .args = args,
                },
            };
        },
        .If => |if_expr| .{
            .If = .{
                .condition = blk: {
                    const cond = try allocator.create(hir.HIR.Expression);
                    cond.* = try lowerExpression(allocator, if_expr.condition.*);
                    break :blk cond;
                },
                .then_branch = blk: {
                    const then = try allocator.create(hir.HIR.Expression);
                    then.* = try lowerExpression(allocator, if_expr.then_branch.*);
                    break :blk then;
                },
                .else_branch = if (if_expr.else_branch) |else_branch| blk: {
                    const else_expr = try allocator.create(hir.HIR.Expression);
                    else_expr.* = try lowerExpression(allocator, else_branch.*);
                    break :blk else_expr;
                } else null,
            },
        },
    };
}
