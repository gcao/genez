const std = @import("std");
const ast = @import("ast.zig");
const hir = @import("hir.zig");

pub fn convert(allocator: std.mem.Allocator, nodes: []const ast.AstNode) !hir.HIR {
    var hir_prog = hir.HIR.init(allocator);
    errdefer hir_prog.deinit();

    // Create main function for top-level code
    var main_func = hir.HIR.Function.init(allocator);
    // Duplicate the name string to avoid freeing a literal
    main_func.name = try allocator.dupe(u8, "main");
    // Add errdefer *after* successful initialization and name allocation
    errdefer main_func.deinit();

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
            .ReturnAddress => |_| .{ .literal = .{ .nil = {} } }, // Fallback for ReturnAddress
            .Function => |_| .{ .literal = .{ .nil = {} } }, // Fallback for Function
            .Variable => |_| .{ .literal = .{ .nil = {} } }, // Fallback for Variable
        },
        .Variable => |var_expr| .{ .variable = .{ .name = try allocator.dupe(u8, var_expr.name) } },
        .If => |if_expr| {
            var condition = try lowerExpression(allocator, if_expr.condition.*);
            errdefer condition.deinit(allocator);

            var then_branch = try lowerExpression(allocator, if_expr.then_branch.*);
            errdefer then_branch.deinit(allocator);

            const condition_ptr = try allocator.create(hir.HIR.Expression);
            errdefer allocator.destroy(condition_ptr);
            condition_ptr.* = condition;

            const then_branch_ptr = try allocator.create(hir.HIR.Expression);
            errdefer allocator.destroy(then_branch_ptr);
            then_branch_ptr.* = then_branch;

            var else_branch_ptr: ?*hir.HIR.Expression = null;
            if (if_expr.else_branch) |else_branch| {
                var else_expr = try lowerExpression(allocator, else_branch.*);
                errdefer else_expr.deinit(allocator);

                else_branch_ptr = try allocator.create(hir.HIR.Expression);
                errdefer allocator.destroy(else_branch_ptr.?);
                else_branch_ptr.?.* = else_expr;
            }

            return hir.HIR.Expression{
                .if_expr = .{
                    .condition = condition_ptr,
                    .then_branch = then_branch_ptr,
                    .else_branch = else_branch_ptr,
                },
            };
        },
        .FuncCall => |func_call| {
            // REMOVED: Logic to detect binary operators disguised as function calls.
            // The parser should generate BinaryOp nodes directly for infix expressions.

            // Regular function call logic
            const func = try lowerExpression(allocator, func_call.func.*);
            // errdefer func.deinit(allocator); // Let caller manage deinit

            const func_ptr = try allocator.create(hir.HIR.Expression);
            errdefer allocator.destroy(func_ptr); // If copy fails
            func_ptr.* = func; // Copy the lowered func expression

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
                errdefer arg_expr.deinit(allocator); // If alloc/append fails

                const arg_ptr = try allocator.create(hir.HIR.Expression);
                errdefer allocator.destroy(arg_ptr); // If copy fails
                arg_ptr.* = arg_expr; // Copy the lowered arg expression

                try args.append(arg_ptr);
            }

            return hir.HIR.Expression{
                .func_call = .{
                    .func = func_ptr,
                    .args = args,
                },
            };
        },
        // Handle binary operations directly
        .BinaryOp => |bin_op| {
            const op_type: hir.HIR.BinaryOpType = op_switch: switch (bin_op.op) { // Added label op_switch:
                .Ident => |ident| {
                    if (std.mem.eql(u8, ident, "+")) {
                        break :op_switch .add; // Break to the label
                    } else if (std.mem.eql(u8, ident, "-")) {
                        break :op_switch .sub;
                    } else if (std.mem.eql(u8, ident, "<")) {
                        break :op_switch .lt;
                    } else if (std.mem.eql(u8, ident, ">")) {
                        break :op_switch .gt;
                    } else {
                        // TODO: Add support for other binary operators like *, /, = etc.
                        std.debug.print("Unsupported binary operator '{s}' during HIR lowering.\n", .{ident});
                        return error.UnsupportedOperator;
                    }
                },
                // Add cases for other potential operator token kinds if needed
                else => {
                    std.debug.print("Unexpected operator type in BinaryOp during HIR lowering.\n", .{});
                    return error.UnexpectedAstNode;
                },
            };

            // Convert left and right operands
            var left = try lowerExpression(allocator, bin_op.left.*);
            // Deinit left if right lowering or subsequent steps fail
            errdefer left.deinit(allocator);

            var right = try lowerExpression(allocator, bin_op.right.*);
            // Deinit right if subsequent steps fail (left errdefer still active)
            errdefer right.deinit(allocator);

            const left_ptr = try allocator.create(hir.HIR.Expression);
            // If left_ptr alloc fails, errdefers for left/right run
            errdefer allocator.destroy(left_ptr); // Destroy ptr if right_ptr alloc fails

            const right_ptr = try allocator.create(hir.HIR.Expression);
            // If right_ptr alloc fails, errdefers for left/right run, and left_ptr errdefer runs
            errdefer allocator.destroy(right_ptr); // Destroy ptr if return fails

            // Copy the lowered expressions into the pointers
            // Ownership of the *content* is effectively transferred here.
            left_ptr.* = left;
            right_ptr.* = right;

            // The original left/right vars are now just containers whose contents
            // have been moved. Their errdefers are cancelled by reaching this point.
            // The ownership of the actual HIR data is now with left_ptr/right_ptr.

            // Ownership of left_ptr and right_ptr is transferred to the returned node
            return hir.HIR.Expression{
                .binary_op = .{
                    .op = op_type,
                    .left = left_ptr,
                    .right = right_ptr,
                },
            };
        },
        .FuncDef => |func_def| {
            // Convert function body
            var body = try lowerExpression(allocator, func_def.body.*);
            errdefer body.deinit(allocator);

            const body_ptr = try allocator.create(hir.HIR.Expression);
            errdefer allocator.destroy(body_ptr);
            body_ptr.* = body;

            // Convert parameters
            var params = try allocator.alloc(hir.HIR.FuncParam, func_def.params.len);
            errdefer {
                for (params) |*param| {
                    param.deinit(allocator);
                }
                allocator.free(params);
            }

            for (func_def.params, 0..) |param, i| {
                params[i] = .{
                    .name = try allocator.dupe(u8, param.name),
                    .param_type = if (param.param_type) |pt| try allocator.dupe(u8, pt) else null,
                };
            }

            return hir.HIR.Expression{
                .func_def = .{
                    .name = try allocator.dupe(u8, func_def.name),
                    .params = params,
                    .body = body_ptr,
                },
            };
        },
        .VarDecl => |var_decl| {
            var value = try lowerExpression(allocator, var_decl.value.*);
            errdefer value.deinit(allocator);

            const value_ptr = try allocator.create(hir.HIR.Expression);
            errdefer allocator.destroy(value_ptr);
            value_ptr.* = value;

            return hir.HIR.Expression{
                .var_decl = .{
                    .name = try allocator.dupe(u8, var_decl.name),
                    .value = value_ptr,
                },
            };
        },
    };
}
