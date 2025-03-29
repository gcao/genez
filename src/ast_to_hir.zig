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
            .ReturnAddress => |_| .{ .literal = .{ .nil = {} } }, // Fallback for ReturnAddress
            .Function => |_| .{ .literal = .{ .nil = {} } }, // Fallback for Function
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
            // Check if it's a potential binary operator call like (+ 1 2)
            if (func_call.func.* == .Variable) {
                const op_name = func_call.func.*.Variable.name;
                var op_type: ?hir.HIR.BinaryOpType = null;

                if (std.mem.eql(u8, op_name, "+")) {
                    op_type = .add;
                } else if (std.mem.eql(u8, op_name, "-")) {
                    op_type = .sub;
                } else if (std.mem.eql(u8, op_name, "<")) {
                    op_type = .lt;
                } else if (std.mem.eql(u8, op_name, ">")) {
                    op_type = .gt;
                } else if (std.mem.eql(u8, op_name, "=")) {
                    // Distinguish between assignment (= in var) and comparison (= in expr)?
                    // Assuming comparison for now if used as a function call.
                    // TODO: Revisit if '=' should be allowed as a binary comparison operator call.
                    // For now, let it fall through to regular func call.
                    // op_type = .eq;
                }
                // Add other operators like *, / here

                if (op_type) |op| {
                    // It's a binary operator call
                    if (func_call.args.items.len != 2) {
                        std.debug.print("Binary operator '{s}' called with {} arguments, expected 2\n", .{ op_name, func_call.args.items.len });
                        return error.InvalidOperatorArity;
                    }

                    var left = try lowerExpression(allocator, func_call.args.items[0].*);
                    errdefer left.deinit(allocator);

                    var right = try lowerExpression(allocator, func_call.args.items[1].*);
                    errdefer right.deinit(allocator);

                    const left_ptr = try allocator.create(hir.HIR.Expression);
                    errdefer allocator.destroy(left_ptr);
                    left_ptr.* = left;

                    const right_ptr = try allocator.create(hir.HIR.Expression);
                    errdefer {
                        left_ptr.deinit(allocator);
                        allocator.destroy(left_ptr);
                        allocator.destroy(right_ptr);
                    }
                    right_ptr.* = right;

                    // Deallocate the original function variable name ('+') as it's not needed
                    allocator.free(op_name);

                    return hir.HIR.Expression{
                        .binary_op = .{
                            .op = op,
                            .left = left_ptr,
                            .right = right_ptr,
                        },
                    };
                }
                // If not a recognized binary operator, fall through to regular function call
            }

            // Regular function call logic
            var func = try lowerExpression(allocator, func_call.func.*);
            errdefer func.deinit(allocator);

            const func_ptr = try allocator.create(hir.HIR.Expression);
            errdefer allocator.destroy(func_ptr);
            func_ptr.* = func;

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

            return hir.HIR.Expression{
                .func_call = .{
                    .func = func_ptr,
                    .args = args,
                },
            };
        },
        // .BinaryOp case is removed as operators are now parsed as FuncCall
        // However, the switch needs to be exhaustive. Add a case that errors.
        .BinaryOp => |_| {
            std.debug.print("Unexpected BinaryOp encountered during HIR lowering.\n", .{});
            return error.UnexpectedAstNode; // Or panic
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
