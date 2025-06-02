const std = @import("std");
const ast = @import("../frontend/ast.zig");
const hir = @import("../ir/hir.zig");

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
            // Removed .Array and .Map cases as they are now handled by top-level ArrayLiteral/MapLiteral
            .ReturnAddress => |_| .{ .literal = .{ .nil = {} } }, // Fallback for ReturnAddress
            .Function => |_| .{ .literal = .{ .nil = {} } }, // Fallback for Function
            .Variable => |_| .{ .literal = .{ .nil = {} } }, // Fallback for Variable
            .BuiltinOperator => |_| .{ .literal = .{ .nil = {} } }, // Fallback for BuiltinOperator
            .Array => |_| unreachable, // Should be handled by ast.Expression.ArrayLiteral
            .Map => |_| unreachable, // Should be handled by ast.Expression.MapLiteral
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

            // else_branch is no longer optional
            var else_expr = try lowerExpression(allocator, if_expr.else_branch.*);
            errdefer else_expr.deinit(allocator);

            const else_branch_ptr = try allocator.create(hir.HIR.Expression);
            errdefer allocator.destroy(else_branch_ptr);
            else_branch_ptr.* = else_expr;

            return hir.HIR.Expression{
                .if_expr = .{
                    .condition = condition_ptr,
                    .then_branch = then_branch_ptr,
                    .else_branch = else_branch_ptr,
                },
            };
        },
        .FuncCall => |func_call| {
            std.debug.print("ast_to_hir: FuncCall.func_expr kind: {any}\n", .{func_call.func.*});
            if (func_call.func.* == .Variable) {
                std.debug.print("ast_to_hir: FuncCall.func_expr.Variable.name: '{s}', len: {}\n", .{
                    func_call.func.*.Variable.name,
                    func_call.func.*.Variable.name.len,
                });
            }
            std.debug.print("ast_to_hir: FuncCall.args.len: {}\n", .{func_call.args.items.len});

            // Process as a regular function call.
            const func = try lowerExpression(allocator, func_call.func.*);

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
        // Handle binary operations as function calls
        .BinaryOp => |bin_op| {
            // In Gene, binary operations are function calls
            // Convert the operator to a variable expression
            const op_name = switch (bin_op.op) {
                .Ident => |ident| ident,
                else => {
                    std.debug.print("Unexpected operator type in BinaryOp during HIR lowering.\n", .{});
                    return error.UnexpectedAstNode;
                },
            };

            // Create a variable expression for the operator
            const func_expr = hir.HIR.Expression{
                .variable = .{ .name = try allocator.dupe(u8, op_name) },
            };

            const func_ptr = try allocator.create(hir.HIR.Expression);
            errdefer allocator.destroy(func_ptr);
            func_ptr.* = func_expr;

            // Create the arguments list
            var args = std.ArrayList(*hir.HIR.Expression).init(allocator);
            errdefer {
                for (args.items) |arg| {
                    arg.deinit(allocator);
                    allocator.destroy(arg);
                }
                args.deinit();
            }

            // Convert left operand
            var left = try lowerExpression(allocator, bin_op.left.*);
            errdefer left.deinit(allocator);

            const left_ptr = try allocator.create(hir.HIR.Expression);
            errdefer allocator.destroy(left_ptr);
            left_ptr.* = left;

            try args.append(left_ptr);

            // Convert right operand
            var right = try lowerExpression(allocator, bin_op.right.*);
            errdefer right.deinit(allocator);

            const right_ptr = try allocator.create(hir.HIR.Expression);
            errdefer allocator.destroy(right_ptr);
            right_ptr.* = right;

            try args.append(right_ptr);

            // Return as a function call
            return hir.HIR.Expression{
                .func_call = .{
                    .func = func_ptr,
                    .args = args,
                },
            };
        },
        .FuncDef => |func_def| {
            var hir_func = hir.HIR.Function.init(allocator);
            errdefer hir_func.deinit();

            hir_func.name = try allocator.dupe(u8, func_def.name);

            // Convert parameters
            std.debug.print("[AST->HIR] FuncDef: name={s}, params.len={}\n", .{ func_def.name, func_def.params.len });
            hir_func.params = try allocator.alloc(hir.HIR.FuncParam, func_def.params.len);
            for (func_def.params, 0..) |param, i| {
                hir_func.params[i] = .{
                    .name = try allocator.dupe(u8, param.name),
                    .param_type = if (param.param_type) |pt| try allocator.dupe(u8, pt) else null,
                };
            }

            // Convert function body statements
            // The body of a FuncDef is a single expression, which needs to be wrapped in a statement
            var body_expr = try lowerExpression(allocator, func_def.body.*);
            errdefer body_expr.deinit(allocator); // Clean up if subsequent steps fail

            try hir_func.body.append(.{ .Expression = body_expr });
            // After appending, body_expr's content has been moved.
            // We need to cancel its errdefer by zeroing it out.
            body_expr = undefined; // Cancel errdefer

            // Transfer ownership of hir_func to the HIR.Expression.function
            const hir_func_ptr = try allocator.create(hir.HIR.Function);
            errdefer allocator.destroy(hir_func_ptr);
            hir_func_ptr.* = hir_func;

            return hir.HIR.Expression{ .function = hir_func_ptr };
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
        .SimpleFuncDef => |func_def| {
            var hir_func = hir.HIR.Function.init(allocator);
            errdefer hir_func.deinit();

            hir_func.name = try allocator.dupe(u8, func_def.getName());

            // Create parameters based on param_count, but without names/types
            hir_func.params = try allocator.alloc(hir.HIR.FuncParam, func_def.param_count);
            for (0..func_def.param_count) |i| {
                // For SimpleFuncDef, parameter names are not explicitly given in AST,
                // so we can use a placeholder or generate one if needed later.
                // For now, just allocate the struct.
                hir_func.params[i] = .{
                    .name = try allocator.dupe(u8, "arg"), // Placeholder name
                    .param_type = null,
                };
            }

            // Convert body_literal to a HIR literal expression
            var body_expr = hir.HIR.Expression{
                .literal = .{
                    .int = func_def.body_literal,
                },
            };
            errdefer body_expr.deinit(allocator); // Clean up if subsequent steps fail

            try hir_func.body.append(.{ .Expression = body_expr });
            // After appending, body_expr's content has been moved.
            // We need to cancel its errdefer by zeroing it out.
            body_expr = undefined; // Cancel errdefer

            // Transfer ownership of hir_func to the HIR.Expression.function
            const hir_func_ptr = try allocator.create(hir.HIR.Function);
            errdefer allocator.destroy(hir_func_ptr);
            hir_func_ptr.* = hir_func;

            return hir.HIR.Expression{ .function = hir_func_ptr };
        },
        .ArrayLiteral => |arr_lit| {
            var elements = std.ArrayList(*hir.HIR.Expression).init(allocator);
            errdefer {
                for (elements.items) |element| {
                    element.deinit(allocator);
                    allocator.destroy(element);
                }
                elements.deinit();
            }

            for (arr_lit.elements) |ast_element_ptr| {
                var hir_element = try lowerExpression(allocator, ast_element_ptr.*);
                errdefer hir_element.deinit(allocator);

                const hir_element_ptr = try allocator.create(hir.HIR.Expression);
                errdefer allocator.destroy(hir_element_ptr);
                hir_element_ptr.* = hir_element;

                try elements.append(hir_element_ptr);
            }

            const elements_slice = try elements.toOwnedSlice();

            return hir.HIR.Expression{
                .array_literal = .{
                    .elements = elements_slice,
                },
            };
        },
        .MapLiteral => |map_lit| {
            var hir_entries_list = std.ArrayList(hir.HIR.MapEntry).init(allocator);
            errdefer { // This errdefer handles cleanup if the function errors out after list initialization
                for (hir_entries_list.items) |*map_entry_item| {
                    // MapEntry.deinit handles deinit of its key and value Expressions and their pointers
                    map_entry_item.deinit(allocator);
                }
                hir_entries_list.deinit(); // Deinitializes the ArrayList struct itself
            }

            for (map_lit.entries) |ast_entry| { // ast_entry is ast.MapEntry = { key: *ast.Expression, value: *ast.Expression }
                // Lower Key
                var lowered_key_expr = try lowerExpression(allocator, ast_entry.key.*);
                errdefer lowered_key_expr.deinit(allocator); // Cleans up if subsequent allocations or append fail

                const hir_key_expr_ptr = try allocator.create(hir.HIR.Expression);
                errdefer allocator.destroy(hir_key_expr_ptr); // Cleans up if subsequent allocations or append fail
                hir_key_expr_ptr.* = lowered_key_expr; // Ownership of lowered_key_expr's content moves

                // Lower Value
                var lowered_value_expr = try lowerExpression(allocator, ast_entry.value.*);
                errdefer lowered_value_expr.deinit(allocator); // Cleans up if subsequent allocations or append fail

                const hir_value_expr_ptr = try allocator.create(hir.HIR.Expression);
                errdefer allocator.destroy(hir_value_expr_ptr); // Cleans up if append fails
                hir_value_expr_ptr.* = lowered_value_expr; // Ownership of lowered_value_expr's content moves

                // Append the new MapEntry. If this append fails, the errdefers for
                // hir_key_expr_ptr, lowered_key_expr, hir_value_expr_ptr, and lowered_value_expr will trigger.
                try hir_entries_list.append(.{
                    .key = hir_key_expr_ptr,
                    .value = hir_value_expr_ptr,
                });
                // If append succeeds, ownership of the pointers is now with the MapEntry in the list.
                // The local errdefers for these specific pointers (destroy) and expressions (deinit)
                // are "cancelled" for this iteration as we proceed. The outer list's errdefer
                // will handle them if a later operation (like toOwnedSlice or a subsequent iteration) fails.
            }

            // Convert ArrayList to a slice. Ownership of the items is transferred.
            // The hir_entries_list errdefer will correctly deinit an empty list if this succeeds.
            const entries_slice = try hir_entries_list.toOwnedSlice();

            return hir.HIR.Expression{
                .map_literal = .{
                    .entries = entries_slice,
                },
            };
        },
        .DoBlock => |do_block| {
            var statements = std.ArrayList(*hir.HIR.Expression).init(allocator);
            errdefer {
                for (statements.items) |stmt| {
                    stmt.deinit(allocator);
                    allocator.destroy(stmt);
                }
                statements.deinit();
            }

            for (do_block.statements) |ast_stmt_ptr| {
                var hir_stmt = try lowerExpression(allocator, ast_stmt_ptr.*);
                errdefer hir_stmt.deinit(allocator);

                const hir_stmt_ptr = try allocator.create(hir.HIR.Expression);
                errdefer allocator.destroy(hir_stmt_ptr);
                hir_stmt_ptr.* = hir_stmt;

                try statements.append(hir_stmt_ptr);
            }

            const statements_slice = try statements.toOwnedSlice();

            return hir.HIR.Expression{
                .do_block = .{
                    .statements = statements_slice,
                },
            };
        },
    };
}
