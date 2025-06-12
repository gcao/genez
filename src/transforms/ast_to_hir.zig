const std = @import("std");
const ast = @import("../frontend/ast.zig");
const hir = @import("../ir/hir.zig");

pub fn convert(allocator: std.mem.Allocator, nodes: []const ast.AstNode) !hir.HIR {
    var hir_prog = hir.HIR.init(allocator);
    errdefer hir_prog.deinit();

    // First pass: Extract all top-level function definitions as separate HIR functions
    var main_nodes = std.ArrayList(ast.AstNode).init(allocator);
    defer main_nodes.deinit();

    for (nodes) |node| {
        switch (node) {
            .Expression => |expr| {
                switch (expr) {
                    .FuncDef => |func_def| {
                        // Create separate HIR function
                        var hir_func = hir.HIR.Function.init(allocator);
                        errdefer hir_func.deinit();

                        hir_func.name = try allocator.dupe(u8, func_def.name);

                        // Convert parameters
                        hir_func.params = try allocator.alloc(hir.HIR.FuncParam, func_def.params.len);
                        for (func_def.params, 0..) |param, i| {
                            hir_func.params[i] = .{
                                .name = try allocator.dupe(u8, param.name),
                                .param_type = if (param.param_type) |pt| try allocator.dupe(u8, pt) else null,
                            };
                        }

                        // Convert function body
                        const body_expr = try lowerExpression(allocator, func_def.body.*);
                        try hir_func.body.append(.{ .Expression = body_expr });

                        // Add function to HIR program
                        try hir_prog.functions.append(hir_func);
                    },
                    .SimpleFuncDef => |func_def| {
                        // Create separate HIR function for SimpleFuncDef too
                        var hir_func = hir.HIR.Function.init(allocator);
                        errdefer hir_func.deinit();

                        hir_func.name = try allocator.dupe(u8, func_def.getName());

                        // Create parameters based on param_count, but without names/types
                        hir_func.params = try allocator.alloc(hir.HIR.FuncParam, func_def.param_count);
                        for (0..func_def.param_count) |i| {
                            hir_func.params[i] = .{
                                .name = try allocator.dupe(u8, "arg"), // Placeholder name
                                .param_type = null,
                            };
                        }

                        // Convert body_literal to a HIR literal expression
                        const body_expr = hir.HIR.Expression{
                            .literal = .{
                                .int = func_def.body_literal,
                            },
                        };

                        try hir_func.body.append(.{ .Expression = body_expr });

                        // Add function to HIR program
                        try hir_prog.functions.append(hir_func);
                    },
                    else => {
                        // Keep other expressions for main function
                        try main_nodes.append(node);
                    },
                }
            },
        }
    }

    // Create main function for remaining top-level code
    var main_func = hir.HIR.Function.init(allocator);
    main_func.name = try allocator.dupe(u8, "main");
    errdefer main_func.deinit();

    // Convert remaining nodes to HIR statements
    for (main_nodes.items) |node| {
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
            .Class => |_| .{ .literal = .{ .nil = {} } }, // Fallback for Class
            .Object => |_| .{ .literal = .{ .nil = {} } }, // Fallback for Object
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
        .ClassDef => |class_def| {
            // Convert AST class definition to HIR class definition
            var hir_class = try allocator.create(hir.HIR.ClassDef);
            errdefer allocator.destroy(hir_class);
            
            // Convert class name
            hir_class.name = try allocator.dupe(u8, class_def.name);
            hir_class.allocator = allocator;
            
            // Convert parent class if present
            hir_class.parent_class = if (class_def.parent_class) |parent|
                try allocator.dupe(u8, parent)
            else
                null;
            
            // Convert traits
            hir_class.traits = try allocator.alloc([]const u8, class_def.traits.len);
            for (class_def.traits, 0..) |trait_name, i| {
                hir_class.traits[i] = try allocator.dupe(u8, trait_name);
            }
            
            // Convert fields
            hir_class.fields = try allocator.alloc(hir.HIR.ClassField, class_def.fields.len);
            for (class_def.fields, 0..) |field, i| {
                hir_class.fields[i] = hir.HIR.ClassField{
                    .name = try allocator.dupe(u8, field.name),
                    .type_annotation = if (field.type_annotation) |type_ann|
                        try allocator.dupe(u8, type_ann)
                    else
                        null,
                    .is_public = field.is_public,
                    .default_value = if (field.default_value) |default_val| blk: {
                        var hir_default = try lowerExpression(allocator, default_val.*);
                        errdefer hir_default.deinit(allocator);
                        const default_ptr = try allocator.create(hir.HIR.Expression);
                        errdefer allocator.destroy(default_ptr);
                        default_ptr.* = hir_default;
                        break :blk default_ptr;
                    } else null,
                };
            }
            
            // Convert methods
            hir_class.methods = try allocator.alloc(hir.HIR.ClassMethod, class_def.methods.len);
            for (class_def.methods, 0..) |method, i| {
                // Convert method parameters
                var hir_params = try allocator.alloc(hir.HIR.FuncParam, method.params.len);
                for (method.params, 0..) |param, j| {
                    hir_params[j] = hir.HIR.FuncParam{
                        .name = try allocator.dupe(u8, param.name),
                        .param_type = if (param.type_annotation) |type_ann|
                            try allocator.dupe(u8, type_ann)
                        else
                            null,
                    };
                }
                
                // Convert method body
                var hir_body = try lowerExpression(allocator, method.body.*);
                errdefer hir_body.deinit(allocator);
                const body_ptr = try allocator.create(hir.HIR.Expression);
                errdefer allocator.destroy(body_ptr);
                body_ptr.* = hir_body;
                
                hir_class.methods[i] = hir.HIR.ClassMethod{
                    .name = try allocator.dupe(u8, method.name),
                    .params = hir_params,
                    .body = body_ptr,
                    .is_public = method.is_public,
                    .is_virtual = method.is_virtual,
                    .is_abstract = method.is_abstract,
                    .is_static = method.is_static,
                };
            }
            
            return hir.HIR.Expression{
                .class_def = hir_class
            };
        },
        .MatchExpr => |match_expr| {
            // Convert AST match expression to HIR match expression
            var hir_match = try allocator.create(hir.HIR.MatchExpr);
            errdefer allocator.destroy(hir_match);
            
            // Convert scrutinee (the value being matched)
            var hir_scrutinee = try lowerExpression(allocator, match_expr.scrutinee.*);
            errdefer hir_scrutinee.deinit(allocator);
            const scrutinee_ptr = try allocator.create(hir.HIR.Expression);
            errdefer allocator.destroy(scrutinee_ptr);
            scrutinee_ptr.* = hir_scrutinee;
            
            hir_match.scrutinee = scrutinee_ptr;
            
            // Convert match arms
            hir_match.arms = try allocator.alloc(hir.HIR.MatchArm, match_expr.arms.len);
            for (match_expr.arms, 0..) |arm, i| {
                // Convert pattern
                var hir_pattern = try convertPattern(allocator, arm.pattern);
                errdefer hir_pattern.deinit(allocator);
                
                // Convert guard if present
                var hir_guard: ?*hir.HIR.Expression = null;
                if (arm.guard) |guard| {
                    var guard_expr = try lowerExpression(allocator, guard.*);
                    errdefer guard_expr.deinit(allocator);
                    const guard_ptr = try allocator.create(hir.HIR.Expression);
                    errdefer allocator.destroy(guard_ptr);
                    guard_ptr.* = guard_expr;
                    hir_guard = guard_ptr;
                }
                
                // Convert body
                var hir_body = try lowerExpression(allocator, arm.body.*);
                errdefer hir_body.deinit(allocator);
                const body_ptr = try allocator.create(hir.HIR.Expression);
                errdefer allocator.destroy(body_ptr);
                body_ptr.* = hir_body;
                
                hir_match.arms[i] = hir.HIR.MatchArm{
                    .pattern = hir_pattern,
                    .guard = hir_guard,
                    .body = body_ptr,
                };
            }
            
            return hir.HIR.Expression{
                .match_expr = hir_match
            };
        },
        .ModuleDef => {
            // TODO: Implement proper HIR module support
            // For now, convert module definitions to nil literals as placeholder
            return hir.HIR.Expression{
                .literal = .{ .nil = {} }
            };
        },
        .ImportStmt => {
            // TODO: Implement proper HIR import support
            // For now, convert import statements to nil literals as placeholder
            return hir.HIR.Expression{
                .literal = .{ .nil = {} }
            };
        },
        .ExportStmt => {
            // TODO: Implement proper HIR export support
            // For now, convert export statements to nil literals as placeholder
            return hir.HIR.Expression{
                .literal = .{ .nil = {} }
            };
        },
        .InstanceCreation => |inst| {
            // Convert InstanceCreation to HIR
            var args = std.ArrayList(*hir.HIR.Expression).init(allocator);
            errdefer {
                for (args.items) |arg| {
                    arg.deinit(allocator);
                    allocator.destroy(arg);
                }
                args.deinit();
            }
            
            for (inst.args.items) |arg| {
                var hir_arg = try lowerExpression(allocator, arg.*);
                errdefer hir_arg.deinit(allocator);
                
                const hir_arg_ptr = try allocator.create(hir.HIR.Expression);
                errdefer allocator.destroy(hir_arg_ptr);
                hir_arg_ptr.* = hir_arg;
                
                try args.append(hir_arg_ptr);
            }
            
            return hir.HIR.Expression{
                .instance_creation = .{
                    .class_name = try allocator.dupe(u8, inst.class_name),
                    .args = args,
                }
            };
        },
        .FieldAccess => |field| {
            // Convert FieldAccess to HIR
            const hir_object = if (field.object) |obj| blk: {
                var hir_obj = try lowerExpression(allocator, obj.*);
                errdefer hir_obj.deinit(allocator);
                const obj_ptr = try allocator.create(hir.HIR.Expression);
                errdefer allocator.destroy(obj_ptr);
                obj_ptr.* = hir_obj;
                break :blk obj_ptr;
            } else null;
            
            return hir.HIR.Expression{
                .field_access = .{
                    .object = hir_object,
                    .field_name = try allocator.dupe(u8, field.field_name),
                }
            };
        },
        .FieldAssignment => |assign| {
            // Convert FieldAssignment to HIR
            const hir_object = if (assign.object) |obj| blk: {
                var hir_obj = try lowerExpression(allocator, obj.*);
                errdefer hir_obj.deinit(allocator);
                const obj_ptr = try allocator.create(hir.HIR.Expression);
                errdefer allocator.destroy(obj_ptr);
                obj_ptr.* = hir_obj;
                break :blk obj_ptr;
            } else null;
            
            var hir_value = try lowerExpression(allocator, assign.value.*);
            errdefer hir_value.deinit(allocator);
            const value_ptr = try allocator.create(hir.HIR.Expression);
            errdefer allocator.destroy(value_ptr);
            value_ptr.* = hir_value;
            
            return hir.HIR.Expression{
                .field_assignment = .{
                    .object = hir_object,
                    .field_name = try allocator.dupe(u8, assign.field_name),
                    .value = value_ptr,
                }
            };
        },
        .MethodCall => |call| {
            // Convert MethodCall to HIR
            var hir_obj = try lowerExpression(allocator, call.object.*);
            errdefer hir_obj.deinit(allocator);
            const obj_ptr = try allocator.create(hir.HIR.Expression);
            errdefer allocator.destroy(obj_ptr);
            obj_ptr.* = hir_obj;
            
            var args = std.ArrayList(*hir.HIR.Expression).init(allocator);
            errdefer {
                for (args.items) |arg| {
                    arg.deinit(allocator);
                    allocator.destroy(arg);
                }
                args.deinit();
            }
            
            for (call.args.items) |arg| {
                var hir_arg = try lowerExpression(allocator, arg.*);
                errdefer hir_arg.deinit(allocator);
                
                const hir_arg_ptr = try allocator.create(hir.HIR.Expression);
                errdefer allocator.destroy(hir_arg_ptr);
                hir_arg_ptr.* = hir_arg;
                
                try args.append(hir_arg_ptr);
            }
            
            return hir.HIR.Expression{
                .method_call = .{
                    .object = obj_ptr,
                    .method_name = try allocator.dupe(u8, call.method_name),
                    .args = args,
                }
            };
        },
    };
}

// Helper function to convert AST patterns to HIR patterns
fn convertPattern(allocator: std.mem.Allocator, pattern: ast.MatchExpr.Pattern) anyerror!hir.HIR.Pattern {
    return switch (pattern) {
        .Literal => |lit| blk: {
            var hir_value = try lowerExpression(allocator, lit.value.*);
            errdefer hir_value.deinit(allocator);
            const value_ptr = try allocator.create(hir.HIR.Expression);
            errdefer allocator.destroy(value_ptr);
            value_ptr.* = hir_value;
            
            break :blk hir.HIR.Pattern{
                .literal = .{ .value = value_ptr }
            };
        },
        .Variable => |var_pat| hir.HIR.Pattern{
            .variable = .{
                .name = try allocator.dupe(u8, var_pat.name),
                .type_annotation = if (var_pat.type_annotation) |type_ann|
                    try allocator.dupe(u8, type_ann)
                else
                    null,
            }
        },
        .Wildcard => hir.HIR.Pattern{ .wildcard = {} },
        .Constructor => |ctor| blk: {
            var hir_fields = try allocator.alloc(hir.HIR.Pattern, ctor.fields.len);
            errdefer allocator.free(hir_fields);
            
            for (ctor.fields, 0..) |field, i| {
                hir_fields[i] = try convertPattern(allocator, field);
                errdefer {
                    // Clean up already converted patterns if later ones fail
                    for (hir_fields[0..i]) |*prev_field| {
                        prev_field.deinit(allocator);
                    }
                }
            }
            
            break :blk hir.HIR.Pattern{
                .constructor = .{
                    .constructor = try allocator.dupe(u8, ctor.constructor),
                    .fields = hir_fields,
                }
            };
        },
        .Array => |arr| blk: {
            var hir_elements = try allocator.alloc(hir.HIR.Pattern, arr.elements.len);
            errdefer allocator.free(hir_elements);
            
            for (arr.elements, 0..) |element, i| {
                hir_elements[i] = try convertPattern(allocator, element);
                errdefer {
                    // Clean up already converted patterns if later ones fail
                    for (hir_elements[0..i]) |*prev_elem| {
                        prev_elem.deinit(allocator);
                    }
                }
            }
            
            break :blk hir.HIR.Pattern{
                .array = .{
                    .elements = hir_elements,
                    .rest = if (arr.rest) |rest|
                        try allocator.dupe(u8, rest)
                    else
                        null,
                }
            };
        },
        .Map => |map| blk: {
            var hir_fields = try allocator.alloc(hir.HIR.Pattern.MapPattern.MapFieldPattern, map.fields.len);
            errdefer allocator.free(hir_fields);
            
            for (map.fields, 0..) |field, i| {
                var hir_pattern = try convertPattern(allocator, field.pattern);
                errdefer hir_pattern.deinit(allocator);
                
                hir_fields[i] = hir.HIR.Pattern.MapPattern.MapFieldPattern{
                    .key = try allocator.dupe(u8, field.key),
                    .pattern = hir_pattern,
                };
                
                errdefer {
                    // Clean up already converted fields if later ones fail
                    for (hir_fields[0..i]) |*prev_field| {
                        prev_field.deinit(allocator);
                        prev_field.pattern.deinit(allocator);
                    }
                }
            }
            
            break :blk hir.HIR.Pattern{
                .map = .{
                    .fields = hir_fields,
                    .rest = if (map.rest) |rest|
                        try allocator.dupe(u8, rest)
                    else
                        null,
                }
            };
        },
        .Or => |or_pat| blk: {
            var hir_patterns = try allocator.alloc(hir.HIR.Pattern, or_pat.patterns.len);
            errdefer allocator.free(hir_patterns);
            
            for (or_pat.patterns, 0..) |sub_pattern, i| {
                hir_patterns[i] = try convertPattern(allocator, sub_pattern);
                errdefer {
                    // Clean up already converted patterns if later ones fail
                    for (hir_patterns[0..i]) |*prev_pattern| {
                        prev_pattern.deinit(allocator);
                    }
                }
            }
            
            break :blk hir.HIR.Pattern{
                .or_pattern = .{ .patterns = hir_patterns }
            };
        },
        .Range => |range| blk: {
            var hir_start = try lowerExpression(allocator, range.start.*);
            errdefer hir_start.deinit(allocator);
            const start_ptr = try allocator.create(hir.HIR.Expression);
            errdefer allocator.destroy(start_ptr);
            start_ptr.* = hir_start;
            
            var hir_end = try lowerExpression(allocator, range.end.*);
            errdefer hir_end.deinit(allocator);
            const end_ptr = try allocator.create(hir.HIR.Expression);
            errdefer allocator.destroy(end_ptr);
            end_ptr.* = hir_end;
            
            break :blk hir.HIR.Pattern{
                .range = .{
                    .start = start_ptr,
                    .end = end_ptr,
                    .inclusive = range.inclusive,
                }
            };
        },
    };
}
