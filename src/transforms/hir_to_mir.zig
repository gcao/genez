const std = @import("std");
const hir = @import("../ir/hir.zig");
const mir = @import("../ir/mir.zig");
const types = @import("../core/types.zig");

pub fn convert(allocator: std.mem.Allocator, hir_prog: hir.HIR) !mir.MIR {
    var mir_prog = mir.MIR.init(allocator);
    errdefer mir_prog.deinit();

    // Convert each HIR function to MIR function
    for (hir_prog.functions.items) |func| {
        const mir_func = try convertFunction(allocator, func);
        try mir_prog.functions.append(mir_func);
    }

    return mir_prog;
}

fn convertFunction(allocator: std.mem.Allocator, func: hir.HIR.Function) !mir.MIR.Function {
    var mir_func = mir.MIR.Function.init(allocator);
    // Free the default name before overwriting it to avoid memory leak
    allocator.free(mir_func.name);
    mir_func.name = try allocator.dupe(u8, func.name);

    // Set parameter count and names
    mir_func.param_count = func.params.len;
    for (func.params) |param| {
        const param_name = try allocator.dupe(u8, param.name);
        try mir_func.param_names.append(param_name);
    }

    // Create entry block
    var entry_block = mir.MIR.Block.init(allocator);

    // Create context for the function body with parameter names
    const func_context = ConversionContext{ .param_names = mir_func.param_names.items };

    // Convert HIR statements to MIR instructions
    for (func.body.items) |stmt| {
        try convertStatementWithContext(&entry_block, stmt, func_context);
    }

    mir_func.blocks.append(entry_block) catch unreachable;
    return mir_func;
}

fn convertStatement(block: *mir.MIR.Block, stmt: hir.HIR.Statement) !void {
    switch (stmt) {
        .Expression => |expr| {
            // REMOVED: Special handling for print statement. Treat it like a regular expression.
            _ = try convertExpression(block, expr); // Evaluate the expression for its side effects or result
        },
    }
}

fn convertStatementWithContext(block: *mir.MIR.Block, stmt: hir.HIR.Statement, context: ConversionContext) !void {
    switch (stmt) {
        .Expression => |expr| {
            try convertExpressionWithContext(block, expr, context);
        },
    }
}

fn isReturnInstruction(instruction: mir.MIR.Instruction) bool {
    return switch (instruction) {
        .Return => true,
        else => false,
    };
}

// Context for converting expressions within functions
const ConversionContext = struct {
    param_names: ?[][]const u8,

    fn isParameter(self: ConversionContext, name: []const u8) ?usize {
        if (self.param_names) |params| {
            for (params, 0..) |param_name, i| {
                if (std.mem.eql(u8, name, param_name)) {
                    return i;
                }
            }
        }
        return null;
    }
};

fn convertExpression(block: *mir.MIR.Block, expr: hir.HIR.Expression) !void {
    const empty_context = ConversionContext{ .param_names = null };
    try convertExpressionWithContext(block, expr, empty_context);
}

fn hirLiteralToValue(allocator: std.mem.Allocator, hir_expr: hir.HIR.Expression) !types.Value {
    return switch (hir_expr) {
        .literal => |lit| switch (lit) {
            .int => |val| types.Value{ .Int = val },
            .string => |val| types.Value{ .String = try allocator.dupe(u8, val) },
            .bool => |val| types.Value{ .Bool = val },
            .float => |val| types.Value{ .Float = val },
            .nil => types.Value{ .Nil = {} },
            .symbol => |val| types.Value{ .Symbol = try allocator.dupe(u8, val) },
            .array => |_| {
                // Nested arrays within a LoadArray instruction are not directly supported.
                // This would require a more complex representation or separate instructions.
                std.debug.print("Nested arrays in hirLiteralToValue are not supported for direct MIR LoadArray conversion.\n", .{});
                return error.UnsupportedExpression;
            },
            .map => |_| {
                // Maps within a LoadArray instruction are not directly supported.
                std.debug.print("Maps in hirLiteralToValue are not supported for direct MIR LoadArray conversion.\n", .{});
                return error.UnsupportedExpression;
            },
        },
        else => {
            // If it's not a literal, it cannot be part of a MIR LoadArray/LoadMap directly.
            // This indicates an issue, as LoadArray/LoadMap expect constant values.
            // A prior compilation stage should have handled complex expressions or
            // a different MIR instruction sequence should be generated.
            std.debug.print("Unexpected non-literal expression type ('{s}') encountered in hirLiteralToValue. LoadArray/LoadMap expect constant values.\n", .{@tagName(hir_expr)});
            @panic("Cannot convert non-literal HIR expression to constant MIR Value for LoadArray/LoadMap.");
        },
    };
}

fn convertExpressionWithContext(block: *mir.MIR.Block, expr: hir.HIR.Expression, context: ConversionContext) anyerror!void {
    switch (expr) {
        .literal => |lit| switch (lit) {
            .int => |val| {
                try block.instructions.append(.{ .LoadInt = val });
            },
            .string => |val| try block.instructions.append(.{ .LoadString = try block.allocator.dupe(u8, val) }),
            .bool => |val| try block.instructions.append(.{ .LoadBool = val }),
            .float => |val| try block.instructions.append(.{ .LoadFloat = val }),
            .nil => try block.instructions.append(.LoadNil),
            .symbol => |val| try block.instructions.append(.{ .LoadSymbol = try block.allocator.dupe(u8, val) }),
            .array => |hir_array_elements| { // hir_array_elements is []*hir.HIR.Expression
                var mir_value_array = try block.allocator.alloc(types.Value, hir_array_elements.len);
                errdefer block.allocator.free(mir_value_array);

                for (hir_array_elements, 0..) |hir_expr_ptr, i| {
                    // Each element must be convertible to a constant types.Value
                    // hirLiteralToValue will panic if hir_expr_ptr.* is not a literal
                    mir_value_array[i] = try hirLiteralToValue(block.allocator, hir_expr_ptr.*);
                    // Ensure deinit for values created by hirLiteralToValue if subsequent conversion fails
                    errdefer mir_value_array[i].deinit(block.allocator);
                }
                try block.instructions.append(.{ .LoadArray = mir_value_array });
            },
            .map => |hir_map| { // hir_map is std.StringHashMap(*hir.HIR.Expression)
                var mir_value_map = std.StringHashMap(types.Value).init(block.allocator);
                errdefer { // Deinit map and its contents if an error occurs
                    var it = mir_value_map.iterator();
                    while (it.next()) |entry| {
                        block.allocator.free(entry.key_ptr.*); // key is []const u8, dupe'd
                        entry.value_ptr.deinit(block.allocator); // value is types.Value
                    }
                    mir_value_map.deinit();
                }

                var hir_map_iter = hir_map.iterator();
                while (hir_map_iter.next()) |hir_entry| {
                    const key_str = try block.allocator.dupe(u8, hir_entry.key_ptr.*);
                    errdefer block.allocator.free(key_str);

                    // Each value must be convertible to a constant types.Value
                    // hirLiteralToValue will panic if hir_entry.value_ptr.*.* is not a literal
                    var mir_value = try hirLiteralToValue(block.allocator, hir_entry.value_ptr.*.*); // Dereference the pointer
                    errdefer (&mir_value).deinit(block.allocator); // Pass pointer to local mir_value

                    try mir_value_map.put(key_str, mir_value);
                }
                try block.instructions.append(.{ .LoadMap = mir_value_map });
            },
        },
        .binary_op => |bin_op| {
            // First convert and load the left operand
            try convertExpressionWithContext(block, bin_op.left.*, context);

            // Then convert and load the right operand
            try convertExpressionWithContext(block, bin_op.right.*, context);

            // Finally add the operation
            switch (bin_op.op) {
                .add => try block.instructions.append(.Add),
                .sub => try block.instructions.append(.Sub),
                .lt => {
                    try block.instructions.append(.LessThan);
                },
                .gt => try block.instructions.append(.GreaterThan), // Added GreaterThan
                .eq => try block.instructions.append(.Equal), // Added Equal
                .mul => try block.instructions.append(.Mul),
                .div => try block.instructions.append(.Div),
                // TODO: Add other MIR binary instructions
            }
        },
        .variable => |var_expr| {
            // Check if this is a parameter reference
            if (context.isParameter(var_expr.name)) |param_index| {
                // Load parameter by index
                try block.instructions.append(.{ .LoadParameter = param_index });
            } else {
                // Load regular variable
                const name_copy = try block.allocator.dupe(u8, var_expr.name);
                try block.instructions.append(.{ .LoadVariable = name_copy });
            }
        },
        .if_expr => |if_expr| {
            // First, evaluate the condition
            try convertExpressionWithContext(block, if_expr.condition.*, context);

            // Create a JumpIfFalse instruction - we'll set the target later
            const jump_if_false_index = block.instructions.items.len;
            try block.instructions.append(.{ .JumpIfFalse = 0 });

            // Generate code for the then branch
            try convertExpressionWithContext(block, if_expr.then_branch.*, context);

            // If there's an else branch, we need to jump over it after the then branch
            const jump_over_else_index = if (if_expr.else_branch != null) block.instructions.items.len else 0;
            if (if_expr.else_branch != null) {
                try block.instructions.append(.{ .Jump = 0 });
            }

            // Now we know where the else branch starts, so we can set the JumpIfFalse target
            const else_branch_index = block.instructions.items.len;
            block.instructions.items[jump_if_false_index].JumpIfFalse = else_branch_index;

            // Generate code for the else branch if it exists
            if (if_expr.else_branch) |else_branch| {
                try convertExpressionWithContext(block, else_branch.*, context);
            }

            // Now we know where the code after the if statement starts, so we can set the Jump target
            const after_if_index = block.instructions.items.len;
            if (if_expr.else_branch != null) {
                block.instructions.items[jump_over_else_index].Jump = after_if_index;
            }
        },
        .func_call => |func_call| {
            // First, evaluate the function
            try convertExpressionWithContext(block, func_call.func.*, context);

            // Then evaluate all the arguments in order
            for (func_call.args.items) |arg| {
                try convertExpressionWithContext(block, arg.*, context);
            }

            // Finally, call the function with the number of arguments
            try block.instructions.append(.{ .Call = func_call.args.items.len });
        },
        .func_def => |func_def| {
            // Create a new function
            var func = mir.MIR.Function.init(block.allocator);
            errdefer func.deinit();

            // Set the function name - now safe to free default since it's allocated
            block.allocator.free(func.name);
            func.name = try block.allocator.dupe(u8, func_def.name);

            // Set the parameter count and names
            func.param_count = func_def.params.len;
            for (func_def.params) |param| {
                const param_name = try block.allocator.dupe(u8, param.name);
                try func.param_names.append(param_name);
            }

            // Create a block for the function body
            var body_block = mir.MIR.Block.init(block.allocator);
            errdefer body_block.deinit();

            // Create context for the function body with parameter names
            const func_context = ConversionContext{ .param_names = func.param_names.items };

            // Convert the function body with parameter context
            try convertExpressionWithContext(&body_block, func_def.body.*, func_context);

            // Add a return instruction if not present
            var needs_return = true;
            if (body_block.instructions.items.len > 0) {
                const last_instr = body_block.instructions.items[body_block.instructions.items.len - 1];
                if (isReturnInstruction(last_instr)) {
                    needs_return = false;
                }
            }
            if (needs_return) {
                try body_block.instructions.append(.Return);
            }

            // Add the block to the function
            try func.blocks.append(body_block);

            // Create a function object with the converted code
            const func_obj = try block.allocator.create(mir.MIR.Function);
            errdefer block.allocator.destroy(func_obj);
            func_obj.* = func;

            // Load the function as a constant
            try block.instructions.append(.{ .LoadFunction = func_obj });

            // Store it in a variable with its name
            const name_copy = try block.allocator.dupe(u8, func_def.name);
            try block.instructions.append(.{ .StoreVariable = name_copy });
        },
        .function => |hir_func_ptr| {
            // When a function is encountered as an expression, it needs to be converted to MIR
            // and then loaded as a function object.
            var mir_func = convertFunction(block.allocator, hir_func_ptr.*) catch |err| {
                std.debug.print("Error converting function: {}\n", .{err});
                return err;
            };
            errdefer mir_func.deinit();

            // Create a function object with the converted MIR code
            const mir_func_obj = try block.allocator.create(mir.MIR.Function);
            errdefer block.allocator.destroy(mir_func_obj);
            mir_func_obj.* = mir_func;

            // Load the MIR function object as a constant
            try block.instructions.append(.{ .LoadFunction = mir_func_obj });

            // Store the function with its name so it can be called later
            const name_copy = try block.allocator.dupe(u8, hir_func_ptr.*.name);
            try block.instructions.append(.{ .StoreVariable = name_copy });
        },
        .var_decl => |var_decl| {
            // Evaluate the variable's value
            try convertExpressionWithContext(block, var_decl.value.*, context);

            // Store it in a variable
            const name_copy = try block.allocator.dupe(u8, var_decl.name);
            try block.instructions.append(.{ .StoreVariable = name_copy });
        },
        .array_literal => |array_lit| {
            // Convert each element in the array
            var mir_elements = std.ArrayList(types.Value).init(block.allocator);
            errdefer {
                for (mir_elements.items) |*val| {
                    val.deinit(block.allocator);
                }
                mir_elements.deinit();
            }

            for (array_lit.elements) |elem_ptr| {
                const elem = elem_ptr.*;
                switch (elem) {
                    .literal => |lit| {
                        const value = switch (lit) {
                            .int => |val| types.Value{ .Int = val },
                            .float => |val| types.Value{ .Float = val },
                            .bool => |val| types.Value{ .Bool = val },
                            .string => |val| types.Value{ .String = try block.allocator.dupe(u8, val) },
                            .nil => types.Value.Nil,
                            .symbol => |val| types.Value{ .Symbol = try block.allocator.dupe(u8, val) },
                            .array => |_| types.Value.Nil, // Nested arrays not yet supported
                            .map => |_| types.Value.Nil, // Nested maps not yet supported
                        };
                        try mir_elements.append(value);
                    },
                    else => {
                        // For complex expressions, we would need to evaluate them first
                        // For now, we'll use a placeholder
                        try mir_elements.append(types.Value.Nil);
                    },
                }
            }

            const elements_slice = try mir_elements.toOwnedSlice();
            try block.instructions.append(.{ .LoadArray = elements_slice });
        },
        .map_literal => |map_lit| {
            // Convert each entry in the map
            var mir_map = std.StringHashMap(types.Value).init(block.allocator);
            errdefer {
                var it = mir_map.iterator();
                while (it.next()) |entry| {
                    block.allocator.free(entry.key_ptr.*);
                    entry.value_ptr.deinit(block.allocator);
                }
                mir_map.deinit();
            }

            for (map_lit.entries) |entry| {
                // Convert key (must be a symbol with ^ prefix in Gene)
                const key_str = switch (entry.key.*) {
                    .literal => |lit| switch (lit) {
                        .symbol => |sym| try block.allocator.dupe(u8, sym),
                        .string => |str| try block.allocator.dupe(u8, str),
                        .int => |val| try std.fmt.allocPrint(block.allocator, "{}", .{val}),
                        .float => |val| try std.fmt.allocPrint(block.allocator, "{}", .{val}),
                        .bool => |val| try std.fmt.allocPrint(block.allocator, "{}", .{val}),
                        .nil => try block.allocator.dupe(u8, "nil"),
                        .array => |_| try block.allocator.dupe(u8, "array_key"),
                        .map => |_| try block.allocator.dupe(u8, "map_key"),
                    },
                    .variable => |var_expr| try block.allocator.dupe(u8, var_expr.name),
                    else => try block.allocator.dupe(u8, "unknown_key"),
                };

                // Convert value
                const value = switch (entry.value.*) {
                    .literal => |lit| switch (lit) {
                        .int => |val| types.Value{ .Int = val },
                        .float => |val| types.Value{ .Float = val },
                        .bool => |val| types.Value{ .Bool = val },
                        .string => |val| types.Value{ .String = try block.allocator.dupe(u8, val) },
                        .nil => types.Value.Nil,
                        .symbol => |val| types.Value{ .Symbol = try block.allocator.dupe(u8, val) },
                        .array => |_| types.Value.Nil, // Nested arrays not yet supported
                        .map => |_| types.Value.Nil, // Nested maps not yet supported
                    },
                    else => types.Value.Nil,
                };

                try mir_map.put(key_str, value);
            }

            try block.instructions.append(.{ .LoadMap = mir_map });
        },
        .do_block => |do_block| {
            // Process each statement in the do block
            for (do_block.statements) |stmt| {
                try convertExpressionWithContext(block, stmt.*, context);
            }
        },
    }
}
