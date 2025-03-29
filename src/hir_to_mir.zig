const std = @import("std");
const hir = @import("hir.zig");
const mir = @import("mir.zig");
const types = @import("types.zig");

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
    mir_func.name = try allocator.dupe(u8, func.name);

    // Create entry block
    var entry_block = mir.MIR.Block.init(allocator);

    // Convert HIR statements to MIR instructions
    for (func.body.items) |stmt| {
        try convertStatement(&entry_block, stmt);
    }

    mir_func.blocks.append(entry_block) catch unreachable;
    return mir_func;
}

fn convertStatement(block: *mir.MIR.Block, stmt: hir.HIR.Statement) !void {
    switch (stmt) {
        .Expression => |expr| {
            if (expr == .variable and std.mem.eql(u8, expr.variable.name, "print")) {
                // This is a print statement - first convert the argument to print
                // The argument should be the next expression in the HIR
                try block.instructions.append(.Print);
            } else {
                try convertExpression(block, expr);
            }
        },
    }
}

fn isReturnInstruction(instruction: mir.MIR.Instruction) bool {
    return switch (instruction) {
        .Return => true,
        else => false,
    };
}

fn convertExpression(block: *mir.MIR.Block, expr: hir.HIR.Expression) !void {
    switch (expr) {
        .literal => |lit| switch (lit) {
            .int => |val| try block.instructions.append(.{ .LoadInt = val }),
            .string => |val| try block.instructions.append(.{ .LoadString = try block.allocator.dupe(u8, val) }),
            .bool => |val| try block.instructions.append(.{ .LoadBool = val }),
            .float => |val| try block.instructions.append(.{ .LoadFloat = val }),
            .nil => try block.instructions.append(.LoadNil),
            .symbol => |val| try block.instructions.append(.{ .LoadSymbol = try block.allocator.dupe(u8, val) }),
            .array => |val| {
                var new_array = try block.allocator.alloc(types.Value, val.len);
                for (val, 0..) |item, i| {
                    new_array[i] = item;
                }
                try block.instructions.append(.{ .LoadArray = new_array });
            },
            .map => |val| {
                var new_map = std.StringHashMap(types.Value).init(block.allocator);
                var it = val.iterator();
                while (it.next()) |entry| {
                    try new_map.put(try block.allocator.dupe(u8, entry.key_ptr.*), entry.value_ptr.*);
                }
                try block.instructions.append(.{ .LoadMap = new_map });
            },
        },
        .binary_op => |bin_op| {
            // First convert and load the left operand
            try convertExpression(block, bin_op.left.*);

            // Then convert and load the right operand
            try convertExpression(block, bin_op.right.*);

            // Finally add the operation
            switch (bin_op.op) {
                .add => try block.instructions.append(.Add),
                .sub => try block.instructions.append(.Sub),
                .lt => try block.instructions.append(.LessThan),
                .gt => try block.instructions.append(.GreaterThan), // Added GreaterThan
                // TODO: Add other MIR binary instructions
            }
        },
        .variable => |var_expr| {
            if (std.mem.eql(u8, var_expr.name, "print")) {
                // Skip loading the print variable - it will be handled by the next expression
            } else {
                const name_copy = try block.allocator.dupe(u8, var_expr.name);
                errdefer block.allocator.free(name_copy);
                try block.instructions.append(.{ .LoadVariable = name_copy });
            }
        },
        .if_expr => |if_expr| {
            // First, evaluate the condition
            try convertExpression(block, if_expr.condition.*);

            // Create a JumpIfFalse instruction - we'll set the target later
            const jump_if_false_index = block.instructions.items.len;
            try block.instructions.append(.{ .JumpIfFalse = 0 });

            // Generate code for the then branch
            try convertExpression(block, if_expr.then_branch.*);

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
                try convertExpression(block, else_branch.*);
            }

            // Now we know where the code after the if statement starts, so we can set the Jump target
            const after_if_index = block.instructions.items.len;
            if (if_expr.else_branch != null) {
                block.instructions.items[jump_over_else_index].Jump = after_if_index;
            }
        },
        .func_call => |func_call| {
            // First, evaluate the function
            try convertExpression(block, func_call.func.*);

            // Then evaluate all the arguments in order
            for (func_call.args.items) |arg| {
                try convertExpression(block, arg.*);
            }

            // Finally, call the function with the number of arguments
            try block.instructions.append(.{ .Call = func_call.args.items.len });
        },
        .func_def => |func_def| {
            // Create a new function
            var func = mir.MIR.Function.init(block.allocator);

            // Set the function name
            block.allocator.free(func.name); // Free the default name "main"
            func.name = try block.allocator.dupe(u8, func_def.name);

            // Create a block for the function body
            var body_block = mir.MIR.Block.init(block.allocator);

            // Convert the function body
            try convertExpression(&body_block, func_def.body.*);

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
            func_obj.* = func;

            // Load the function as a constant
            try block.instructions.append(.{ .LoadFunction = func_obj });

            // Store it in a variable with its name
            const name_copy = try block.allocator.dupe(u8, func_def.name);
            try block.instructions.append(.{ .StoreVariable = name_copy });
        },
        .var_decl => |var_decl| {
            // Evaluate the variable's value
            try convertExpression(block, var_decl.value.*);

            // Store it in a variable
            const name_copy = try block.allocator.dupe(u8, var_decl.name);
            try block.instructions.append(.{ .StoreVariable = name_copy });
        },
    }
}
