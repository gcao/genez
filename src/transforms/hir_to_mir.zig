const std = @import("std");
const hir = @import("../ir/hir.zig");
const mir = @import("../ir/mir.zig");
const types = @import("../core/types.zig");

pub fn convert(allocator: std.mem.Allocator, hir_prog: hir.HIR) !mir.MIR {
    var mir_prog = mir.MIR.init(allocator);
    errdefer mir_prog.deinit();

    // Convert each HIR function to MIR function
    for (hir_prog.functions.items, 0..) |func, i| {
        var mir_func = try convertFunction(allocator, func);

        // If this is the main function and we have imports with selective items,
        // add instructions to create bindings for those items
        if (i == hir_prog.functions.items.len - 1 and std.mem.eql(u8, func.name, "main")) {
            // Process imports at the beginning of the main function
            if (hir_prog.imports.items.len > 0 and mir_func.blocks.items.len > 0) {
                var new_instructions = std.ArrayList(mir.MIR.Instruction).init(allocator);
                defer new_instructions.deinit();

                // Add import binding instructions
                for (hir_prog.imports.items) |import| {
                    // Get the module name for storage (use alias if provided, otherwise extract from path)
                    const module_name = import.alias orelse blk: {
                        var path = import.module_path;
                        if (std.mem.startsWith(u8, path, "./")) {
                            path = path[2..];
                        }
                        if (std.mem.endsWith(u8, path, ".gene")) {
                            path = path[0 .. path.len - 5];
                        }
                        break :blk path;
                    };
                    
                    // Load the module using the same ID that was registered
                    try new_instructions.append(.{ .LoadModule = try allocator.dupe(u8, module_name) });
                    
                    // Store the module in a variable
                    try new_instructions.append(.{ .StoreVariable = try allocator.dupe(u8, module_name) });
                    
                    // If there are selective imports, create bindings for them
                    if (import.items) |items| {
                        // Create bindings for each imported item
                        for (items) |item| {
                            // Load the module
                            try new_instructions.append(.{ .LoadVariable = try allocator.dupe(u8, module_name) });

                            // Load the member name
                            try new_instructions.append(.{ .LoadString = try allocator.dupe(u8, item.name) });

                            // Access the member (module/member)
                            try new_instructions.append(.{ .CallMethod = .{
                                .method_name = try allocator.dupe(u8, "get_member"),
                                .arg_count = 1,
                            } });

                            // Store it in a variable
                            const var_name = item.alias orelse item.name;
                            try new_instructions.append(.{ .StoreVariable = try allocator.dupe(u8, var_name) });
                        }
                    }
                }

                // Prepend the new instructions to the main function's first block
                var first_block = &mir_func.blocks.items[0];
                const original_instructions = try first_block.instructions.toOwnedSlice();
                defer allocator.free(original_instructions);

                // Add all new instructions first
                for (new_instructions.items) |instr| {
                    try first_block.instructions.append(instr);
                }

                // Then add the original instructions
                for (original_instructions) |instr| {
                    try first_block.instructions.append(instr);
                }
            }
        }

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
    var func_context = ConversionContext{ .param_names = mir_func.param_names.items, .allocator = allocator, .next_temp_id = 0 };

    // Convert HIR statements to MIR instructions
    for (func.body.items) |stmt| {
        try convertStatementWithContext(&entry_block, stmt, &func_context);
    }

    // Add a return instruction if not present to ensure valid jump targets
    var needs_return = true;
    if (entry_block.instructions.items.len > 0) {
        const last_instr = entry_block.instructions.items[entry_block.instructions.items.len - 1];
        if (isReturnInstruction(last_instr)) {
            needs_return = false;
        }
    }
    if (needs_return) {
        try entry_block.instructions.append(.Return);
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

fn convertStatementWithContext(block: *mir.MIR.Block, stmt: hir.HIR.Statement, context: *ConversionContext) !void {
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
    allocator: std.mem.Allocator,
    next_temp_id: u32,

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
    var empty_context = ConversionContext{ .param_names = null, .allocator = block.allocator, .next_temp_id = 0 };
    try convertExpressionWithContext(block, expr, &empty_context);
}


fn convertExpressionWithContext(block: *mir.MIR.Block, expr: hir.HIR.Expression, context: *ConversionContext) anyerror!void {
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
            .array => |hir_array_elements| { 
                // Convert array literals using CreateArray
                for (hir_array_elements) |hir_expr_ptr| {
                    try convertExpressionWithContext(block, hir_expr_ptr.*, context);
                }
                try block.instructions.append(.{ .CreateArray = hir_array_elements.len });
            },
            .map => |hir_map| { 
                // Convert map literals using CreateMap
                var hir_map_iter = hir_map.iterator();
                while (hir_map_iter.next()) |hir_entry| {
                    // Push key
                    const key_str = hir_entry.key_ptr.*;
                    try block.instructions.append(.{ .LoadString = try block.allocator.dupe(u8, key_str) });
                    
                    // Push value
                    try convertExpressionWithContext(block, hir_entry.value_ptr.*.*, context);
                }
                try block.instructions.append(.{ .CreateMap = hir_map.count() });
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
            // The then branch will leave its result on the stack
            try convertExpressionWithContext(block, if_expr.then_branch.*, context);

            // If there's an else branch, we need to jump over it after the then branch
            var jump_over_else_index: usize = 0;
            if (if_expr.else_branch != null) {
                jump_over_else_index = block.instructions.items.len;
                try block.instructions.append(.{ .Jump = 0 });
            }

            // Now we know where the else branch starts, so we can set the JumpIfFalse target
            const else_branch_index = block.instructions.items.len;
            block.instructions.items[jump_if_false_index].JumpIfFalse = else_branch_index;

            // Generate code for the else branch if it exists
            if (if_expr.else_branch) |else_branch| {
                // The else branch will leave its result on the stack
                try convertExpressionWithContext(block, else_branch.*, context);
            } else {
                // If there's no else branch, push nil as the result
                try block.instructions.append(.LoadNil);
            }

            // Now we know where the code after the if statement starts, so we can set the Jump target
            // The after_if_index should point to the next instruction that would be added,
            // which is the current length of the instruction array
            if (if_expr.else_branch != null) {
                const after_if_index = block.instructions.items.len;
                block.instructions.items[jump_over_else_index].Jump = after_if_index;
            }

            // At this point, either branch will have left a value on the stack
            // This value is the result of the if expression
        },
        .func_call => |func_call| {
            // Check if the function is a builtin operator like + or - so we can
            // emit a direct instruction instead of a generic call. This avoids
            // the argument order issues that currently cause runtime errors.
            if (func_call.func.* == .variable) {
                const name = func_call.func.*.variable.name;
                if (func_call.args.items.len == 2) {
                    if (std.mem.eql(u8, name, "+")) {
                        try convertExpressionWithContext(block, func_call.args.items[0].*, context);
                        try convertExpressionWithContext(block, func_call.args.items[1].*, context);
                        try block.instructions.append(.Add);
                        return;
                    } else if (std.mem.eql(u8, name, "-")) {
                        try convertExpressionWithContext(block, func_call.args.items[0].*, context);
                        try convertExpressionWithContext(block, func_call.args.items[1].*, context);
                        try block.instructions.append(.Sub);
                        return;
                    } else if (std.mem.eql(u8, name, "<")) {
                        try convertExpressionWithContext(block, func_call.args.items[0].*, context);
                        try convertExpressionWithContext(block, func_call.args.items[1].*, context);
                        try block.instructions.append(.LessThan);
                        return;
                    }
                }
            }

            // Fallback to generic function call
            try convertExpressionWithContext(block, func_call.func.*, context);

            for (func_call.args.items) |arg| {
                try convertExpressionWithContext(block, arg.*, context);
            }

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
            var func_context = ConversionContext{ .param_names = func.param_names.items, .allocator = block.allocator, .next_temp_id = 0 };

            // Convert the function body with parameter context
            try convertExpressionWithContext(&body_block, func_def.body.*, &func_context);

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

            // Don't store anonymous functions - they'll be stored by var declarations
            // Only store if it's not an anonymous function (doesn't start with "anon_")
            if (!std.mem.startsWith(u8, func_def.name, "anon_")) {
                // Store it in a variable with its name
                const name_copy = try block.allocator.dupe(u8, func_def.name);
                try block.instructions.append(.{ .StoreVariable = name_copy });
            }
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

            // Don't store anonymous functions - they'll be stored by var declarations
            // Only store if it's not an anonymous function (doesn't start with "anon_")
            if (!std.mem.startsWith(u8, hir_func_ptr.*.name, "anon_")) {
                // Store the function with its name so it can be called later
                const name_copy = try block.allocator.dupe(u8, hir_func_ptr.*.name);
                try block.instructions.append(.{ .StoreVariable = name_copy });
            }
        },
        .var_decl => |var_decl| {
            // Evaluate the variable's value
            try convertExpressionWithContext(block, var_decl.value.*, context);

            // Store it in a variable
            const name_copy = try block.allocator.dupe(u8, var_decl.name);
            try block.instructions.append(.{ .StoreVariable = name_copy });
        },
        .array_literal => |array_lit| {
            // Always use CreateArray for consistency
            // For each element, evaluate it and push to stack
            for (array_lit.elements) |elem_ptr| {
                try convertExpressionWithContext(block, elem_ptr.*, context);
            }

            // Create array from stack elements
            try block.instructions.append(.{ .CreateArray = array_lit.elements.len });
        },
        .map_literal => |map_lit| {
            // Always use CreateMap for consistency
            // For each entry, push key then value to stack
            for (map_lit.entries) |entry| {
                // Push key (convert to string)
                const key_str = switch (entry.key.*) {
                    .literal => |lit| switch (lit) {
                        .symbol => |sym| sym,
                        .string => |str| str,
                        else => "unknown_key",
                    },
                    else => "unknown_key",
                };
                try block.instructions.append(.{ .LoadString = try block.allocator.dupe(u8, key_str) });

                // Push value (evaluate expression)
                try convertExpressionWithContext(block, entry.value.*, context);
            }

            // Create map from stack elements (key-value pairs)
            try block.instructions.append(.{ .CreateMap = map_lit.entries.len });
        },
        .do_block => |do_block| {
            // Process each statement in the do block
            for (do_block.statements) |stmt| {
                try convertExpressionWithContext(block, stmt.*, context);
            }
        },
        .class_def => |class_def| {
            // Create a MIR class definition
            var mir_class_def = mir.MIR.ClassDefinition{
                .name = try block.allocator.dupe(u8, class_def.name),
                .parent_name = if (class_def.parent_class) |parent|
                    try block.allocator.dupe(u8, parent)
                else
                    null,
                .fields = try block.allocator.alloc([]const u8, class_def.fields.len),
                .methods = std.StringHashMap(*mir.MIR.Function).init(block.allocator),
            };

            // Copy field names
            for (class_def.fields, 0..) |field, i| {
                mir_class_def.fields[i] = try block.allocator.dupe(u8, field.name);
            }

            // Convert methods to MIR functions
            for (class_def.methods) |method| {
                // Create a MIR function for the method
                var method_func = mir.MIR.Function.init(block.allocator);
                method_func.name = try block.allocator.dupe(u8, method.name);
                method_func.param_count = method.params.len + 1; // +1 for 'self' parameter

                // Add 'self' as first parameter
                try method_func.param_names.append(try block.allocator.dupe(u8, "self"));

                // Add method parameters
                for (method.params) |param| {
                    try method_func.param_names.append(try block.allocator.dupe(u8, param.name));
                }

                // Create a block for the method body
                var method_block = mir.MIR.Block.init(block.allocator);

                // Create context with method parameters
                var method_context = ConversionContext{ .param_names = method_func.param_names.items, .allocator = block.allocator, .next_temp_id = 0 };

                // Convert method body
                try convertExpressionWithContext(&method_block, method.body.*, &method_context);

                // Add return if needed
                var needs_return = true;
                if (method_block.instructions.items.len > 0) {
                    const last_instr = method_block.instructions.items[method_block.instructions.items.len - 1];
                    if (isReturnInstruction(last_instr)) {
                        needs_return = false;
                    }
                }
                if (needs_return) {
                    try method_block.instructions.append(.Return);
                }

                // Add block to method function
                try method_func.blocks.append(method_block);

                // Store method function
                const method_func_ptr = try block.allocator.create(mir.MIR.Function);
                method_func_ptr.* = method_func;
                try mir_class_def.methods.put(method.name, method_func_ptr);
            }

            // Emit the DefineClass instruction
            try block.instructions.append(.{ .DefineClass = mir_class_def });

            // Store the class in a variable with its name
            try block.instructions.append(.{ .StoreVariable = try block.allocator.dupe(u8, class_def.name) });
        },
        .instance_creation => |inst_creation| {
            // Load the class (it should be stored as a variable)
            try block.instructions.append(.{ .LoadVariable = try block.allocator.dupe(u8, inst_creation.class_name) });

            // Evaluate constructor arguments
            for (inst_creation.args.items) |arg| {
                try convertExpressionWithContext(block, arg.*, context);
            }

            // Create the instance (constructor will be called automatically)
            try block.instructions.append(.{ .CreateInstance = .{
                .class_name = try block.allocator.dupe(u8, inst_creation.class_name),
                .arg_count = inst_creation.args.items.len,
            } });
        },
        .method_call => |method_call| {
            // Evaluate the instance
            try convertExpressionWithContext(block, method_call.object.*, context);

            // Evaluate method arguments
            for (method_call.args.items) |arg| {
                try convertExpressionWithContext(block, arg.*, context);
            }

            // Call the method
            try block.instructions.append(.{ .CallMethod = .{
                .method_name = try block.allocator.dupe(u8, method_call.method_name),
                .arg_count = method_call.args.items.len,
            } });
        },
        .field_access => |field_access| {
            // Field access x/a compiles to (x .get_member 'a')
            if (field_access.object) |obj| {
                // Evaluate the object
                try convertExpressionWithContext(block, obj.*, context);
            } else {
                // No object means implicit self - check if it's a parameter
                if (context.isParameter("self")) |param_index| {
                    try block.instructions.append(.{ .LoadParameter = param_index });
                } else {
                    // Fall back to loading as variable
                    try block.instructions.append(.{ .LoadVariable = try block.allocator.dupe(u8, "self") });
                }
            }

            // Load the field name as a string
            try block.instructions.append(.{ .LoadString = try block.allocator.dupe(u8, field_access.field_name) });

            // Call get_member method with 1 argument
            try block.instructions.append(.{ .CallMethod = .{
                .method_name = try block.allocator.dupe(u8, "get_member"),
                .arg_count = 1,
            } });
        },
        .path_assignment => |path_assign| {
            // Path assignment needs special handling
            // We need to put object, key, and value on the stack for Set instruction

            // Check if the path is a PathAccess (the common case)
            if (path_assign.path.* == .method_call and
                std.mem.eql(u8, path_assign.path.method_call.method_name, "get_member"))
            {
                // This is a PathAccess compiled as get_member method call
                // We need to extract the object and key
                const method_call = path_assign.path.method_call;

                // Evaluate the object
                try convertExpressionWithContext(block, method_call.object.*, context);

                // Evaluate the key (first argument to get_member)
                if (method_call.args.items.len > 0) {
                    try convertExpressionWithContext(block, method_call.args.items[0].*, context);
                } else {
                    return error.InvalidPathAccess;
                }

                // Evaluate the value to be assigned
                try convertExpressionWithContext(block, path_assign.value.*, context);

                // Emit the Set instruction
                try block.instructions.append(.Set);
            } else {
                // For other path types, we'd need different handling
                // For now, this is an error
                return error.UnsupportedPathAssignment;
            }
        },
        .match_expr => |match_expr| {
            // Pattern matching compilation to MIR
            // This is a simplified implementation - full pattern matching would require
            // more sophisticated compilation with decision trees and backtracking

            // Evaluate the scrutinee (value being matched)
            try convertExpressionWithContext(block, match_expr.scrutinee.*, context);

            // Store scrutinee in a temporary variable for pattern matching
            try block.instructions.append(.{ .StoreVariable = try block.allocator.dupe(u8, "__match_scrutinee") });

            // For now, implement a simple linear matching approach
            // In a full implementation, this would be optimized with decision trees

            var branch_ends = std.ArrayList(usize).init(block.allocator);
            defer branch_ends.deinit();

            var arm_jump_patches = std.ArrayList(usize).init(block.allocator);
            defer arm_jump_patches.deinit();

            for (match_expr.arms, 0..) |arm, arm_index| {
                // Patch previous arm's failure jump to here
                if (arm_index > 0 and arm_jump_patches.items.len > 0) {
                    const last_jump_index = arm_jump_patches.items[arm_jump_patches.items.len - 1];
                    block.instructions.items[last_jump_index].JumpIfFalse = block.instructions.items.len;
                }

                // Load the scrutinee for this arm's pattern matching
                try block.instructions.append(.{ .LoadVariable = try block.allocator.dupe(u8, "__match_scrutinee") });

                // Generate pattern matching logic for this arm
                const pattern_jump_index = try compilePattern(block, arm.pattern, context);

                // If we have a conditional jump from pattern matching, we need to handle it
                if (pattern_jump_index != 0) {
                    // Pattern might fail - add to patches for next arm
                    try arm_jump_patches.append(pattern_jump_index);
                }

                // If pattern matched (or always matches like variable/wildcard), evaluate guard if present
                if (arm.guard) |guard| {
                    try convertExpressionWithContext(block, guard.*, context);
                    // Jump to next arm if guard fails
                    const guard_fail_jump = block.instructions.items.len;
                    try block.instructions.append(.{ .JumpIfFalse = 0 }); // Will be patched
                    try arm_jump_patches.append(guard_fail_jump);
                }

                // Pattern (and guard if present) succeeded, evaluate body
                try convertExpressionWithContext(block, arm.body.*, context);

                // Jump to end of match expression
                const branch_end_jump = block.instructions.items.len;
                try block.instructions.append(.{ .Jump = 0 }); // Will be patched
                try branch_ends.append(branch_end_jump);
            }

            // Patch any remaining failure jumps to the default case
            for (arm_jump_patches.items) |jump_index| {
                if (block.instructions.items[jump_index].JumpIfFalse == 0) {
                    block.instructions.items[jump_index].JumpIfFalse = block.instructions.items.len;
                }
            }

            // If no pattern matched, this would be a runtime error
            // For now, just push nil
            try block.instructions.append(.LoadNil);

            // Patch all branch end jumps to point here
            const match_end = block.instructions.items.len;
            for (branch_ends.items) |jump_index| {
                block.instructions.items[jump_index].Jump = match_end;
            }
        },
        .macro_def => {
            // Macro definitions are not evaluated at runtime in the traditional sense
            // They need to be stored in the environment for later expansion
            // For now, we'll skip them in MIR as they need special handling
            // TODO: Implement proper macro storage and expansion
            try block.instructions.append(.LoadNil);
        },
        .macro_call => |macro_call| {
            // Macro calls need special handling for lazy evaluation
            // For now, we'll evaluate them as regular function calls
            // TODO: Implement proper lazy evaluation for macro arguments

            // Evaluate the macro expression
            try convertExpressionWithContext(block, macro_call.macro.*, context);

            // Evaluate arguments (should be lazy, but for now evaluate eagerly)
            for (macro_call.args) |arg| {
                try convertExpressionWithContext(block, arg.*, context);
            }

            // Call with argument count
            try block.instructions.append(.{ .Call = @intCast(macro_call.args.len) });
        },
        .for_loop => |for_ptr| {
            // Implement a simple for-in loop for arrays
            // Desugar to:
            // var _arr = iterable
            // var _i = 0
            // while (_i < _arr.length) {
            //   var iterator = _arr.at(_i)
            //   body
            //   _i = _i + 1
            // }

            // Generate unique temp variable names
            const array_var = try std.fmt.allocPrint(context.allocator, "_for_arr_{}", .{context.next_temp_id});
            const index_var = try std.fmt.allocPrint(context.allocator, "_for_idx_{}", .{context.next_temp_id});
            context.next_temp_id += 1;

            // Store the iterable in a temp variable
            try convertExpressionWithContext(block, for_ptr.iterable.*, context);
            try block.instructions.append(.{ .StoreVariable = array_var });

            // Initialize index to 0
            try block.instructions.append(.{ .LoadInt = 0 });
            try block.instructions.append(.{ .StoreVariable = index_var });

            // Start of loop - record position for jump back
            // IMPORTANT: This is where we'll jump back to
            const loop_start = block.instructions.items.len;

            // Check condition: index < array.length
            try block.instructions.append(.{ .LoadVariable = index_var });
            try block.instructions.append(.{ .LoadVariable = array_var });
            try block.instructions.append(.Length); // Get array length
            try block.instructions.append(.LessThan);

            // Jump to end if false
            const jump_if_false_index = block.instructions.items.len;
            try block.instructions.append(.{ .JumpIfFalse = 0 }); // Will patch later

            // Get current element: array[index]
            try block.instructions.append(.{ .LoadVariable = array_var });
            try block.instructions.append(.{ .LoadVariable = index_var });
            try block.instructions.append(.ArrayGet); // array[index]

            // Store in iterator variable
            try block.instructions.append(.{ .StoreVariable = try block.allocator.dupe(u8, for_ptr.iterator) });

            // Execute loop body
            try convertExpressionWithContext(block, for_ptr.body.*, context);

            // Increment index: index = index + 1
            try block.instructions.append(.{ .LoadVariable = index_var });
            try block.instructions.append(.{ .LoadInt = 1 });
            try block.instructions.append(.Add);
            try block.instructions.append(.{ .StoreVariable = index_var });

            // Jump back to start
            try block.instructions.append(.{ .Jump = loop_start });

            // Patch the conditional jump to here
            const end_pos = block.instructions.items.len;
            block.instructions.items[jump_if_false_index] = .{ .JumpIfFalse = end_pos };

            // Clean up temp variables by loading nil
            try block.instructions.append(.LoadNil);
        },
        .return_expr => |ret_ptr| {
            // Handle return statement
            if (ret_ptr.value) |val| {
                // Evaluate the return value
                try convertExpressionWithContext(block, val.*, context);
            } else {
                // Push nil for bare return
                try block.instructions.append(.LoadNil);
            }
            // Add return instruction
            try block.instructions.append(.Return);
        },
        .import_stmt => |import_ptr| {
            // Get the module name for storage (use alias if provided, otherwise extract from path)
            const module_name = import_ptr.alias orelse blk: {
                var path = import_ptr.module_path;
                if (std.mem.startsWith(u8, path, "./")) {
                    path = path[2..];
                }
                if (std.mem.endsWith(u8, path, ".gene")) {
                    path = path[0 .. path.len - 5];
                }
                break :blk path;
            };
            
            // Load the module using the same ID that was registered
            try block.instructions.append(.{ .LoadModule = try block.allocator.dupe(u8, module_name) });
            
            // Store the module in a variable
            try block.instructions.append(.{ .StoreVariable = try block.allocator.dupe(u8, module_name) });
            
            // Import statements create runtime bindings for selective imports
            if (import_ptr.items) |items| {
                // For each imported item, create a binding
                for (items) |item| {
                    // Load the module
                    try block.instructions.append(.{ .LoadVariable = try block.allocator.dupe(u8, module_name) });

                    // Load the member name
                    try block.instructions.append(.{ .LoadString = try block.allocator.dupe(u8, item.name) });

                    // Access the member (module/member)
                    try block.instructions.append(.{ .CallMethod = .{
                        .method_name = try block.allocator.dupe(u8, "get_member"),
                        .arg_count = 1,
                    } });

                    // Store it in a variable (use alias if provided, otherwise use the original name)
                    const var_name = item.alias orelse item.name;
                    try block.instructions.append(.{ .StoreVariable = try block.allocator.dupe(u8, var_name) });
                }
            }

            // Push nil as the result of the import expression
            try block.instructions.append(.LoadNil);
        },
        .module_access => |mod_access| {
            // Module access: load module, then get member
            // 1. Load the module
            try block.instructions.append(.{ .LoadVariable = try block.allocator.dupe(u8, mod_access.module) });
            
            // 2. Load the member name as a string
            try block.instructions.append(.{ .LoadString = try block.allocator.dupe(u8, mod_access.member) });
            
            // 3. Call get_member method
            try block.instructions.append(.{ .CallMethod = .{
                .method_name = try block.allocator.dupe(u8, "get_member"),
                .arg_count = 1,
            } });
        },
        .namespace_decl => |ns_ptr| {
            // Create a namespace at runtime
            // 1. Create namespace
            try block.instructions.append(.{ .LoadString = try block.allocator.dupe(u8, ns_ptr.name) });
            try block.instructions.append(.CreateNamespace);
            
            // 2. Make it the current namespace for body evaluation
            try block.instructions.append(.PushNamespace);
            
            // 3. Evaluate the namespace body (defines members in the namespace)
            try convertExpressionWithContext(block, ns_ptr.body.*, context);
            
            // 4. Pop namespace and store it in a variable with the namespace name
            try block.instructions.append(.PopNamespace);
            try block.instructions.append(.{ .StoreVariable = try block.allocator.dupe(u8, ns_ptr.name) });
        },
    }
}

// Helper function to compile HIR patterns to MIR instructions
// Returns the instruction index where pattern matching succeeds
fn compilePattern(block: *mir.MIR.Block, pattern: hir.HIR.Pattern, context: *ConversionContext) !usize {
    switch (pattern) {
        .literal => |lit| {
            // Generate code to compare scrutinee with literal value
            try convertExpressionWithContext(block, lit.value.*, context);
            try block.instructions.append(.Equal);

            // Jump to next pattern if not equal
            const jump_to_next = block.instructions.items.len;
            try block.instructions.append(.{ .JumpIfFalse = 0 }); // Will be patched by caller

            return jump_to_next;
        },
        .variable => |var_pat| {
            // Variable patterns always match and bind the value
            // Store the scrutinee in the variable
            const var_name_copy = try block.allocator.dupe(u8, var_pat.name);
            try block.instructions.append(.{ .StoreVariable = var_name_copy });

            // Variables always match, so no conditional jump needed
            return 0;
        },
        .wildcard => {
            // Wildcard patterns always match and don't bind anything
            // The scrutinee is already on the stack, we just need to pop it
            // since we don't use it for anything
            // In MIR, we don't have a Pop instruction, so we'll leave it on stack
            // The code generation or VM should handle stack cleanup
            return 0;
        },
        .constructor => |ctor| {
            // Constructor pattern matching would involve:
            // 1. Check if scrutinee is of the right type/constructor
            // 2. Extract fields and match sub-patterns
            // For now, just push a placeholder
            _ = ctor;
            try block.instructions.append(.LoadNil);
            return 0;
        },
        .array => |arr| {
            // Array pattern matching implementation
            // The scrutinee (array being matched) is already on the stack

            // For empty array pattern [], check length == 0
            if (arr.elements.len == 0) {
                // Get length of scrutinee (it's on stack)
                try block.instructions.append(.Duplicate); // Keep scrutinee for later
                try block.instructions.append(.Length);
                // Load 0 for comparison
                try block.instructions.append(.{ .LoadInt = 0 });
                try block.instructions.append(.Equal);
                // Jump if not equal (pattern fails)
                const jump_index = block.instructions.items.len;
                try block.instructions.append(.{ .JumpIfFalse = 0 });
                // Pop the scrutinee on success
                try block.instructions.append(.Pop);
                return jump_index;
            }

            // For non-empty patterns, check array length first
            try block.instructions.append(.Duplicate); // Keep scrutinee on stack
            try block.instructions.append(.Length);
            try block.instructions.append(.{ .LoadInt = @intCast(arr.elements.len) });
            try block.instructions.append(.Equal);

            const length_check_jump = block.instructions.items.len;
            try block.instructions.append(.{ .JumpIfFalse = 0 });

            // Length matches, now extract and bind each element
            // We need to load the scrutinee again for each element extraction
            for (arr.elements, 0..) |elem_pattern, i| {
                // Load the scrutinee array again
                try block.instructions.append(.{ .LoadVariable = try block.allocator.dupe(u8, "__match_scrutinee") });

                // Load the index
                try block.instructions.append(.{ .LoadInt = @intCast(i) });

                // Get array element at index i
                try block.instructions.append(.ArrayGet);

                // Now handle the element pattern
                switch (elem_pattern) {
                    .variable => |var_pat| {
                        // Store the extracted value in the variable
                        const var_name_copy = try block.allocator.dupe(u8, var_pat.name);
                        try block.instructions.append(.{ .StoreVariable = var_name_copy });
                    },
                    .literal => |lit| {
                        // Compare with literal
                        try convertExpressionWithContext(block, lit.value.*, context);
                        try block.instructions.append(.Equal);
                        // For now, we don't handle nested pattern failures properly
                        // A full implementation would need to track all failure points
                    },
                    .wildcard => {
                        // Just discard the extracted value
                        try block.instructions.append(.Pop);
                    },
                    else => {
                        // For other patterns, just pop for now
                        try block.instructions.append(.Pop);
                    },
                }
            }

            // Pop the original scrutinee that we duplicated at the beginning
            try block.instructions.append(.Pop);

            return length_check_jump;
        },
        .map => |map| {
            // Map pattern matching - simplified version
            // For now, just succeed and bind variables

            _ = map;

            // Map patterns always succeed for now
            return 0;
        },
        .or_pattern => |or_pat| {
            // Or pattern matching would involve:
            // 1. Try each alternative pattern
            // 2. Succeed if any pattern matches
            _ = or_pat;
            try block.instructions.append(.LoadNil);
            return 0;
        },
        .range => |range| {
            // Range pattern matching would involve:
            // 1. Evaluate range bounds
            // 2. Check if scrutinee is within range
            _ = range;
            try block.instructions.append(.LoadNil);
            return 0;
        },
    }
}
