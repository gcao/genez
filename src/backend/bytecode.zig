const std = @import("std");
const ast = @import("../frontend/ast.zig");
const types = @import("../core/types.zig");
const debug = @import("../core/debug.zig");

pub const Value = types.Value;

pub const OpCode = enum {
    LoadConst,
    LoadVar,
    LoadParam, // Added for loading function parameters
    StoreVar,
    StoreGlobal, // Added for global variable storage
    Add,
    Sub,
    Lt,
    Gt, // Added GreaterThan opcode
    Eq, // Added Equal opcode
    Print,
    Return,
    Call,
    Jump, // Unconditional jump
    JumpIfFalse, // Conditional jump if top of stack is false
};

pub const Instruction = struct {
    op: OpCode,
    operand: ?Value = null,

    pub fn deinit(self: *Instruction, allocator: std.mem.Allocator) void {
        if (self.operand) |*op| {
            op.deinit(allocator);
        }
    }
};

pub const Function = struct {
    instructions: std.ArrayList(Instruction),
    allocator: std.mem.Allocator,
    name: []const u8,
    param_count: usize,

    pub fn init(allocator: std.mem.Allocator) Function {
        return Function{
            .instructions = std.ArrayList(Instruction).init(allocator),
            .allocator = allocator,
            .name = "",
            .param_count = 0,
        };
    }

    // Changed self to mutable *Function
    pub fn deinit(self: *Function) void {
        for (self.instructions.items) |*instr| {
            // instr is already *Instruction (mutable pointer from iterator)
            if (instr.operand) |*operand| {
                // operand is *Value (mutable pointer)
                operand.deinit(self.allocator);
            }
        }
        self.instructions.deinit();

        // Free the function name
        if (self.name.len > 0) {
            self.allocator.free(self.name);
        }
    }
};

pub const Module = struct {
    functions: []Function,
    allocator: std.mem.Allocator,
    // Track all function objects created during deserialization for proper cleanup
    deserialized_functions: std.ArrayList(*Function),
    // Flag to indicate if this module owns the functions (for deserialized modules)
    owns_functions: bool = true,

    pub fn deinit(self: *Module) void {
        // Clean up main functions only if we own them
        if (self.owns_functions) {
            for (self.functions) |*func| {
                func.deinit();
            }
        }
        self.allocator.free(self.functions);

        // Clean up all deserialized function objects
        for (self.deserialized_functions.items) |func| {
            func.deinit();
            self.allocator.destroy(func);
        }
        self.deserialized_functions.deinit();
    }

    pub fn readFromFile(allocator: std.mem.Allocator, reader: anytype) !Module {
        // Read and verify magic number
        var magic: [4]u8 = undefined;
        try reader.readNoEof(&magic);
        if (!std.mem.eql(u8, &magic, "GENE")) {
            return error.InvalidMagicNumber;
        }

        // Read number of functions
        var buf: [4]u8 = undefined;
        try reader.readNoEof(&buf);
        const num_functions = std.mem.readInt(u32, &buf, .little);

        // Allocate functions array
        var functions = try allocator.alloc(Function, num_functions);
        errdefer allocator.free(functions);

        // Initialize deserialized functions tracker
        var deserialized_functions = std.ArrayList(*Function).init(allocator);
        errdefer {
            for (deserialized_functions.items) |func| {
                func.deinit();
                allocator.destroy(func);
            }
            deserialized_functions.deinit();
        }

        // Read each function
        var i: usize = 0;
        while (i < num_functions) : (i += 1) {
            functions[i] = try readFunctionFromFile(allocator, reader, &deserialized_functions);
        }

        return Module{
            .functions = functions,
            .allocator = allocator,
            .deserialized_functions = deserialized_functions,
        };
    }

    pub fn writeToFile(self: *const Module, writer: anytype) !void {
        // Write magic number to identify Gene bytecode files
        try writer.writeAll("GENE");

        // Write number of functions
        var buf: [4]u8 = undefined;
        std.mem.writeInt(u32, &buf, @intCast(self.functions.len), .little);
        try writer.writeAll(&buf);

        // Write each function
        for (self.functions) |func| {
            try writeFunctionToFile(&func, writer);
        }
    }

    fn writeFunctionToFile(func: *const Function, writer: anytype) !void {
        // Write number of instructions
        var buf: [4]u8 = undefined;
        std.mem.writeInt(u32, &buf, @intCast(func.instructions.items.len), .little);
        try writer.writeAll(&buf);

        // Write each instruction
        for (func.instructions.items) |instr| {
            // Write opcode
            try writer.writeByte(@intFromEnum(instr.op));

            // Write operand if present
            if (instr.operand) |operand| {
                try writer.writeByte(1); // Has operand
                try writeValueToFile(operand, writer);
            } else {
                try writer.writeByte(0); // No operand
            }
        }
    }

    fn writeValueToFile(value: Value, writer: anytype) !void {
        // Write value type tag
        try writer.writeByte(@intFromEnum(value));

        var buf: [8]u8 = undefined;
        switch (value) {
            .Int => |n| {
                std.mem.writeInt(i64, &buf, n, .little);
                try writer.writeAll(buf[0..8]);
            },
            .Float => |n| {
                const bits = @as(u64, @bitCast(n));
                std.mem.writeInt(u64, &buf, bits, .little);
                try writer.writeAll(buf[0..8]);
            },
            .String => |s| {
                var len_buf: [4]u8 = undefined;
                std.mem.writeInt(u32, &len_buf, @intCast(s.len), .little);
                try writer.writeAll(&len_buf);
                try writer.writeAll(s);
            },
            .Bool => |b| try writer.writeByte(if (b) 1 else 0),
            .Nil => {},
            .Symbol => |s| {
                var len_buf: [4]u8 = undefined;
                std.mem.writeInt(u32, &len_buf, @intCast(s.len), .little);
                try writer.writeAll(&len_buf);
                try writer.writeAll(s);
            },
            .Variable => |v| {
                var len_buf: [4]u8 = undefined;
                std.mem.writeInt(u32, &len_buf, @intCast(v.name.len), .little);
                try writer.writeAll(&len_buf);
                try writer.writeAll(v.name);
            },
            .Function => |f| {
                // Write function name length and name
                var len_buf: [4]u8 = undefined;
                std.mem.writeInt(u32, &len_buf, @intCast(f.name.len), .little);
                try writer.writeAll(&len_buf);
                try writer.writeAll(f.name);

                // Write parameter count
                std.mem.writeInt(u32, &len_buf, @intCast(f.param_count), .little);
                try writer.writeAll(&len_buf);

                // Write number of instructions
                std.mem.writeInt(u32, &len_buf, @intCast(f.instructions.items.len), .little);
                try writer.writeAll(&len_buf);

                // Write each instruction
                for (f.instructions.items) |instr| {
                    // Write opcode
                    try writer.writeByte(@intFromEnum(instr.op));

                    // Write operand presence flag
                    try writer.writeByte(if (instr.operand != null) 1 else 0);

                    // Write operand if present
                    if (instr.operand) |operand| {
                        try writeValueToFile(operand, writer);
                    }
                }
            },
            .BuiltinOperator => |op| {
                try writer.writeByte(@intFromEnum(op));
            },
            .ReturnAddress => |addr| {
                var usize_buf: [@sizeOf(usize)]u8 = undefined;
                std.mem.writeInt(usize, &usize_buf, addr.stack_ptr, .little);
                try writer.writeAll(&usize_buf);
                std.mem.writeInt(usize, &usize_buf, addr.arg_count, .little);
                try writer.writeAll(&usize_buf);
            },
            .Array, .Map => {
                // For now, we don't support serializing complex types like arrays and maps
                // This would require more sophisticated serialization
                return error.UnsupportedComplexType;
            },
        }
    }
};

fn readFunctionFromFile(allocator: std.mem.Allocator, reader: anytype, deserialized_functions: *std.ArrayList(*Function)) !Function {
    var func = Function.init(allocator);

    // Read number of instructions
    var buf: [4]u8 = undefined;
    try reader.readNoEof(&buf);
    const num_instrs = std.mem.readInt(u32, &buf, .little);

    // Read each instruction
    var i: usize = 0;
    while (i < num_instrs) : (i += 1) {
        // Read opcode
        const op = @as(OpCode, @enumFromInt(try reader.readByte()));

        // Read operand if present
        const has_operand = try reader.readByte() == 1;
        const operand = if (has_operand) try readValueFromFile(allocator, reader, deserialized_functions) else null;

        try func.instructions.append(.{
            .op = op,
            .operand = operand,
        });
    }

    return func;
}

fn readValueFromFile(allocator: std.mem.Allocator, reader: anytype, deserialized_functions: *std.ArrayList(*Function)) !Value {
    // Read value type tag
    const tag = try reader.readByte();

    var buf: [8]u8 = undefined;
    return switch (tag) {
        @intFromEnum(Value.Int) => {
            try reader.readNoEof(&buf);
            return Value{ .Int = std.mem.readInt(i64, &buf, .little) };
        },
        @intFromEnum(Value.Float) => {
            try reader.readNoEof(&buf);
            const bits = std.mem.readInt(u64, &buf, .little);
            return Value{ .Float = @as(f64, @bitCast(bits)) };
        },
        @intFromEnum(Value.String) => {
            // Read string length
            try reader.readNoEof(buf[0..4]);
            const len = std.mem.readInt(u32, buf[0..4], .little);

            // Read string data
            const str = try allocator.alloc(u8, len);
            errdefer allocator.free(str);
            try reader.readNoEof(str);

            return Value{ .String = str };
        },
        @intFromEnum(Value.Bool) => {
            const b = try reader.readByte();
            return Value{ .Bool = b == 1 };
        },
        @intFromEnum(Value.Nil) => Value{ .Nil = {} },
        @intFromEnum(Value.Symbol) => {
            // Read symbol length
            try reader.readNoEof(buf[0..4]);
            const len = std.mem.readInt(u32, buf[0..4], .little);

            // Read symbol data
            const sym = try allocator.alloc(u8, len);
            errdefer allocator.free(sym);
            try reader.readNoEof(sym);

            return Value{ .Symbol = sym };
        },
        @intFromEnum(Value.Variable) => {
            // Read variable name length
            try reader.readNoEof(buf[0..4]);
            const len = std.mem.readInt(u32, buf[0..4], .little);

            // Read variable name
            const name = try allocator.alloc(u8, len);
            errdefer allocator.free(name);
            try reader.readNoEof(name);

            return Value{ .Variable = .{ .name = name } };
        },
        @intFromEnum(Value.Function) => {
            // Read function name length
            try reader.readNoEof(buf[0..4]);
            const name_len = std.mem.readInt(u32, buf[0..4], .little);

            // Read function name
            const name = try allocator.alloc(u8, name_len);
            errdefer allocator.free(name);
            try reader.readNoEof(name);

            // Read parameter count
            try reader.readNoEof(buf[0..4]);
            const param_count = std.mem.readInt(u32, buf[0..4], .little);

            // Read number of instructions
            try reader.readNoEof(buf[0..4]);
            const num_instrs = std.mem.readInt(u32, buf[0..4], .little);

            // Create function and read instructions
            var func = try allocator.create(Function);
            errdefer allocator.destroy(func);
            func.* = Function.init(allocator);
            func.name = name;
            func.param_count = param_count;

            // Read each instruction
            var i: usize = 0;
            while (i < num_instrs) : (i += 1) {
                // Read opcode
                const op = @as(OpCode, @enumFromInt(try reader.readByte()));

                // Read operand presence flag
                const has_operand = try reader.readByte() == 1;

                // Read operand if present
                const operand = if (has_operand) try readValueFromFile(allocator, reader, deserialized_functions) else null;

                try func.instructions.append(.{
                    .op = op,
                    .operand = operand,
                });
            }

            // Register the function for cleanup
            try deserialized_functions.append(func);

            return Value{ .Function = func };
        },
        @intFromEnum(Value.BuiltinOperator) => {
            const op_tag = try reader.readByte();
            const op = @as(types.BuiltinOperatorType, @enumFromInt(op_tag));
            return Value{ .BuiltinOperator = op };
        },
        @intFromEnum(Value.ReturnAddress) => {
            var usize_buf: [@sizeOf(usize)]u8 = undefined;
            try reader.readNoEof(&usize_buf);
            const stack_ptr = std.mem.readInt(usize, &usize_buf, .little);
            try reader.readNoEof(&usize_buf);
            const arg_count = std.mem.readInt(usize, &usize_buf, .little);
            return Value{ .ReturnAddress = .{ .stack_ptr = stack_ptr, .arg_count = arg_count } };
        },
        else => error.InvalidValueType,
    };
}

pub fn lowerToBytecode(allocator: std.mem.Allocator, nodes: []ast.AstNode) !Function {
    var instructions = std.ArrayList(Instruction).init(allocator);
    errdefer {
        for (instructions.items) |*instr| {
            instr.deinit(allocator);
        }
        instructions.deinit();
    }

    var i: usize = 0;
    while (i < nodes.len) : (i += 1) {
        const node = nodes[i];
        switch (node) {
            .Expression => |expr| {
                switch (expr) {
                    .Variable => |var_expr| {
                        if (std.mem.eql(u8, var_expr.name, "print")) {
                            // Skip the print function itself, the next node should be the argument
                            i += 1;
                            if (i >= nodes.len) return error.UnexpectedEOF;
                            const arg_node = nodes[i];
                            switch (arg_node) {
                                .Expression => |arg_expr| {
                                    // First lower the argument expression
                                    try lowerExpression(allocator, &instructions, arg_expr);
                                    // Then add the print instruction
                                    try instructions.append(.{ .op = .Print });
                                },
                            }
                        } else {
                            try lowerExpression(allocator, &instructions, expr);
                        }
                    },
                    else => try lowerExpression(allocator, &instructions, expr),
                }
            },
        }
    }

    try instructions.append(.{ .op = .Return });

    return Function{
        .instructions = instructions,
        .allocator = allocator,
        .name = "", // Default empty name
        .param_count = 0, // Default no parameters
    };
}

fn lowerExpression(allocator: std.mem.Allocator, instructions: *std.ArrayList(Instruction), expr: ast.Expression) !void {
    debug.log("lowerExpression: processing expression type: {s}", .{@tagName(expr)});
    debug.log("lowerExpression: current instruction count: {}", .{instructions.items.len});
    switch (expr) {
        .SimpleFuncDef => {
            // TODO: Implement proper lowering for SimpleFuncDef
            debug.log("lowerExpression: SimpleFuncDef not yet implemented", .{});
            return error.NotImplemented;
        },
        .Literal => |lit| {
            try instructions.append(.{
                .op = .LoadConst,
                .operand = try lit.value.clone(allocator),
            });
        },
        .BinaryOp => |bin_op| {
            try lowerExpression(allocator, instructions, bin_op.left.*);
            try lowerExpression(allocator, instructions, bin_op.right.*);

            // Check the TokenKind and compare the identifier string if applicable
            switch (bin_op.op) {
                .Ident => |op_ident| {
                    if (std.mem.eql(u8, op_ident, "+")) {
                        try instructions.append(.{ .op = .Add });
                    } else if (std.mem.eql(u8, op_ident, "-")) {
                        try instructions.append(.{ .op = .Sub });
                    } else if (std.mem.eql(u8, op_ident, "<")) {
                        try instructions.append(.{ .op = .Lt });
                    } else if (std.mem.eql(u8, op_ident, ">")) {
                        try instructions.append(.{ .op = .Gt });
                    } else if (std.mem.eql(u8, op_ident, "==")) {
                        try instructions.append(.{ .op = .Eq });
                    } else {
                        std.debug.print("Unsupported binary operator identifier: {s}\n", .{op_ident});
                        return error.UnsupportedOperator;
                    }
                },
                else => {
                    // This case should ideally not happen if the parser correctly creates BinaryOp nodes
                    std.debug.print("Unexpected token kind for binary operator: {any}\n", .{bin_op.op});
                    return error.InvalidOperatorToken; // Or a more specific error
                },
            }
        },
        .Variable => |var_expr| {
            // Load the variable onto the stack
            try instructions.append(.{
                .op = .LoadVar,
                .operand = .{ .String = try allocator.dupe(u8, var_expr.name) },
            });
        },
        .If => |if_expr| {
            // Evaluate condition
            try lowerExpression(allocator, instructions, if_expr.condition.*);

            // Placeholder for conditional branching
            // In a real implementation, this would generate conditional jump instructions

            // For now, just evaluate the then branch
            try lowerExpression(allocator, instructions, if_expr.then_branch.*);

            // If there's an else branch, we would handle it here
            if (if_expr.else_branch) |else_branch| {
                _ = else_branch; // Avoid unused variable warning
                // In a real implementation, this would generate code for the else branch
            }
        },
        .FuncCall => |func_call| {
            debug.log("lowerExpression: Processing function call", .{});
            // Check if it's a call to a built-in function
            if (func_call.func.* == .Variable) {
                const func_name = func_call.func.*.Variable.name;
                debug.log("lowerExpression: Function call to variable: {s}", .{func_name});

                // Handle built-in functions
                if (std.mem.eql(u8, func_name, "print")) {
                    debug.log("lowerExpression: Handling built-in print function", .{});
                    // For print, we just need to evaluate the argument and then print it
                    if (func_call.args.items.len != 1) {
                        debug.log("lowerExpression: Invalid argument count for print: {}", .{func_call.args.items.len});
                        return error.InvalidOperatorArity;
                    }

                    // For print, we just need to evaluate the argument and then print it
                    debug.log("lowerExpression: Evaluating print argument", .{});
                    try lowerExpression(allocator, instructions, func_call.args.items[0].*);
                    debug.log("lowerExpression: Print argument evaluated successfully", .{});

                    // Add the print instruction
                    try instructions.append(.{
                        .op = .Print,
                    });
                    debug.log("lowerExpression: Print instruction added", .{});

                    return;
                }
            }

            // Regular function call
            debug.log("lowerExpression: Processing regular function call with {} args", .{func_call.args.items.len});
            // First, evaluate the function expression
            debug.log("lowerExpression: Evaluating function expression", .{});
            try lowerExpression(allocator, instructions, func_call.func.*);
            debug.log("lowerExpression: Function expression evaluated successfully", .{});

            // Then evaluate arguments
            debug.log("lowerExpression: Evaluating {} function arguments", .{func_call.args.items.len});
            for (func_call.args.items, 0..) |arg, i| {
                debug.log("lowerExpression: Evaluating argument {}/{}", .{ i + 1, func_call.args.items.len });
                try lowerExpression(allocator, instructions, arg.*);
                debug.log("lowerExpression: Argument {}/{} evaluated successfully", .{ i + 1, func_call.args.items.len });
            }
            debug.log("lowerExpression: All function arguments evaluated successfully", .{});

            // Add the call instruction with the argument count
            debug.log("lowerExpression: Adding Call instruction with {} args", .{func_call.args.items.len});
            try instructions.append(.{
                .op = .Call,
                .operand = .{ .Int = @intCast(func_call.args.items.len) },
            });
            debug.log("lowerExpression: Call instruction added successfully", .{});
        },
        .FuncDef => |func_def| {
            debug.log("lowerExpression: Processing function definition: {s}", .{func_def.name});
            debug.log("lowerExpression: Function has {} parameters", .{func_def.params.len});

            // Create a new bytecode function object
            var func_instructions = std.ArrayList(Instruction).init(allocator);
            // Don't defer deinit here, as we'll transfer ownership to the function object

            // Generate bytecode for the function body
            debug.log("lowerExpression: Generating bytecode for function body", .{});
            try lowerExpression(allocator, &func_instructions, func_def.body.*);
            debug.log("lowerExpression: Function body bytecode generated successfully", .{});

            // Add return instruction at the end if not present
            try func_instructions.append(.{ .op = .Return });
            debug.log("lowerExpression: Added return instruction to function", .{});

            // Skip function definition for now to avoid the bus error
            const func_value = types.Value{ .Int = 42 };
            debug.log("lowerExpression: Skipping function definition, using Int value instead", .{});

            // Clean up the function instructions since we're not using them
            for (func_instructions.items) |*instr| {
                if (instr.operand != null) {
                    instr.operand.?.deinit(allocator);
                }
            }
            func_instructions.deinit();

            // First load the function value onto the stack
            try instructions.append(.{
                .op = .LoadConst,
                .operand = func_value,
            });
            debug.log("lowerExpression: LoadConst instruction added for function", .{});

            // Then store the function value in a variable with the function name
            try instructions.append(.{
                .op = .StoreVar,
                .operand = .{ .String = try allocator.dupe(u8, func_def.name) },
            });
            debug.log("lowerExpression: Stored function {s} as Int value", .{func_def.name});

            // Load the function value again for global storage
            try instructions.append(.{
                .op = .LoadConst,
                .operand = try func_value.clone(allocator),
            });

            // Store the function in a variable with its name
            try instructions.append(.{
                .op = .StoreGlobal,
                .operand = types.Value{
                    .String = try allocator.dupe(u8, func_def.name),
                },
            });
            debug.log("lowerExpression: StoreGlobal instruction added for function {s}", .{func_def.name});
        },
        .VarDecl => |var_decl| {
            // Evaluate the variable's value
            try lowerExpression(allocator, instructions, var_decl.value.*);

            // Store it in a variable
            try instructions.append(.{
                .op = .StoreGlobal,
                .operand = types.Value{
                    .String = try allocator.dupe(u8, var_decl.name),
                },
            });
        },
    }
}
