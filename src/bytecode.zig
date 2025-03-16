const std = @import("std");
const AstNode = @import("ast.zig").AstNode;
const types = @import("types.zig");

// Use AstNode.Value directly instead of types.Value
pub const Value = AstNode.Value;

pub const OpCode = enum(u8) {
    LoadConst,
    LoadVar,
    StoreVar,
    Add,
    Sub,
    Mul,
    Div,
    Lt,
    Gt,
    Eq,
    Jump,
    JumpIfFalse,
    DefineFunction,
    Call,
    Print,
    Return,
    Pop,
    // Add other opcodes as needed
};

pub const Instruction = struct {
    op: OpCode,
    operand: ?Value = null,

    pub fn deinit(self: *Instruction, allocator: std.mem.Allocator) void {
        if (self.operand) |*op| {
            // Create a temporary node to deinit the value
            var tmp_node = AstNode{ .tag = .{ .Expression = .{ .Literal = .{ .value = op.* } } }, .loc = .{ .start = 0, .end = 0 } };
            tmp_node.deinit(allocator);
        }
    }
};

pub const Function = struct {
    instructions: std.ArrayList(Instruction),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Function {
        return Function{
            .instructions = std.ArrayList(Instruction).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *const Function) void {
        for (self.instructions.items) |*instr| {
            instr.deinit(self.allocator);
        }
        self.instructions.deinit();
    }
};

pub const Module = struct {
    functions: []Function,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *Module) void {
        for (self.functions) |*func| {
            func.deinit();
        }
        self.allocator.free(self.functions);
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

        // Read each function
        var i: usize = 0;
        while (i < num_functions) : (i += 1) {
            functions[i] = try readFunctionFromFile(allocator, reader);
        }

        return Module{
            .functions = functions,
            .allocator = allocator,
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
            else => return error.UnsupportedValueType,
        }
    }
};

fn readValueFromFile(allocator: std.mem.Allocator, reader: anytype) !Value {
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
        else => error.InvalidValueType,
    };
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
    const num_funcs = std.mem.readInt(u32, &buf, .little);

    var functions = try allocator.alloc(Function, num_funcs);

    // Read each function
    var i: usize = 0;
    while (i < num_funcs) : (i += 1) {
        functions[i] = try readFunctionFromFile(allocator, reader);
    }

    return Module{
        .functions = functions,
        .allocator = allocator,
    };
}

fn readFunctionFromFile(allocator: std.mem.Allocator, reader: anytype) !Function {
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
        const operand = if (has_operand) try readValueFromFile(allocator, reader) else null;

        try func.instructions.append(.{
            .op = op,
            .operand = operand,
        });
    }

    return func;
}

pub fn lowerToBytecode(allocator: std.mem.Allocator, nodes: []AstNode) !struct {
    func: Function,
    func_map: std.StringHashMap(Function),
} {
    std.debug.print("Entering lowerToBytecode\n", .{});
    std.debug.print("AST Nodes: {any}\n", .{nodes});
    var instructions = std.ArrayList(Instruction).init(allocator);
    errdefer {
        for (instructions.items) |*instr| {
            instr.deinit(allocator);
        }
        instructions.deinit();
    }

    var function_map = std.StringHashMap(Function).init(allocator);
    errdefer function_map.deinit();

    var i: usize = 0;
    while (i < nodes.len) : (i += 1) {
        const node = nodes[i];
        switch (node.tag) {
            .Expression => |expr| {
                try lowerExpression(allocator, &instructions, expr, &function_map);
            },
        }
    }

    try instructions.append(.{ .op = .Return });

    return .{
        .func = Function{
            .instructions = instructions,
            .allocator = allocator,
        },
        .func_map = function_map,
    };
}

fn lowerExpression(
    allocator: std.mem.Allocator,
    instructions: *std.ArrayList(Instruction),
    expr: AstNode.Expression,
    function_map: *std.StringHashMap(Function),
) !void {
    switch (expr) {
        .Literal => |lit| {
            try instructions.append(.{
                .op = .LoadConst,
                .operand = try lit.value.clone(allocator),
            });
        },
        .BinaryOp => |bin_op| {
            try lowerExpression(allocator, instructions, bin_op.left.*, function_map);
            try lowerExpression(allocator, instructions, bin_op.right.*, function_map);
            const op = switch (bin_op.op) {
                .add => OpCode.Add,
                .sub => OpCode.Sub,
                .mul => OpCode.Mul,
                .div => OpCode.Div,
                .lt => OpCode.Lt,
                .gt => OpCode.Gt,
                .eq => OpCode.Eq,
            };
            try instructions.append(.{ .op = op });
        },
        .Variable => |var_expr| {
            try instructions.append(.{
                .op = .LoadVar,
                .operand = .{ .Symbol = try allocator.dupe(u8, var_expr.name) },
            });
        },
        .IfExpr => |if_expr| {
            try lowerExpression(allocator, instructions, if_expr.condition.*, function_map);
            const jump_if_false_index = instructions.items.len;
            try instructions.append(.{ .op = .JumpIfFalse, .operand = null }); // Placeholder

            try lowerExpression(allocator, instructions, if_expr.then_branch.*, function_map);
            const jump_index = instructions.items.len;
            try instructions.append(.{ .op = .Jump, .operand = null }); // Placeholder

            // Update JumpIfFalse operand with the correct offset
            instructions.items[jump_if_false_index].operand = .{ .Int = @intCast(instructions.items.len) };

            try lowerExpression(allocator, instructions, if_expr.else_branch.*, function_map);

            // Update Jump operand with the correct offset
            instructions.items[jump_index].operand = .{ .Int = @intCast(instructions.items.len) };
        },
        .FuncDef => |func_def| {
            // Create a new Function for the function body
            var body_instructions = std.ArrayList(Instruction).init(allocator);
            errdefer {
                for (body_instructions.items) |*instr| {
                    instr.deinit(allocator);
                }
                body_instructions.deinit();
            }

            // Lower the function body
            try lowerExpression(allocator, &body_instructions, func_def.body.*, function_map);
            try body_instructions.append(.{ .op = .Return });

            const func = Function{
                .instructions = body_instructions,
                .allocator = allocator,
            };

            // Store the function in the function map
            const name_copy = try allocator.dupe(u8, func_def.name);
            try function_map.put(name_copy, func);

            // Append a DefineFunction instruction
            try instructions.append(.{
                .op = .DefineFunction,
                .operand = .{ .String = name_copy },
            });
        },
        .FuncCall => |func_call| {
            // Lower arguments first
            for (func_call.args.items) |arg| {
                try lowerExpression(allocator, instructions, arg.*, function_map);
            }

            // Push function name onto stack
            switch (func_call.func.*) {
                .Variable => |v| try instructions.append(.{
                    .op = .LoadVar,
                    .operand = .{ .Symbol = try allocator.dupe(u8, v.name) },
                }),
                else => {},
            }

            // Append a Call instruction with the number of arguments
            try instructions.append(.{
                .op = .Call,
                .operand = .{ .Int = @intCast(func_call.args.items.len) },
            });
        },
        .If => |if_expr| {
            // Generate condition code
            try lowerExpression(allocator, instructions, if_expr.condition.*, function_map);

            // Emit conditional jump (will be patched later)
            const cond_jump_pos = instructions.items.len;
            try instructions.append(.{ .op = .JumpIfFalse, .operand = null });
            try instructions.append(.{ .op = .Jump, .operand = null });

            // Generate then branch code
            try lowerExpression(allocator, instructions, if_expr.then_branch.*, function_map);

            // Generate else branch code if it exists
            if (if_expr.else_branch) |else_branch| {
                try lowerExpression(allocator, instructions, else_branch.*, function_map);
            }

            // Patch conditional jump
            instructions.items[cond_jump_pos].operand = .{ .Int = @intCast(instructions.items.len) };
            instructions.items[cond_jump_pos + 1].operand = .{ .Int = @intCast(instructions.items.len) };
        },
    }
}

fn patchJumpOffset(self: *std.ArrayList(Instruction), pos: usize, offset: u16) void {
    // Write a 16-bit offset at the given position
    self.items[pos] = Instruction{ .op = .Jump, .operand = .{ .Int = @intCast(offset & 0xFF) } };
    self.items[pos + 1] = Instruction{ .op = .Jump, .operand = .{ .Int = @intCast((offset >> 8) & 0xFF) } };
}
