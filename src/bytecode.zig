const std = @import("std");
const ast = @import("ast.zig");
const types = @import("types.zig");

pub const Value = types.Value;

pub const OpCode = enum {
    LoadConst,
    LoadVar,
    StoreVar,
    Add,
    Print,
    Return,
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

    pub fn init(allocator: std.mem.Allocator) Function {
        return Function{
            .instructions = std.ArrayList(Instruction).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *const Function) void {
        for (self.instructions.items) |*instr| {
            if (instr.operand) |*operand| {
                operand.deinit(self.allocator);
            }
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
    };
}

fn lowerExpression(allocator: std.mem.Allocator, instructions: *std.ArrayList(Instruction), expr: ast.Expression) !void {
    switch (expr) {
        .Literal => |lit| {
            try instructions.append(.{
                .op = .LoadConst,
                .operand = try lit.value.clone(allocator),
            });
        },
        .BinaryOp => |bin_op| {
            try lowerExpression(allocator, instructions, bin_op.left.*);
            try lowerExpression(allocator, instructions, bin_op.right.*);
            try instructions.append(.{ .op = .Add });
        },
        .Variable => |var_expr| {
            if (std.mem.eql(u8, var_expr.name, "print")) {
                try instructions.append(.{ .op = .Print });
            } else {
                // Handle other variables here
            }
        },
    }
}
