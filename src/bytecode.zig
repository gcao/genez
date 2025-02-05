const std = @import("std");
const ast = @import("ast.zig");
const types = @import("types.zig");

pub const Value = types.Value;

pub const OpCode = enum {
    LoadConst,
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
    instructions: []Instruction,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *Function) void {
        for (self.instructions) |*instr| {
            instr.deinit(self.allocator);
        }
        self.allocator.free(self.instructions);
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
};

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
        .instructions = try instructions.toOwnedSlice(),
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
