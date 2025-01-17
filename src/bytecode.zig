const std = @import("std");
const ast = @import("ast.zig");

pub const BytecodeInstr = struct {
    code: InstructionCode,
};

pub const InstructionCode = union(enum) {
    LoadString: struct { value: []const u8, owned: bool = false },
    LoadInt: struct { value: i64 },
    Print,
};

pub const Function = struct {
    instructions: []const BytecodeInstr,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *Function) void {
        for (self.instructions) |instr| {
            if (instr.code == .LoadString and instr.code.LoadString.owned) {
                self.allocator.free(instr.code.LoadString.value);
            }
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

pub fn lowerToBytecode(allocator: *std.mem.Allocator, nodes: []ast.AstNode) !Module {
    std.debug.print("Starting bytecode generation for {} nodes\n", .{nodes.len});

    var functions = std.ArrayList(Function).init(allocator.*);
    defer functions.deinit();

    var instructions = std.ArrayList(BytecodeInstr).init(allocator.*);
    defer instructions.deinit();

    for (nodes) |node| {
        switch (node) {
            .Stmt => |stmt| switch (stmt) {
                .ExprStmt => |expr| switch (expr) {
                    .StrLit => |value| {
                        std.debug.print("  Generating bytecode for string literal: {s}\n", .{value});
                        // Allocate and copy the string
                        const owned_value = try allocator.alloc(u8, value.len);
                        @memcpy(owned_value, value);
                        try instructions.append(BytecodeInstr{
                            .code = .{ .LoadString = .{ .value = owned_value, .owned = true } },
                        });
                        try instructions.append(BytecodeInstr{
                            .code = .Print,
                        });
                    },
                    .Ident => |value| {
                        std.debug.print("  Generating bytecode for identifier: {s}\n", .{value});
                        if (std.mem.eql(u8, value, "print")) {
                            try instructions.append(BytecodeInstr{
                                .code = .Print,
                            });
                        }
                    },
                    else => {
                        std.debug.print("  Unknown expression type\n", .{});
                    },
                },
                else => {
                    std.debug.print("  Unknown statement type\n", .{});
                },
            },
            else => {
                std.debug.print("  Unknown node type\n", .{});
            },
        }
    }

    const instruction_count = instructions.items.len;
    const function_instructions = try instructions.toOwnedSlice();

    if (instruction_count == 0) {
        return error.NoInstructionsGenerated;
    }

    try functions.append(Function{
        .instructions = function_instructions,
        .allocator = allocator.*,
    });

    std.debug.print("Generated {} functions with {} total instructions\n", .{
        functions.items.len,
        instruction_count,
    });

    return Module{
        .functions = try functions.toOwnedSlice(),
        .allocator = allocator.*,
    };
}
