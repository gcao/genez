const std = @import("std");
const ast = @import("ast.zig");

pub const BytecodeInstr = struct {
    code: InstructionCode,
};

pub const InstructionCode = union(enum) {
    LoadString: struct { value: []const u8 },
    Print,
};

pub const Function = struct {
    instructions: []const BytecodeInstr,
};

pub const Module = struct {
    functions: []Function,
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
                        try instructions.append(BytecodeInstr{
                            .code = .{ .LoadString = .{ .value = value } },
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

    try functions.append(Function{
        .instructions = try instructions.toOwnedSlice(),
    });

    std.debug.print("Generated {} functions with {} total instructions\n", .{
        functions.items.len,
        instructions.items.len,
    });

    return Module{
        .functions = try functions.toOwnedSlice(),
    };
}
