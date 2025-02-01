const std = @import("std");

pub const Value = union(enum) {
    int: i64,
    string: []const u8,
    nil,
};
const ast = @import("ast.zig");

pub const BytecodeInstr = struct {
    code: InstructionCode,
};

pub const InstructionCode = union(enum) {
    LoadString: struct { value: []const u8, owned: bool = false },
    LoadInt: struct { value: i64 },
    Print,
    Return,
    NewClass: struct { class_name: []const u8 },
    GetProperty: struct { property_name: []const u8 },
    SetProperty: struct { property_name: []const u8 },
    CallMethod: struct { method_name: []const u8, arg_count: u32 },
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
    var functions = std.ArrayList(Function).init(allocator.*);
    defer functions.deinit();

    var instructions = std.ArrayList(BytecodeInstr).init(allocator.*);
    defer instructions.deinit();

    for (nodes) |node| {
        switch (node) {
            .Statement => |stmt_ptr| switch (stmt_ptr.*) {
                .Expression => |expr_ptr| switch (expr_ptr.*) {
                    .Literal => |lit_ptr| switch (lit_ptr.value) {
                        .String => |value| {
                            // Allocate and copy the string
                            const owned_value = try allocator.alloc(u8, value.len);
                            @memcpy(owned_value, value);
                            try instructions.append(BytecodeInstr{
                                .code = .{ .LoadString = .{ .value = owned_value, .owned = true } },
                            });
                            try instructions.append(BytecodeInstr{
                                .code = .Return,
                            });
                        },
                        else => {},
                    },
                    .Variable => |var_ptr| {
                        if (std.mem.eql(u8, var_ptr.name, "print")) {
                            try instructions.append(BytecodeInstr{
                                .code = .Print,
                            });
                        }
                    },
                    .Call => |call_ptr| {
                        // Check if we're calling a print function
                        if (call_ptr.function.* == .Variable and
                            std.mem.eql(u8, call_ptr.function.Variable.name, "print"))
                        {
                            // Generate code for arguments first
                            for (call_ptr.args.items) |arg| {
                                switch (arg) {
                                    .Expression => |call_expr_ptr| switch (call_expr_ptr.*) {
                                        .Literal => |lit_ptr| switch (lit_ptr.value) {
                                            .String => |value| {
                                                const owned_value = try allocator.alloc(u8, value.len);
                                                @memcpy(owned_value, value);
                                                try instructions.append(BytecodeInstr{
                                                    .code = .{ .LoadString = .{ .value = owned_value, .owned = true } },
                                                });
                                            },
                                            else => {},
                                        },
                                        else => {},
                                    },
                                    else => {},
                                }
                            }
                            try instructions.append(BytecodeInstr{
                                .code = .Print,
                            });
                        }
                    },
                    else => {},
                },
                else => {},
            },
            .Class => |class_node| {
                // Generate instructions for class definition
                const class_name = try allocator.alloc(u8, class_node.name.len);
                @memcpy(class_name, class_node.name);

                // Generate property initialization
                for (class_node.fields) |prop| {
                    const prop_name = try allocator.alloc(u8, prop.name.len);
                    @memcpy(prop_name, prop.name);

                    try instructions.append(BytecodeInstr{
                        .code = .{ .SetProperty = .{ .property_name = prop_name } },
                    });
                }
            },
            else => {},
        }
    }

    const instruction_count = instructions.items.len;
    const function_instructions = try instructions.toOwnedSlice();

    if (instruction_count == 0) {
        // Allow empty modules by returning empty bytecode
    }

    try functions.append(Function{
        .instructions = function_instructions,
        .allocator = allocator.*,
    });

    return Module{
        .functions = try functions.toOwnedSlice(),
        .allocator = allocator.*,
    };
}
