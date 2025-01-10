const std = @import("std");

pub const BytecodeInstr = struct {
    code: InstructionCode,
};

pub const InstructionCode = union(enum) {
    LoadInt: struct { value: i64 },
    Print,
};

pub const Function = struct {
    instructions: []const BytecodeInstr,
};

pub const Module = struct {
    functions: []Function,
};

pub fn lowerToBytecode(allocator: *std.mem.Allocator) !Module {
    _ = allocator;
    // TODO: Implement actual bytecode generation
    return Module{
        .functions = &[_]Function{},
    };
}
