const std = @import("std");

pub const Module = struct {
    functions: []Function,
};

pub const Function = struct {
    // TODO: Add function fields
};

pub fn lowerToBytecode(allocator: *std.mem.Allocator) !Module {
    _ = allocator;
    // TODO: Implement actual bytecode generation
    return Module{
        .functions = &[_]Function{},
    };
}
