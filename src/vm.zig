const std = @import("std");
const bytecode = @import("bytecode.zig");

pub const VM = struct {
    // TODO: Add VM state fields

    pub fn init() VM {
        return VM{};
    }

    pub fn runFunction(self: *VM, function: bytecode.Function) !void {
        _ = self;
        _ = function;
        // TODO: Implement function execution
    }
};
