const std = @import("std");
const bytecode = @import("bytecode.zig");

pub const VM = struct {
    stack: std.ArrayList([]const u8),
    allocator: std.mem.Allocator,

    pub fn init() VM {
        var gpa = std.heap.GeneralPurposeAllocator(.{}){};
        return VM{
            .stack = std.ArrayList([]const u8).init(gpa.allocator()),
            .allocator = gpa.allocator(),
        };
    }

    pub fn runFunction(self: *VM, function: bytecode.Function) !void {
        for (function.instructions) |instr| {
            switch (instr.code) {
                .LoadString => |load| {
                    try self.stack.append(load.value);
                },
                .Print => {
                    const value = self.stack.pop();
                    std.debug.print("{s}\n", .{value});
                },
            }
        }
    }
};
