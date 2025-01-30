const std = @import("std");
const bytecode = @import("bytecode.zig");

pub const VM = struct {
    pub fn runModule(self: *VM, module: *const bytecode.Module, writer: anytype) anyerror!void {
        for (module.functions) |func| {
            _ = try self.runFunction(&func, writer);
        }
    }
    stack: std.ArrayList([]u8),
    arena: std.heap.ArenaAllocator,

    pub fn init() !VM {
        return VM{
            .stack = std.ArrayList([]u8).init(std.heap.page_allocator),
            .arena = std.heap.ArenaAllocator.init(std.heap.page_allocator),
        };
    }

    pub fn deinit(self: *VM) void {
        self.stack.deinit();
        self.arena.deinit();
    }

    pub fn runFunction(self: *VM, func: *const bytecode.Function, writer: anytype) anyerror!bytecode.Value {
        defer self.stack.clearRetainingCapacity();

        for (func.instructions) |instr| {
            switch (instr.code) {
                .LoadInt => |val| {
                    const num_str = try std.fmt.allocPrint(self.arena.allocator(), "{d}", .{val.value});
                    try self.stack.append(num_str);
                },
                .LoadString => |str_val| {
                    const str_copy = try self.arena.allocator().dupe(u8, str_val.value);
                    try self.stack.append(str_copy);
                },
                .Print => {
                    if (self.stack.items.len == 0) return error.StackUnderflow;
                    const value = self.stack.pop();
                    try writer.writeAll(value);
                    try writer.writeAll("\n");
                },
                .Return => {
                    if (self.stack.items.len == 0) return bytecode.Value{ .int = 0 };
                    const value_str = self.stack.pop();
                    const value_int = try std.fmt.parseInt(i64, value_str, 10);
                    return bytecode.Value{ .int = value_int };
                },
                else => return error.UnimplementedOpcode,
            }
        }

        return bytecode.Value{ .int = 0 };
    }
};
