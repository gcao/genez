const std = @import("std");
const bytecode = @import("bytecode.zig");

pub const VM = struct {
    pub fn runModule(self: *VM, module: *const bytecode.Module, writer: anytype) anyerror!bytecode.Value {
        var last_value = bytecode.Value{ .int = 0 };
        for (module.functions) |func| {
            last_value = try self.runFunction(&func, writer);
        }
        return last_value;
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
                .LoadBool => |bool_val| {
                    const bool_str = try std.fmt.allocPrint(self.arena.allocator(), "{}", .{bool_val.value});
                    try self.stack.append(bool_str);
                },
                .Add => {
                    if (self.stack.items.len < 2) return error.StackUnderflow;
                    const b = try std.fmt.parseInt(i64, self.stack.pop(), 10);
                    const a = try std.fmt.parseInt(i64, self.stack.pop(), 10);
                    const result = try std.fmt.allocPrint(self.arena.allocator(), "{d}", .{a + b});
                    try self.stack.append(result);
                },
                .Print => {
                    if (self.stack.items.len == 0) return error.StackUnderflow;
                    const value = self.stack.pop();
                    try writer.writeAll(value);
                    try writer.writeAll("\n");
                    return bytecode.Value{ .nil = {} };
                },
                .Return => {
                    if (self.stack.items.len == 0) return bytecode.Value{ .int = 0 };
                    const value = self.stack.pop();
                    // Try to parse as integer first
                    const int_val = std.fmt.parseInt(i64, value, 10) catch null;
                    if (int_val) |val| {
                        return bytecode.Value{ .int = val };
                    }
                    // Try to parse as boolean
                    if (std.mem.eql(u8, value, "true")) {
                        return bytecode.Value{ .bool = true };
                    } else if (std.mem.eql(u8, value, "false")) {
                        return bytecode.Value{ .bool = false };
                    }
                    // If not an integer or boolean, return as string
                    return bytecode.Value{ .string = value };
                },
                else => {},
            }
        }

        // If we reach here without a return, return nil
        return bytecode.Value{ .nil = {} };
    }
};
