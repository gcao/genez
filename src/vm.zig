const std = @import("std");
const bytecode = @import("bytecode.zig");
const types = @import("types.zig");

pub const VM = struct {
    stack: std.ArrayList(types.Value),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) !VM {
        return VM{
            .stack = std.ArrayList(types.Value).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *VM) void {
        for (self.stack.items) |*value| {
            value.deinit(self.allocator);
        }
        self.stack.deinit();
    }

    pub fn execute(self: *VM, func: bytecode.Function) !void {
        var ip: usize = 0;
        while (ip < func.instructions.len) : (ip += 1) {
            const instr = func.instructions[ip];

            switch (instr.op) {
                .LoadConst => {
                    if (instr.operand) |operand| {
                        const value = try operand.clone(self.allocator);
                        try self.stack.append(value);
                    }
                },
                .Add => {
                    if (self.stack.items.len < 2) return error.StackUnderflow;
                    var right = self.stack.pop();
                    var left = self.stack.pop();
                    defer right.deinit(self.allocator);
                    defer left.deinit(self.allocator);

                    const result = switch (left) {
                        .Int => |left_val| switch (right) {
                            .Int => |right_val| types.Value{ .Int = left_val + right_val },
                            .Float => |right_val| types.Value{ .Float = @as(f64, @floatFromInt(left_val)) + right_val },
                            else => return error.TypeMismatch,
                        },
                        .Float => |left_val| switch (right) {
                            .Int => |right_val| types.Value{ .Float = left_val + @as(f64, @floatFromInt(right_val)) },
                            .Float => |right_val| types.Value{ .Float = left_val + right_val },
                            else => return error.TypeMismatch,
                        },
                        .String => |left_val| switch (right) {
                            .String => |right_val| blk: {
                                const result_str = try std.mem.concat(self.allocator, u8, &[_][]const u8{ left_val, right_val });
                                break :blk types.Value{ .String = result_str };
                            },
                            else => return error.TypeMismatch,
                        },
                        else => return error.TypeMismatch,
                    };

                    try self.stack.append(result);
                },
                .Print => {
                    if (self.stack.items.len < 1) return error.StackUnderflow;
                    var value = self.stack.pop();
                    defer value.deinit(self.allocator);

                    switch (value) {
                        .Int => |val| std.debug.print("{d}\n", .{val}),
                        .Float => |val| std.debug.print("{d}\n", .{val}),
                        .String => |val| std.debug.print("{s}\n", .{val}),
                        .Bool => |val| std.debug.print("{}\n", .{val}),
                        .Symbol => |val| std.debug.print(":{s}\n", .{val}),
                        .Array => |val| {
                            std.debug.print("[", .{});
                            for (val, 0..) |item, i| {
                                if (i > 0) std.debug.print(", ", .{});
                                switch (item) {
                                    .Int => |n| std.debug.print("{d}", .{n}),
                                    .Float => |n| std.debug.print("{d}", .{n}),
                                    .String => |s| std.debug.print("\"{s}\"", .{s}),
                                    .Bool => |b| std.debug.print("{}", .{b}),
                                    .Symbol => |s| std.debug.print(":{s}", .{s}),
                                    .Nil => std.debug.print("nil", .{}),
                                    else => std.debug.print("...", .{}),
                                }
                            }
                            std.debug.print("]\n", .{});
                        },
                        .Map => |map| {
                            std.debug.print("{{\n", .{});
                            var iter = map.iterator();
                            while (iter.next()) |entry| {
                                std.debug.print("  {s}: ", .{entry.key_ptr.*});
                                switch (entry.value_ptr.*) {
                                    .Int => |n| std.debug.print("{d}", .{n}),
                                    .Float => |n| std.debug.print("{d}", .{n}),
                                    .String => |s| std.debug.print("\"{s}\"", .{s}),
                                    .Bool => |b| std.debug.print("{}", .{b}),
                                    .Symbol => |s| std.debug.print(":{s}", .{s}),
                                    .Nil => std.debug.print("nil", .{}),
                                    else => std.debug.print("...", .{}),
                                }
                                std.debug.print("\n", .{});
                            }
                            std.debug.print("}}\n", .{});
                        },
                        .Nil => std.debug.print("nil\n", .{}),
                    }
                },
                .Return => return,
            }
        }
    }
};
