const std = @import("std");
const bytecode = @import("bytecode.zig");

pub const VM = struct {
    stack: std.ArrayList([]align(16) const u8),
    arena: std.heap.ArenaAllocator,
    temp_allocator: std.heap.FixedBufferAllocator,

    pub fn init() !VM {
        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        var stack = std.ArrayList([]align(16) const u8).init(arena.allocator());
        try stack.ensureTotalCapacity(1024);

        var temp_buffer: [65536]u8 align(16) = undefined;
        const temp_allocator = std.heap.FixedBufferAllocator.init(&temp_buffer);
        std.debug.print("Initialized temp allocator with buffer at: {*}\n", .{temp_buffer[0..].ptr});

        return VM{
            .arena = arena,
            .stack = stack,
            .temp_allocator = temp_allocator,
        };
    }

    pub fn deinit(self: *VM) void {
        self.stack.clearAndFree();
        self.stack.deinit();
        _ = self.arena.deinit();
    }

    pub fn runModule(self: *VM, module: *const bytecode.Module) !void {
        if (module.functions.len == 0) {
            return error.NoFunctionsToRun;
        }
        try self.runFunction(&module.functions[0]);
    }

    pub fn runFunction(self: *VM, function: *const bytecode.Function) !void {
        std.debug.print("Running function with {d} instructions\n", .{function.instructions.len});

        for (function.instructions) |instr| {
            switch (instr.code) {
                .LoadString => |load| {
                    std.debug.print("Loading string: {s} (len: {d})\n", .{ load.value, load.value.len });
                    if (load.value.len > 4096) {
                        return error.StringTooLarge;
                    }

                    self.temp_allocator.reset();
                    const allocator = self.temp_allocator.allocator();
                    const len = load.value.len;
                    const str_copy = try allocator.alloc(u8, len);

                    @memcpy(str_copy, load.value);
                    std.debug.print("Copied {d} bytes from {*} to {*}\n", .{ len, load.value.ptr, str_copy.ptr });

                    const aligned_str: []align(16) const u8 = @alignCast(str_copy);
                    try self.stack.append(aligned_str);
                    std.debug.print("Stack size: {d}, top: {*} (aligned: {*})\n", .{ self.stack.items.len, str_copy.ptr, aligned_str.ptr });
                },
                .LoadInt => |load| {
                    std.debug.print("Loading integer: {d}\n", .{load.value});
                    const int_ptr = try self.temp_allocator.allocator().create(i64);
                    int_ptr.* = load.value;
                    const aligned_int: []align(16) const u8 = @alignCast(@as([*]u8, @ptrCast(int_ptr))[0..@sizeOf(i64)]);
                    try self.stack.append(aligned_int);
                    std.debug.print("Stack size: {d}, top: {*}\n", .{ self.stack.items.len, int_ptr });
                },
                .Print => {
                    if (self.stack.items.len == 0) {
                        return error.StackUnderflow;
                    }
                    const value = self.stack.pop();
                    std.debug.print("Printing value: {s}\n", .{value});
                    std.debug.print("{s}\n", .{value});
                    std.debug.print("Stack size after print: {d}\n", .{self.stack.items.len});
                },
            }
        }

        while (self.stack.popOrNull()) |value| {
            std.debug.print("Cleaning up stack value: {s}\n", .{value});
        }
    }
};
