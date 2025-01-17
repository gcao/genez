const std = @import("std");
const bytecode = @import("bytecode.zig");

/// Virtual Machine for executing bytecode
pub const VM = struct {
    stack: std.ArrayList([]const u8),
    arena: std.heap.ArenaAllocator,
    temp_allocator: std.heap.FixedBufferAllocator,

    /// Initialize a new VM instance
    /// Returns error if memory allocation fails
    pub fn init() !VM {
        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        var stack = std.ArrayList([]const u8).init(arena.allocator());
        try stack.ensureTotalCapacity(1024);

        var temp_buffer: [65536]u8 = undefined;
        const temp_allocator = std.heap.FixedBufferAllocator.init(&temp_buffer);

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
        const stdout = std.io.getStdOut();
        try self.runFunction(&module.functions[0], @TypeOf(stdout.writer()), .{ .writer = stdout.writer() });
    }

    /// Execute a bytecode function
    /// function: The function to execute
    /// Writer: Type of the output writer
    /// options: Configuration containing the writer instance
    /// Returns error if execution fails
    pub fn runFunction(self: *VM, function: *const bytecode.Function, comptime Writer: type, options: struct { writer: Writer }) !void {
        for (function.instructions) |instr| {
            switch (instr.code) {
                .LoadString => |load| {
                    if (load.value.len > 4096) {
                        return error.StringTooLarge;
                    }

                    self.temp_allocator.reset();
                    const allocator = self.temp_allocator.allocator();
                    const str_copy = try allocator.dupe(u8, load.value);
                    try self.stack.append(str_copy);
                },
                .LoadInt => |load| {
                    const int_ptr = try self.temp_allocator.allocator().create(i64);
                    int_ptr.* = load.value;
                    const int_bytes = @as([*]u8, @ptrCast(int_ptr))[0..@sizeOf(i64)];
                    try self.stack.append(int_bytes);
                },
                .Print => {
                    if (self.stack.items.len == 0) {
                        return error.StackUnderflow;
                    }
                    const value = self.stack.pop();
                    try options.writer.print("{s}\n", .{value});
                },
            }
        }
    }
};
