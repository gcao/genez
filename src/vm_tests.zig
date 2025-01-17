const std = @import("std");
const main = @import("main.zig");
const vm = main.vm;
const bytecode = main.bytecode;

// Custom writer that writes to both stdout and buffer
const TeeWriter = struct {
    const Self = @This();
    stdout_writer: std.fs.File.Writer,
    buffer_writer: std.io.FixedBufferStream([]u8).Writer,

    pub const Writer = std.io.Writer(*Self, std.fs.File.WriteError, write);

    fn write(self: *Self, bytes: []const u8) std.fs.File.WriteError!usize {
        _ = try self.buffer_writer.write(bytes);
        return try self.stdout_writer.write(bytes);
    }
};

test "vm basic operations" {
    // Initialize VM and ensure proper cleanup
    var my_vm = try vm.VM.init();
    defer my_vm.deinit();

    // Create test instructions
    var instrs: [4]bytecode.BytecodeInstr = .{
        .{ .code = .{ .LoadInt = .{ .value = 10 } } },
        .{ .code = .{ .LoadString = .{ .value = "test", .owned = false } } },
        .{ .code = .Print }, // Should print "test"
        .{ .code = .Print }, // Should print "10"
    };

    // Create arena allocator for test data
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    // Create test function
    const func = bytecode.Function{
        .instructions = try arena.allocator().dupe(bytecode.BytecodeInstr, instrs[0..]),
        .allocator = arena.allocator(),
    };

    // Create stdout capture buffer
    var stdout_buf: [1024]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&stdout_buf);

    var tee_writer = TeeWriter{
        .stdout_writer = std.io.getStdOut().writer(),
        .buffer_writer = fbs.writer(),
    };
    const writer = TeeWriter.Writer{ .context = &tee_writer };

    // Run the function with our tee writer
    try my_vm.runFunction(&func, TeeWriter.Writer, .{ .writer = writer });

    // Verify output
    const output = fbs.getWritten();
    try std.testing.expectEqualStrings("test\n10\n", output);
}

test "vm error handling" {
    // Initialize VM and ensure proper cleanup
    var my_vm = try vm.VM.init();
    defer my_vm.deinit();

    // Create test instructions that should cause stack underflow
    var instrs: [1]bytecode.BytecodeInstr = .{
        .{ .code = .Print }, // Attempt to print with empty stack
    };

    // Create arena allocator for test data
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    // Create test function
    const func = bytecode.Function{
        .instructions = try arena.allocator().dupe(bytecode.BytecodeInstr, instrs[0..]),
        .allocator = arena.allocator(),
    };

    // Create stdout capture buffer
    var stdout_buf: [1024]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&stdout_buf);

    // Create tee writer
    var tee_writer = TeeWriter{
        .stdout_writer = std.io.getStdOut().writer(),
        .buffer_writer = fbs.writer(),
    };
    const writer = TeeWriter.Writer{ .context = &tee_writer };

    // Verify error is thrown
    try std.testing.expectError(error.StackUnderflow, my_vm.runFunction(&func, TeeWriter.Writer, .{ .writer = writer }));
}
