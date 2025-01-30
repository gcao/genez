const std = @import("std");
const main = @import("main.zig");
const vm = main.vm;
const bytecode = main.bytecode;
const parser = @import("parser.zig");

// Custom writer that writes to both stdout and buffer
const TeeWriter = struct {
    const Self = @This();
    stdout_writer: std.fs.File.Writer,
    buffer_writer: std.io.FixedBufferStream([]u8).Writer,

    pub const Writer = struct {
        context: *Self,

        pub const Error = std.fs.File.WriteError;

        pub fn write(self: @This(), bytes: []const u8) Error!usize {
            return self.context.write(bytes);
        }

        pub fn writeAll(self: @This(), bytes: []const u8) Error!void {
            var remaining = bytes;
            while (remaining.len > 0) {
                const written = try self.write(remaining);
                remaining = remaining[written..];
            }
        }

        pub fn flush(self: @This()) Error!void {
            return self.context.flush();
        }
    };

    fn write(self: *Self, bytes: []const u8) std.fs.File.WriteError!usize {
        _ = try self.buffer_writer.write(bytes);
        return try self.stdout_writer.write(bytes);
    }

    fn flush(_: *Self) std.fs.File.WriteError!void {
        // No need to flush stdout writer as it's unbuffered
    }

    fn writer(self: *Self) Writer {
        return .{ .context = self };
    }
};

test "test gene execution helper" {
    const source = "\"hello world\"";
    const expected = "hello world\n";

    // Use the helper to test execution
    try testGeneExecution(source, expected);
}

fn testGeneExecution(source: []const u8, expected: []const u8) !void {
    // Initialize VM and ensure proper cleanup
    var my_vm = try vm.VM.init();
    defer my_vm.deinit();

    // Use the testing allocator directly
    var allocator = std.testing.allocator;

    // Parse source into AST nodes
    const parsed = parser.parseGeneSource(&allocator, source) catch |err| {
        return err;
    };

    // Convert to bytecode from parsed AST
    var module = try bytecode.lowerToBytecode(&allocator, parsed);
    defer module.deinit();

    // Create stdout capture buffer
    var stdout_buf: [1024]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&stdout_buf);

    // Create tee writer
    var tee_writer = TeeWriter{
        .stdout_writer = std.io.getStdOut().writer(),
        .buffer_writer = fbs.writer(),
    };
    const writer = TeeWriter.Writer{ .context = &tee_writer };

    // Run the module
    _ = try my_vm.runModule(&module, writer);

    // Verify output
    const output = fbs.getWritten();
    try std.testing.expectEqualStrings(expected, output);
}

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

    // Use the testing allocator directly
    var allocator = std.testing.allocator;

    // Create test function
    const func = bytecode.Function{
        .instructions = try allocator.dupe(bytecode.BytecodeInstr, instrs[0..]),
        .allocator = allocator,
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
    _ = try my_vm.runFunction(&func, writer);

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

    // Use the testing allocator directly
    var allocator = std.testing.allocator;

    // Create test function
    const func = bytecode.Function{
        .instructions = try allocator.dupe(bytecode.BytecodeInstr, instrs[0..]),
        .allocator = allocator,
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
    try std.testing.expectError(error.StackUnderflow, my_vm.runFunction(&func, writer));
}
