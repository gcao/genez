const std = @import("std");
const main = @import("main.zig");
const vm = main.vm;
const bytecode = main.bytecode;
const parser = @import("parser.zig");

fn testGeneExecution(source: []const u8, expected: bytecode.Value) !void {
    // Initialize VM and ensure proper cleanup
    var my_vm = try vm.VM.init();
    defer my_vm.deinit();

    // Create a test allocator
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var allocator_instance = arena.allocator();
    const allocator: *std.mem.Allocator = &allocator_instance;

    // Parse and compile the test source
    const parsed = try parser.parseGeneSource(allocator, source);
    defer allocator.free(parsed);

    // Convert to bytecode
    var module = try bytecode.lowerToBytecode(allocator, parsed);
    defer module.deinit();

    // Run the module and get the result
    const result = try my_vm.runModule(&module, std.io.getStdOut().writer());

    // Verify result matches expected value
    try std.testing.expectEqual(expected, result);
}

test "string literal execution" {
    const source = "\"hello world\"";
    const expected = bytecode.Value{ .string = source[1 .. source.len - 1] };
    try testGeneExecution(source, expected);
}

test "integer literal execution" {
    const source = "42";
    const expected = bytecode.Value{ .int = 42 };
    try testGeneExecution(source, expected);
}

test "print returns nil" {
    const source = "(print \"hello\")";
    const expected = bytecode.Value{ .nil = {} };
    try testGeneExecution(source, expected);
}

test "boolean true literal execution" {
    const source = "true";
    const expected = bytecode.Value{ .bool = true };
    try testGeneExecution(source, expected);
}

test "boolean false literal execution" {
    const source = "false";
    const expected = bytecode.Value{ .bool = false };
    try testGeneExecution(source, expected);
}

// TODO: Add more test cases as we implement additional features
