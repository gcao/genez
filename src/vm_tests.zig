const std = @import("std");
const parser = @import("parser.zig");
const bytecode = @import("bytecode.zig");
const vm = @import("vm.zig");
const types = @import("types.zig");
const testing = std.testing;

fn testGeneExecution(source: []const u8, expected: types.Value) !void {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const nodes = try parser.parseGeneSource(allocator, source);
    const func = try bytecode.lowerToBytecode(allocator, nodes.items);
    var gene_vm = vm.VM.init(allocator, std.io.getStdOut().writer());
    defer gene_vm.deinit();

    try gene_vm.execute(&func);
    try testing.expect(gene_vm.stack.items.len == 1);
    const result = gene_vm.stack.items[0];

    switch (expected) {
        .String => |exp_str| {
            try testing.expectEqualStrings(exp_str, result.String);
        },
        .Int => |exp_int| {
            try testing.expectEqual(exp_int, result.Int);
        },
        .Bool => |exp_bool| {
            try testing.expectEqual(exp_bool, result.Bool);
        },
        .Float => |exp_float| {
            try testing.expectEqual(exp_float, result.Float);
        },
        else => unreachable,
    }
}

test "execute string literal" {
    try testGeneExecution("\"hello\"", .{ .String = "hello" });
}

test "execute integer literal" {
    try testGeneExecution("42", .{ .Int = 42 });
}

test "execute binary operation" {
    try testGeneExecution("(+ 1 2)", .{ .Int = 3 });
}
