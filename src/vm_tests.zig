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
    const lowered = try bytecode.lowerToBytecode(allocator, nodes.items);
    var func = bytecode.Function.init(allocator);
    func.instructions = lowered.func.instructions;
    const function_map = std.StringHashMap(bytecode.Function).init(allocator);
    var gene_vm = vm.VM.init(allocator, std.io.getStdOut().writer(), function_map);
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

test "less than comparison" {
    try testGeneExecution("(< 1 2)", .{ .Bool = true });
    try testGeneExecution("(< 2 1)", .{ .Bool = false });
    try testGeneExecution("(< 2 2)", .{ .Bool = false });
}

test "simple recursion (factorial)" {
    const factorial_source =
        \\(fn fact [n int]
        \\  (if (n < 2)
        \\    1
        \\  else
        \\    (n * (fact (n - 1)))
        \\  )
        \\)
        \\(fact 5)
    ;
    try testGeneExecution(factorial_source, .{ .Int = 120 });
}

test "fibonacci" {
    const fibonacci_source =
        \\(fn fib [n int]
        \\  (if (n < 2)
        \\    n
        \\  else
        \\    ((fib (n - 1)) + (fib (n - 2)))
        \\  )
        \\)
        \\(fib 10)
    ;
    try testGeneExecution(fibonacci_source, .{ .Int = 55 });
}
