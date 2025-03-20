const std = @import("std");
const parser = @import("parser.zig");
const ast = @import("ast.zig");
const types = @import("types.zig");
const testing = std.testing;

test "parse string literal" {
    const allocator = std.testing.allocator;

    const source = "(print \"hello\")";
    var result = try parser.parseGeneSource(allocator, source);
    defer {
        for (result.items) |*node| {
            node.deinit(allocator);
        }
        result.deinit();
    }

    try testing.expectEqual(@as(usize, 1), result.items.len);

    const node = result.items[0];
    try testing.expect(node == .Expression);
}

test "parse integer literal" {
    const allocator = std.testing.allocator;

    const source = "(print 42)";
    var result = try parser.parseGeneSource(allocator, source);
    defer {
        for (result.items) |*node| {
            node.deinit(allocator);
        }
        result.deinit();
    }

    try testing.expectEqual(@as(usize, 1), result.items.len);

    const node = result.items[0];
    try testing.expect(node == .Expression);
}

test "parse binary operation" {
    const allocator = std.testing.allocator;

    const source = "(print (+ 1 2))";
    var result = try parser.parseGeneSource(allocator, source);
    defer {
        for (result.items) |*node| {
            node.deinit(allocator);
        }
        result.deinit();
    }

    try testing.expectEqual(@as(usize, 1), result.items.len);

    const node = result.items[0];
    try testing.expect(node == .Expression);
}

test "parse function definition" {
    const allocator = std.testing.allocator;

    const source = "(fn fib [n int]\n  (if (n < 2)\n    n\n  else\n    ((fib (n - 1)) + (fib (n - 2)))\n  )\n)";
    var result = try parser.parseGeneSource(allocator, source);
    defer {
        for (result.items) |*node| {
            node.deinit(allocator);
        }
        result.deinit();
    }

    try testing.expectEqual(@as(usize, 1), result.items.len);

    const node = result.items[0];
    try testing.expect(node == .Expression); // TODO: expect Function node
}
