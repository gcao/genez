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

test "parse fibonacci example" {
    const allocator = std.testing.allocator;

    // Content from examples/fibonacci.gene, excluding the shebang
    const source =
        \\(fn fib [n int]
        \\  (if (n < 2)
        \\    n
        \\  else
        \\    ((fib (n - 1)) + (fib (n - 2)))
        \\  )
        \\)
        \\
        \\(var i = 10)
        \\(print "(fib " i ") = " (fib i))
    ;
    var result = try parser.parseGeneSource(allocator, source);
    defer {
        for (result.items) |*node| {
            node.deinit(allocator);
        }
        result.deinit();
    }

    // Expecting 3 top-level nodes: fn definition, var declaration, print expression
    try testing.expectEqual(@as(usize, 3), result.items.len);

    // TODO: Add more specific assertions about the parsed nodes if needed
    // For now, just check the count and that parsing succeeded.
}
