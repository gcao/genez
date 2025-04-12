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

    // Verify the AST structure
    const expr = node.Expression;
    try testing.expect(expr == .FuncCall);
    const func_call = expr.FuncCall;

    // Check function name
    try testing.expect(func_call.func.* == .Variable);
    try testing.expectEqualStrings("print", func_call.func.*.Variable.name);

    // Check argument
    try testing.expectEqual(@as(usize, 1), func_call.args.items.len);
    try testing.expect(func_call.args.items[0].* == .Literal);
    try testing.expectEqualStrings("hello", func_call.args.items[0].*.Literal.value.String);
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

    // Verify the AST structure
    const expr = node.Expression;
    try testing.expect(expr == .FuncCall);
    const func_call = expr.FuncCall;

    // Check function name
    try testing.expect(func_call.func.* == .Variable);
    try testing.expectEqualStrings("print", func_call.func.*.Variable.name);

    // Check argument
    try testing.expectEqual(@as(usize, 1), func_call.args.items.len);
    try testing.expect(func_call.args.items[0].* == .Literal);
    try testing.expectEqual(@as(i64, 42), func_call.args.items[0].*.Literal.value.Int);
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

    // Verify the AST structure
    const expr = node.Expression;
    try testing.expect(expr == .FuncCall);
    const func_call = expr.FuncCall;

    // Check function name
    try testing.expect(func_call.func.* == .Variable);
    try testing.expectEqualStrings("print", func_call.func.*.Variable.name);

    // Check argument (which is a function call)
    try testing.expectEqual(@as(usize, 1), func_call.args.items.len);
    try testing.expect(func_call.args.items[0].* == .FuncCall);

    const nested_call = func_call.args.items[0].*.FuncCall;
    try testing.expect(nested_call.func.* == .Variable);
    try testing.expectEqualStrings("+", nested_call.func.*.Variable.name);

    // Check the arguments to the + operation
    try testing.expectEqual(@as(usize, 2), nested_call.args.items.len);
    try testing.expect(nested_call.args.items[0].* == .Literal);
    try testing.expectEqual(@as(i64, 1), nested_call.args.items[0].*.Literal.value.Int);
    try testing.expect(nested_call.args.items[1].* == .Literal);
    try testing.expectEqual(@as(i64, 2), nested_call.args.items[1].*.Literal.value.Int);
}

test "parse infix notation" {
    const allocator = std.testing.allocator;

    const source = "(print (1 + 2))";
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

    // Verify the AST structure
    const expr = node.Expression;
    try testing.expect(expr == .FuncCall);
    const func_call = expr.FuncCall;

    // Check function name
    try testing.expect(func_call.func.* == .Variable);
    try testing.expectEqualStrings("print", func_call.func.*.Variable.name);

    // Check argument (which should be a binary operation)
    try testing.expectEqual(@as(usize, 1), func_call.args.items.len);
    try testing.expect(func_call.args.items[0].* == .BinaryOp);

    const binary_op = func_call.args.items[0].*.BinaryOp;
    try testing.expectEqualStrings("+", binary_op.op.Ident);

    // Check the operands
    try testing.expect(binary_op.left.* == .Literal);
    try testing.expectEqual(@as(i64, 1), binary_op.left.*.Literal.value.Int);
    try testing.expect(binary_op.right.* == .Literal);
    try testing.expectEqual(@as(i64, 2), binary_op.right.*.Literal.value.Int);
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
