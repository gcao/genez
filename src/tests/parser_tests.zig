const std = @import("std");
const parser = @import("../frontend/parser.zig");
const ast = @import("../frontend/ast.zig");
const types = @import("../core/types.zig");
const testing = std.testing;

test "parse string literal" {
    const allocator = std.testing.allocator;

    const source = "(print \"hello\")";
    var parse_result = try parser.parseGeneSource(allocator, source);
    defer {
        // Clean up the arena after we're done with the AST
        parse_result.arena.deinit();
    }

    const ast_nodes = parser.getLastParseNodes() orelse return error.NoAstNodesFound;
    try testing.expectEqual(@as(usize, 1), ast_nodes.len);

    const node = ast_nodes[0];
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
    var parse_result = try parser.parseGeneSource(allocator, source);
    defer {
        // Clean up the arena after we're done with the AST
        parse_result.arena.deinit();
    }

    const ast_nodes = parser.getLastParseNodes() orelse return error.NoAstNodesFound;
    try testing.expectEqual(@as(usize, 1), ast_nodes.len);

    const node = ast_nodes[0];
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
    var parse_result = try parser.parseGeneSource(allocator, source);
    defer {
        // Clean up the arena after we're done with the AST
        parse_result.arena.deinit();
    }

    const ast_nodes = parser.getLastParseNodes() orelse return error.NoAstNodesFound;
    try testing.expectEqual(@as(usize, 1), ast_nodes.len);

    const node = ast_nodes[0];
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
    var parse_result = try parser.parseGeneSource(allocator, source);
    defer {
        // Clean up the arena after we're done with the AST
        parse_result.arena.deinit();
    }

    const ast_nodes = parser.getLastParseNodes() orelse return error.NoAstNodesFound;
    try testing.expectEqual(@as(usize, 1), ast_nodes.len);

    const node = ast_nodes[0];
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
    var parse_result = try parser.parseGeneSource(allocator, source);
    defer {
        // Clean up the arena after we're done with the AST
        parse_result.arena.deinit();
    }

    const ast_nodes = parser.getLastParseNodes() orelse return error.NoAstNodesFound;
    try testing.expectEqual(@as(usize, 1), ast_nodes.len);

    const node = ast_nodes[0];
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
    var parse_result = try parser.parseGeneSource(allocator, source);
    defer {
        // Clean up the arena after we're done with the AST
        parse_result.arena.deinit();
    }

    // Expecting 3 top-level nodes: fn definition, var declaration, print expression
    const ast_nodes = parser.getLastParseNodes() orelse return error.NoAstNodesFound;
    try testing.expectEqual(@as(usize, 3), ast_nodes.len);

    // TODO: Add more specific assertions about the parsed nodes if needed
    // For now, just check the count and that parsing succeeded.
}
