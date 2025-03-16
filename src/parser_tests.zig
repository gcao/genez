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
    try testing.expect(node.tag == .Expression);
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
    try testing.expect(node.tag == .Expression);
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
    try testing.expect(node.tag == .Expression);
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
    try testing.expect(node.tag == .Expression); // TODO: expect Function node
}

test "parse if expression" {
    const allocator = testing.allocator;
    const source = "(if true 1)";

    var nodes = try parser.parseGeneSource(allocator, source);
    defer {
        for (nodes.items) |*node| node.deinit(allocator);
        nodes.deinit();
    }

    try testing.expect(nodes.items.len == 1);
    const expr = nodes.items[0].tag.Expression;

    // Verify it's an If expression
    try testing.expect(expr == .If);

    // Check condition
    const cond = expr.If.condition.*;
    try testing.expect(cond == .Literal);
    try testing.expect(cond.Literal.value == .Bool);
    try testing.expect(cond.Literal.value.Bool == true);

    // Check then branch
    const then_branch = expr.If.then_branch.*;
    try testing.expect(then_branch == .Literal);
    try testing.expect(then_branch.Literal.value == .Int);
    try testing.expect(then_branch.Literal.value.Int == 1);
}

test "parse if expression with else" {
    const allocator = testing.allocator;
    const source = "(if (n < 2) 1 else 2)";

    var nodes = try parser.parseGeneSource(allocator, source);
    defer {
        for (nodes.items) |*node| node.deinit(allocator);
        nodes.deinit();
    }

    try testing.expect(nodes.items.len == 1);
    const expr = nodes.items[0].tag.Expression;

    try testing.expect(expr == .If);

    // Check condition is comparison
    const cond = expr.If.condition.*;
    try testing.expect(cond == .BinaryOp);
    try testing.expect(cond.BinaryOp.op == .lt);

    // Verify left operand 'n'
    const left = cond.BinaryOp.left.*;
    try testing.expect(left == .Variable);
    try testing.expect(std.mem.eql(u8, left.Variable.name, "n"));

    // Verify right operand 2
    const right = cond.BinaryOp.right.*;
    try testing.expect(right == .Literal);
    try testing.expect(right.Literal.value.Int == 2);

    // Check then branch
    const then_branch = expr.If.then_branch.*;
    try testing.expect(then_branch == .Literal);
    try testing.expect(then_branch.Literal.value.Int == 1);

    // Check else branch
    const else_branch = expr.If.else_branch.?.*;
    try testing.expect(else_branch == .Literal);
    try testing.expect(else_branch.Literal.value.Int == 2);
}

test "parse recursive fib if expression" {
    const allocator = testing.allocator;
    const source =
        \\(if (n < 2)
        \\  n
        \\else
        \\  ((fib (n - 1)) + (fib (n - 2)))
    ;

    var nodes = try parser.parseGeneSource(allocator, source);
    defer {
        for (nodes.items) |*node| node.deinit(allocator);
        nodes.deinit();
    }

    try testing.expect(nodes.items.len == 1);
    const expr = nodes.items[0].tag.Expression;

    try testing.expect(expr == .If);

    // Verify condition (n < 2)
    const cond = expr.If.condition.*;
    try testing.expect(cond == .BinaryOp);
    try testing.expect(cond.BinaryOp.op == .lt);

    // Check left operand 'n'
    const cond_left = cond.BinaryOp.left.*;
    try testing.expect(cond_left == .Variable);
    try testing.expect(std.mem.eql(u8, cond_left.Variable.name, "n"));

    // Check right operand 2
    const cond_right = cond.BinaryOp.right.*;
    try testing.expect(cond_right == .Literal);
    try testing.expect(cond_right.Literal.value.Int == 2);

    // Verify then branch is just 'n'
    const then_branch = expr.If.then_branch.*;
    try testing.expect(then_branch == .Variable);
    try testing.expect(std.mem.eql(u8, then_branch.Variable.name, "n"));

    // Verify else branch is addition of two fib calls
    const else_expr = expr.If.else_branch.?.*;
    try testing.expect(else_expr == .BinaryOp);
    try testing.expect(else_expr.BinaryOp.op == .add);

    // Verify left fib call (n - 1)
    const left_call = else_expr.BinaryOp.left.*;
    try testing.expect(left_call == .FuncCall);
    try testing.expect(std.mem.eql(u8, left_call.FuncCall.func.Variable.name, "fib"));

    const left_arg = left_call.FuncCall.args.items[0].*;
    try testing.expect(left_arg == .BinaryOp);
    try testing.expect(left_arg.BinaryOp.op == .sub);
    try testing.expect(left_arg.BinaryOp.left.* == .Variable and
        std.mem.eql(u8, left_arg.BinaryOp.left.*.Variable.name, "n"));
    try testing.expect(left_arg.BinaryOp.right.* == .Literal and
        left_arg.BinaryOp.right.*.Literal.value.Int == 1);

    // Verify right fib call (n - 2)
    const right_call = else_expr.BinaryOp.right.*;
    try testing.expect(right_call == .FuncCall);
    try testing.expect(std.mem.eql(u8, right_call.FuncCall.func.Variable.name, "fib"));

    const right_arg = right_call.FuncCall.args.items[0].*;
    try testing.expect(right_arg == .BinaryOp);
    try testing.expect(right_arg.BinaryOp.op == .sub);
    try testing.expect(right_arg.BinaryOp.left.* == .Variable and
        std.mem.eql(u8, right_arg.BinaryOp.left.*.Variable.name, "n"));
    try testing.expect(right_arg.BinaryOp.right.* == .Literal and
        right_arg.BinaryOp.right.*.Literal.value.Int == 2);
}
