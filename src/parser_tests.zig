const std = @import("std");
const parser = @import("parser.zig");
const ast = @import("ast.zig");
const types = @import("types.zig");
const testing = std.testing;

test "parse string literal" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "\"hello\"";
    const result = try parser.parseGeneSource(allocator, source);

    try testing.expectEqual(@as(usize, 1), result.len);
    try testing.expect(result[0] == .Expression);
    const expr = result[0].Expression;
    try testing.expect(expr == .Literal);
    const lit = expr.Literal;
    try testing.expect(lit.value == .String);
    try testing.expectEqualStrings("hello", lit.value.String);
}

test "parse integer literal" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "42";
    const result = try parser.parseGeneSource(allocator, source);

    try testing.expectEqual(@as(usize, 1), result.len);
    try testing.expect(result[0] == .Expression);
    const expr = result[0].Expression;
    try testing.expect(expr == .Literal);
    const lit = expr.Literal;
    try testing.expect(lit.value == .Int);
    try testing.expectEqual(@as(i64, 42), lit.value.Int);
}

test "parse addition" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "(1 + 2)";
    const result = try parser.parseGeneSource(allocator, source);

    try testing.expectEqual(@as(usize, 1), result.len);
    try testing.expect(result[0] == .Expression);
    const expr = result[0].Expression;
    try testing.expect(expr == .BinaryOp);
    const bin_op = expr.BinaryOp;
    try testing.expectEqual(ast.BinaryOpType.Add, bin_op.op);

    // Check left operand
    try testing.expect(bin_op.left.* == .Literal);
    const left_lit = bin_op.left.*.Literal;
    try testing.expect(left_lit.value == .Int);
    try testing.expectEqual(@as(i64, 1), left_lit.value.Int);

    // Check right operand
    try testing.expect(bin_op.right.* == .Literal);
    const right_lit = bin_op.right.*.Literal;
    try testing.expect(right_lit.value == .Int);
    try testing.expectEqual(@as(i64, 2), right_lit.value.Int);
}
