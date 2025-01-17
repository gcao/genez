const std = @import("std");
const parser = @import("parser.zig");
const ast = @import("ast.zig");

test "basic string parsing" {
    var allocator = std.testing.allocator;
    const source = "\"hello world\"";

    const result = parser.parseGeneSource(&allocator, source) catch @panic("parse error");
    defer allocator.free(result);

    try std.testing.expectEqual(@as(usize, 1), result.len);
    try std.testing.expect(result[0].Stmt.ExprStmt == .StrLit);
    try std.testing.expectEqualStrings("hello world", result[0].Stmt.ExprStmt.StrLit);
}

test "basic identifier parsing" {
    var allocator = std.testing.allocator;
    const source = "test";

    const result = parser.parseGeneSource(&allocator, source) catch @panic("parse error");
    defer allocator.free(result);

    try std.testing.expectEqual(@as(usize, 1), result.len);
    try std.testing.expect(result[0].Stmt.ExprStmt == .Ident);
    try std.testing.expectEqualStrings("test", result[0].Stmt.ExprStmt.Ident);
}

test "print statement parsing" {
    var allocator = std.testing.allocator;
    const source = "print \"hello\"";

    const result = parser.parseGeneSource(&allocator, source) catch @panic("parse error");
    defer allocator.free(result);

    try std.testing.expectEqual(@as(usize, 2), result.len);
    try std.testing.expect(result[0].Stmt.ExprStmt == .Ident);
    try std.testing.expectEqualStrings("print", result[0].Stmt.ExprStmt.Ident);
    try std.testing.expect(result[1].Stmt.ExprStmt == .StrLit);
    try std.testing.expectEqualStrings("hello", result[1].Stmt.ExprStmt.StrLit);
}
