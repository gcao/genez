const std = @import("std");
const ast = @import("ast.zig");

pub fn ParseError() type {
    return error{
        UnexpectedToken,
        IncompleteInput,
        // ...
    };
}

pub fn parseGeneSource(allocator: *std.mem.Allocator, source: []const u8) ![]ast.AstNode {
    // A skeleton parser:
    // 1) tokenize or do s-expression parse
    // 2) build ast.AstNode array
    // For example, we'll just produce an empty array for now:

    // In a real parser, you'd do something like:
    // var tokens = tokenize(source) catch return ...
    // var nodes = parseTokens(allocator, tokens) catch return ...
    var result = try allocator.alloc(ast.AstNode, 0); // empty for example
    return result;
}
