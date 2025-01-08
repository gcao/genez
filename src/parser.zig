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
    _ = source; // Acknowledge source parameter until we implement parsing

    // Changed var to const since it's not mutated
    const result = try allocator.alloc(ast.AstNode, 0); // empty for example
    return result;
}
