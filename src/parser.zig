const std = @import("std");
const ast = @import("ast.zig");

pub const ParseError = error{
    UnexpectedToken,
    IncompleteInput,
    InvalidSyntax,
    InvalidStringLiteral,
    UnknownToken,
    MemoryAllocationFailed,
    OutOfMemory,
};

pub fn parseGeneSource(allocator: *std.mem.Allocator, source: []const u8) ![]ast.AstNode {
    var nodes = std.ArrayList(ast.AstNode).init(allocator.*);
    defer nodes.deinit();

    var in_string = false;
    var current_token = std.ArrayList(u8).init(allocator.*);
    defer current_token.deinit();

    var i: usize = 0;
    while (i < source.len) : (i += 1) {
        const c = source[i];
        if (c == '"') {
            if (in_string) {
                // End of string
                try current_token.append('"');
                const value = current_token.items[1 .. current_token.items.len - 1];
                const value_copy = try allocator.alloc(u8, value.len);
                var index: usize = 0;
                while (index < value.len) : (index += 1) {
                    value_copy[index] = value[index];
                }
                try nodes.append(ast.AstNode{
                    .Stmt = ast.Stmt{
                        .ExprStmt = ast.Expr{
                            .StrLit = value_copy,
                        },
                    },
                });
                current_token = std.ArrayList(u8).init(allocator.*);
                in_string = false;
            } else {
                // Start of string
                if (current_token.items.len > 0) {
                    try nodes.append(ast.AstNode{
                        .Stmt = ast.Stmt{
                            .ExprStmt = ast.Expr{
                                .Ident = try current_token.toOwnedSlice(),
                            },
                        },
                    });
                    current_token = std.ArrayList(u8).init(allocator.*);
                }
                try current_token.append('"');
                in_string = true;
            }
        } else if (in_string) {
            try current_token.append(c);
        } else if (std.mem.indexOfScalar(u8, " \n\r\t", c) != null) {
            if (current_token.items.len > 0) {
                const token = try current_token.toOwnedSlice();
                if (std.mem.eql(u8, token, "print")) {
                    try nodes.append(ast.AstNode{
                        .Stmt = ast.Stmt{
                            .ExprStmt = ast.Expr{
                                .Ident = "print",
                            },
                        },
                    });
                } else {
                    try nodes.append(ast.AstNode{
                        .Stmt = ast.Stmt{
                            .ExprStmt = ast.Expr{
                                .Ident = token,
                            },
                        },
                    });
                }
                current_token = std.ArrayList(u8).init(allocator.*);
            }
        } else {
            try current_token.append(c);
        }
    }

    if (current_token.items.len > 0) {
        const token = try current_token.toOwnedSlice();
        try nodes.append(ast.AstNode{
            .Stmt = ast.Stmt{
                .ExprStmt = ast.Expr{
                    .Ident = token,
                },
            },
        });
    }

    if (nodes.items.len == 0) {
        return error.IncompleteInput;
    }

    return nodes.toOwnedSlice();
}
