const std = @import("std");
const ast = @import("ast.zig");

pub fn parseGeneSource(allocator: std.mem.Allocator, source: []const u8) ![]ast.AstNode {
    var tokens = std.ArrayList(Token).init(allocator);
    defer {
        // Free any allocated strings in tokens before deinit
        for (tokens.items) |token| {
            switch (token.kind) {
                .String => |str| allocator.free(str),
                .Ident => |ident| allocator.free(ident),
                else => {},
            }
        }
        tokens.deinit();
    }

    var i: usize = 0;
    while (i < source.len) : (i += 1) {
        const c = source[i];

        // Skip whitespace
        if (std.ascii.isWhitespace(c)) {
            continue;
        }

        // Handle integers
        if (std.ascii.isDigit(c)) {
            const start: usize = i;
            while (i < source.len and std.ascii.isDigit(source[i])) : (i += 1) {}
            i -= 1;  // Back up one since loop will increment
            const int_str = source[start..i + 1];
            const int_val = try std.fmt.parseInt(i64, int_str, 10);
            try tokens.append(.{ .kind = .{ .Int = int_val } });
            continue;
        }

        // Handle strings
        if (c == '"') {
            const start: usize = i + 1;
            i += 1;
            while (i < source.len and source[i] != '"') : (i += 1) {}
            const str = try allocator.dupe(u8, source[start..i]);
            try tokens.append(.{ .kind = .{ .String = str } });
            continue;
        }

        // Handle identifiers
        if (std.ascii.isAlphabetic(c)) {
            const start: usize = i;
            while (i < source.len and (std.ascii.isAlphabetic(source[i]) or std.ascii.isDigit(source[i]))) : (i += 1) {}
            i -= 1;  // Back up one since loop will increment
            const ident = try allocator.dupe(u8, source[start..i + 1]);
            try tokens.append(.{ .kind = .{ .Ident = ident } });
            continue;
        }

        // Handle operators and delimiters
        switch (c) {
            '(' => try tokens.append(.{ .kind = .LParen }),
            ')' => try tokens.append(.{ .kind = .RParen }),
            '+' => try tokens.append(.{ .kind = .Plus }),
            else => {},
        }
    }

    return try parseTokens(allocator, tokens.items);
}

const TokenKind = union(enum) {
    Int: i64,
    String: []const u8,
    Ident: []const u8,
    LParen,
    RParen,
    Plus,
};

const Token = struct {
    kind: TokenKind,
};

fn parseTokens(allocator: std.mem.Allocator, tokens: []const Token) ![]ast.AstNode {
    var nodes = std.ArrayList(ast.AstNode).init(allocator);
    errdefer {
        for (nodes.items) |node| {
            switch (node) {
                .Expression => |expr| switch (expr) {
                    .Literal => |lit| switch (lit.value) {
                        .String => |str| allocator.free(str),
                        else => {},
                    },
                    .BinaryOp => |bin_op| {
                        allocator.destroy(bin_op.left);
                        allocator.destroy(bin_op.right);
                    },
                    .Variable => |var_expr| allocator.free(var_expr.name),
                },
            }
        }
        nodes.deinit();
    }

    var i: usize = 0;
    while (i < tokens.len) {
        const token = tokens[i];
        switch (token.kind) {
            .Int => |value| {
                try nodes.append(.{ .Expression = .{ .Literal = .{ .value = .{ .Int = value } } } });
            },
            .String => |value| {
                const str = try allocator.dupe(u8, value);
                try nodes.append(.{ .Expression = .{ .Literal = .{ .value = .{ .String = str } } } });
            },
            .Ident => |value| {
                const name = try allocator.dupe(u8, value);
                try nodes.append(.{ .Expression = .{ .Variable = .{ .name = name } } });
            },
            .LParen => {
                i += 1;
                if (i >= tokens.len) return error.UnexpectedEOF;

                // Handle (print expr)
                if (tokens[i].kind == .Ident) {
                    const ident = tokens[i].kind.Ident;
                    if (std.mem.eql(u8, ident, "print")) {
                        i += 1;
                        if (i >= tokens.len) return error.UnexpectedEOF;

                        // Parse the expression to print
                        const expr = try parseExpr(allocator, tokens[i..]);
                        i += expr.len;

                        // Create print node
                        const print_name = try allocator.dupe(u8, "print");
                        errdefer allocator.free(print_name);

                        try nodes.append(.{ .Expression = .{ .Variable = .{ .name = print_name } } });
                        try nodes.append(.{ .Expression = expr.value });

                        if (i >= tokens.len or tokens[i].kind != .RParen) return error.ExpectedRParen;
                    } else {
                        // Handle other function calls here
                        return error.UnexpectedToken;
                    }
                } else {
                    // Handle (expr1 + expr2)
                    const left = try parseExpr(allocator, tokens[i..]);
                    i += left.len;
                    if (i >= tokens.len) return error.UnexpectedEOF;

                    const op = tokens[i];
                    i += 1;
                    if (i >= tokens.len) return error.UnexpectedEOF;

                    const right = try parseExpr(allocator, tokens[i..]);
                    i += right.len;
                    if (i >= tokens.len or tokens[i].kind != .RParen) return error.ExpectedRParen;

                    switch (op.kind) {
                        .Plus => {
                            const left_expr = try allocator.create(ast.Expression);
                            errdefer allocator.destroy(left_expr);
                            left_expr.* = left.value;

                            const right_expr = try allocator.create(ast.Expression);
                            errdefer allocator.destroy(right_expr);
                            right_expr.* = right.value;

                            try nodes.append(.{ .Expression = .{ .BinaryOp = .{
                                .op = .Add,
                                .left = left_expr,
                                .right = right_expr,
                            } } });
                        },
                        else => return error.UnexpectedToken,
                    }
                }
            },
            .RParen => return error.UnexpectedToken,
            .Plus => return error.UnexpectedToken,
        }
        i += 1;
    }

    return nodes.toOwnedSlice();
}

const ExprResult = struct {
    value: ast.Expression,
    len: usize,
};

fn parseExpr(allocator: std.mem.Allocator, tokens: []const Token) !ExprResult {
    if (tokens.len == 0) return error.UnexpectedEOF;

    const token = tokens[0];
    switch (token.kind) {
        .Int => |value| {
            return ExprResult{
                .value = .{ .Literal = .{ .value = .{ .Int = value } } },
                .len = 1,
            };
        },
        .String => |value| {
            const str = try allocator.dupe(u8, value);
            return ExprResult{
                .value = .{ .Literal = .{ .value = .{ .String = str } } },
                .len = 1,
            };
        },
        .Ident => |value| {
            const name = try allocator.dupe(u8, value);
            return ExprResult{
                .value = .{ .Variable = .{ .name = name } },
                .len = 1,
            };
        },
        .LParen => {
            if (tokens.len < 4) return error.UnexpectedEOF;
            const left = try parseExpr(allocator, tokens[1..]);
            const op = tokens[1 + left.len];
            if (op.kind != .Plus) return error.UnexpectedToken;
            const right = try parseExpr(allocator, tokens[2 + left.len ..]);
            if (tokens.len < 3 + left.len + right.len or tokens[2 + left.len + right.len].kind != .RParen) return error.ExpectedRParen;

            const left_expr = try allocator.create(ast.Expression);
            errdefer allocator.destroy(left_expr);
            left_expr.* = left.value;

            const right_expr = try allocator.create(ast.Expression);
            errdefer allocator.destroy(right_expr);
            right_expr.* = right.value;

            return ExprResult{
                .value = .{ .BinaryOp = .{
                    .op = .Add,
                    .left = left_expr,
                    .right = right_expr,
                } },
                .len = 3 + left.len + right.len,
            };
        },
        .RParen, .Plus => return error.UnexpectedToken,
    }
}
