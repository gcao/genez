const std = @import("std");
const ast = @import("ast.zig");
const debug = @import("debug.zig");

pub fn parseGeneSource(allocator: std.mem.Allocator, source: []const u8) ![]ast.AstNode {
    debug.log("Starting to parse Gene source of length {}", .{source.len});
    
    var tokens = std.ArrayList(Token).init(allocator);
    defer {
        for (tokens.items) |*token| {
            switch (token.kind) {
                .String => |str| allocator.free(str),
                .Ident => |ident| allocator.free(ident),
                else => {},
            }
        }
        tokens.deinit();
    }
    
    debug.log("Tokenizing source...", .{});
    
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
            debug.log("Found integer: {}", .{int_val});
            try tokens.append(.{ .kind = .{ .Int = int_val } });
            continue;
        }

        // Handle strings
        if (c == '"') {
            const start: usize = i + 1;
            i += 1;
            while (i < source.len and source[i] != '"') : (i += 1) {}
            if (i >= source.len) return error.UnterminatedString;
            const str = try allocator.dupe(u8, source[start..i]);
            debug.log("Found string: {s}", .{str});
            try tokens.append(.{ .kind = .{ .String = str } });
            continue;
        }

        // Handle identifiers
        if (std.ascii.isAlphabetic(c)) {
            const start: usize = i;
            while (i < source.len and (std.ascii.isAlphabetic(source[i]) or std.ascii.isDigit(source[i]))) : (i += 1) {}
            i -= 1;  // Back up one since loop will increment
            const ident = try allocator.dupe(u8, source[start..i + 1]);
            debug.log("Found identifier: {s}", .{ident});
            try tokens.append(.{ .kind = .{ .Ident = ident } });
            continue;
        }

        // Handle operators and delimiters
        switch (c) {
            '(' => {
                debug.log("Found left paren", .{});
                try tokens.append(.{ .kind = .LParen });
            },
            ')' => {
                debug.log("Found right paren", .{});
                try tokens.append(.{ .kind = .RParen });
            },
            '+' => {
                debug.log("Found plus operator", .{});
                try tokens.append(.{ .kind = .Plus });
            },
            else => {},
        }
    }

    debug.log("Finished tokenizing, found {} tokens", .{tokens.items.len});
    debug.log("Starting to parse tokens into AST", .{});

    const ParseResult = struct {
        nodes: []ast.AstNode,
        consumed: usize,
    };

    const parse = struct {
        fn parseExpr(alloc: std.mem.Allocator, toks: []const Token) !struct { expr: ast.Expression, consumed: usize } {
            if (toks.len == 0) return error.UnexpectedEOF;

            if (toks[0].kind == .LParen) {
                if (toks.len < 4) return error.UnexpectedEOF;

                const left = switch (toks[1].kind) {
                    .Int => |value| ast.Expression{ .Literal = .{ .value = .{ .Int = value } } },
                    else => return error.UnexpectedToken,
                };

                if (toks[2].kind != .Plus) return error.UnexpectedToken;

                const right = switch (toks[3].kind) {
                    .Int => |value| ast.Expression{ .Literal = .{ .value = .{ .Int = value } } },
                    else => return error.UnexpectedToken,
                };

                // Create left expression first
                const left_expr = try alloc.create(ast.Expression);
                errdefer alloc.destroy(left_expr);
                left_expr.* = left;

                // Create right expression
                const right_expr = try alloc.create(ast.Expression);
                errdefer {
                    alloc.destroy(right_expr);
                    left_expr.deinit(alloc);
                    alloc.destroy(left_expr);
                }
                right_expr.* = right;

                // Verify RParen exists before returning
                if (toks.len < 5 or toks[4].kind != .RParen) {
                    // Clean up on error
                    right_expr.deinit(alloc);
                    alloc.destroy(right_expr);
                    left_expr.deinit(alloc);
                    alloc.destroy(left_expr);
                    return error.ExpectedRParen;
                }

                return .{
                    .expr = ast.Expression{ .BinaryOp = .{
                        .op = .Add,
                        .left = left_expr,
                        .right = right_expr,
                    }},
                    .consumed = 5,
                };
            } else {
                const expr = switch (toks[0].kind) {
                    .Int => |value| ast.Expression{ .Literal = .{ .value = .{ .Int = value } } },
                    .String => |value| {
                        const str_copy = try alloc.dupe(u8, value);
                        errdefer alloc.free(str_copy);
                        return .{ .expr = ast.Expression{ .Literal = .{ .value = .{ .String = str_copy } } }, .consumed = 1 };
                    },
                    .Ident => |value| {
                        const name_copy = try alloc.dupe(u8, value);
                        errdefer alloc.free(name_copy);
                        return .{ .expr = ast.Expression{ .Variable = .{ .name = name_copy } }, .consumed = 1 };
                    },
                    else => return error.UnexpectedToken,
                };
                return .{ .expr = expr, .consumed = 1 };
            }
        }

        fn parseTokenList(alloc: std.mem.Allocator, toks: []const Token) !ParseResult {
            var result_nodes = std.ArrayList(ast.AstNode).init(alloc);
            errdefer {
                for (result_nodes.items) |*node| {
                    node.deinit(alloc);
                }
                result_nodes.deinit();
            }

            var pos: usize = 0;
            while (pos < toks.len) : (pos += 1) {
                const token = toks[pos];
                switch (token.kind) {
                    .Int => |value| {
                        try result_nodes.append(.{ .Expression = .{ .Literal = .{ .value = .{ .Int = value } } } });
                    },
                    .String => |value| {
                        const str_copy = try alloc.dupe(u8, value);
                        errdefer alloc.free(str_copy);
                        const str_node = ast.AstNode{ .Expression = .{ .Literal = .{ .value = .{ .String = str_copy } } } };
                        try result_nodes.append(str_node);
                    },
                    .Ident => |value| {
                        const name_copy = try alloc.dupe(u8, value);
                        errdefer alloc.free(name_copy);
                        const var_node = ast.AstNode{ .Expression = .{ .Variable = .{ .name = name_copy } } };
                        try result_nodes.append(var_node);
                    },
                    .LParen => {
                        pos += 1;
                        if (pos >= toks.len) return error.UnexpectedEOF;

                        if (toks[pos].kind == .Ident) {
                            const ident = toks[pos].kind.Ident;
                            if (std.mem.eql(u8, ident, "print")) {
                                // Create print node
                                const print_name = try alloc.dupe(u8, "print");
                                errdefer alloc.free(print_name);
                                const print_node = ast.AstNode{ .Expression = .{ .Variable = .{ .name = print_name } } };
                                try result_nodes.append(print_node);

                                // Parse the expression to print
                                pos += 1;
                                if (pos >= toks.len) return error.UnexpectedEOF;

                                var expr_result = try parseExpr(alloc, toks[pos..]);
                                errdefer expr_result.expr.deinit(alloc);
                                const expr_node = ast.AstNode{ .Expression = expr_result.expr };
                                try result_nodes.append(expr_node);

                                // Update position based on consumed tokens
                                pos += expr_result.consumed;

                                if (pos >= toks.len or toks[pos].kind != .RParen) {
                                    return error.ExpectedRParen;
                                }
                            } else {
                                return error.UnexpectedToken;
                            }
                        } else {
                            var expr_result = try parseExpr(alloc, toks[pos - 1..]);
                            errdefer expr_result.expr.deinit(alloc);
                            const expr_node = ast.AstNode{ .Expression = expr_result.expr };
                            try result_nodes.append(expr_node);

                            // Update position based on consumed tokens
                            pos += expr_result.consumed - 1;
                        }
                    },
                    .RParen => {
                        return ParseResult{
                            .nodes = try result_nodes.toOwnedSlice(),
                            .consumed = pos,
                        };
                    },
                    else => {},
                }
            }

            return ParseResult{
                .nodes = try result_nodes.toOwnedSlice(),
                .consumed = pos,
            };
        }
    }.parseTokenList;

    const result = try parse(allocator, tokens.items);
    errdefer {
        for (result.nodes) |*node| {
            node.deinit(allocator);
        }
        allocator.free(result.nodes);
    }
    return result.nodes;
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
