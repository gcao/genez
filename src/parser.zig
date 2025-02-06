const std = @import("std");
const ast = @import("ast.zig");
const debug = @import("debug.zig");

pub fn parseGeneSource(allocator: std.mem.Allocator, source: []const u8) !std.ArrayList(ast.AstNode) {
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
    
    var source_to_parse = source;
    if (std.mem.startsWith(u8, source, "#!")) {
        if (std.mem.indexOf(u8, source, "\n")) |newline_pos| {
            source_to_parse = source[newline_pos + 1..];
        }
    }

    var i: usize = 0;
    while (i < source_to_parse.len) : (i += 1) {
        const c = source_to_parse[i];

        // Skip whitespace
        if (std.ascii.isWhitespace(c)) {
            continue;
        }

        // Handle integers
        if (std.ascii.isDigit(c)) {
            const start: usize = i;
            while (i < source_to_parse.len and std.ascii.isDigit(source_to_parse[i])) : (i += 1) {}
            i -= 1;  // Back up one since loop will increment
            const int_str = source_to_parse[start..i + 1];
            const int_val = try std.fmt.parseInt(i64, int_str, 10);
            debug.log("Found integer: {}", .{int_val});
            try tokens.append(.{ .kind = .{ .Int = int_val } });
            continue;
        }

        // Handle strings
        if (c == '"') {
            const start: usize = i + 1;
            i += 1;
            while (i < source_to_parse.len and source_to_parse[i] != '"') : (i += 1) {}
            if (i >= source_to_parse.len) return error.UnterminatedString;
            const str = try allocator.dupe(u8, source_to_parse[start..i]);
            debug.log("Found string: {s}", .{str});
            try tokens.append(.{ .kind = .{ .String = str } });
            continue;
        }

        // Handle identifiers
        if (std.ascii.isAlphabetic(c)) {
            const start: usize = i;
            while (i < source_to_parse.len and (std.ascii.isAlphabetic(source_to_parse[i]) or std.ascii.isDigit(source_to_parse[i]))) : (i += 1) {}
            i -= 1;  // Back up one since loop will increment
            const ident = try allocator.dupe(u8, source_to_parse[start..i + 1]);
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

    var result_nodes = std.ArrayList(ast.AstNode).init(allocator);
    errdefer {
        for (result_nodes.items) |*node| {
            node.deinit(allocator);
        }
        result_nodes.deinit();
    }

    var pos: usize = 0;
    while (pos < tokens.items.len) : (pos += 1) {
        const tok = tokens.items[pos];
        switch (tok.kind) {
            .LParen => {
                pos += 1;
                if (pos >= tokens.items.len) return error.UnexpectedEOF;

                // Parse function call
                if (tokens.items[pos].kind == .Ident) {
                    const ident = tokens.items[pos].kind.Ident;
                    if (std.mem.eql(u8, ident, "print")) {
                        // Parse the argument to print
                        pos += 1;
                        if (pos >= tokens.items.len) return error.UnexpectedEOF;

                        const parse_result = try parseExpr(allocator, tokens.items[pos..]);
                        pos += parse_result.consumed - 1;  // -1 because loop will increment

                        // Create print node
                        const print_name = try allocator.dupe(u8, "print");
                        errdefer allocator.free(print_name);
                        var print_node = ast.AstNode{ .Expression = .{ .Variable = .{ .name = print_name } } };
                        errdefer print_node.deinit(allocator);

                        // Add the nodes in order: argument, then print
                        try result_nodes.append(.{ .Expression = parse_result.expr });
                        try result_nodes.append(print_node);

                        // Verify RParen exists
                        pos += 1;
                        if (pos >= tokens.items.len or tokens.items[pos].kind != .RParen) {
                            return error.ExpectedRParen;
                        }
                    } else {
                        // Handle other function calls
                        const name = try allocator.dupe(u8, ident);
                        errdefer allocator.free(name);
                        try result_nodes.append(.{ .Expression = .{ .Variable = .{ .name = name } } });
                    }
                }
            },
            else => {},
        }
    }

    return result_nodes;
}

fn parseExpr(alloc: std.mem.Allocator, toks: []const Token) !struct { expr: ast.Expression, consumed: usize } {
    if (toks.len == 0) return error.UnexpectedEOF;

    var pos: usize = 0;
    const tok = toks[pos];

    switch (tok.kind) {
        .Int => |val| {
            return .{
                .expr = .{ .Literal = .{ .value = .{ .Int = val } } },
                .consumed = 1,
            };
        },
        .String => |str| {
            const str_copy = try alloc.dupe(u8, str);
            return .{
                .expr = .{ .Literal = .{ .value = .{ .String = str_copy } } },
                .consumed = 1,
            };
        },
        .LParen => {
            pos += 1;
            if (pos >= toks.len) return error.UnexpectedEOF;

            // Check if this is a binary operation
            if (toks[pos].kind == .Int or toks[pos].kind == .String) {
                const left_result = try parseExpr(alloc, toks[pos..]);
                pos += left_result.consumed;

                if (pos >= toks.len) return error.UnexpectedEOF;

                // Check for operator
                if (toks[pos].kind == .Plus) {
                    pos += 1;
                    if (pos >= toks.len) return error.UnexpectedEOF;

                    const right_result = try parseExpr(alloc, toks[pos..]);
                    pos += right_result.consumed;

                    // Create binary op node
                    const left = try alloc.create(ast.Expression);
                    errdefer alloc.destroy(left);
                    left.* = left_result.expr;

                    const right = try alloc.create(ast.Expression);
                    errdefer alloc.destroy(right);
                    right.* = right_result.expr;

                    // Verify RParen exists
                    if (pos >= toks.len or toks[pos].kind != .RParen) {
                        return error.ExpectedRParen;
                    }

                    return .{
                        .expr = .{ .BinaryOp = .{
                            .op = .add,
                            .left = left,
                            .right = right,
                        } },
                        .consumed = pos + 1,
                    };
                }
            }
        },
        else => {},
    }

    return error.InvalidExpression;
}

fn parseTokenList(alloc: std.mem.Allocator, toks: []const Token) !struct { nodes: []ast.AstNode, consumed: usize } {
    var result_nodes = std.ArrayList(ast.AstNode).init(alloc);
    errdefer {
        for (result_nodes.items) |*node| {
            node.deinit(alloc);
        }
        result_nodes.deinit();
    }

    var pos: usize = 0;
    while (pos < toks.len) : (pos += 1) {
        switch (toks[pos].kind) {
            .LParen => {
                const list_result = try parseTokenList(alloc, toks[pos + 1..]);
                pos += list_result.consumed;

                // Add all nodes from the sublist
                for (list_result.nodes) |node| {
                    try result_nodes.append(node);
                }

                // Free the sublist array (but not its contents, which were moved)
                alloc.free(list_result.nodes);
            },
            .RParen => {
                return .{
                    .nodes = try result_nodes.toOwnedSlice(),
                    .consumed = pos + 1,
                };
            },
            else => {},
        }
    }

    return .{
        .nodes = try result_nodes.toOwnedSlice(),
        .consumed = pos,
    };
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
