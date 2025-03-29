const std = @import("std");
const ast = @import("ast.zig");
const debug = @import("debug.zig");

pub const ParserError = error{
    UnexpectedEOF,
    UnterminatedString,
    ExpectedVariableName,
    ExpectedEquals,
    ExpectedRParen,
    ExpectedFunctionName,
    ExpectedParameterName,
    InvalidTypeAnnotation,
    ExpectedRBracket,
    MaxRecursionDepthExceeded,
    UnexpectedToken,
    EmptyExpression,
    InvalidExpression,
    OutOfMemory,
};

// --- Tokenizer (Mostly unchanged, minor fixes) ---
pub const Token = struct {
    kind: TokenKind,
    loc: usize = 0, // Optional: for error reporting
};

pub const TokenKind = union(enum) {
    Int: i64,
    Bool: bool,
    String: []const u8,
    Ident: []const u8, // Includes operators like +, -, < etc.
    LParen,
    RParen,
    LBracket,
    RBracket,
    Equals, // Only for var assignment
    If, // Keyword
    Else, // Keyword
    Var, // Keyword
    Fn, // Keyword
};

fn tokenize(allocator: std.mem.Allocator, source: []const u8) !std.ArrayList(Token) {
    var tokens = std.ArrayList(Token).init(allocator);
    errdefer { // Ensure tokens are cleaned up on tokenizer error
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
            source_to_parse = source[newline_pos + 1 ..];
        }
    }

    var i: usize = 0;
    while (i < source_to_parse.len) {
        const c = source_to_parse[i];

        // Skip whitespace and comments
        if (std.ascii.isWhitespace(c)) {
            i += 1;
            continue;
        }
        if (c == ';') {
            while (i < source_to_parse.len and source_to_parse[i] != '\n') : (i += 1) {}
            continue;
        }

        // Handle delimiters first
        switch (c) {
            '(' => {
                debug.log("Found left paren", .{});
                try tokens.append(.{ .kind = .LParen, .loc = i });
                i += 1;
                continue;
            },
            ')' => {
                debug.log("Found right paren", .{});
                try tokens.append(.{ .kind = .RParen, .loc = i });
                i += 1;
                continue;
            },
            '[' => {
                debug.log("Found left bracket", .{});
                try tokens.append(.{ .kind = .LBracket, .loc = i });
                i += 1;
                continue;
            },
            ']' => {
                debug.log("Found right bracket", .{});
                try tokens.append(.{ .kind = .RBracket, .loc = i });
                i += 1;
                continue;
            },
            '=' => {
                debug.log("Found equals operator", .{});
                try tokens.append(.{ .kind = .Equals, .loc = i });
                i += 1;
                continue;
            },
            else => {},
        }

        // Handle integers
        if (std.ascii.isDigit(c)) {
            const start = i;
            while (i + 1 < source_to_parse.len and std.ascii.isDigit(source_to_parse[i + 1])) : (i += 1) {}
            const int_str = source_to_parse[start .. i + 1];
            const int_val = std.fmt.parseInt(i64, int_str, 10) catch |err| {
                std.debug.print("Error parsing int '{s}': {any}\n", .{ int_str, err });
                return err;
            };
            debug.log("Found integer: {}", .{int_val});
            try tokens.append(.{ .kind = .{ .Int = int_val }, .loc = start });
            i += 1;
            continue;
        }

        // Handle strings
        if (c == '"') {
            const start = i + 1;
            i += 1;
            while (i < source_to_parse.len and source_to_parse[i] != '"') : (i += 1) {}
            if (i >= source_to_parse.len) return error.UnterminatedString;
            const str = try allocator.dupe(u8, source_to_parse[start..i]);
            debug.log("Found string: {s}", .{str});
            try tokens.append(.{ .kind = .{ .String = str }, .loc = start - 1 }); // Added space
            i += 1; // Move past closing quote
            continue;
        }

        // Handle identifiers and keywords (including operators)
        const ident_chars = "_!$%&*+-./:<=>?@^~";
        if (std.ascii.isAlphabetic(c) or std.mem.indexOfScalar(u8, ident_chars, c) != null) {
            const start = i;
            while (i + 1 < source_to_parse.len and (std.ascii.isAlphanumeric(source_to_parse[i + 1]) or std.mem.indexOfScalar(u8, ident_chars, source_to_parse[i + 1]) != null)) : (i += 1) {}
            const word = source_to_parse[start .. i + 1];
            var token_kind: TokenKind = undefined;

            if (std.mem.eql(u8, word, "if")) token_kind = .If else if (std.mem.eql(u8, word, "else")) token_kind = .Else else if (std.mem.eql(u8, word, "var")) token_kind = .Var else if (std.mem.eql(u8, word, "fn")) token_kind = .Fn else if (std.mem.eql(u8, word, "true")) token_kind = .{ .Bool = true } else if (std.mem.eql(u8, word, "false")) token_kind = .{ .Bool = false } else {
                const ident = try allocator.dupe(u8, word);
                errdefer allocator.free(ident); // Free if append fails
                token_kind = .{ .Ident = ident };
            }
            debug.log("Found identifier/keyword: {s}", .{word});
            try tokens.append(.{ .kind = token_kind, .loc = start });
            i += 1;
            continue;
        }

        debug.log("Skipping unknown character: {c}", .{c});
        i += 1;
    }
    debug.log("Finished tokenizing, found {} tokens", .{tokens.items.len});
    return tokens;
}

// --- Parser ---
const ParseResult = struct {
    node: ast.AstNode,
    consumed: usize,
};

const MAX_RECURSION_DEPTH = 50;

// Forward declarations removed

pub fn parseGeneSource(allocator: std.mem.Allocator, source: []const u8) !std.ArrayList(ast.AstNode) {
    var tokens = try tokenize(allocator, source);
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

    debug.log("Starting to parse tokens into AST", .{});
    var result_nodes = std.ArrayList(ast.AstNode).init(allocator);
    errdefer {
        for (result_nodes.items) |*node| node.deinit(allocator);
        result_nodes.deinit();
    }

    var pos: usize = 0;
    while (pos < tokens.items.len) {
        const result = try parseExpression(allocator, tokens.items[pos..], 0);
        errdefer @constCast(&result.node).deinit(allocator); // Deinit if append fails
        try result_nodes.append(result.node); // Transfer ownership
        pos += result.consumed;
    }

    return result_nodes;
}

// Main recursive parse function
fn parseExpression(alloc: std.mem.Allocator, toks: []const Token, depth: usize) ParserError!ParseResult {
    if (depth > MAX_RECURSION_DEPTH) return error.MaxRecursionDepthExceeded;
    if (toks.len == 0) return error.EmptyExpression;
    debug.log("parseExpression depth={} tokens[0]={any}", .{ depth, toks[0] });

    const tok = toks[0];
    return switch (tok.kind) {
        .Int => |val| .{ .node = .{ .Expression = .{ .Literal = .{ .value = .{ .Int = val } } } }, .consumed = 1 },
        .Bool => |val| .{ .node = .{ .Expression = .{ .Literal = .{ .value = .{ .Bool = val } } } }, .consumed = 1 },
        .String => |str| {
            const str_copy = try alloc.dupe(u8, str);
            errdefer alloc.free(str_copy);
            return .{ .node = .{ .Expression = .{ .Literal = .{ .value = .{ .String = str_copy } } } }, .consumed = 1 };
        },
        .Ident => |ident| {
            const name_copy = try alloc.dupe(u8, ident);
            errdefer alloc.free(name_copy);
            return .{ .node = .{ .Expression = .{ .Variable = .{ .name = name_copy } } }, .consumed = 1 };
        },
        .LParen => parseList(alloc, toks, depth),
        else => error.InvalidExpression,
    };
}

// Parses content within parentheses (...)
fn parseList(alloc: std.mem.Allocator, toks: []const Token, depth: usize) !ParseResult {
    if (toks[0].kind != .LParen) return error.UnexpectedToken;
    if (toks.len < 2) return error.UnexpectedEOF;

    var current_pos: usize = 1;

    if (toks[current_pos].kind == .RParen) {
        return .{ .node = .{ .Expression = .{ .Literal = .{ .value = .Nil } } }, .consumed = 2 };
    }

    const first = toks[current_pos];
    current_pos += 1;

    return switch (first.kind) {
        .If => parseIf(alloc, toks, current_pos, depth),
        .Var => parseVar(alloc, toks, current_pos, depth),
        .Fn => parseFn(alloc, toks, current_pos, depth),
        .Ident => |ident_val| parseCall(alloc, toks, current_pos, depth, ident_val),
        .LParen => parseExpression(alloc, toks[current_pos - 1 ..], depth + 1), // Re-parse from the '(' token
        else => error.InvalidExpression,
    };
}

fn parseIf(alloc: std.mem.Allocator, toks: []const Token, start_pos: usize, depth: usize) !ParseResult {
    var current_pos = start_pos;

    if (current_pos >= toks.len) return error.UnexpectedEOF;
    var cond_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
    errdefer cond_result.node.deinit(alloc); // Use alloc parameter
    current_pos += cond_result.consumed;

    if (current_pos >= toks.len) return error.UnexpectedEOF;
    var then_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
    errdefer then_result.node.deinit(alloc); // Use alloc parameter
    current_pos += then_result.consumed;

    var else_node: ?ast.AstNode = null;
    if (current_pos < toks.len and toks[current_pos].kind == .Else) {
        current_pos += 1;
        if (current_pos >= toks.len) return error.UnexpectedEOF;
        var else_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
        errdefer else_result.node.deinit(alloc); // Use alloc parameter
        current_pos += else_result.consumed;
        else_node = else_result.node;
    }

    if (current_pos >= toks.len or toks[current_pos].kind != .RParen) return error.ExpectedRParen;

    const cond_ptr = try alloc.create(ast.Expression);
    errdefer alloc.destroy(cond_ptr);
    cond_ptr.* = cond_result.node.Expression;

    const then_ptr = try alloc.create(ast.Expression);
    errdefer {
        cond_ptr.deinit(alloc);
        alloc.destroy(cond_ptr);
        alloc.destroy(then_ptr);
    }
    then_ptr.* = then_result.node.Expression;

    var else_ptr: ?*ast.Expression = null;
    if (else_node) |en| {
        else_ptr = try alloc.create(ast.Expression);
        errdefer {
            cond_ptr.deinit(alloc);
            alloc.destroy(cond_ptr);
            then_ptr.deinit(alloc);
            alloc.destroy(then_ptr);
            if (else_ptr) |ep| alloc.destroy(ep);
        }
        else_ptr.?.* = en.Expression;
    }

    return .{
        .node = .{ .Expression = .{ .If = .{ .condition = cond_ptr, .then_branch = then_ptr, .else_branch = else_ptr } } },
        .consumed = current_pos + 1,
    };
}

fn parseVar(alloc: std.mem.Allocator, toks: []const Token, start_pos: usize, depth: usize) !ParseResult {
    var current_pos = start_pos;
    if (current_pos >= toks.len or toks[current_pos].kind != .Ident) return error.ExpectedVariableName;
    const name = toks[current_pos].kind.Ident;
    current_pos += 1;

    if (current_pos >= toks.len or toks[current_pos].kind != .Equals) return error.ExpectedEquals;
    current_pos += 1;

    if (current_pos >= toks.len) return error.UnexpectedEOF;
    var value_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
    errdefer value_result.node.deinit(alloc); // Use alloc parameter
    current_pos += value_result.consumed;

    if (current_pos >= toks.len or toks[current_pos].kind != .RParen) return error.ExpectedRParen;

    const name_copy = try alloc.dupe(u8, name);
    errdefer alloc.free(name_copy);
    const value_ptr = try alloc.create(ast.Expression);
    errdefer alloc.destroy(value_ptr);
    value_ptr.* = value_result.node.Expression;

    return .{
        .node = .{ .Expression = .{ .VarDecl = .{ .name = name_copy, .value = value_ptr } } },
        .consumed = current_pos + 1,
    };
}

fn parseFn(alloc: std.mem.Allocator, toks: []const Token, start_pos: usize, depth: usize) !ParseResult {
    var current_pos = start_pos;
    if (current_pos >= toks.len or toks[current_pos].kind != .Ident) return error.ExpectedFunctionName;
    const name = toks[current_pos].kind.Ident;
    current_pos += 1;

    var params = std.ArrayList(ast.FuncParam).init(alloc);
    errdefer {
        for (params.items) |*p| p.deinit(alloc);
        params.deinit();
    }

    if (current_pos < toks.len and toks[current_pos].kind == .LBracket) {
        current_pos += 1;
        while (current_pos < toks.len and toks[current_pos].kind != .RBracket) {
            if (toks[current_pos].kind != .Ident) return error.ExpectedParameterName;
            const param_name = toks[current_pos].kind.Ident;
            current_pos += 1;
            var param_type: ?[]const u8 = null;
            if (current_pos < toks.len and toks[current_pos].kind == .Ident) {
                param_type = try alloc.dupe(u8, toks[current_pos].kind.Ident);
                errdefer if (param_type) |pt| alloc.free(pt);
                current_pos += 1;
            }
            const param_name_copy = try alloc.dupe(u8, param_name);
            errdefer alloc.free(param_name_copy);
            try params.append(.{ .name = param_name_copy, .param_type = param_type });
        }
        if (current_pos >= toks.len or toks[current_pos].kind != .RBracket) return error.ExpectedRBracket;
        current_pos += 1;
    }

    if (current_pos >= toks.len) return error.UnexpectedEOF;
    var body_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
    errdefer body_result.node.deinit(alloc); // Use alloc parameter
    const final_pos_after_body = current_pos + body_result.consumed;

    const name_copy = try alloc.dupe(u8, name);
    errdefer alloc.free(name_copy);
    const body_ptr = try alloc.create(ast.Expression);
    errdefer alloc.destroy(body_ptr);
    body_ptr.* = body_result.node.Expression;

    return .{
        .node = .{ .Expression = .{ .FuncDef = .{ .name = name_copy, .params = try params.toOwnedSlice(), .body = body_ptr } } },
        .consumed = final_pos_after_body - start_pos,
    };
}

fn parseCall(alloc: std.mem.Allocator, toks: []const Token, start_pos: usize, depth: usize, func_ident: []const u8) !ParseResult {
    var current_pos = start_pos;
    var args = std.ArrayList(*ast.Expression).init(alloc);
    errdefer {
        for (args.items) |arg| {
            arg.deinit(alloc);
            alloc.destroy(arg);
        }
        args.deinit();
    }

    while (current_pos < toks.len and toks[current_pos].kind != .RParen) {
        var arg_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
        errdefer arg_result.node.deinit(alloc); // Use alloc parameter
        current_pos += arg_result.consumed;

        const arg_ptr = try alloc.create(ast.Expression);
        errdefer alloc.destroy(arg_ptr);
        arg_ptr.* = arg_result.node.Expression; // Transfer ownership
        try args.append(arg_ptr);
    }

    if (current_pos >= toks.len or toks[current_pos].kind != .RParen) return error.ExpectedRParen;

    const func_name_copy = try alloc.dupe(u8, func_ident);
    errdefer alloc.free(func_name_copy);
    const func_expr_ptr = try alloc.create(ast.Expression);
    errdefer alloc.destroy(func_expr_ptr);
    func_expr_ptr.* = .{ .Variable = .{ .name = func_name_copy } }; // Transfer ownership

    return .{
        .node = .{ .Expression = .{ .FuncCall = .{ .func = func_expr_ptr, .args = args } } }, // Transfer ownership
        .consumed = current_pos + 1, // Consume final RParen
    };
}
