const std = @import("std");
const ast = @import("ast.zig");
const debug = @import("../core/debug.zig");

pub const ParserError = error{
    UnexpectedEOF,
    UnterminatedString,
    ExpectedVariableName,
    ExpectedEquals,
    ExpectedRParen,
    UnexpectedRParen,
    ExpectedElseKeyword, // Added for if expressions
    ExpectedFunctionName,
    ExpectedParameterName,
    InvalidTypeAnnotation,
    ExpectedRBracket,
    MaxRecursionDepthExceeded,
    UnexpectedToken,
    EmptyExpression,
    InvalidExpression,
    OutOfMemory,
    ExpectedClassName,
    ExpectedPropertyName,
    ExpectedMethodName,
    ExpectedMethodBody,
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
    LBrace, // New: for map literals
    RBrace, // New: for map literals
    Equals, // Only for var assignment
    If, // Keyword
    Else, // Keyword
    Var, // Keyword
    Fn, // Keyword
    Do, // New: for do blocks
    Class, // New: for class definitions
    New, // New: for object instantiation
    Dot, // New: for field/method access
};

/// Tokenize a Gene source string.
///
/// Ownership: The caller is responsible for freeing the returned tokens using `tokens.deinit()`.
/// If using an arena allocator, the tokens will be freed when the arena is freed.
///
/// Args:
///   allocator: The allocator to use for allocating the tokens.
///   source: The source string to tokenize.
///
/// Returns: An ArrayList of tokens.
fn tokenize(allocator: std.mem.Allocator, source: []const u8) !std.ArrayList(Token) {
    var tokens = std.ArrayList(Token).init(allocator);
    errdefer { // Ensure tokens are cleaned up on tokenizer error
        for (tokens.items) |*token| {
            switch (token.kind) {
                .String => |str| allocator.free(str),
                // .Ident => |ident| allocator.free(ident), // Don't free Ident slices from source
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
                const token = Token{ .kind = .LParen, .loc = i };
                try tokens.append(token);
                i += 1;
                continue;
            },
            ')' => {
                const token = Token{ .kind = .RParen, .loc = i };
                try tokens.append(token);
                i += 1;
                continue;
            },
            '[' => {
                try tokens.append(.{ .kind = .LBracket, .loc = i });
                i += 1;
                continue;
            },
            ']' => {
                try tokens.append(.{ .kind = .RBracket, .loc = i });
                i += 1;
                continue;
            },
            '{' => {
                try tokens.append(.{ .kind = .LBrace, .loc = i });
                i += 1;
                continue;
            },
            '}' => {
                try tokens.append(.{ .kind = .RBrace, .loc = i });
                i += 1;
                continue;
            },
            '=' => {
                // Check for == operator
                if (i + 1 < source_to_parse.len and source_to_parse[i + 1] == '=') {
                    // Create an identifier token for ==
                    const op_str = source_to_parse[i .. i + 2];
                    try tokens.append(.{ .kind = .{ .Ident = op_str }, .loc = i });
                    i += 2;
                } else {
                    try tokens.append(.{ .kind = .Equals, .loc = i });
                    i += 1;
                }
                continue;
            },
            '.' => {
                try tokens.append(.{ .kind = .Dot, .loc = i });
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
            // Allocate string here
            const str = try allocator.dupe(u8, source_to_parse[start..i]);
            try tokens.append(.{ .kind = .{ .String = str }, .loc = start - 1 });
            i += 1; // Move past closing quote
            continue;
        }

        // Handle identifiers and keywords (including operators)
        const ident_chars = "_!$%&*+-/:<=>?@^~"; // Removed '.' from here as it's now a separate token
        if (std.ascii.isAlphabetic(c) or std.mem.indexOfScalar(u8, ident_chars, c) != null) {
            const start = i;
            while (i + 1 < source_to_parse.len and (std.ascii.isAlphanumeric(source_to_parse[i + 1]) or std.mem.indexOfScalar(u8, ident_chars, source_to_parse[i + 1]) != null)) : (i += 1) {}
            const word = source_to_parse[start .. i + 1];
            var token_kind: TokenKind = undefined;

            // Don't allocate Ident tokens here, let the parser handle ownership if needed
            if (std.mem.eql(u8, word, "if")) token_kind = .If else if (std.mem.eql(u8, word, "else")) token_kind = .Else else if (std.mem.eql(u8, word, "var")) token_kind = .Var else if (std.mem.eql(u8, word, "fn")) token_kind = .Fn else if (std.mem.eql(u8, word, "do")) token_kind = .Do // New keyword
            else if (std.mem.eql(u8, word, "class")) token_kind = .Class // New keyword
            else if (std.mem.eql(u8, word, "new")) token_kind = .New // New keyword
            else if (std.mem.eql(u8, word, "true")) token_kind = .{ .Bool = true } else if (std.mem.eql(u8, word, "false")) token_kind = .{ .Bool = false } else {
                // Store the slice directly, parser will dupe if needed
                token_kind = .{ .Ident = word };
            }

            try tokens.append(.{ .kind = token_kind, .loc = start });
            i += 1;
            continue;
        }

        debug.log("Skipping unknown character: {c}", .{c});
        i += 1;
    }
    return tokens;
}

// --- Parser ---
const ParseResult = struct {
    node: ast.AstNode,
    consumed: usize,
};

const MAX_RECURSION_DEPTH = 50;

// Forward declarations removed

fn isBinaryOperator(op_str: []const u8) bool {
    return std.mem.eql(u8, op_str, "+") or
        std.mem.eql(u8, op_str, "-") or
        std.mem.eql(u8, op_str, "*") or
        std.mem.eql(u8, op_str, "/") or
        std.mem.eql(u8, op_str, "<") or
        std.mem.eql(u8, op_str, ">") or
        std.mem.eql(u8, op_str, "<=") or
        std.mem.eql(u8, op_str, ">=") or
        std.mem.eql(u8, op_str, "==") or
        std.mem.eql(u8, op_str, "!=");
}

/// Parse a Gene source string into an AST.
///
/// This function uses an arena allocator internally for all AST nodes.
/// The caller is responsible for freeing the returned nodes and the arena.
///
/// Example:
/// ```
/// var parse_result = try parser.parseGeneSource(allocator, source);
/// defer {
///     parse_result.arena.deinit();
/// }
/// ```
pub const ParseSourceResult = struct {
    arena: *std.heap.ArenaAllocator,
    nodes: []ast.AstNode,
};

/// Parse Gene source code using an arena allocator and return the arena
/// along with the parsed nodes.
pub fn parseGeneSource(parent_allocator: std.mem.Allocator, source: []const u8) !ParseSourceResult {
    // Create an arena allocator for all AST allocations
    const arena = try parent_allocator.create(std.heap.ArenaAllocator);
    arena.* = std.heap.ArenaAllocator.init(parent_allocator);
    errdefer {
        arena.deinit();
        parent_allocator.destroy(arena);
    }

    const arena_allocator = arena.allocator();

    // Use the arena allocator for tokens as well
    var tokens = try tokenize(arena_allocator, source);
    // No need to defer cleanup for tokens since they'll be freed with the arena

    // Use arena allocator for everything to avoid mixing allocators
    var result_nodes = std.ArrayList(ast.AstNode).init(arena_allocator);
    errdefer {
        // No need to deinit individual nodes since the arena will be freed
        result_nodes.deinit();
    }

    var pos: usize = 0;
    while (pos < tokens.items.len) {
        // Skip any unexpected RParen tokens
        if (tokens.items[pos].kind == .RParen) {
            pos += 1;
            continue;
        }

        const result = parseExpression(arena_allocator, tokens.items[pos..], 0) catch |err| {
            if (err == error.UnexpectedRParen) {
                // Skip the RParen and continue
                pos += 1;
                continue;
            } else {
                return err;
            }
        };
        // Don't add errdefer here, ownership is transferred to result_nodes
        try result_nodes.append(result.node); // Transfer ownership
        pos += result.consumed;
    }

    debug.log("parseGeneSource: about to return with {} nodes", .{result_nodes.items.len});
    debug.log("parseGeneSource: returning successfully", .{});

    // Store the result in the arena and return arena pointer
    debug.log("parseGeneSource: parsed {} nodes successfully", .{result_nodes.items.len});

    // Convert ArrayList to slice and store in arena
    const nodes_slice = try arena_allocator.alloc(ast.AstNode, result_nodes.items.len);
    for (result_nodes.items, 0..) |node, i| {
        nodes_slice[i] = node;
    }

    debug.log("parseGeneSource: returning arena and nodes", .{});
    return ParseSourceResult{
        .arena = arena,
        .nodes = nodes_slice,
    };
}

/// Parse a Gene expression.
///
/// Ownership: The caller is responsible for freeing the returned node using `node.deinit(allocator)`
/// unless the node is transferred to another data structure.
///
/// Args:
///   alloc: The allocator to use for allocating the AST nodes.
///   toks: The tokens to parse.
///   depth: The current recursion depth.
///
/// Returns: A ParseResult containing the parsed node and the number of tokens consumed.
fn parseExpression(alloc: std.mem.Allocator, toks: []const Token, depth: usize) ParserError!ParseResult {
    if (depth > MAX_RECURSION_DEPTH) return error.MaxRecursionDepthExceeded;
    if (toks.len == 0) return error.EmptyExpression;
    if (toks[0].kind == .LParen and toks.len > 1) {
        debug.log("  Next token: {any}", .{toks[1]});
    }

    const tok = toks[0];
    return switch (tok.kind) {
        .Int => |val| .{ .node = .{ .Expression = .{ .Literal = .{ .value = .{ .Int = val } } } }, .consumed = 1 },
        .Bool => |val| .{ .node = .{ .Expression = .{ .Literal = .{ .value = .{ .Bool = val } } } }, .consumed = 1 },
        .String => |str| {
            // String tokens are already allocated by tokenizer
            return .{ .node = .{ .Expression = .{ .Literal = .{ .value = .{ .String = str } } } }, .consumed = 1 };
        },
        .Ident => |ident| {
            // Debug print for specific identifiers
            const ops_to_debug = [_][]const u8{ "+", "-", "*", "/", "<", ">", "==" };
            for (ops_to_debug) |op_str| {
                if (std.mem.eql(u8, ident, op_str)) {
                    std.debug.print("Parser: Creating Variable from Ident: '{s}'\n", .{ident});
                    break;
                }
            }
            // Duplicate the identifier string here for the Variable node
            const name_copy = try alloc.dupe(u8, ident);
            // Arena allocator handles cleanup
            return .{ .node = .{ .Expression = .{ .Variable = .{ .name = name_copy } } }, .consumed = 1 };
        },
        .LParen => {
            // Ensure there's a token after LParen to check its kind
            if (toks.len < 2) {
                // This handles `()` which parseList turns into Nil, or `(` which is UnexpectedEOF.
                return parseList(alloc, toks, depth + 1);
            }
            // Dispatch based on the token *after* LParen
            switch (toks[1].kind) {
                .Fn => return parseFn(alloc, toks, depth + 1),
                .If => return parseIf(alloc, toks, depth + 1),
                .Var => return parseVar(alloc, toks, depth + 1),
                .Do => return parseDoBlock(alloc, toks, depth + 1),
                .Class => return parseClass(alloc, toks, depth + 1),
                .New => return parseNew(alloc, toks, depth + 1),
                .Ident => {
                    // Per instructions, assume (Ident ...) is a call for now.
                    // parseList would handle (Ident op Expr) if it were to receive it.
                    return parseCall(alloc, toks, depth + 1);
                },
                else => return parseList(alloc, toks, depth + 1), // Fallback for other S-expressions e.g. ((...)) or (Literal ...)
            }
        },
        .LBracket => parseArray(alloc, toks, depth + 1),
        .LBrace => parseMap(alloc, toks, depth + 1),
        .Dot => {
            // TODO: Implement proper dot/method access parsing
            // For now, treat as a symbol literal
            const dot_symbol = try alloc.dupe(u8, ".");
            return .{ .node = .{ .Expression = .{ .Literal = .{ .value = .{ .Symbol = dot_symbol } } } }, .consumed = 1 };
        },
        .RParen => error.UnexpectedRParen,
        else => error.InvalidExpression,
    };
}

/// Parse a Gene array literal.
///
/// Args:
///   alloc: The allocator to use for allocating the AST nodes.
///   toks: The tokens to parse.
///   depth: The current recursion depth.
///
/// Returns: A ParseResult containing the parsed node and the number of tokens consumed.
fn parseArray(alloc: std.mem.Allocator, toks: []const Token, depth: usize) !ParseResult {
    if (depth > MAX_RECURSION_DEPTH) return error.MaxRecursionDepthExceeded;
    if (toks.len == 0 or toks[0].kind != .LBracket) return error.UnexpectedToken;

    var current_pos: usize = 1; // Skip '['
    var elements = std.ArrayList(*ast.Expression).init(alloc);

    while (current_pos < toks.len and toks[current_pos].kind != .RBracket) {
        const element_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
        current_pos += element_result.consumed;

        const element_ptr = try alloc.create(ast.Expression);
        element_ptr.* = element_result.node.Expression;
        try elements.append(element_ptr);
    }

    if (current_pos >= toks.len or toks[current_pos].kind != .RBracket) {
        return error.ExpectedRBracket;
    }
    current_pos += 1; // Skip ']'

    const elements_slice = try alloc.alloc(*ast.Expression, elements.items.len);
    for (elements.items, 0..) |element, i| {
        elements_slice[i] = element;
    }

    return .{
        .node = .{ .Expression = .{ .ArrayLiteral = .{ .elements = elements_slice } } },
        .consumed = current_pos,
    };
}

/// Parse a Gene map literal.
///
/// Args:
///   alloc: The allocator to use for allocating the AST nodes.
///   toks: The tokens to parse.
///   depth: The current recursion depth.
///
/// Returns: A ParseResult containing the parsed node and the number of tokens consumed.
fn parseMap(alloc: std.mem.Allocator, toks: []const Token, depth: usize) !ParseResult {
    if (depth > MAX_RECURSION_DEPTH) return error.MaxRecursionDepthExceeded;
    if (toks.len == 0 or toks[0].kind != .LBrace) return error.UnexpectedToken;

    var current_pos: usize = 1; // Skip '{'
    var entries = std.ArrayList(ast.MapEntry).init(alloc);

    while (current_pos < toks.len and toks[current_pos].kind != .RBrace) {
        // Parse key (expression)
        const key_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
        current_pos += key_result.consumed;

        // Parse value (expression)
        const value_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
        current_pos += value_result.consumed;

        const key_ptr = try alloc.create(ast.Expression);
        key_ptr.* = key_result.node.Expression;

        const value_ptr = try alloc.create(ast.Expression);
        value_ptr.* = value_result.node.Expression;

        try entries.append(.{ .key = key_ptr, .value = value_ptr });
    }

    if (current_pos >= toks.len or toks[current_pos].kind != .RBrace) {
        return error.ExpectedRParen; // Re-using ExpectedRParen for RBrace for now
    }
    current_pos += 1; // Skip '}'

    const entries_slice = try alloc.alloc(ast.MapEntry, entries.items.len);
    for (entries.items, 0..) |entry, i| {
        entries_slice[i] = entry;
    }

    return .{
        .node = .{ .Expression = .{ .MapLiteral = .{ .entries = entries_slice } } },
        .consumed = current_pos,
    };
}

/// Parse a Gene list expression (content within parentheses).
///
/// Ownership: The caller is responsible for freeing the returned node using `node.deinit(allocator)`
/// unless the node is transferred to another data structure.
///
/// Args:
///   alloc: The allocator to use for allocating the AST nodes.
///   toks: The tokens to parse.
///   depth: The current recursion depth.
///
/// Returns: A ParseResult containing the parsed node and the number of tokens consumed.
fn parseList(alloc: std.mem.Allocator, toks: []const Token, depth: usize) !ParseResult {
    debug.log("parseList: depth={}", .{depth});
    if (toks[0].kind != .LParen) return error.UnexpectedToken;
    if (toks.len < 2) return error.UnexpectedEOF;

    const current_pos: usize = 1;

    if (toks[current_pos].kind == .RParen) {
        return .{ .node = .{ .Expression = .{ .Literal = .{ .value = .Nil } } }, .consumed = 2 };
    }

    const first = toks[current_pos]; // This is toks[1]
    const current_pos_after_first = current_pos + 1; // This is 2, points after `first`

    // Special case for nested expressions like ((fib (n - 1)) + (fib (n - 2)))
    // or function calls like ((get-adder) 5)
    if (first.kind == .LParen) {
        // `first` is LParen, so `toks` is `( (L) ... )`
        // `current_pos` (which is 1) points to the inner LParen.
        const first_sexpr_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
        // `pos_after_first_sexpr` is index relative to `toks` pointing after the first S-expression `(L)`
        const pos_after_first_sexpr = current_pos + first_sexpr_result.consumed;

        // Check for infix binary operator: ((L) op R)
        if (pos_after_first_sexpr < toks.len and toks[pos_after_first_sexpr].kind == .Ident) {
            const op_token = toks[pos_after_first_sexpr];
            if (op_token.kind == .Ident) { // Should always be true if kind is .Ident
                const op_str = op_token.kind.Ident;
                if (isBinaryOperator(op_str)) {
                    var current_pos_for_binop_rhs = pos_after_first_sexpr + 1; // Skip operator

                    if (current_pos_for_binop_rhs >= toks.len) return error.UnexpectedEOF;
                    const right_result = try parseExpression(alloc, toks[current_pos_for_binop_rhs..], depth + 1);
                    current_pos_for_binop_rhs += right_result.consumed;

                    if (current_pos_for_binop_rhs >= toks.len or toks[current_pos_for_binop_rhs].kind != .RParen) return error.ExpectedRParen;

                    const left_ptr = try alloc.create(ast.Expression);
                    left_ptr.* = try first_sexpr_result.node.Expression.clone(alloc);
                    const right_ptr = try alloc.create(ast.Expression);
                    right_ptr.* = try right_result.node.Expression.clone(alloc);
                    const op_ident_copy = try alloc.dupe(u8, op_str);

                    return .{
                        .node = .{ .Expression = .{ .BinaryOp = .{ .op = .{ .Ident = op_ident_copy }, .left = left_ptr, .right = right_ptr } } },
                        .consumed = current_pos_for_binop_rhs + 1, // +1 for the closing RParen of the outer list
                    };
                }
            }
        }

        // If not an infix binary op, then first_sexpr_result.node is either:
        // 1. A standalone expression: ((L)) which should evaluate to L
        // 2. A function part of a call: ((L) arg1 ...)
        var args = std.ArrayList(*ast.Expression).init(alloc);
        var arg_parsing_pos = pos_after_first_sexpr;
        while (arg_parsing_pos < toks.len and toks[arg_parsing_pos].kind != .RParen) {
            const arg_result = try parseExpression(alloc, toks[arg_parsing_pos..], depth + 1);
            const arg_ptr = try alloc.create(ast.Expression);
            arg_ptr.* = arg_result.node.Expression;
            try args.append(arg_ptr);
            arg_parsing_pos += arg_result.consumed;
        }

        if (arg_parsing_pos >= toks.len or toks[arg_parsing_pos].kind != .RParen) return error.ExpectedRParen;

        if (args.items.len == 0) { // Case 1: ((L))
            return .{
                .node = first_sexpr_result.node,
                .consumed = arg_parsing_pos + 1,
            };
        } else { // Case 2: ((L) arg1 ...)
            const func_expr_ptr = try alloc.create(ast.Expression);
            func_expr_ptr.* = first_sexpr_result.node.Expression;
            return .{
                .node = .{ .Expression = .{ .FuncCall = .{ .func = func_expr_ptr, .args = args } } },
                .consumed = arg_parsing_pos + 1,
            };
        }
    }

    // Handle (Literal op Expr)
    // `first` is toks[1]. `current_pos_after_first` is 2 (points to token after `first`)
    // Due to parseExpression dispatch, `first` here will not be Fn, If, Var, Do, Ident.
    // So it's likely a literal Int, Bool, String.
    var left_literal_node: ?ast.AstNode = null;

    switch (first.kind) {
        .Int => |val| left_literal_node = .{ .Expression = .{ .Literal = .{ .value = .{ .Int = val } } } },
        .Bool => |val| left_literal_node = .{ .Expression = .{ .Literal = .{ .value = .{ .Bool = val } } } },
        .String => |str| left_literal_node = .{ .Expression = .{ .Literal = .{ .value = .{ .String = str } } } },
        else => {
            // This case implies something like `([ ...)` or `({ ...)` if not caught by parseExpression,
            // or other unhandled token kinds as the first element of a list.
            debug.log("parseList: Expected LParen or Literal after initial LParen, found {any}", .{first});
            return error.InvalidExpression;
        },
    }

    // Check for operator at toks[current_pos_after_first] (i.e., toks[2])
    if (current_pos_after_first < toks.len and toks[current_pos_after_first].kind == .Ident) {
        const op_token = toks[current_pos_after_first];
        const op_str = op_token.kind.Ident;
        if (isBinaryOperator(op_str)) {
            var pos_for_right_expr = current_pos_after_first + 1; // after operator

            if (pos_for_right_expr >= toks.len) return error.UnexpectedEOF;
            const right_result = try parseExpression(alloc, toks[pos_for_right_expr..], depth + 1);
            pos_for_right_expr += right_result.consumed;

            if (pos_for_right_expr >= toks.len or toks[pos_for_right_expr].kind != .RParen) return error.ExpectedRParen;

            const left_ptr = try alloc.create(ast.Expression);
            left_ptr.* = try left_literal_node.?.Expression.clone(alloc); // left_literal_node is guaranteed non-null here
            const right_ptr = try alloc.create(ast.Expression);
            right_ptr.* = try right_result.node.Expression.clone(alloc);
            const op_ident_copy = try alloc.dupe(u8, op_str);

            return .{
                .node = .{ .Expression = .{ .BinaryOp = .{ .op = .{ .Ident = op_ident_copy }, .left = left_ptr, .right = right_ptr } } },
                .consumed = pos_for_right_expr + 1, // +1 for RParen
            };
        }
    }

    // If not ((L)...) and not (Literal op Expr), it might be a generic list of expressions
    // e.g. (1 2 3) or ("a" "b").
    // The current parser doesn't have a generic "List of Expressions" AST node.
    // For now, if it's not a recognized binary op form, it's an error.
    // This maintains consistency with the previous behavior where (Literal) or (Literal Literal) without an op would error.
    debug.log("parseList: List is not a recognized binary op or nested call structure: {any}", .{toks});
    return error.InvalidExpression;
}

/// Parse a Gene do block.
///
/// Args:
///   alloc: The allocator to use for allocating the AST nodes.
///   toks: The tokens to parse.
///   start_pos: The position in the token stream to start parsing from.
///   depth: The current recursion depth.
///
/// Returns: A ParseResult containing the parsed node and the number of tokens consumed.
fn parseDoBlock(alloc: std.mem.Allocator, toks: []const Token, depth: usize) !ParseResult {
    // Expects (do stmt1 stmt2 ...)
    // toks[0] is LParen, toks[1] is Do
    if (toks.len < 2 or toks[0].kind != .LParen or toks[1].kind != .Do) return error.UnexpectedToken; // Minimum is `(do)` which becomes `(do RParen)`
    if (depth > MAX_RECURSION_DEPTH) return error.MaxRecursionDepthExceeded;

    var current_pos: usize = 2; // Skip LParen and Do
    var statements = std.ArrayList(*ast.Expression).init(alloc);
    errdefer statements.deinit(); // Ensure cleanup if an error occurs before transfer

    while (current_pos < toks.len and toks[current_pos].kind != .RParen) {
        const stmt_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
        current_pos += stmt_result.consumed;

        const stmt_ptr = try alloc.create(ast.Expression);
        stmt_ptr.* = stmt_result.node.Expression;
        try statements.append(stmt_ptr); // Ownership of stmt_ptr transferred
    }

    if (current_pos >= toks.len or toks[current_pos].kind != .RParen) {
        return error.ExpectedRParen;
    }
    current_pos += 1; // Consume RParen

    const statements_slice = try statements.toOwnedSlice(); // Transfers ownership

    return .{
        .node = .{ .Expression = .{ .DoBlock = .{ .statements = statements_slice } } },
        .consumed = current_pos, // Total consumed tokens
    };
}

/// Parse a Gene if expression.
///
/// Ownership: The caller is responsible for freeing the returned node using `node.deinit(allocator)`
/// unless the node is transferred to another data structure.
///
/// Args:
///   alloc: The allocator to use for allocating the AST nodes.
///   toks: The tokens to parse.
///   start_pos: The position in the token stream to start parsing from.
///   depth: The current recursion depth.
///
/// Returns: A ParseResult containing the parsed node and the number of tokens consumed.
fn parseIf(alloc: std.mem.Allocator, toks: []const Token, depth: usize) !ParseResult {
    // Expects (if condition then_branch else_branch) or (if condition then_branch else else_branch)
    // toks[0] is LParen, toks[1] is If
    // Minimum valid: (if c t e) -> LParen, If, c, t, e, RParen (6 tokens)
    if (toks.len < 6 or toks[0].kind != .LParen or toks[1].kind != .If) return error.UnexpectedToken;
    if (depth > MAX_RECURSION_DEPTH) return error.MaxRecursionDepthExceeded;

    var current_pos: usize = 2; // Skip LParen and If

    // Parse condition
    if (current_pos >= toks.len) return error.UnexpectedEOF;
    const cond_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
    current_pos += cond_result.consumed;

    // Parse then_branch
    if (current_pos >= toks.len) return error.UnexpectedEOF;
    const then_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
    current_pos += then_result.consumed;

    // Check if there's an explicit 'else' keyword
    var else_result: ParseResult = undefined;
    if (current_pos < toks.len and toks[current_pos].kind == .Else) {
        current_pos += 1; // Consume 'else'

        // Parse else_branch after 'else' keyword
        if (current_pos >= toks.len) return error.UnexpectedEOF;
        else_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
        current_pos += else_result.consumed;
    } else {
        // No 'else' keyword, the next expression is the else branch
        if (current_pos >= toks.len) return error.UnexpectedEOF;
        else_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
        current_pos += else_result.consumed;
    }

    // Expect and consume RParen
    if (current_pos >= toks.len or toks[current_pos].kind != .RParen) return error.ExpectedRParen;
    current_pos += 1; // Consume RParen

    const cond_ptr = try alloc.create(ast.Expression);
    cond_ptr.* = try cond_result.node.Expression.clone(alloc);
    const then_ptr = try alloc.create(ast.Expression);
    then_ptr.* = try then_result.node.Expression.clone(alloc);
    const else_ptr = try alloc.create(ast.Expression);
    else_ptr.* = try else_result.node.Expression.clone(alloc);

    return .{
        .node = .{ .Expression = .{ .If = .{ .condition = cond_ptr, .then_branch = then_ptr, .else_branch = else_ptr } } },
        .consumed = current_pos, // Total consumed tokens
    };
}

/// Parse a Gene variable declaration.
///
/// Ownership: The caller is responsible for freeing the returned node using `node.deinit(allocator)`
/// unless the node is transferred to another data structure.
///
/// Args:
///   alloc: The allocator to use for allocating the AST nodes.
///   toks: The tokens to parse.
///   start_pos: The position in the token stream to start parsing from.
///   depth: The current recursion depth.
///
/// Returns: A ParseResult containing the parsed node and the number of tokens consumed.
fn parseVar(alloc: std.mem.Allocator, toks: []const Token, depth: usize) !ParseResult {
    // Expects (var name = value)
    // toks[0] is LParen, toks[1] is Var
    // Minimum valid: (var x = 1) -> LParen, Var, Ident, Equals, Int, RParen (6 tokens)
    if (toks.len < 6 or toks[0].kind != .LParen or toks[1].kind != .Var) return error.UnexpectedToken;
    if (depth > MAX_RECURSION_DEPTH) return error.MaxRecursionDepthExceeded;

    var current_pos: usize = 2; // Skip LParen and Var

    if (current_pos >= toks.len or toks[current_pos].kind != .Ident) return error.ExpectedVariableName;
    const name_slice = toks[current_pos].kind.Ident;
    current_pos += 1;

    // Make equals sign optional
    if (current_pos < toks.len and toks[current_pos].kind == .Equals) {
        current_pos += 1; // Skip Equals if present
    }

    if (current_pos >= toks.len) return error.UnexpectedEOF;
    const value_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
    current_pos += value_result.consumed;

    // Expect and consume RParen
    if (current_pos >= toks.len or toks[current_pos].kind != .RParen) return error.ExpectedRParen;
    current_pos += 1; // Consume RParen

    const name_copy = try alloc.dupe(u8, name_slice);
    const value_ptr = try alloc.create(ast.Expression);
    value_ptr.* = try value_result.node.Expression.clone(alloc);

    return .{
        .node = .{ .Expression = .{ .VarDecl = .{ .name = name_copy, .value = value_ptr } } },
        .consumed = current_pos, // Total consumed tokens
    };
}

/// Parse a Gene function definition.
///
/// Ownership: The caller is responsible for freeing the returned node using `node.deinit(allocator)`
/// unless the node is transferred to another data structure.
///
/// Args:
///   alloc: The allocator to use for allocating the AST nodes.
///   toks: The tokens to parse.
///   start_pos: The position in the token stream to start parsing from.
///   depth: The current recursion depth.
///
/// Returns: A ParseResult containing the parsed node and the number of tokens consumed.
fn parseFn(alloc: std.mem.Allocator, toks: []const Token, depth: usize) !ParseResult {
    // Expects (fn name? [params]? body)
    // toks[0] is LParen, toks[1] is Fn
    // Minimum: (fn body) -> LParen, Fn, body, RParen (4 tokens)
    // (fn name body) -> 5 tokens
    // (fn [p] body) -> 6 tokens (LParen, Fn, LBracket, Ident, RBracket, body, RParen) - actually more if body is complex
    if (toks.len < 4 or toks[0].kind != .LParen or toks[1].kind != .Fn) return error.UnexpectedToken;
    if (depth > MAX_RECURSION_DEPTH) return error.MaxRecursionDepthExceeded;

    var current_pos: usize = 2; // Skip LParen and Fn

    var name_copy: []const u8 = "";
    if (current_pos < toks.len and toks[current_pos].kind == .Ident) {
        const name_slice = toks[current_pos].kind.Ident;
        name_copy = try alloc.dupe(u8, name_slice);
        current_pos += 1;
    } else {
        name_copy = try alloc.dupe(u8, "anonymous");
    }

    var params_list = std.ArrayList(ast.FuncParam).init(alloc);
    errdefer params_list.deinit(); // Cleanup if error before transfer

    if (current_pos < toks.len and toks[current_pos].kind == .LBracket) {
        current_pos += 1; // Skip '['
        while (current_pos < toks.len and toks[current_pos].kind != .RBracket) {
            if (current_pos >= toks.len or toks[current_pos].kind != .Ident) return error.ExpectedParameterName;
            const param_name_slice = toks[current_pos].kind.Ident;
            const param_name_copy = try alloc.dupe(u8, param_name_slice);
            current_pos += 1;

            // Check if next token is a type annotation (skip it)
            var param_type: ?[]const u8 = null;
            if (current_pos < toks.len and toks[current_pos].kind == .Ident) {
                const potential_type = toks[current_pos].kind.Ident;
                // Common type names to recognize as type annotations
                if (std.mem.eql(u8, potential_type, "int") or 
                    std.mem.eql(u8, potential_type, "float") or 
                    std.mem.eql(u8, potential_type, "string") or 
                    std.mem.eql(u8, potential_type, "bool")) {
                    param_type = try alloc.dupe(u8, potential_type);
                    current_pos += 1; // Skip the type annotation
                }
            }

            try params_list.append(.{ .name = param_name_copy, .param_type = param_type });
        }
        if (current_pos >= toks.len or toks[current_pos].kind != .RBracket) return error.ExpectedRBracket;
        current_pos += 1; // Skip ']'
    }

    if (current_pos >= toks.len) return error.UnexpectedEOF; // Expected body
    const body_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
    current_pos += body_result.consumed;

    if (current_pos >= toks.len or toks[current_pos].kind != .RParen) return error.ExpectedRParen;
    current_pos += 1; // Consume RParen

    // Simplified body_literal extraction for SimpleFuncDef, review if complex bodies are needed for it
    var body_literal_for_simple_fn: i64 = 0;
    if (params_list.items.len == 0) { // Only relevant for SimpleFuncDef
        switch (body_result.node.Expression) {
            .Literal => |lit| switch (lit.value) {
                .Int => |val| body_literal_for_simple_fn = val,
                else => body_literal_for_simple_fn = 42, // Default
            },
            else => body_literal_for_simple_fn = 42, // Default
        }
    }

    const fn_node: ast.AstNode = if (params_list.items.len > 0) blk: {
        const params_slice = try params_list.toOwnedSlice(); // Transfers ownership
        const body_ptr = try alloc.create(ast.Expression);
        body_ptr.* = body_result.node.Expression;
        break :blk .{
            .Expression = .{
                .FuncDef = .{ .name = name_copy, .params = params_slice, .body = body_ptr },
            },
        };
    } else blk: {
        params_list.deinit(); // Not used, deinit explicitly
        break :blk .{
            .Expression = .{
                .SimpleFuncDef = .{ .name = name_copy, .param_count = 0, .body_literal = body_literal_for_simple_fn },
            },
        };
    };

    return .{
        .node = fn_node,
        .consumed = current_pos, // Total consumed tokens
    };
}

/// Parse a Gene function call.
///
/// Ownership: The caller is responsible for freeing the returned node using `node.deinit(allocator)`
/// unless the node is transferred to another data structure.
///
/// Args:
///   alloc: The allocator to use for allocating the AST nodes.
///   toks: The tokens to parse.
///   start_pos: The position in the token stream to start parsing from.
///   depth: The current recursion depth.
///   depth: The current recursion depth.
///
/// Returns: A ParseResult containing the parsed node and the number of tokens consumed.
fn parseCall(alloc: std.mem.Allocator, toks: []const Token, depth: usize) !ParseResult {
    // Expects (Ident arg1 arg2 ...)
    // toks[0] is LParen, toks[1] is Ident (function)
    // Minimum: (f) -> LParen, Ident, RParen (3 tokens)
    if (toks.len < 3 or toks[0].kind != .LParen or toks[1].kind != .Ident) return error.UnexpectedToken;
    if (depth > MAX_RECURSION_DEPTH) return error.MaxRecursionDepthExceeded;

    const func_ident_slice = toks[1].kind.Ident;
    debug.log("parseCall: function={s} depth={}", .{ func_ident_slice, depth });

    var current_pos: usize = 2; // Start parsing arguments from index 2 (after LParen, Ident)
    var args_list = std.ArrayList(*ast.Expression).init(alloc);
    errdefer args_list.deinit(); // Cleanup if error before transfer

    while (current_pos < toks.len and toks[current_pos].kind != .RParen) {
        const arg_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
        current_pos += arg_result.consumed;

        const arg_ptr = try alloc.create(ast.Expression);
        arg_ptr.* = arg_result.node.Expression;
        try args_list.append(arg_ptr); // Ownership of arg_ptr transferred
    }

    if (current_pos >= toks.len or toks[current_pos].kind != .RParen) return error.ExpectedRParen;
    current_pos += 1; // Consume RParen

    const func_name_copy = try alloc.dupe(u8, func_ident_slice);
    const func_expr_ptr = try alloc.create(ast.Expression);
    func_expr_ptr.* = .{ .Variable = .{ .name = func_name_copy } };

    // ArrayList ownership transferred to FuncCall node
    return ParseResult{
        .node = .{ .Expression = .{ .FuncCall = .{ .func = func_expr_ptr, .args = args_list } } },
        .consumed = current_pos, // Total consumed tokens
    };
}

fn parseClass(alloc: std.mem.Allocator, toks: []const Token, depth: usize) !ParseResult {
    // Expects (class ClassName (field1 field2 ...) (method1 ...) (method2 ...))
    // Minimum: (class Name) -> LParen, Class, Ident, RParen (4 tokens)
    if (toks.len < 4 or toks[0].kind != .LParen or toks[1].kind != .Class) return error.UnexpectedToken;
    if (depth > MAX_RECURSION_DEPTH) return error.MaxRecursionDepthExceeded;

    var current_pos: usize = 2; // Skip LParen and Class

    // Get class name
    if (current_pos >= toks.len or toks[current_pos].kind != .Ident) return error.ExpectedClassName;
    const class_name = try alloc.dupe(u8, toks[current_pos].kind.Ident);
    current_pos += 1;

    var fields = std.ArrayList(ast.ClassDef.ClassField).init(alloc);
    errdefer fields.deinit();
    
    var methods = std.ArrayList(ast.ClassDef.ClassMethod).init(alloc);
    errdefer methods.deinit();

    // Parse class body - looking for field and method definitions
    while (current_pos < toks.len and toks[current_pos].kind != .RParen) {
        if (toks[current_pos].kind == .LParen) {
            // Check what kind of definition this is
            if (current_pos + 1 < toks.len) {
                if (toks[current_pos + 1].kind == .Ident and 
                    toks[current_pos + 1].kind.Ident.len > 0 and 
                    toks[current_pos + 1].kind.Ident[0] == '^') {
                    // Field definition like (^x ^y) or (.prop x Int)
                    const field_result = try parseClassFields(alloc, toks[current_pos..], depth + 1);
                    for (field_result.fields) |field| {
                        try fields.append(field);
                    }
                    current_pos += field_result.consumed;
                } else if (toks[current_pos + 1].kind == .Ident) {
                    const ident = toks[current_pos + 1].kind.Ident;
                    if (std.mem.eql(u8, ident, ".fn") or std.mem.eql(u8, ident, "fn")) {
                        // Method definition
                        const method_result = try parseClassMethod(alloc, toks[current_pos..], depth + 1);
                        try methods.append(method_result.method);
                        current_pos += method_result.consumed;
                    } else if (std.mem.eql(u8, ident, ".prop")) {
                        // Property definition with type
                        const field_result = try parseClassProperty(alloc, toks[current_pos..], depth + 1);
                        try fields.append(field_result.field);
                        current_pos += field_result.consumed;
                    } else {
                        // Unknown construct, skip it
                        var paren_count: i32 = 1;
                        var skip_pos: usize = current_pos + 1;
                        while (skip_pos < toks.len and paren_count > 0) {
                            switch (toks[skip_pos].kind) {
                                .LParen => paren_count += 1,
                                .RParen => paren_count -= 1,
                                else => {},
                            }
                            skip_pos += 1;
                        }
                        current_pos = skip_pos;
                    }
                } else {
                    // Skip unknown construct
                    var paren_count: i32 = 1;
                    var skip_pos: usize = current_pos + 1;
                    while (skip_pos < toks.len and paren_count > 0) {
                        switch (toks[skip_pos].kind) {
                            .LParen => paren_count += 1,
                            .RParen => paren_count -= 1,
                            else => {},
                        }
                        skip_pos += 1;
                    }
                    current_pos = skip_pos;
                }
            }
        } else {
            // Skip non-paren tokens
            current_pos += 1;
        }
    }

    if (current_pos >= toks.len or toks[current_pos].kind != .RParen) return error.ExpectedRParen;
    current_pos += 1;

    const fields_slice = try fields.toOwnedSlice();
    const methods_slice = try methods.toOwnedSlice();

    return ParseResult{
        .node = .{ 
            .Expression = .{ 
                .ClassDef = .{
                    .name = class_name,
                    .fields = fields_slice,
                    .methods = methods_slice,
                    .parent_class = null,
                    .traits = &[_][]const u8{},
                }
            }
        },
        .consumed = current_pos,
    };
}

const FieldParseResult = struct {
    fields: []ast.ClassDef.ClassField,
    consumed: usize,
};

fn parseClassFields(alloc: std.mem.Allocator, toks: []const Token, depth: usize) !FieldParseResult {
    // Parse (^field1 ^field2 ...) format
    if (toks.len < 3 or toks[0].kind != .LParen) return error.UnexpectedToken;
    if (depth > MAX_RECURSION_DEPTH) return error.MaxRecursionDepthExceeded;
    
    var fields = std.ArrayList(ast.ClassDef.ClassField).init(alloc);
    errdefer fields.deinit();
    
    var current_pos: usize = 1; // Skip LParen
    
    while (current_pos < toks.len and toks[current_pos].kind != .RParen) {
        if (toks[current_pos].kind == .Ident and 
            toks[current_pos].kind.Ident.len > 0 and 
            toks[current_pos].kind.Ident[0] == '^') {
            // Skip the ^ prefix
            const field_name = try alloc.dupe(u8, toks[current_pos].kind.Ident[1..]);
            try fields.append(.{
                .name = field_name,
                .type_annotation = null,
                .default_value = null,
                .is_public = true,
            });
            current_pos += 1;
        } else {
            // Skip unexpected tokens
            current_pos += 1;
        }
    }
    
    if (current_pos >= toks.len or toks[current_pos].kind != .RParen) return error.ExpectedRParen;
    current_pos += 1;
    
    return FieldParseResult{
        .fields = try fields.toOwnedSlice(),
        .consumed = current_pos,
    };
}

const PropertyParseResult = struct {
    field: ast.ClassDef.ClassField,
    consumed: usize,
};

fn parseClassProperty(alloc: std.mem.Allocator, toks: []const Token, depth: usize) !PropertyParseResult {
    // Parse (.prop name Type) format
    if (toks.len < 5 or toks[0].kind != .LParen) return error.UnexpectedToken;
    if (depth > MAX_RECURSION_DEPTH) return error.MaxRecursionDepthExceeded;
    
    var current_pos: usize = 2; // Skip LParen and .prop
    
    if (current_pos >= toks.len or toks[current_pos].kind != .Ident) return error.ExpectedPropertyName;
    const prop_name = try alloc.dupe(u8, toks[current_pos].kind.Ident);
    current_pos += 1;
    
    var type_annotation: ?[]const u8 = null;
    if (current_pos < toks.len and toks[current_pos].kind == .Ident) {
        type_annotation = try alloc.dupe(u8, toks[current_pos].kind.Ident);
        current_pos += 1;
    }
    
    if (current_pos >= toks.len or toks[current_pos].kind != .RParen) return error.ExpectedRParen;
    current_pos += 1;
    
    return PropertyParseResult{
        .field = .{
            .name = prop_name,
            .type_annotation = type_annotation,
            .default_value = null,
            .is_public = true,
        },
        .consumed = current_pos,
    };
}

const MethodParseResult = struct {
    method: ast.ClassDef.ClassMethod,
    consumed: usize,
};

fn parseClassMethod(alloc: std.mem.Allocator, toks: []const Token, depth: usize) !MethodParseResult {
    // Parse (.fn name [params] body) format
    if (toks.len < 5 or toks[0].kind != .LParen) return error.UnexpectedToken;
    if (depth > MAX_RECURSION_DEPTH) return error.MaxRecursionDepthExceeded;
    
    var current_pos: usize = 2; // Skip LParen and .fn
    
    // Get method name
    if (current_pos >= toks.len or toks[current_pos].kind != .Ident) return error.ExpectedMethodName;
    const method_name = try alloc.dupe(u8, toks[current_pos].kind.Ident);
    current_pos += 1;
    
    // Parse parameters
    var params = std.ArrayList(ast.ClassDef.Parameter).init(alloc);
    errdefer params.deinit();
    
    if (current_pos < toks.len and toks[current_pos].kind == .LBracket) {
        current_pos += 1; // Skip '['
        while (current_pos < toks.len and toks[current_pos].kind != .RBracket) {
            if (toks[current_pos].kind != .Ident) return error.ExpectedParameterName;
            const param_name = try alloc.dupe(u8, toks[current_pos].kind.Ident);
            current_pos += 1;
            
            var param_type: ?[]const u8 = null;
            if (current_pos < toks.len and toks[current_pos].kind == .Ident) {
                // Check if it looks like a type
                const potential_type = toks[current_pos].kind.Ident;
                if (std.mem.eql(u8, potential_type, "Int") or 
                    std.mem.eql(u8, potential_type, "Float") or 
                    std.mem.eql(u8, potential_type, "String") or 
                    std.mem.eql(u8, potential_type, "Bool")) {
                    param_type = try alloc.dupe(u8, potential_type);
                    current_pos += 1;
                }
            }
            
            try params.append(.{
                .name = param_name,
                .type_annotation = param_type,
                .default_value = null,
            });
        }
        if (current_pos >= toks.len or toks[current_pos].kind != .RBracket) return error.ExpectedRBracket;
        current_pos += 1; // Skip ']'
    }
    
    // Parse body
    if (current_pos >= toks.len) return error.ExpectedMethodBody;
    const body_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
    current_pos += body_result.consumed;
    
    const body_ptr = try alloc.create(ast.Expression);
    body_ptr.* = try body_result.node.Expression.clone(alloc);
    
    if (current_pos >= toks.len or toks[current_pos].kind != .RParen) return error.ExpectedRParen;
    current_pos += 1;
    
    return MethodParseResult{
        .method = .{
            .name = method_name,
            .params = try params.toOwnedSlice(),
            .return_type = null,
            .body = body_ptr,
            .is_public = true,
            .is_virtual = false,
            .is_abstract = false,
            .is_static = false,
        },
        .consumed = current_pos,
    };
}

fn parseNew(alloc: std.mem.Allocator, toks: []const Token, depth: usize) !ParseResult {
    // Expects (new ClassName arg1 arg2 ...)
    // Minimum: (new ClassName) -> LParen, New, Ident, RParen (4 tokens)
    if (toks.len < 4 or toks[0].kind != .LParen or toks[1].kind != .New) return error.UnexpectedToken;
    if (depth > MAX_RECURSION_DEPTH) return error.MaxRecursionDepthExceeded;

    var current_pos: usize = 2; // Skip LParen and New

    // Get class name
    if (current_pos >= toks.len or toks[current_pos].kind != .Ident) return error.ExpectedClassName;
    const class_name = try alloc.dupe(u8, toks[current_pos].kind.Ident);
    current_pos += 1;

    // Parse constructor arguments
    var args = std.ArrayList(*ast.Expression).init(alloc);
    errdefer args.deinit();

    while (current_pos < toks.len and toks[current_pos].kind != .RParen) {
        const arg_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
        const arg_ptr = try alloc.create(ast.Expression);
        arg_ptr.* = try arg_result.node.Expression.clone(alloc);
        try args.append(arg_ptr);
        current_pos += arg_result.consumed;
    }

    if (current_pos >= toks.len or toks[current_pos].kind != .RParen) return error.ExpectedRParen;
    current_pos += 1;

    return ParseResult{
        .node = .{
            .Expression = .{
                .InstanceCreation = .{
                    .class_name = class_name,
                    .args = args,
                },
            },
        },
        .consumed = current_pos,
    };
}
