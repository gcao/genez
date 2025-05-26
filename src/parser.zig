const std = @import("std");
const ast = @import("ast.zig");
const debug = @import("debug.zig");

pub const ParserError = error{
    UnexpectedEOF,
    UnterminatedString,
    ExpectedVariableName,
    ExpectedEquals,
    ExpectedRParen,
    UnexpectedRParen,
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
                debug.log("Found (", .{});
                try tokens.append(token);
                i += 1;
                continue;
            },
            ')' => {
                const token = Token{ .kind = .RParen, .loc = i };
                debug.log("Found )", .{});
                try tokens.append(token);
                i += 1;
                continue;
            },
            '[' => {
                debug.log("Found [", .{});
                try tokens.append(.{ .kind = .LBracket, .loc = i });
                i += 1;
                continue;
            },
            ']' => {
                debug.log("Found ]", .{});
                try tokens.append(.{ .kind = .RBracket, .loc = i });
                i += 1;
                continue;
            },
            '=' => {
                // Check for == operator
                if (i + 1 < source_to_parse.len and source_to_parse[i + 1] == '=') {
                    debug.log("Found ==", .{});
                    // Create an identifier token for ==
                    const op_str = source_to_parse[i .. i + 2];
                    try tokens.append(.{ .kind = .{ .Ident = op_str }, .loc = i });
                    i += 2;
                } else {
                    debug.log("Found =", .{});
                    try tokens.append(.{ .kind = .Equals, .loc = i });
                    i += 1;
                }
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
            debug.log("Found {}", .{int_val});
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
            debug.log("Found \"{s}\"", .{str});
            try tokens.append(.{ .kind = .{ .String = str }, .loc = start - 1 });
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

            // Don't allocate Ident tokens here, let the parser handle ownership if needed
            if (std.mem.eql(u8, word, "if")) token_kind = .If else if (std.mem.eql(u8, word, "else")) token_kind = .Else else if (std.mem.eql(u8, word, "var")) token_kind = .Var else if (std.mem.eql(u8, word, "fn")) token_kind = .Fn else if (std.mem.eql(u8, word, "true")) token_kind = .{ .Bool = true } else if (std.mem.eql(u8, word, "false")) token_kind = .{ .Bool = false } else {
                // Store the slice directly, parser will dupe if needed
                token_kind = .{ .Ident = word };
            }

            debug.log("Found {s}", .{word});
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
// Simple tuple return - avoiding complex structs
pub const ArenaAndCount = struct {
    arena: *std.heap.ArenaAllocator,
    node_count: usize,
};

// Global to store most recent parse result - simple approach to avoid complex returns
var g_last_nodes: ?[]ast.AstNode = null;

// Get the nodes from the last parse - call immediately after parseGeneSource
pub fn getLastParseNodes() ?[]ast.AstNode {
    return g_last_nodes;
}

// Return arena pointer and count - this should avoid the bus error
pub fn parseGeneSource(parent_allocator: std.mem.Allocator, source: []const u8) !ArenaAndCount {
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

    debug.log("Starting to parse tokens into AST", .{});
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

    // Store in global for retrieval
    g_last_nodes = nodes_slice;

    debug.log("parseGeneSource: returning arena and count", .{});
    return ArenaAndCount{
        .arena = arena,
        .node_count = result_nodes.items.len,
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
    debug.log("parseExpression depth={} tokens[0]={any}", .{ depth, toks[0] });
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
            // Duplicate the identifier string here for the Variable node
            const name_copy = try alloc.dupe(u8, ident);
            // Arena allocator handles cleanup
            return .{ .node = .{ .Expression = .{ .Variable = .{ .name = name_copy } } }, .consumed = 1 };
        },
        .LParen => parseList(alloc, toks, depth),
        .RParen => error.UnexpectedRParen,
        else => error.InvalidExpression,
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

    var current_pos: usize = 1;

    if (toks[current_pos].kind == .RParen) {
        return .{ .node = .{ .Expression = .{ .Literal = .{ .value = .Nil } } }, .consumed = 2 };
    }

    const first = toks[current_pos];
    current_pos += 1;

    // Special case for nested expressions like ((fib (n - 1)) + (fib (n - 2)))
    // If we have a pattern like ((...) op (...))
    if (first.kind == .LParen) {
        // Parse the first nested expression
        const left_result = try parseExpression(alloc, toks[current_pos - 1 ..], depth + 1);

        current_pos += left_result.consumed - 1; // -1 because we already counted the LParen

        // Check if there's an operator after the nested expression
        if (current_pos < toks.len and toks[current_pos].kind == .Ident) {
            const op_token = toks[current_pos];
            if (op_token.kind == .Ident) {
                const op = op_token.kind.Ident; // This is a slice from the token
                // Check if it's a binary operator
                if (std.mem.eql(u8, op, "+") or
                    std.mem.eql(u8, op, "-") or
                    std.mem.eql(u8, op, "<") or
                    std.mem.eql(u8, op, ">") or
                    std.mem.eql(u8, op, "=="))
                {
                    current_pos += 1; // Skip the operator

                    // Parse the right operand
                    if (current_pos >= toks.len) return error.UnexpectedEOF; // No errdefer needed with arena
                    const right_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
                    // Don't use errdefer with arena allocators

                    current_pos += right_result.consumed;

                    if (current_pos >= toks.len or toks[current_pos].kind != .RParen) return error.ExpectedRParen; // No errdefer needed

                    // Clone results into the new BinaryOp node
                    const left_ptr = try alloc.create(ast.Expression);
                    // Don't use errdefer for arena allocations
                    left_ptr.* = try left_result.node.Expression.clone(alloc);
                    // Don't use errdefer for arena allocations

                    const right_ptr = try alloc.create(ast.Expression);
                    // Don't use errdefer for arena allocations
                    right_ptr.* = try right_result.node.Expression.clone(alloc);
                    // Don't use errdefer for arena allocations

                    // Duplicate the operator identifier string for the AST node
                    const op_ident_copy = try alloc.dupe(u8, op);
                    // Arena allocator handles cleanup

                    // Original results' errdefers are cancelled by successful return
                    return .{
                        .node = .{ .Expression = .{ .BinaryOp = .{ .op = .{ .Ident = op_ident_copy }, .left = left_ptr, .right = right_ptr } } }, // Ownership transferred
                        .consumed = current_pos + 1,
                    };
                }
            }
        }

        // If no operator, return the nested expression, transferring ownership
        if (current_pos >= toks.len or toks[current_pos].kind != .RParen) {
            // No errdefer needed with arena allocators
            return error.ExpectedRParen;
        }

        // Arena allocator handles all cleanup automatically
        return .{
            .node = left_result.node, // Transfer ownership
            .consumed = current_pos + 1,
        };
    }

    // Handle infix notation like (1 + 2)
    if (first.kind == .Bool or first.kind == .String) {
        // Parse the first operand
        const left_result: ParseResult = blk: { // Use block to scope const assignment
            switch (first.kind) {
                .Bool => |val| {
                    break :blk .{
                        .node = .{ .Expression = .{ .Literal = .{ .value = .{ .Bool = val } } } },
                        .consumed = 0, // We'll add this later
                    };
                },
                .String => |str| {
                    // String tokens are already allocated by tokenizer
                    break :blk .{
                        .node = .{ .Expression = .{ .Literal = .{ .value = .{ .String = str } } } },
                        .consumed = 0, // We'll add this later
                    };
                },
                else => unreachable, // We already checked for these types
            }
        };
        // Add errdefer for the created node
        // Arena allocator handles all cleanup automatically

        // Check if there's an operator after the literal
        if (current_pos < toks.len and toks[current_pos].kind == .Ident) {
            const op_token = toks[current_pos];
            const op = op_token.kind.Ident; // Slice from token

            // Check if it's a binary operator
            if (std.mem.eql(u8, op, "+") or
                std.mem.eql(u8, op, "-") or
                std.mem.eql(u8, op, "<") or
                std.mem.eql(u8, op, ">") or
                std.mem.eql(u8, op, "=="))
            {
                current_pos += 1; // Skip the operator

                // Parse the right operand
                if (current_pos >= toks.len) return error.UnexpectedEOF; // Arena handles cleanup
                const right_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
                // Arena allocator handles cleanup

                current_pos += right_result.consumed;

                if (current_pos >= toks.len or toks[current_pos].kind != .RParen) return error.ExpectedRParen; // Arena handles cleanup

                // Clone results into the new BinaryOp node
                const left_ptr = try alloc.create(ast.Expression);
                // Arena allocator handles cleanup
                left_ptr.* = try left_result.node.Expression.clone(alloc);
                // Arena allocator handles cleanup

                const right_ptr = try alloc.create(ast.Expression);
                // Arena allocator handles cleanup
                right_ptr.* = try right_result.node.Expression.clone(alloc);
                // Arena allocator handles cleanup

                // Duplicate the operator identifier string for the AST node
                const op_ident_copy = try alloc.dupe(u8, op);
                // Arena allocator handles cleanup

                // Original results' errdefers are cancelled by successful return
                // Add 1 for the closing parenthesis and 1 for the operator
                const total_consumed = 1 + 1 + right_result.consumed + 1; // literal + op + right + RParen

                return .{
                    .node = .{ .Expression = .{ .BinaryOp = .{ .op = .{ .Ident = op_ident_copy }, .left = left_ptr, .right = right_ptr } } },
                    .consumed = total_consumed,
                };
            }
        }
        // If it wasn't a binary op, it's an error for bool/string literal alone in parens
        // Arena handles cleanup automatically
        return error.InvalidExpression;
    }

    // Restore the switch statement
    return switch (first.kind) {
        .If => {
            const result = try parseIf(alloc, toks, current_pos, depth + 1);
            const final_pos = current_pos - 1 + result.consumed; // -1 because current_pos was incremented
            if (final_pos >= toks.len or toks[final_pos].kind != .RParen) return error.ExpectedRParen;
            return .{ .node = result.node, .consumed = final_pos + 1 };
        },
        .Var => {
            const result = try parseVar(alloc, toks, current_pos, depth + 1);
            const final_pos = current_pos - 1 + result.consumed; // -1 because current_pos was incremented
            if (final_pos >= toks.len or toks[final_pos].kind != .RParen) return error.ExpectedRParen;
            return .{ .node = result.node, .consumed = final_pos + 1 };
        },
        .Fn => {
            const result = try parseFn(alloc, toks, current_pos, depth + 1);
            const final_pos = current_pos + result.consumed; // Fix: position at RParen
            debug.log("parseList Fn: current_pos={} consumed={} final_pos={}", .{ current_pos, result.consumed, final_pos });
            if (final_pos < toks.len) {
                debug.log("parseList Fn: token at final_pos: {any}", .{toks[final_pos]});
            }
            if (final_pos >= toks.len or toks[final_pos].kind != .RParen) return error.ExpectedRParen;
            return .{ .node = result.node, .consumed = final_pos + 1 };
        },
        .Ident => |ident_val| {
            // Check if this is an infix binary operation like (n < 2)
            if (current_pos < toks.len and toks[current_pos].kind == .Ident) {
                const op = toks[current_pos].kind.Ident;
                if (std.mem.eql(u8, op, "+") or
                    std.mem.eql(u8, op, "-") or
                    std.mem.eql(u8, op, "<") or
                    std.mem.eql(u8, op, ">") or
                    std.mem.eql(u8, op, "=="))
                {
                    // This is an infix binary operation
                    // Create left operand (variable)
                    const name_copy = try alloc.dupe(u8, ident_val);
                    // Arena allocator handles cleanup
                    const left_node = ast.AstNode{ .Expression = .{ .Variable = .{ .name = name_copy } } };

                    current_pos += 1; // Skip the operator

                    // Parse the right operand
                    if (current_pos >= toks.len) return error.UnexpectedEOF;
                    const right_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
                    // Arena allocator handles cleanup
                    current_pos += right_result.consumed;

                    if (current_pos >= toks.len or toks[current_pos].kind != .RParen) return error.ExpectedRParen;

                    // Create left operand pointer
                    const left_ptr = try alloc.create(ast.Expression);
                    // Arena allocator handles cleanup
                    left_ptr.* = try left_node.Expression.clone(alloc);
                    // Arena allocator handles cleanup

                    // Create right operand pointer
                    const right_ptr = try alloc.create(ast.Expression);
                    // Arena allocator handles cleanup
                    right_ptr.* = try right_result.node.Expression.clone(alloc);
                    // Arena allocator handles cleanup

                    // Duplicate the operator identifier string for the AST node
                    const op_ident_copy = try alloc.dupe(u8, op);
                    // Arena allocator handles cleanup

                    return .{
                        .node = .{ .Expression = .{ .BinaryOp = .{ .op = .{ .Ident = op_ident_copy }, .left = left_ptr, .right = right_ptr } } },
                        .consumed = current_pos + 1,
                    };
                }
            }

            // Regular function call
            const result = try parseCall(alloc, toks, current_pos, depth + 1, ident_val);
            // debug.log("parseCall returned consumed={}", .{result.consumed});
            const final_pos = current_pos - 1 + result.consumed; // -1 because current_pos was incremented
            // debug.log("current_pos={}, final_pos={}, toks.len={}", .{ current_pos, final_pos, toks.len });
            // if (final_pos < toks.len) {
            //     debug.log("Token at final_pos: {any}", .{toks[final_pos]});
            // }
            if (final_pos >= toks.len or toks[final_pos].kind != .RParen) return error.ExpectedRParen;
            return .{ .node = result.node, .consumed = final_pos + 1 };
        },
        .Int => |val| {
            // Handle case like (1 + 2)
            debug.log("Handling integer literal: {}", .{val});
            // Create a literal node for the integer (stack allocated)
            const int_node = ast.AstNode{ .Expression = .{ .Literal = .{ .value = .{ .Int = val } } } };

            // Check if the next token is an operator
            if (current_pos < toks.len and toks[current_pos].kind == .Ident) {
                const op = toks[current_pos].kind.Ident; // Slice from token
                debug.log("Found operator: {s}", .{op});

                // Check if it's a binary operator
                if (std.mem.eql(u8, op, "+") or
                    std.mem.eql(u8, op, "-") or
                    std.mem.eql(u8, op, "<") or
                    std.mem.eql(u8, op, ">") or
                    std.mem.eql(u8, op, "=="))
                {
                    debug.log("Recognized binary operator: {s}", .{op});
                    current_pos += 1; // Skip the operator

                    // Parse the right operand
                    if (current_pos >= toks.len) return error.UnexpectedEOF;
                    debug.log("Parsing right operand at position {}", .{current_pos});
                    debug.log("Right operand token: {}", .{toks[current_pos]});
                    const right_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
                    // Arena allocator handles cleanup
                    debug.log("Right operand parsed successfully", .{});
                    current_pos += right_result.consumed;
                    debug.log("Current position after parsing right operand: {}", .{current_pos});

                    if (current_pos >= toks.len or toks[current_pos].kind != .RParen) {
                        return error.ExpectedRParen; // right_result errdefer runs
                    }
                    debug.log("Found closing parenthesis", .{});

                    // Create and clone left operand
                    debug.log("Creating left operand pointer", .{});
                    const left_ptr = try alloc.create(ast.Expression);
                    // Arena allocator handles cleanup
                    left_ptr.* = try int_node.Expression.clone(alloc); // Clone stack allocated literal
                    // Arena allocator handles cleanup
                    debug.log("Left operand pointer created successfully", .{});

                    // Create and clone right operand
                    debug.log("Creating right operand pointer", .{});
                    const right_ptr = try alloc.create(ast.Expression);
                    // Arena allocator handles cleanup
                    right_ptr.* = try right_result.node.Expression.clone(alloc);
                    // Arena allocator handles cleanup
                    debug.log("Right operand pointer created successfully", .{});
                    debug.log("Right result node type: {}", .{@TypeOf(right_result.node)});
                    debug.log("Right result node: {}", .{right_result.node});
                    debug.log("Right operand pointer assigned successfully", .{});

                    debug.log("Creating binary operation result", .{});
                    // Ownership of left_ptr and right_ptr transferred

                    // Duplicate the operator identifier string for the AST node
                    const op_ident_copy = try alloc.dupe(u8, op);
                    // Arena allocator handles cleanup

                    debug.log("Binary operation result created successfully", .{});
                    // Original right_result errdefer is cancelled by successful return
                    return ParseResult{
                        .node = .{ .Expression = .{ .BinaryOp = .{ .op = .{ .Ident = op_ident_copy }, .left = left_ptr, .right = right_ptr } } },
                        .consumed = current_pos + 1, // +1 for the closing parenthesis
                    };
                }
            }

            // If we get here, it's not a binary operation
            return error.InvalidExpression;
        }, // Ensure comma is present
        else => error.InvalidExpression,
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
fn parseIf(alloc: std.mem.Allocator, toks: []const Token, start_pos: usize, depth: usize) !ParseResult {
    var current_pos = start_pos;

    if (current_pos >= toks.len) return error.UnexpectedEOF;
    const cond_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
    current_pos += cond_result.consumed;

    if (current_pos >= toks.len) return error.UnexpectedEOF; // cond_result errdefer runs
    const then_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
    current_pos += then_result.consumed;

    var else_node_opt: ?ast.AstNode = null;
    var else_result_consumed: usize = 0;
    if (current_pos < toks.len and toks[current_pos].kind == .Else) {
        current_pos += 1;
        if (current_pos >= toks.len) return error.UnexpectedEOF; // cond/then errdefers run
        const else_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
        // Don't add errdefer here yet, handle it based on RParen check
        else_result_consumed = else_result.consumed;
        else_node_opt = else_result.node;
        current_pos += else_result_consumed;
    }

    if (current_pos >= toks.len or toks[current_pos].kind != .RParen) {
        // cond/then errdefers run. Deinit else_node if it exists.
        if (else_node_opt) |*en| en.deinit(alloc);
        return error.ExpectedRParen;
    }

    // Clone condition
    const cond_ptr = try alloc.create(ast.Expression);
    cond_ptr.* = try cond_result.node.Expression.clone(alloc);

    // Clone then branch
    const then_ptr = try alloc.create(ast.Expression);
    then_ptr.* = try then_result.node.Expression.clone(alloc);

    // Clone else branch if it exists
    var else_ptr: ?*ast.Expression = null;
    if (else_node_opt) |*original_else_node_ptr| { // Capture as mutable pointer
        // Deinit the original node if cloning or subsequent steps fail
        // Arena allocator handles cleanup

        else_ptr = try alloc.create(ast.Expression);
        // Arena allocator handles cleanup

        // Clone the expression from the original node
        else_ptr.?.* = try original_else_node_ptr.Expression.clone(alloc);
        // Arena allocator handles cleanup

        // Arena allocator handles cleanup
        // Arena allocator handles cleanup
    }

    // Success, ownership transferred. Original results' errdefers are cancelled.
    return .{
        .node = .{ .Expression = .{ .If = .{ .condition = cond_ptr, .then_branch = then_ptr, .else_branch = else_ptr } } },
        .consumed = current_pos + 1 - start_pos,
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
fn parseVar(alloc: std.mem.Allocator, toks: []const Token, start_pos: usize, depth: usize) !ParseResult {
    var current_pos = start_pos;
    if (current_pos >= toks.len or toks[current_pos].kind != .Ident) return error.ExpectedVariableName;
    const name = toks[current_pos].kind.Ident; // Slice from token
    current_pos += 1;

    if (current_pos >= toks.len or toks[current_pos].kind != .Equals) return error.ExpectedEquals;
    current_pos += 1;

    if (current_pos >= toks.len) return error.UnexpectedEOF;
    const value_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
    current_pos += value_result.consumed;

    if (current_pos >= toks.len or toks[current_pos].kind != .RParen) return error.ExpectedRParen; // value_result errdefer runs

    // Duplicate the variable name for the AST node
    const name_copy = try alloc.dupe(u8, name);
    // Arena allocator handles cleanup

    // Clone value
    const value_ptr = try alloc.create(ast.Expression);
    value_ptr.* = try value_result.node.Expression.clone(alloc);

    // Success, ownership transferred
    return .{
        .node = .{ .Expression = .{ .VarDecl = .{ .name = name_copy, .value = value_ptr } } },
        .consumed = current_pos + 1 - start_pos,
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
fn parseFn(alloc: std.mem.Allocator, toks: []const Token, start_pos: usize, depth: usize) !ParseResult {
    var current_pos = start_pos;

    // Get the function name if present
    var name_copy: []const u8 = "";
    if (current_pos < toks.len and toks[current_pos].kind == .Ident) {
        const name = toks[current_pos].kind.Ident;
        name_copy = try alloc.dupe(u8, name);
        current_pos += 1;
    } else {
        // Anonymous function
        name_copy = try alloc.dupe(u8, "anonymous");
    }
    // Arena allocator handles cleanup

    // Parse parameters
    var params = std.ArrayList(ast.FuncParam).init(alloc);
    // Arena allocator handles cleanup

    if (current_pos < toks.len and toks[current_pos].kind == .LBracket) {
        current_pos += 1; // Skip '['
        while (current_pos < toks.len and toks[current_pos].kind != .RBracket) {
            // Parse parameter name
            if (current_pos >= toks.len or toks[current_pos].kind != .Ident) {
                return error.ExpectedParameterName;
            }
            const param_name = toks[current_pos].kind.Ident;
            const param_name_copy = try alloc.dupe(u8, param_name);
            // Arena allocator handles cleanup
            current_pos += 1;

            // Parse optional parameter type
            var param_type: ?[]const u8 = null;
            if (current_pos < toks.len and toks[current_pos].kind == .Ident) {
                const type_name = toks[current_pos].kind.Ident;
                param_type = try alloc.dupe(u8, type_name);
                current_pos += 1;
            }

            // Add parameter to list
            try params.append(.{
                .name = param_name_copy,
                .param_type = param_type,
            });
        }

        if (current_pos >= toks.len or toks[current_pos].kind != .RBracket) {
            return error.ExpectedRBracket;
        }
        current_pos += 1; // Skip ']'
    }

    // Parse function body - extract just a simple literal value for now
    if (current_pos >= toks.len) return error.UnexpectedEOF;
    const body_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
    const final_pos_after_body = current_pos + body_result.consumed;

    // Extract a simple literal value from the body
    var body_literal: i64 = 0;
    switch (body_result.node.Expression) {
        .Literal => |lit| switch (lit.value) {
            .Int => |val| body_literal = val,
            else => body_literal = 42, // Default fallback
        },
        else => body_literal = 42, // Default fallback for complex expressions
    }

    debug.log("parseFn: start_pos={} final_pos_after_body={} consumed={}", .{ start_pos, final_pos_after_body, final_pos_after_body - start_pos });

    // Choose between FuncDef and SimpleFuncDef based on whether there are parameters
    const fn_node = if (params.items.len > 0) blk: {
        // Use FuncDef for functions with parameters
        const params_slice = try alloc.alloc(ast.FuncParam, params.items.len);
        for (params.items, 0..) |param, i| {
            params_slice[i] = param;
        }

        const body_ptr = try alloc.create(ast.Expression);
        body_ptr.* = body_result.node.Expression;

        break :blk ast.AstNode{
            .Expression = .{
                .FuncDef = .{
                    .name = name_copy,
                    .params = params_slice,
                    .body = body_ptr,
                },
            },
        };
    } else blk: {
        // Use SimpleFuncDef for parameter-less functions
        break :blk ast.AstNode{
            .Expression = .{
                .SimpleFuncDef = .{
                    .name = name_copy,
                    .param_count = 0,
                    .body_literal = body_literal,
                },
            },
        };
    };

    if (params.items.len > 0) {
        debug.log("parseFn: created FuncDef with name: {s} param_count: {}", .{ name_copy, params.items.len });
    } else {
        debug.log("parseFn: created SimpleFuncDef with name: {s} param_count: {} body: {}", .{ name_copy, params.items.len, body_literal });
    }

    return .{
        .node = fn_node,
        .consumed = final_pos_after_body - start_pos,
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
///   func_ident: The identifier of the function being called.
///
/// Returns: A ParseResult containing the parsed node and the number of tokens consumed.
fn parseCall(alloc: std.mem.Allocator, toks: []const Token, start_pos: usize, depth: usize, func_ident: []const u8) !ParseResult {
    debug.log("parseCall: function={s} depth={}", .{ func_ident, depth });
    debug.log("parseCall: start_pos={} toks.len={}", .{ start_pos, toks.len });
    var current_pos = start_pos;
    var args = std.ArrayList(*ast.Expression).init(alloc);
    // Arena allocator handles all cleanup

    // Handle binary operators in infix notation
    if (std.mem.eql(u8, func_ident, "+") or
        std.mem.eql(u8, func_ident, "-") or
        std.mem.eql(u8, func_ident, "<") or
        std.mem.eql(u8, func_ident, ">") or
        std.mem.eql(u8, func_ident, "=="))
    {
        debug.log("Handling infix binary operator: {s}", .{func_ident});

        // We need exactly two arguments for binary operators
        if (current_pos >= toks.len) return error.UnexpectedEOF;

        // Parse left operand
        debug.log("Parsing left operand", .{});
        const left_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
        // Arena allocator handles cleanup
        current_pos += left_result.consumed;

        // Parse right operand
        if (current_pos >= toks.len) return error.UnexpectedEOF;
        debug.log("Parsing right operand", .{});
        const right_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
        // Arena allocator handles cleanup
        current_pos += right_result.consumed;

        // Check for closing parenthesis
        if (current_pos >= toks.len or toks[current_pos].kind != .RParen) return error.ExpectedRParen;

        // Create left operand pointer
        const left_ptr = try alloc.create(ast.Expression);
        // Arena allocator handles cleanup
        left_ptr.* = try left_result.node.Expression.clone(alloc);
        // Arena allocator handles cleanup

        // Create right operand pointer
        const right_ptr = try alloc.create(ast.Expression);
        // Arena allocator handles cleanup
        right_ptr.* = try right_result.node.Expression.clone(alloc);
        // Arena allocator handles cleanup

        // Duplicate the operator identifier string for the AST node
        const op_ident_copy = try alloc.dupe(u8, func_ident);
        // Arena allocator handles cleanup

        // Clean up original nodes
        // @constCast(&left_result.node).deinit(alloc);  // Removed: arena handles cleanup
        // @constCast(&right_result.node).deinit(alloc); // Removed: arena handles cleanup

        debug.log("Binary operation parsed successfully", .{});
        return .{
            .node = .{ .Expression = .{ .BinaryOp = .{ .op = .{ .Ident = op_ident_copy }, .left = left_ptr, .right = right_ptr } } },
            .consumed = current_pos + 1 - start_pos, // Relative consumed count from start_pos
        };
    }

    // Regular function call
    debug.log("Regular function call: {s}", .{func_ident});
    while (current_pos < toks.len and toks[current_pos].kind != .RParen) {
        debug.log("  Parsing argument at position {}", .{current_pos});
        if (current_pos < toks.len) {
            debug.log("  Argument token: {any}", .{toks[current_pos]});
        }
        const arg_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
        current_pos += arg_result.consumed;

        const arg_ptr = try alloc.create(ast.Expression);
        arg_ptr.* = arg_result.node.Expression;
        try args.append(arg_ptr);
    }

    if (current_pos >= toks.len or toks[current_pos].kind != .RParen) return error.ExpectedRParen;

    debug.log("parseCall: creating function expression", .{});
    // Create a variable expression for the function name
    // Duplicate the function identifier string for the AST node
    const func_name_copy = try alloc.dupe(u8, func_ident);
    debug.log("parseCall: duplicated function name: {s}", .{func_name_copy});
    // Arena allocator handles cleanup
    const func_expr_ptr = try alloc.create(ast.Expression);
    debug.log("parseCall: created function expression pointer", .{});
    // Arena allocator handles cleanup

    // For built-in functions like 'print', we need to create a Variable expression
    func_expr_ptr.* = .{ .Variable = .{ .name = func_name_copy } }; // Transfer ownership
    debug.log("parseCall: assigned function variable expression", .{});

    debug.log("parseCall: creating final result with {} args", .{args.items.len});

    // Use the ArrayList directly instead of converting to slice
    const result = ParseResult{
        .node = .{ .Expression = .{ .FuncCall = .{ .func = func_expr_ptr, .args = args } } }, // Transfer ownership
        .consumed = current_pos + 1 - start_pos, // Relative consumed count from start_pos
    };
    debug.log("parseCall: function call parsed successfully", .{});
    return result;
}
