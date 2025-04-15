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
                debug.log("Found token: {s}", .{debug.formatToken(token)});
                try tokens.append(token);
                i += 1;
                continue;
            },
            ')' => {
                const token = Token{ .kind = .RParen, .loc = i };
                debug.log("Found token: {s}", .{debug.formatToken(token)});
                try tokens.append(token);
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
            // Allocate string here
            const str = try allocator.dupe(u8, source_to_parse[start..i]);
            debug.log("Found string: {s}", .{str});
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
    // Tokenizer no longer allocates Ident strings
    var tokens = try tokenize(allocator, source);
    // Defer cleanup for the token list itself, but not the strings within,
    // as their ownership is transferred to the AST.
    defer tokens.deinit();

    debug.log("Starting to parse tokens into AST", .{});
    var result_nodes = std.ArrayList(ast.AstNode).init(allocator);
    errdefer {
        for (result_nodes.items) |*node| node.deinit(allocator);
        result_nodes.deinit();
    }

    var pos: usize = 0;
    while (pos < tokens.items.len) {
        // Skip any unexpected RParen tokens
        if (tokens.items[pos].kind == .RParen) {
            pos += 1;
            continue;
        }

        const result = parseExpression(allocator, tokens.items[pos..], 0) catch |err| {
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

    return result_nodes;
}

// Main recursive parse function
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
            errdefer alloc.free(name_copy);
            return .{ .node = .{ .Expression = .{ .Variable = .{ .name = name_copy } } }, .consumed = 1 };
        },
        .LParen => parseList(alloc, toks, depth),
        .RParen => error.UnexpectedRParen,
        else => error.InvalidExpression,
    };
}

// Parses content within parentheses (...)
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
        errdefer @constCast(&left_result.node).deinit(alloc); // Deinit if subsequent steps fail

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
                    std.mem.eql(u8, op, ">"))
                {
                    current_pos += 1; // Skip the operator

                    // Parse the right operand
                    if (current_pos >= toks.len) return error.UnexpectedEOF; // left_result errdefer runs
                    const right_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
                    errdefer @constCast(&right_result.node).deinit(alloc); // Deinit if subsequent steps fail

                    current_pos += right_result.consumed;

                    if (current_pos >= toks.len or toks[current_pos].kind != .RParen) return error.ExpectedRParen; // left/right errdefers run

                    // Clone results into the new BinaryOp node
                    const left_ptr = try alloc.create(ast.Expression);
                    errdefer alloc.destroy(left_ptr);
                    left_ptr.* = try left_result.node.Expression.clone(alloc);
                    errdefer left_ptr.deinit(alloc);

                    const right_ptr = try alloc.create(ast.Expression);
                    errdefer alloc.destroy(right_ptr);
                    right_ptr.* = try right_result.node.Expression.clone(alloc);
                    errdefer right_ptr.deinit(alloc);

                    // Duplicate the operator identifier string for the AST node
                    const op_ident_copy = try alloc.dupe(u8, op);
                    errdefer alloc.free(op_ident_copy);

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
            // left_result errdefer runs
            return error.ExpectedRParen;
        }

        // Original left_result errdefer is cancelled by successful return
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
        errdefer @constCast(&left_result.node).deinit(alloc);

        // Check if there's an operator after the literal
        if (current_pos < toks.len and toks[current_pos].kind == .Ident) {
            const op_token = toks[current_pos];
            const op = op_token.kind.Ident; // Slice from token

            // Check if it's a binary operator
            if (std.mem.eql(u8, op, "+") or
                std.mem.eql(u8, op, "-") or
                std.mem.eql(u8, op, "<") or
                std.mem.eql(u8, op, ">"))
            {
                current_pos += 1; // Skip the operator

                // Parse the right operand
                if (current_pos >= toks.len) return error.UnexpectedEOF; // left_result errdefer runs
                const right_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
                errdefer @constCast(&right_result.node).deinit(alloc); // Deinit if subsequent steps fail

                current_pos += right_result.consumed;

                if (current_pos >= toks.len or toks[current_pos].kind != .RParen) return error.ExpectedRParen; // left/right errdefers run

                // Clone results into the new BinaryOp node
                const left_ptr = try alloc.create(ast.Expression);
                errdefer alloc.destroy(left_ptr);
                left_ptr.* = try left_result.node.Expression.clone(alloc);
                errdefer left_ptr.deinit(alloc);

                const right_ptr = try alloc.create(ast.Expression);
                errdefer alloc.destroy(right_ptr);
                right_ptr.* = try right_result.node.Expression.clone(alloc);
                errdefer right_ptr.deinit(alloc);

                // Duplicate the operator identifier string for the AST node
                const op_ident_copy = try alloc.dupe(u8, op);
                errdefer alloc.free(op_ident_copy);

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
        // left_result errdefer runs
        return error.InvalidExpression;
    }

    // Restore the switch statement
    return switch (first.kind) {
        .If => parseIf(alloc, toks, current_pos, depth),
        .Var => parseVar(alloc, toks, current_pos, depth),
        .Fn => parseFn(alloc, toks, current_pos, depth),
        .Ident => |ident_val| parseCall(alloc, toks, current_pos, depth, ident_val), // ident_val is slice from token
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
                    std.mem.eql(u8, op, ">"))
                {
                    debug.log("Recognized binary operator: {s}", .{op});
                    current_pos += 1; // Skip the operator

                    // Parse the right operand
                    if (current_pos >= toks.len) return error.UnexpectedEOF;
                    debug.log("Parsing right operand at position {}", .{current_pos});
                    debug.log("Right operand token: {}", .{toks[current_pos]});
                    const right_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
                    errdefer @constCast(&right_result.node).deinit(alloc); // Deinit if subsequent steps fail
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
                    errdefer alloc.destroy(left_ptr);
                    left_ptr.* = try int_node.Expression.clone(alloc); // Clone stack allocated literal
                    errdefer left_ptr.deinit(alloc);
                    debug.log("Left operand pointer created successfully", .{});

                    // Create and clone right operand
                    debug.log("Creating right operand pointer", .{});
                    const right_ptr = try alloc.create(ast.Expression);
                    errdefer alloc.destroy(right_ptr);
                    right_ptr.* = try right_result.node.Expression.clone(alloc);
                    errdefer right_ptr.deinit(alloc);
                    debug.log("Right operand pointer created successfully", .{});
                    debug.log("Right result node type: {}", .{@TypeOf(right_result.node)});
                    debug.log("Right result node: {}", .{right_result.node});
                    debug.log("Right operand pointer assigned successfully", .{});

                    debug.log("Creating binary operation result", .{});
                    // Ownership of left_ptr and right_ptr transferred

                    // Duplicate the operator identifier string for the AST node
                    const op_ident_copy = try alloc.dupe(u8, op);
                    errdefer alloc.free(op_ident_copy);

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

fn parseIf(alloc: std.mem.Allocator, toks: []const Token, start_pos: usize, depth: usize) !ParseResult {
    var current_pos = start_pos;

    if (current_pos >= toks.len) return error.UnexpectedEOF;
    const cond_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
    errdefer @constCast(&cond_result.node).deinit(alloc); // Deinit if subsequent steps fail
    current_pos += cond_result.consumed;

    if (current_pos >= toks.len) return error.UnexpectedEOF; // cond_result errdefer runs
    const then_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
    errdefer @constCast(&then_result.node).deinit(alloc); // Deinit if subsequent steps fail
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
    errdefer alloc.destroy(cond_ptr); // If clone fails
    cond_ptr.* = try cond_result.node.Expression.clone(alloc);
    errdefer cond_ptr.deinit(alloc); // If then/else cloning fails

    // Clone then branch
    const then_ptr = try alloc.create(ast.Expression);
    errdefer alloc.destroy(then_ptr); // If clone fails
    then_ptr.* = try then_result.node.Expression.clone(alloc);
    errdefer then_ptr.deinit(alloc); // If else cloning fails

    // Clone else branch if it exists
    var else_ptr: ?*ast.Expression = null;
    if (else_node_opt) |*original_else_node_ptr| { // Capture as mutable pointer
        // Deinit the original node if cloning or subsequent steps fail
        errdefer original_else_node_ptr.deinit(alloc);

        else_ptr = try alloc.create(ast.Expression);
        // If else_ptr allocation fails, the errdefer above cleans the original node
        errdefer alloc.destroy(else_ptr.?); // If clone fails, destroy the container

        // Clone the expression from the original node
        else_ptr.?.* = try original_else_node_ptr.Expression.clone(alloc);
        // If clone fails, the errdefer above destroys the container, and the first errdefer cleans the original node

        // If clone succeeds, add errdefer to clean the *cloned* node if return fails
        errdefer else_ptr.?.deinit(alloc);
    }

    // Success, ownership transferred. Original results' errdefers are cancelled.
    return .{
        .node = .{ .Expression = .{ .If = .{ .condition = cond_ptr, .then_branch = then_ptr, .else_branch = else_ptr } } },
        .consumed = current_pos + 1,
    };
}

fn parseVar(alloc: std.mem.Allocator, toks: []const Token, start_pos: usize, depth: usize) !ParseResult {
    var current_pos = start_pos;
    if (current_pos >= toks.len or toks[current_pos].kind != .Ident) return error.ExpectedVariableName;
    const name = toks[current_pos].kind.Ident; // Slice from token
    current_pos += 1;

    if (current_pos >= toks.len or toks[current_pos].kind != .Equals) return error.ExpectedEquals;
    current_pos += 1;

    if (current_pos >= toks.len) return error.UnexpectedEOF;
    const value_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
    errdefer @constCast(&value_result.node).deinit(alloc); // Deinit if subsequent steps fail
    current_pos += value_result.consumed;

    if (current_pos >= toks.len or toks[current_pos].kind != .RParen) return error.ExpectedRParen; // value_result errdefer runs

    // Duplicate the variable name for the AST node
    const name_copy = try alloc.dupe(u8, name);
    errdefer alloc.free(name_copy); // If value cloning fails

    // Clone value
    const value_ptr = try alloc.create(ast.Expression);
    errdefer alloc.destroy(value_ptr); // If clone fails
    value_ptr.* = try value_result.node.Expression.clone(alloc);
    errdefer value_ptr.deinit(alloc); // If return fails

    // Success, ownership transferred
    return .{
        .node = .{ .Expression = .{ .VarDecl = .{ .name = name_copy, .value = value_ptr } } },
        .consumed = current_pos + 1,
    };
}

fn parseFn(alloc: std.mem.Allocator, toks: []const Token, start_pos: usize, depth: usize) !ParseResult {
    var current_pos = start_pos;
    if (current_pos >= toks.len or toks[current_pos].kind != .Ident) return error.ExpectedFunctionName;
    const name = toks[current_pos].kind.Ident; // Slice from token
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
            const param_name = toks[current_pos].kind.Ident; // Slice from token
            current_pos += 1;
            var param_type: ?[]const u8 = null;
            if (current_pos < toks.len and toks[current_pos].kind == .Ident) {
                // Duplicate type identifier string
                param_type = try alloc.dupe(u8, toks[current_pos].kind.Ident);
                errdefer if (param_type) |pt| alloc.free(pt);
                current_pos += 1;
            }
            // Duplicate param name string
            const param_name_copy = try alloc.dupe(u8, param_name);
            errdefer alloc.free(param_name_copy);
            try params.append(.{ .name = param_name_copy, .param_type = param_type });
        }
        if (current_pos >= toks.len or toks[current_pos].kind != .RBracket) return error.ExpectedRBracket;
        current_pos += 1;
    }

    if (current_pos >= toks.len) return error.UnexpectedEOF; // Params errdefer runs
    const body_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
    errdefer @constCast(&body_result.node).deinit(alloc); // Deinit if subsequent steps fail
    const final_pos_after_body = current_pos + body_result.consumed;

    // Duplicate function name for AST node
    const name_copy = try alloc.dupe(u8, name);
    errdefer alloc.free(name_copy); // If params/body cloning fails

    const owned_params = try params.toOwnedSlice();
    // params list errdefer is cancelled by success
    errdefer { // If body cloning fails, clean up owned_params
        for (owned_params) |*p| p.deinit(alloc);
        alloc.free(owned_params);
    }

    // Clone body
    const body_ptr = try alloc.create(ast.Expression);
    errdefer alloc.destroy(body_ptr); // If clone fails
    body_ptr.* = try body_result.node.Expression.clone(alloc);
    errdefer body_ptr.deinit(alloc); // If return fails

    // Success, ownership transferred
    return .{
        .node = .{ .Expression = .{ .FuncDef = .{ .name = name_copy, .params = owned_params, .body = body_ptr } } },
        .consumed = final_pos_after_body, // This is the index *after* the body in the original slice
    };
}

fn parseCall(alloc: std.mem.Allocator, toks: []const Token, start_pos: usize, depth: usize, func_ident: []const u8) !ParseResult {
    debug.log("parseCall: function={s} depth={}", .{ func_ident, depth });
    var current_pos = start_pos;
    var args = std.ArrayList(*ast.Expression).init(alloc);
    errdefer {
        for (args.items) |arg| {
            arg.deinit(alloc);
            alloc.destroy(arg);
        }
        args.deinit();
    }

    // Special case for binary operators (handled in parseList)
    // This check should ideally not be needed if parseList handles infix correctly
    // Keeping it defensively for now, but it might indicate a parser logic issue
    if (std.mem.eql(u8, func_ident, "+") or
        std.mem.eql(u8, func_ident, "-") or
        std.mem.eql(u8, func_ident, "<") or
        std.mem.eql(u8, func_ident, ">"))
    {
        // This path should likely return an error or be unreachable if parseList is correct
        std.debug.print("WARNING: parseCall encountered binary operator '{s}' - should be handled by parseList\n", .{func_ident});
        return error.InvalidExpression; // Or handle appropriately if this path is valid
    }

    // Regular function call
    debug.log("Regular function call: {s}", .{func_ident});
    while (current_pos < toks.len and toks[current_pos].kind != .RParen) {
        debug.log("  Parsing argument at position {}", .{current_pos});
        if (current_pos < toks.len) {
            debug.log("  Argument token: {any}", .{toks[current_pos]});
        }
        const arg_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
        // Deinit arg_result if subsequent cloning or append fails
        errdefer @constCast(&arg_result.node).deinit(alloc);
        debug.log("  Argument parsed successfully, consumed {} tokens", .{arg_result.consumed});
        current_pos += arg_result.consumed;

        const arg_ptr = try alloc.create(ast.Expression);
        errdefer alloc.destroy(arg_ptr); // If clone fails
        // Clone the expression to ensure the FuncCall owns its own copy
        arg_ptr.* = try arg_result.node.Expression.clone(alloc);
        // arg_result errdefer is cancelled by successful clone.
        // Deinit the original node now that it's cloned.
        @constCast(&arg_result.node).deinit(alloc);
        // Add errdefer for the cloned node in case append fails
        errdefer arg_ptr.deinit(alloc);
        try args.append(arg_ptr); // Ownership transferred to args list
        // Cloned node's errdefer is cancelled by successful append
    }

    if (current_pos >= toks.len or toks[current_pos].kind != .RParen) return error.ExpectedRParen;

    // Create a variable expression for the function name
    // Duplicate the function identifier string for the AST node
    const func_name_copy = try alloc.dupe(u8, func_ident);
    errdefer alloc.free(func_name_copy);
    const func_expr_ptr = try alloc.create(ast.Expression);
    errdefer alloc.destroy(func_expr_ptr);

    // For built-in functions like 'print', we need to create a Variable expression
    func_expr_ptr.* = .{ .Variable = .{ .name = func_name_copy } }; // Transfer ownership

    return .{
        .node = .{ .Expression = .{ .FuncCall = .{ .func = func_expr_ptr, .args = args } } }, // Transfer ownership
        .consumed = current_pos + 1, // Consume final RParen
    };
}
