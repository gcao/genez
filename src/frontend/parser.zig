const std = @import("std");
pub const ast = @import("ast.zig");
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
    ExpectedRBrace,
    MaxRecursionDepthExceeded,
    UnexpectedToken,
    UnexpectedEndOfInput,
    EmptyExpression,
    InvalidExpression,
    OutOfMemory,
    ExpectedClassName,
    ExpectedPropertyName,
    ExpectedProperty,
    ExpectedMethodName,
    ExpectedMethodBody,
    ExpectedParentClassName,
    ExpectedSlashForInstanceVar,
    ExpectedInstanceVarName,
    ExpectedFieldName,
    UnexpectedTokenInPackageGene,
};

// --- Tokenizer (Mostly unchanged, minor fixes) ---
pub const Token = struct {
    kind: TokenKind,
    loc: usize = 0, // Optional: for error reporting
};

pub const TokenKind = union(enum) {
    Int: i64,
    Float: f64,
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
    Fnx, // Keyword for anonymous functions
    Do, // New: for do blocks
    Class, // New: for class definitions
    New, // New: for object instantiation
    Dot, // New: for field/method access
    Slash, // New: for field access in Gene
    Match, // New: for pattern matching
    Macro, // New: for pseudo macro definitions
    Percent, // New: for unquote syntax (%identifier)
    Ns, // New: for namespace declarations
    Import, // New: for import statements
    For, // New: for for-in loops
    In, // New: for for-in loops
    Return, // New: for return statements
    Nil, // New: for nil literal
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
pub fn tokenize(allocator: std.mem.Allocator, source: []const u8) !std.ArrayList(Token) {
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
        if (c == '#') {
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
                // Check for => operator
                if (i + 1 < source_to_parse.len and source_to_parse[i + 1] == '>') {
                    // Create an identifier token for =>
                    const op_str = source_to_parse[i .. i + 2];
                    try tokens.append(.{ .kind = .{ .Ident = op_str }, .loc = i });
                    i += 2;
                // Check for == operator
                } else if (i + 1 < source_to_parse.len and source_to_parse[i + 1] == '=') {
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
            '/' => {
                try tokens.append(.{ .kind = .Slash, .loc = i });
                i += 1;
                continue;
            },
            '%' => {
                try tokens.append(.{ .kind = .Percent, .loc = i });
                i += 1;
                continue;
            },
            else => {},
        }

        // Handle numbers (integers and floats)
        // Numbers can start with optional sign followed by digits
        if (std.ascii.isDigit(c) or ((c == '+' or c == '-') and i + 1 < source_to_parse.len and std.ascii.isDigit(source_to_parse[i + 1]))) {
            const start = i;
            var has_sign = false;
            var has_dot = false;
            
            // Handle optional sign
            if (c == '+' or c == '-') {
                has_sign = true;
                i += 1;
            }
            
            // Parse integer part (required for floats)
            while (i < source_to_parse.len and std.ascii.isDigit(source_to_parse[i])) : (i += 1) {}
            
            // Check for decimal point
            if (i < source_to_parse.len and source_to_parse[i] == '.') {
                has_dot = true;
                i += 1; // Skip the dot
                
                // Parse optional decimal part
                while (i < source_to_parse.len and std.ascii.isDigit(source_to_parse[i])) : (i += 1) {}
            }
            
            const num_str = source_to_parse[start..i];
            
            if (has_dot) {
                // Parse as float
                const float_val = std.fmt.parseFloat(f64, num_str) catch |err| {
                    std.debug.print("Error parsing float '{s}': {any}\n", .{ num_str, err });
                    return err;
                };
                try tokens.append(.{ .kind = .{ .Float = float_val }, .loc = start });
            } else if (has_sign) {
                // Signed integer
                const int_val = std.fmt.parseInt(i64, num_str, 10) catch |err| {
                    std.debug.print("Error parsing int '{s}': {any}\n", .{ num_str, err });
                    return err;
                };
                try tokens.append(.{ .kind = .{ .Int = int_val }, .loc = start });
            } else {
                // Unsigned integer
                const int_val = std.fmt.parseInt(i64, num_str, 10) catch |err| {
                    std.debug.print("Error parsing int '{s}': {any}\n", .{ num_str, err });
                    return err;
                };
                try tokens.append(.{ .kind = .{ .Int = int_val }, .loc = start });
            }
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
        const ident_chars = "_!$&*:<=>?@^~+-"; // Added '+' and '-' to allow multi-character operators like ++
        const ident_start_chars = "_!$&*+:<=>?@^~-"; // '+' and '-' can start an identifier if not followed by digit
        if (std.ascii.isAlphabetic(c) or std.mem.indexOfScalar(u8, ident_start_chars, c) != null) {
            const start = i;
            while (i + 1 < source_to_parse.len and (std.ascii.isAlphanumeric(source_to_parse[i + 1]) or std.mem.indexOfScalar(u8, ident_chars, source_to_parse[i + 1]) != null)) : (i += 1) {}
            const word = source_to_parse[start .. i + 1];
            var token_kind: TokenKind = undefined;

            // Don't allocate Ident tokens here, let the parser handle ownership if needed
            if (std.mem.eql(u8, word, "if")) token_kind = .If else if (std.mem.eql(u8, word, "else")) token_kind = .Else else if (std.mem.eql(u8, word, "var")) token_kind = .Var else if (std.mem.eql(u8, word, "fn")) token_kind = .Fn else if (std.mem.eql(u8, word, "fnx")) token_kind = .Fnx else if (std.mem.eql(u8, word, "do")) token_kind = .Do // New keyword
            else if (std.mem.eql(u8, word, "class")) token_kind = .Class // New keyword
            else if (std.mem.eql(u8, word, "new")) token_kind = .New // New keyword
            else if (std.mem.eql(u8, word, "match")) token_kind = .Match // New keyword
            else if (std.mem.eql(u8, word, "macro")) token_kind = .Macro // New keyword
            else if (std.mem.eql(u8, word, "ns")) {
                debug.log("Tokenizing 'ns' as Ns keyword", .{});
                token_kind = .Ns; // New keyword
            }
            else if (std.mem.eql(u8, word, "import")) {
                debug.log("Tokenizing 'import' as Import keyword", .{});
                token_kind = .Import; // New keyword
            }
            else if (std.mem.eql(u8, word, "for")) token_kind = .For // New keyword
            else if (std.mem.eql(u8, word, "in")) token_kind = .In // New keyword
            else if (std.mem.eql(u8, word, "return")) token_kind = .Return // New keyword
            else if (std.mem.eql(u8, word, "true")) token_kind = .{ .Bool = true } else if (std.mem.eql(u8, word, "false")) token_kind = .{ .Bool = false } else if (std.mem.eql(u8, word, "nil")) token_kind = .Nil else {
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
    return parseGeneSourceWithFilename(parent_allocator, source, null);
}

pub fn parseGeneSourceWithFilename(parent_allocator: std.mem.Allocator, source: []const u8, filename: ?[]const u8) !ParseSourceResult {
    _ = filename; // No longer needed since we handle properties in all .gene files
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
    
    // Debug first few tokens
    // std.debug.print("DEBUG: First 10 tokens after tokenization:\n", .{});
    var tok_i: usize = 0;
    while (tok_i < @min(10, tokens.items.len)) : (tok_i += 1) {
        // std.debug.print("  [{d}] {s} at loc {d}\n", .{tok_i, @tagName(tokens.items[tok_i].kind), tokens.items[tok_i].loc});
    }

    // Parse into a GeneDocument structure with properties and expressions
    var properties = std.ArrayList(ast.MapEntry).init(arena_allocator);
    var expressions = std.ArrayList(ast.AstNode).init(arena_allocator);
    errdefer {
        // No need to deinit individual nodes since the arena will be freed
        properties.deinit();
        expressions.deinit();
    }

    var pos: usize = 0;
    while (pos < tokens.items.len) {
        // std.debug.print("DEBUG: parseGeneSource loop iteration, pos={d}, next token: {s}\n", .{pos, @tagName(tokens.items[pos].kind)});
        
        // Skip any unexpected RParen tokens
        if (tokens.items[pos].kind == .RParen) {
            pos += 1;
            continue;
        }

        // Check for bare property syntax (^name value)
        if (tokens.items[pos].kind == .Ident) {
            const ident = tokens.items[pos].kind.Ident;
            if (std.mem.startsWith(u8, ident, "^")) {
                const prop_result = try parseTopLevelProperty(arena_allocator, tokens.items[pos..], 0);
                try properties.append(prop_result.entry);
                pos += prop_result.consumed;
                continue;
            }
        }

        // Parse regular expression
        // std.debug.print("DEBUG: Calling parseExpression at pos={d}\n", .{pos});
        const result = parseExpression(arena_allocator, tokens.items[pos..], 0) catch |err| {
            // std.debug.print("DEBUG: parseExpression failed with error: {}\n", .{err});
            return err;
        };
        // Don't add errdefer here, ownership is transferred to expressions
        try expressions.append(result.node); // Transfer ownership
        // std.debug.print("DEBUG: parseExpression consumed {d} tokens\n", .{result.consumed});
        pos += result.consumed;
    }

    debug.log("parseGeneSource: parsed {} properties and {} expressions", .{properties.items.len, expressions.items.len});

    // If we have properties, wrap everything in a GeneDocument map
    if (properties.items.len > 0) {
        // Create a map containing all properties
        const map_expr = ast.Expression{ .MapLiteral = .{ .entries = try properties.toOwnedSlice() } };
        
        // If there are also expressions, we need to combine them
        // For now, if there are expressions along with properties, we'll return the map as the first node
        // and the expressions as subsequent nodes
        var all_nodes = try arena_allocator.alloc(ast.AstNode, 1 + expressions.items.len);
        all_nodes[0] = .{ .Expression = map_expr };
        
        for (expressions.items, 0..) |expr_node, i| {
            all_nodes[i + 1] = expr_node;
        }
        
        return ParseSourceResult{
            .arena = arena,
            .nodes = all_nodes,
        };
    } else {
        // No properties, just return expressions as before
        const nodes_slice = try arena_allocator.alloc(ast.AstNode, expressions.items.len);
        for (expressions.items, 0..) |node, i| {
            nodes_slice[i] = node;
        }
        
        return ParseSourceResult{
            .arena = arena,
            .nodes = nodes_slice,
        };
    }
}

/// Result from parsing a top-level property in package.gene
const PackagePropertyResult = struct {
    entry: ast.MapEntry,
    consumed: usize,
};

/// Parse a package.gene file with bare properties at the top level
fn parsePackageGene(parent_allocator: std.mem.Allocator, source: []const u8) !ParseSourceResult {
    // Create arena allocator
    const arena = try parent_allocator.create(std.heap.ArenaAllocator);
    arena.* = std.heap.ArenaAllocator.init(parent_allocator);
    errdefer {
        arena.deinit();
        parent_allocator.destroy(arena);
    }
    const arena_allocator = arena.allocator();
    
    // Tokenize
    var tokens = try tokenize(arena_allocator, source);
    
    // Create a list to hold map entries
    var entries = std.ArrayList(ast.MapEntry).init(arena_allocator);
    
    var pos: usize = 0;
    while (pos < tokens.items.len) {
        const token = tokens.items[pos];
        
        // Skip empty lines (RParen tokens from tokenizer quirks)
        if (token.kind == .RParen) {
            pos += 1;
            continue;
        }
        
        // Expect property or end of file
        switch (token.kind) {
            .Ident => |ident| {
                // Check if it's a property (starts with ^)
                if (std.mem.startsWith(u8, ident, "^")) {
                    // Parse property
                    const prop_result = try parseTopLevelProperty(
                        arena_allocator, 
                        tokens.items[pos..], 
                        0
                    );
                    try entries.append(prop_result.entry);
                    pos += prop_result.consumed;
                } else {
                    return error.ExpectedProperty;
                }
            },
            else => {
                return error.UnexpectedTokenInPackageGene;
            }
        }
    }
    
    // Create map expression
    const entries_slice = try arena_allocator.alloc(ast.MapEntry, entries.items.len);
    for (entries.items, 0..) |entry, i| {
        entries_slice[i] = entry;
    }
    
    const map_ptr = try arena_allocator.create(ast.Expression);
    map_ptr.* = ast.Expression{ .MapLiteral = .{ .entries = entries_slice } };
    
    // Create single node containing the map
    const nodes_slice = try arena_allocator.alloc(ast.AstNode, 1);
    nodes_slice[0] = ast.AstNode{ .Expression = map_ptr.* };
    
    return ParseSourceResult{
        .arena = arena,
        .nodes = nodes_slice,
    };
}

/// Parse a top-level property in package.gene format
fn parseTopLevelProperty(alloc: std.mem.Allocator, toks: []const Token, depth: usize) !PackagePropertyResult {
    if (toks.len < 2) return error.UnexpectedEOF;
    if (depth > MAX_RECURSION_DEPTH) return error.MaxRecursionDepthExceeded;
    
    // First token should be property (^name)
    const prop_token = toks[0];
    if (prop_token.kind != .Ident) return error.ExpectedProperty;
    
    const ident = prop_token.kind.Ident;
    if (!std.mem.startsWith(u8, ident, "^")) {
        return error.ExpectedProperty;
    }
    
    // Extract property name (remove ^)
    const prop_name = ident[1..];
    
    // Create string key
    const key_ptr = try alloc.create(ast.Expression);
    key_ptr.* = ast.Expression{
        .Literal = .{ .value = .{ .String = try alloc.dupe(u8, prop_name) } }
    };
    
    // Parse value
    const value_result = try parseExpression(alloc, toks[1..], depth + 1);
    const value_ptr = try alloc.create(ast.Expression);
    value_ptr.* = value_result.node.Expression;
    
    return PackagePropertyResult{
        .entry = .{ .key = key_ptr, .value = value_ptr },
        .consumed = 1 + value_result.consumed,
    };
}

/// Parse a primary expression and handle postfix operators like field access
fn parsePostfixExpression(alloc: std.mem.Allocator, toks: []const Token, primary: ParseResult) ParserError!ParseResult {
    var current_pos = primary.consumed;
    var current_expr = primary.node.Expression;
    
    // Check for field access with /
    while (current_pos < toks.len and toks[current_pos].kind == .Slash) {
        current_pos += 1; // Skip /
        
        if (current_pos >= toks.len or toks[current_pos].kind != .Ident) {
            return error.UnexpectedToken;
        }
        
        const field_name = try alloc.dupe(u8, toks[current_pos].kind.Ident);
        current_pos += 1;
        
        const obj_ptr = try alloc.create(ast.Expression);
        obj_ptr.* = current_expr;
        
        current_expr = .{ .FieldAccess = .{
            .object = obj_ptr,
            .field_name = field_name,
        }};
    }
    
    return .{
        .node = .{ .Expression = current_expr },
        .consumed = current_pos,
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
    debug.log("parseExpression: About to process token at location {}: kind={s}", .{
        tok.loc,
        @tagName(tok.kind),
    });
    if (tok.kind == .Class) {
        std.debug.print("ERROR: Class token found in parseExpression context!\n", .{});
        std.debug.print("Token stream context:\n", .{});
        var i: usize = 0;
        while (i < @min(5, toks.len)) : (i += 1) {
            // std.debug.print("  [{d}] {s} at loc {d}\n", .{i, @tagName(toks[i].kind), toks[i].loc});
        }
    }
    return switch (tok.kind) {
        .Int => |val| {
            const literal_result = ParseResult{ .node = .{ .Expression = .{ .Literal = .{ .value = .{ .Int = val } } } }, .consumed = 1 };
            return parsePostfixExpression(alloc, toks, literal_result);
        },
        .Float => |val| {
            const literal_result = ParseResult{ .node = .{ .Expression = .{ .Literal = .{ .value = .{ .Float = val } } } }, .consumed = 1 };
            return parsePostfixExpression(alloc, toks, literal_result);
        },
        .Bool => |val| {
            const literal_result = ParseResult{ .node = .{ .Expression = .{ .Literal = .{ .value = .{ .Bool = val } } } }, .consumed = 1 };
            return parsePostfixExpression(alloc, toks, literal_result);
        },
        .String => |str| {
            // String tokens are already allocated by tokenizer
            const literal_result = ParseResult{ .node = .{ .Expression = .{ .Literal = .{ .value = .{ .String = str } } } }, .consumed = 1 };
            return parsePostfixExpression(alloc, toks, literal_result);
        },
        .Nil => {
            const literal_result = ParseResult{ .node = .{ .Expression = .{ .Literal = .{ .value = .{ .Nil = {} } } } }, .consumed = 1 };
            return parsePostfixExpression(alloc, toks, literal_result);
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
            // Check for postfix field access
            const var_result = ParseResult{ .node = .{ .Expression = .{ .Variable = .{ .name = name_copy } } }, .consumed = 1 };
            return parsePostfixExpression(alloc, toks, var_result);
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
                .Fnx => return parseFnx(alloc, toks, depth + 1),
                .If => return parseIf(alloc, toks, depth + 1),
                .Var => return parseVar(alloc, toks, depth + 1),
                .Do => return parseDoBlock(alloc, toks, depth + 1),
                .Class => {
                    // std.debug.print("DEBUG: Dispatching to parseClass\n", .{});
                    const class_result = parseClass(alloc, toks, depth + 1) catch |err| {
                        // std.debug.print("DEBUG: parseClass failed with error: {}\n", .{err});
                        return err;
                    };
                    // std.debug.print("DEBUG: parseClass succeeded, consumed={d}\n", .{class_result.consumed});
                    return class_result;
                },
                .New => return parseNew(alloc, toks, depth + 1),
                .Match => return parseMatch(alloc, toks, depth + 1),
                .Macro => return parseMacro(alloc, toks, depth + 1),
                .Ns => return parseNamespace(alloc, toks, depth + 1),
                .Import => return parseImport(alloc, toks, depth + 1),
                .For => return parseFor(alloc, toks, depth + 1),
                .Return => return parseReturn(alloc, toks, depth + 1),
                .Ident => {
                    // Check if this is a method call pattern (obj .method ...)
                    if (toks.len > 2 and toks[2].kind == .Dot) {
                        return parseMethodCall(alloc, toks, depth + 1);
                    }
                    // For more complex cases, we need to check if the identifier starts an expression
                    // that is followed by a dot (e.g., (t/data .method))
                    // First, try to parse as a general list to see if it's a method call
                    const list_result = parseList(alloc, toks, depth + 1) catch {
                        // If parseList fails, try parseCall
                        return parseCall(alloc, toks, depth + 1);
                    };
                    return list_result;
                },
                .Equals => {
                    // Handle field assignment: (= obj/field value)
                    return parseFieldAssignment(alloc, toks, depth + 1);
                },
                .Slash => {
                    // Check if this is a simple field access pattern: (/field)
                    if (toks.len == 4 and toks[2].kind == .Ident and toks[3].kind == .RParen) {
                        // This is (/field) - parse as implicit self field access
                        const field_name = try alloc.dupe(u8, toks[2].kind.Ident);
                        return .{
                            .node = .{ .Expression = .{ .FieldAccess = .{
                                .object = null, // implicit self
                                .field_name = field_name,
                            }}},
                            .consumed = 4, // (LParen, Slash, Ident, RParen)
                        };
                    }
                    // Check if this is an assignment pattern: (/field = value)
                    else if (toks.len > 3 and toks[2].kind == .Ident and toks[3].kind == .Equals) {
                        // This is (/field = value) which should be parsed as (= /field value)
                        // Create modified tokens to parse as assignment
                        var modified_toks = try alloc.alloc(Token, toks.len);
                        defer alloc.free(modified_toks);
                        
                        // Rearrange tokens: (/ field = value) -> (= /field value)
                        modified_toks[0] = toks[0]; // LParen
                        modified_toks[1] = toks[3]; // Equals
                        modified_toks[2] = toks[1]; // Slash
                        modified_toks[3] = toks[2]; // field name
                        // Copy remaining tokens
                        if (toks.len > 4) {
                            @memcpy(modified_toks[4..], toks[4..]);
                        }
                        
                        return parseFieldAssignment(alloc, modified_toks, depth + 1);
                    } else {
                        // Handle division operator as function call: (/ 10 5)
                        // Create an operator token as if it were an identifier
                        var modified_toks = try alloc.alloc(Token, toks.len);
                        @memcpy(modified_toks, toks);
                        modified_toks[1] = .{ .kind = .{ .Ident = "/" }, .loc = toks[1].loc };
                        defer alloc.free(modified_toks);
                        return parseCall(alloc, modified_toks, depth + 1);
                    }
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
        .Slash => {
            // Parse field access like /msg or obj/field
            if (toks.len < 2) return error.UnexpectedEOF;
            if (toks[1].kind != .Ident) return error.UnexpectedToken;
            
            const field_name = try alloc.dupe(u8, toks[1].kind.Ident);
            return .{
                .node = .{ .Expression = .{ .FieldAccess = .{
                    .object = null, // /field means self.field
                    .field_name = field_name,
                }}},
                .consumed = 2,
            };
        },
        .Percent => {
            // Parse unquote syntax %identifier
            if (toks.len < 2) return error.UnexpectedEOF;
            if (toks[1].kind != .Ident) return error.UnexpectedToken;
            
            const ident_name = toks[1].kind.Ident;
            // Create variable name with % prefix to indicate unquote
            const var_name = try std.fmt.allocPrint(alloc, "%{s}", .{ident_name});
            
            return .{
                .node = .{ .Expression = .{ .Variable = .{ .name = var_name } } },
                .consumed = 2,
            };
        },
        .RParen => error.UnexpectedRParen,
        else => {
            debug.log("ERROR: Unhandled token kind in parseExpression: {s} at location {}", .{@tagName(tok.kind), tok.loc});
            return error.InvalidExpression;
        },
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
        // Check if this is a property syntax (^name)
        var key_ptr: *ast.Expression = undefined;
        if (current_pos < toks.len and toks[current_pos].kind == .Ident) {
            const ident = toks[current_pos].kind.Ident;
            if (std.mem.startsWith(u8, ident, "^")) {
                // Property syntax: convert ^name to string "name"
                const property_name = ident[1..]; // Remove the ^ prefix
                
                // Handle boolean property shortcuts
                if (property_name.len > 0 and property_name[0] == '^') {
                    // ^^name means ^name true
                    const actual_name = property_name[1..];
                    key_ptr = try alloc.create(ast.Expression);
                    key_ptr.* = .{ .Literal = .{ .value = .{ .String = try alloc.dupe(u8, actual_name) } } };
                    current_pos += 1;
                    
                    // Value is true
                    const value_ptr = try alloc.create(ast.Expression);
                    value_ptr.* = .{ .Literal = .{ .value = .{ .Bool = true } } };
                    
                    try entries.append(.{ .key = key_ptr, .value = value_ptr });
                    continue;
                } else if (property_name.len > 0 and property_name[0] == '!') {
                    // ^!name means ^name nil
                    const actual_name = property_name[1..];
                    key_ptr = try alloc.create(ast.Expression);
                    key_ptr.* = .{ .Literal = .{ .value = .{ .String = try alloc.dupe(u8, actual_name) } } };
                    current_pos += 1;
                    
                    // Value is nil
                    const value_ptr = try alloc.create(ast.Expression);
                    value_ptr.* = .{ .Literal = .{ .value = .Nil } };
                    
                    try entries.append(.{ .key = key_ptr, .value = value_ptr });
                    continue;
                } else {
                    // Regular property: ^name becomes string "name"
                    key_ptr = try alloc.create(ast.Expression);
                    key_ptr.* = .{ .Literal = .{ .value = .{ .String = try alloc.dupe(u8, property_name) } } };
                    current_pos += 1;
                }
            } else {
                // Regular expression key
                const key_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
                current_pos += key_result.consumed;
                key_ptr = try alloc.create(ast.Expression);
                key_ptr.* = key_result.node.Expression;
            }
        } else {
            // Regular expression key
            const key_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
            current_pos += key_result.consumed;
            key_ptr = try alloc.create(ast.Expression);
            key_ptr.* = key_result.node.Expression;
        }

        // Parse value (expression)
        const value_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
        current_pos += value_result.consumed;

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

    // Check for implicit self method call pattern: (.method args...)
    if (first.kind == .Dot) {
        // Parse as (self .method args...)
        var parsing_pos = current_pos + 1; // Skip .
        
        if (parsing_pos >= toks.len or toks[parsing_pos].kind != .Ident) {
            return error.ExpectedMethodName;
        }
        
        const method_name = try alloc.dupe(u8, toks[parsing_pos].kind.Ident);
        parsing_pos += 1;
        
        // Parse arguments
        var args = std.ArrayList(*ast.Expression).init(alloc);
        errdefer args.deinit();
        
        while (parsing_pos < toks.len and toks[parsing_pos].kind != .RParen) {
            const arg_result = try parseExpression(alloc, toks[parsing_pos..], depth + 1);
            const arg_ptr = try alloc.create(ast.Expression);
            arg_ptr.* = arg_result.node.Expression;
            try args.append(arg_ptr);
            parsing_pos += arg_result.consumed;
        }
        
        if (parsing_pos >= toks.len or toks[parsing_pos].kind != .RParen) {
            return error.ExpectedRParen;
        }
        
        // Create implicit self variable
        const self_ptr = try alloc.create(ast.Expression);
        self_ptr.* = .{ .Variable = .{ .name = try alloc.dupe(u8, "self") } };
        
        return .{
            .node = .{
                .Expression = .{
                    .MethodCall = .{
                        .object = self_ptr,
                        .method_name = method_name,
                        .args = args,
                    },
                },
            },
            .consumed = parsing_pos + 1,
        };
    }

    // Check for field assignment pattern: (/field = value)
    if (first.kind == .Slash) {
        // Parse field assignment
        var parsing_pos = current_pos + 1; // Skip /
        
        if (parsing_pos >= toks.len or toks[parsing_pos].kind != .Ident) {
            return error.UnexpectedToken;
        }
        
        const field_name = try alloc.dupe(u8, toks[parsing_pos].kind.Ident);
        parsing_pos += 1;
        
        // Expect equals sign
        if (parsing_pos >= toks.len or toks[parsing_pos].kind != .Equals) {
            return error.ExpectedEquals;
        }
        parsing_pos += 1;
        
        // Parse the value
        if (parsing_pos >= toks.len) return error.UnexpectedEOF;
        const value_result = try parseExpression(alloc, toks[parsing_pos..], depth + 1);
        parsing_pos += value_result.consumed;
        
        // Expect closing paren
        if (parsing_pos >= toks.len or toks[parsing_pos].kind != .RParen) {
            return error.ExpectedRParen;
        }
        
        const value_ptr = try alloc.create(ast.Expression);
        value_ptr.* = value_result.node.Expression;
        
        return .{
            .node = .{ .Expression = .{ .FieldAssignment = .{
                .object = null, // null means implicit self
                .field_name = field_name,
                .value = value_ptr,
            }}},
            .consumed = parsing_pos + 1,
        };
    }

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
        .Ident => {
            // Handle identifier - could be start of a function call, field access, or method call
            // First check if it's a simple method call pattern: (ident .method ...)
            if (current_pos_after_first < toks.len and toks[current_pos_after_first].kind == .Dot) {
                return parseMethodCall(alloc, toks, depth + 1);
            }
            
            // Check if this might be a field access followed by method call
            // We need to look ahead to see if there's a pattern like: ident/field .method
            var lookahead_pos = current_pos_after_first;
            var has_field_access = false;
            
            // Look for field access pattern
            while (lookahead_pos < toks.len and toks[lookahead_pos].kind == .Slash) {
                lookahead_pos += 1; // Skip slash
                if (lookahead_pos >= toks.len or toks[lookahead_pos].kind != .Ident) {
                    break;
                }
                lookahead_pos += 1; // Skip field name
                has_field_access = true;
            }
            
            // Check if after field access we have a dot (method call)
            if (has_field_access and lookahead_pos < toks.len and toks[lookahead_pos].kind == .Dot) {
                // This is a method call on field access - parseMethodCall should handle it
                return parseMethodCall(alloc, toks, depth + 1);
            }
            
            // Otherwise, it's a regular function call
            return parseCall(alloc, toks, depth + 1);
        },
        else => {
            // This case implies something like `([ ...)` or `({ ...)` if not caught by parseExpression,
            // or other unhandled token kinds as the first element of a list.
            debug.log("parseList: Expected LParen or Literal after initial LParen, found {any}", .{first});
            return error.InvalidExpression;
        },
    }

    // Check for method call pattern: (literal .method ...)
    if (current_pos_after_first < toks.len and toks[current_pos_after_first].kind == .Dot) {
        // This is a method call on a literal
        return parseMethodCall(alloc, toks, depth + 1);
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

    // Check for special _ parameter (without brackets)
    if (current_pos < toks.len and toks[current_pos].kind == .Ident) {
        const ident = toks[current_pos].kind.Ident;
        if (std.mem.eql(u8, ident, "_")) {
            // _ means ignore all parameters
            // We'll create a special parameter to indicate this
            const underscore_param = try alloc.dupe(u8, "_");
            try params_list.append(.{ .name = underscore_param, .param_type = null });
            current_pos += 1; // Skip _
        } else {
            // This is not a valid parameter syntax - parameters must be in brackets
            // Continue to body parsing
        }
    } else if (current_pos < toks.len and toks[current_pos].kind == .LBracket) {
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
    
    // Parse the body expressions (can be multiple)
    var body_expressions = std.ArrayList(*ast.Expression).init(alloc);
    errdefer body_expressions.deinit();
    
    while (current_pos < toks.len and toks[current_pos].kind != .RParen) {
        const expr_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
        current_pos += expr_result.consumed;
        
        const expr_ptr = try alloc.create(ast.Expression);
        expr_ptr.* = expr_result.node.Expression;
        try body_expressions.append(expr_ptr);
    }

    if (current_pos >= toks.len or toks[current_pos].kind != .RParen) return error.ExpectedRParen;
    current_pos += 1; // Consume RParen

    // Always create a proper FuncDef with the full body expression, regardless of parameter count
    const params_slice = try params_list.toOwnedSlice(); // Transfers ownership
    
    // Create the body expression - if multiple expressions, wrap in a DoBlock
    const body_ptr = try alloc.create(ast.Expression);
    if (body_expressions.items.len == 1) {
        // Single expression - use it directly
        body_ptr.* = body_expressions.items[0].*;
        // Free the wrapper but not the expression itself
        alloc.destroy(body_expressions.items[0]);
        body_expressions.deinit();
    } else if (body_expressions.items.len > 1) {
        // Multiple expressions - wrap in a DoBlock
        const statements_slice = try body_expressions.toOwnedSlice();
        body_ptr.* = .{ .DoBlock = .{ .statements = statements_slice } };
    } else {
        // No body expressions - create a nil literal
        body_expressions.deinit();
        body_ptr.* = .{ .Literal = .{ .value = .{ .Nil = {} } } };
    }
    
    const fn_node: ast.AstNode = .{
        .Expression = .{
            .FuncDef = .{ .name = name_copy, .params = params_slice, .body = body_ptr },
        },
    };

    return .{
        .node = fn_node,
        .consumed = current_pos, // Total consumed tokens
    };
}

/// Parse a Gene anonymous function (fnx).
fn parseFnx(alloc: std.mem.Allocator, toks: []const Token, depth: usize) !ParseResult {
    // Expects (fnx [params]? body)
    // toks[0] is LParen, toks[1] is Fnx
    // Minimum: (fnx body) -> LParen, Fnx, body, RParen (4 tokens)
    if (toks.len < 4 or toks[0].kind != .LParen or toks[1].kind != .Fnx) return error.UnexpectedToken;
    if (depth > MAX_RECURSION_DEPTH) return error.MaxRecursionDepthExceeded;

    var current_pos: usize = 2; // Skip LParen and Fnx

    // Anonymous functions need unique names to avoid conflicts
    // Generate a name based on position in the token stream
    const unique_name = try std.fmt.allocPrint(alloc, "anon_{}", .{@intFromPtr(toks.ptr)});
    const name_copy = unique_name;

    var params_list = std.ArrayList(ast.FuncParam).init(alloc);
    errdefer params_list.deinit(); // Cleanup if error before transfer

    // Check for special _ parameter (without brackets)
    if (current_pos < toks.len and toks[current_pos].kind == .Ident) {
        const ident = toks[current_pos].kind.Ident;
        if (std.mem.eql(u8, ident, "_")) {
            // _ means ignore all parameters
            // We'll create a special parameter to indicate this
            const underscore_param = try alloc.dupe(u8, "_");
            try params_list.append(.{ .name = underscore_param, .param_type = null });
            current_pos += 1; // Skip _
        } else {
            // This is not a valid parameter syntax - parameters must be in brackets
            // Continue to body parsing
        }
    } else if (current_pos < toks.len and toks[current_pos].kind == .LBracket) {
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

    if (current_pos >= toks.len) return error.UnexpectedEOF;

    // Parse body expressions until RParen
    var body_expressions = std.ArrayList(*ast.Expression).init(alloc);
    errdefer body_expressions.deinit();
    
    while (current_pos < toks.len and toks[current_pos].kind != .RParen) {
        const body_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
        current_pos += body_result.consumed;
        
        const expr_ptr = try alloc.create(ast.Expression);
        expr_ptr.* = body_result.node.Expression;
        try body_expressions.append(expr_ptr);
    }

    if (current_pos >= toks.len or toks[current_pos].kind != .RParen) return error.ExpectedRParen;
    current_pos += 1; // Skip RParen

    // Always create a proper FuncDef with the full body expression, regardless of parameter count
    const params_slice = try params_list.toOwnedSlice(); // Transfers ownership
    
    // Create the body expression - if multiple expressions, wrap in a DoBlock
    const body_ptr = try alloc.create(ast.Expression);
    if (body_expressions.items.len == 1) {
        // Single expression - use it directly
        body_ptr.* = body_expressions.items[0].*;
        // Free the wrapper but not the expression itself
        alloc.destroy(body_expressions.items[0]);
        body_expressions.deinit();
    } else if (body_expressions.items.len > 1) {
        // Multiple expressions - wrap in a DoBlock
        const statements_slice = try body_expressions.toOwnedSlice();
        body_ptr.* = .{ .DoBlock = .{ .statements = statements_slice } };
    } else {
        // No body expressions - create a nil literal
        body_expressions.deinit();
        body_ptr.* = .{ .Literal = .{ .value = .{ .Nil = {} } } };
    }
    
    const fn_node: ast.AstNode = .{
        .Expression = .{
            .FuncDef = .{ .name = name_copy, .params = params_slice, .body = body_ptr },
        },
    };

    return .{
        .node = fn_node,
        .consumed = current_pos, // Total consumed tokens
    };
}

/// Parse a Gene pseudo macro definition.
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
fn parseMacro(alloc: std.mem.Allocator, toks: []const Token, depth: usize) !ParseResult {
    // Expects (macro name [params] body)
    // toks[0] is LParen, toks[1] is Macro
    // Minimum: (macro name [params] body) -> at least 7 tokens
    if (toks.len < 7 or toks[0].kind != .LParen or toks[1].kind != .Macro) return error.UnexpectedToken;
    if (depth > MAX_RECURSION_DEPTH) return error.MaxRecursionDepthExceeded;

    var current_pos: usize = 2; // Skip LParen and Macro

    // Parse macro name
    if (current_pos >= toks.len or toks[current_pos].kind != .Ident) return error.ExpectedFunctionName;
    const name_slice = toks[current_pos].kind.Ident;
    const name_copy = try alloc.dupe(u8, name_slice);
    current_pos += 1;

    // Parse parameters
    var params_list = std.ArrayList(ast.PseudoMacroDef.MacroParam).init(alloc);
    errdefer params_list.deinit();

    if (current_pos >= toks.len or toks[current_pos].kind != .LBracket) return error.ExpectedRBracket;
    current_pos += 1; // Skip '['

    while (current_pos < toks.len and toks[current_pos].kind != .RBracket) {
        if (current_pos >= toks.len or toks[current_pos].kind != .Ident) return error.ExpectedParameterName;
        const param_name_slice = toks[current_pos].kind.Ident;
        const param_name_copy = try alloc.dupe(u8, param_name_slice);
        current_pos += 1;

        // Check if this is a variadic parameter (ends with ...)
        var is_variadic = false;
        if (std.mem.endsWith(u8, param_name_slice, "...")) {
            is_variadic = true;
            // TODO: Handle variadic parameters properly
        }

        try params_list.append(.{ .name = param_name_copy, .is_variadic = is_variadic });
    }

    if (current_pos >= toks.len or toks[current_pos].kind != .RBracket) return error.ExpectedRBracket;
    current_pos += 1; // Skip ']'

    // Parse body - for now, parse multiple expressions as a do block
    var body_exprs = std.ArrayList(*ast.Expression).init(alloc);
    errdefer body_exprs.deinit();
    
    while (current_pos < toks.len and toks[current_pos].kind != .RParen) {
        const expr_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
        current_pos += expr_result.consumed;
        
        const expr_ptr = try alloc.create(ast.Expression);
        expr_ptr.* = expr_result.node.Expression;
        try body_exprs.append(expr_ptr);
    }
    
    if (current_pos >= toks.len or toks[current_pos].kind != .RParen) return error.ExpectedRParen;
    current_pos += 1; // Consume RParen
    
    // Create a do block for the body if there are multiple expressions
    const body_ptr = try alloc.create(ast.Expression);
    if (body_exprs.items.len == 1) {
        body_ptr.* = body_exprs.items[0].*;
        alloc.destroy(body_exprs.items[0]);
    } else {
        body_ptr.* = .{ .DoBlock = .{ .statements = try body_exprs.toOwnedSlice() } };
    }

    const params_slice = try params_list.toOwnedSlice();

    return .{
        .node = .{ .Expression = .{ .PseudoMacroDef = .{
            .name = name_copy,
            .params = params_slice,
            .body = body_ptr,
        }}},
        .consumed = current_pos,
    };
}

/// Parse a namespace declaration.
///
/// Expects (ns name body)
/// Example: (ns geometry (class Point ...))
fn parseNamespace(alloc: std.mem.Allocator, toks: []const Token, depth: usize) !ParseResult {
    if (toks.len < 4 or toks[0].kind != .LParen or toks[1].kind != .Ns) return error.UnexpectedToken;
    if (depth > MAX_RECURSION_DEPTH) return error.MaxRecursionDepthExceeded;

    var current_pos: usize = 2; // Skip LParen and Ns

    // Parse namespace name/path
    if (current_pos >= toks.len or toks[current_pos].kind != .Ident) return error.UnexpectedToken;
    const ns_name = try alloc.dupe(u8, toks[current_pos].kind.Ident);
    current_pos += 1;

    // Parse namespace body (usually expressions that define classes, functions, etc.)
    var body_exprs = std.ArrayList(*ast.Expression).init(alloc);
    errdefer body_exprs.deinit();

    while (current_pos < toks.len and toks[current_pos].kind != .RParen) {
        const expr_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
        current_pos += expr_result.consumed;
        
        const expr_ptr = try alloc.create(ast.Expression);
        expr_ptr.* = expr_result.node.Expression;
        try body_exprs.append(expr_ptr);
    }

    if (current_pos >= toks.len or toks[current_pos].kind != .RParen) return error.ExpectedRParen;
    current_pos += 1; // Consume RParen

    // Create namespace body (do block if multiple expressions, single expression otherwise)
    const body_ptr = try alloc.create(ast.Expression);
    if (body_exprs.items.len == 1) {
        body_ptr.* = body_exprs.items[0].*;
        alloc.destroy(body_exprs.items[0]);
        body_exprs.deinit();
    } else {
        body_ptr.* = .{ .DoBlock = .{ .statements = try body_exprs.toOwnedSlice() } };
    }

    return .{
        .node = .{ .Expression = .{ .NamespaceDecl = .{
            .name = ns_name,
            .body = body_ptr,
        }}},
        .consumed = current_pos,
    };
}

/// Parse an import statement.
///
/// Supports various forms:
/// - (import "module/path")              - Import all
/// - (import "module/path" => alias)     - Import with alias
/// - (import "module/path" :as alias)    - Import with alias (backward compatibility)
/// - (import "module/path" [item1 item2]) - Import specific items
/// - (import "module/path" [[old new]])   - Import with renaming
fn parseImport(alloc: std.mem.Allocator, toks: []const Token, depth: usize) !ParseResult {
    if (toks.len < 3 or toks[0].kind != .LParen or toks[1].kind != .Import) return error.UnexpectedToken;
    if (depth > MAX_RECURSION_DEPTH) return error.MaxRecursionDepthExceeded;

    var current_pos: usize = 2; // Skip LParen and Import

    // Parse module path (string or identifier, possibly with slashes and wildcards)
    if (current_pos >= toks.len) return error.UnexpectedEOF;
    
    // Handle module paths like genex/http/* which might be tokenized as multiple parts
    var path_parts = std.ArrayList(u8).init(alloc);
    defer path_parts.deinit();
    
    // Start with the first part
    switch (toks[current_pos].kind) {
        .String => |str| try path_parts.appendSlice(str),
        .Ident => |ident| try path_parts.appendSlice(ident),
        else => return error.UnexpectedToken,
    }
    current_pos += 1;
    
    // Handle paths with slashes and wildcards
    while (current_pos < toks.len) {
        if (toks[current_pos].kind == .Slash) {
            try path_parts.append('/');
            current_pos += 1;
            
            if (current_pos >= toks.len) return error.UnexpectedEOF;
            
            // After slash, expect identifier or *
            switch (toks[current_pos].kind) {
                .Ident => |ident| {
                    try path_parts.appendSlice(ident);
                    current_pos += 1;
                },
                else => return error.UnexpectedToken,
            }
        } else {
            break;
        }
    }
    
    const module_path = try path_parts.toOwnedSlice();

    var alias: ?[]const u8 = null;
    var items: ?[]ast.ImportStmt.ImportItem = null;

    // Check for :as alias or specific imports
    debug.log("After parsing module path '{s}', current_pos={}, token={any}", .{module_path, current_pos, if (current_pos < toks.len) toks[current_pos] else null});
    if (current_pos < toks.len and toks[current_pos].kind != .RParen) {
        // Check for => pattern (alias)
        if (toks[current_pos].kind == .Ident and std.mem.eql(u8, toks[current_pos].kind.Ident, "=>")) {
            current_pos += 1; // Skip =>
            if (current_pos >= toks.len or toks[current_pos].kind != .Ident) return error.UnexpectedToken;
            alias = try alloc.dupe(u8, toks[current_pos].kind.Ident);
            current_pos += 1;
        // For backward compatibility, also support :as
        } else if (toks[current_pos].kind == .Ident and std.mem.eql(u8, toks[current_pos].kind.Ident, ":as")) {
            current_pos += 1; // Skip :as
            if (current_pos >= toks.len or toks[current_pos].kind != .Ident) return error.UnexpectedToken;
            alias = try alloc.dupe(u8, toks[current_pos].kind.Ident);
            current_pos += 1;
        } else if (toks[current_pos].kind == .LBracket) {
            // Parse specific imports [item1 item2 ...] or [[old new] ...]
            current_pos += 1; // Skip LBracket
            
            var import_items = std.ArrayList(ast.ImportStmt.ImportItem).init(alloc);
            errdefer import_items.deinit();
            
            while (current_pos < toks.len and toks[current_pos].kind != .RBracket) {
                if (toks[current_pos].kind == .LBracket) {
                    // Parse [old new] for renaming
                    current_pos += 1; // Skip inner LBracket
                    
                    if (current_pos >= toks.len or toks[current_pos].kind != .Ident) return error.UnexpectedToken;
                    const old_name = try alloc.dupe(u8, toks[current_pos].kind.Ident);
                    current_pos += 1;
                    
                    if (current_pos >= toks.len or toks[current_pos].kind != .Ident) return error.UnexpectedToken;
                    const new_name = try alloc.dupe(u8, toks[current_pos].kind.Ident);
                    current_pos += 1;
                    
                    if (current_pos >= toks.len or toks[current_pos].kind != .RBracket) return error.ExpectedRBracket;
                    current_pos += 1; // Skip inner RBracket
                    
                    try import_items.append(.{ .name = old_name, .alias = new_name });
                } else if (toks[current_pos].kind == .Ident) {
                    // Simple import item
                    const item_name = try alloc.dupe(u8, toks[current_pos].kind.Ident);
                    current_pos += 1;
                    try import_items.append(.{ .name = item_name, .alias = null });
                } else {
                    return error.UnexpectedToken;
                }
            }
            
            if (current_pos >= toks.len or toks[current_pos].kind != .RBracket) return error.ExpectedRBracket;
            current_pos += 1; // Skip RBracket
            
            items = try import_items.toOwnedSlice();
        }
    }

    if (current_pos >= toks.len or toks[current_pos].kind != .RParen) return error.ExpectedRParen;
    current_pos += 1; // Consume RParen

    return .{
        .node = .{ .Expression = .{ .ImportStmt = .{
            .module_path = module_path,
            .alias = alias,
            .items = items,
        }}},
        .consumed = current_pos,
    };
}

/// Parse a for-in loop.
///
/// Expects (for iterator in iterable body)
/// Example: (for x in [1 2 3] (print x))
fn parseFor(alloc: std.mem.Allocator, toks: []const Token, depth: usize) !ParseResult {
    // Minimum: (for var in expr body) -> at least 6 tokens
    if (toks.len < 6 or toks[0].kind != .LParen or toks[1].kind != .For) return error.UnexpectedToken;
    if (depth > MAX_RECURSION_DEPTH) return error.MaxRecursionDepthExceeded;

    var current_pos: usize = 2; // Skip LParen and For

    // Parse iterator variable name
    if (current_pos >= toks.len or toks[current_pos].kind != .Ident) return error.UnexpectedToken;
    const iterator_name = try alloc.dupe(u8, toks[current_pos].kind.Ident);
    current_pos += 1;

    // Expect 'in' keyword
    if (current_pos >= toks.len or toks[current_pos].kind != .In) return error.UnexpectedToken;
    current_pos += 1;

    // Parse iterable expression
    if (current_pos >= toks.len) return error.UnexpectedEOF;
    const iterable_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
    current_pos += iterable_result.consumed;

    // Parse body - collect all remaining expressions until RParen
    var body_exprs = std.ArrayList(*ast.Expression).init(alloc);
    errdefer body_exprs.deinit();
    
    while (current_pos < toks.len and toks[current_pos].kind != .RParen) {
        const expr_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
        current_pos += expr_result.consumed;
        
        const expr_ptr = try alloc.create(ast.Expression);
        expr_ptr.* = expr_result.node.Expression;
        try body_exprs.append(expr_ptr);
    }

    if (current_pos >= toks.len or toks[current_pos].kind != .RParen) return error.ExpectedRParen;
    current_pos += 1; // Consume RParen

    // Create body expression - wrap multiple expressions in DoBlock
    const body_ptr = try alloc.create(ast.Expression);
    if (body_exprs.items.len == 1) {
        body_ptr.* = body_exprs.items[0].*;
        alloc.destroy(body_exprs.items[0]);
    } else {
        body_ptr.* = .{ .DoBlock = .{ .statements = try body_exprs.toOwnedSlice() } };
    }

    // Create iterable expression pointer
    const iterable_ptr = try alloc.create(ast.Expression);
    iterable_ptr.* = iterable_result.node.Expression;

    return .{
        .node = .{ .Expression = .{ .ForLoop = .{
            .iterator = iterator_name,
            .iterable = iterable_ptr,
            .body = body_ptr,
        }}},
        .consumed = current_pos,
    };
}

/// Parse a return statement.
///
/// Expects (return) or (return value)
/// Example: (return 42), (return)
fn parseReturn(alloc: std.mem.Allocator, toks: []const Token, depth: usize) !ParseResult {
    // Minimum: (return) -> at least 3 tokens
    if (toks.len < 3 or toks[0].kind != .LParen or toks[1].kind != .Return) return error.UnexpectedToken;
    if (depth > MAX_RECURSION_DEPTH) return error.MaxRecursionDepthExceeded;

    var current_pos: usize = 2; // Skip LParen and Return

    // Check if there's a value to return
    var return_value: ?*ast.Expression = null;
    if (current_pos < toks.len and toks[current_pos].kind != .RParen) {
        // Parse the return value
        const value_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
        current_pos += value_result.consumed;
        
        const value_ptr = try alloc.create(ast.Expression);
        value_ptr.* = value_result.node.Expression;
        return_value = value_ptr;
    }

    if (current_pos >= toks.len or toks[current_pos].kind != .RParen) return error.ExpectedRParen;
    current_pos += 1; // Consume RParen

    return .{
        .node = .{ .Expression = .{ .Return = .{ .value = return_value } } },
        .consumed = current_pos,
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
fn parseFieldAssignment(alloc: std.mem.Allocator, toks: []const Token, depth: usize) !ParseResult {
    // Expects (= obj/field value) or (= /field value)
    // toks[0] is LParen, toks[1] is Equals
    // Minimum: (= target value) -> LParen, Equals, target, value, RParen (5 tokens)
    if (toks.len < 5 or toks[0].kind != .LParen or toks[1].kind != .Equals) return error.UnexpectedToken;
    if (depth > MAX_RECURSION_DEPTH) return error.MaxRecursionDepthExceeded;

    var current_pos: usize = 2; // Skip LParen and Equals
    
    // Parse the target (should be a field access)
    const target_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
    current_pos += target_result.consumed;
    
    // Verify target is a field access
    if (target_result.node.Expression != .FieldAccess) {
        return error.InvalidExpression; // Assignment target must be a field access
    }
    
    // Parse the value
    const value_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
    current_pos += value_result.consumed;
    
    if (current_pos >= toks.len or toks[current_pos].kind != .RParen) return error.ExpectedRParen;
    current_pos += 1; // Consume RParen
    
    const value_ptr = try alloc.create(ast.Expression);
    value_ptr.* = value_result.node.Expression;
    
    return ParseResult{
        .node = .{ .Expression = .{ .FieldAssignment = .{
            .object = target_result.node.Expression.FieldAccess.object,
            .field_name = target_result.node.Expression.FieldAccess.field_name,
            .value = value_ptr,
        }}},
        .consumed = current_pos,
    };
}

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
    // std.debug.print("DEBUG: parseClass called with {} tokens\n", .{toks.len});
    if (toks.len < 4 or toks[0].kind != .LParen or toks[1].kind != .Class) return error.UnexpectedToken;
    if (depth > MAX_RECURSION_DEPTH) return error.MaxRecursionDepthExceeded;

    var current_pos: usize = 2; // Skip LParen and Class

    // Get class name
    if (current_pos >= toks.len or toks[current_pos].kind != .Ident) return error.ExpectedClassName;
    const class_name = try alloc.dupe(u8, toks[current_pos].kind.Ident);
    current_pos += 1;
    
    // Check for inheritance with < syntax
    var parent_class: ?[]const u8 = null;
    if (current_pos < toks.len and toks[current_pos].kind == .Ident) {
        const ident = toks[current_pos].kind.Ident;
        if (std.mem.eql(u8, ident, "<")) {
            current_pos += 1; // Skip <
            if (current_pos >= toks.len or toks[current_pos].kind != .Ident) return error.ExpectedParentClassName;
            parent_class = try alloc.dupe(u8, toks[current_pos].kind.Ident);
            current_pos += 1;
        }
    }

    var fields = std.ArrayList(ast.ClassDef.ClassField).init(alloc);
    errdefer fields.deinit();
    
    var methods = std.ArrayList(ast.ClassDef.ClassMethod).init(alloc);
    errdefer methods.deinit();

    // Parse class body - looking for field and method definitions
    debug.log("Parsing class body for {s}", .{class_name});
    while (current_pos < toks.len and toks[current_pos].kind != .RParen) {
        debug.log("Class body token at {}: {any}", .{current_pos, toks[current_pos].kind});
        if (toks[current_pos].kind == .LParen) {
            // Check what kind of definition this is
            if (current_pos + 1 < toks.len) {
                debug.log("  Next token at {}: {any}", .{current_pos + 1, toks[current_pos + 1].kind});
                if (current_pos + 2 < toks.len) {
                    debug.log("  Token at {}: {any}", .{current_pos + 2, toks[current_pos + 2].kind});
                }
                if (toks[current_pos + 1].kind == .Dot and current_pos + 2 < toks.len) {
                    // Handle .method syntax (.fn, .prop, .ctor)
                    const token_kind = toks[current_pos + 2].kind;
                    
                    // Check if it's .fn (keyword) or .prop/.ctor (identifier)
                    if (token_kind == .Fn) {
                        // Method definition using .fn keyword
                        debug.log("Found .fn method in class body", .{});
                        debug.log("Parsing .fn method definition", .{});
                        const method_result = try parseClassMethod(alloc, toks[current_pos..], depth + 1);
                        try methods.append(method_result.method);
                        current_pos += method_result.consumed;
                        debug.log("Added method: {s}", .{method_result.method.name});
                    } else if (token_kind == .Ident) {
                        const method_type = token_kind.Ident;
                        debug.log("Found dot-method in class body: .{s}", .{method_type});
                        if (std.mem.eql(u8, method_type, "prop")) {
                            // Property definition with type
                            const field_result = try parseClassProperty(alloc, toks[current_pos..], depth + 1);
                            try fields.append(field_result.field);
                            current_pos += field_result.consumed;
                        } else if (std.mem.eql(u8, method_type, "ctor")) {
                            // Constructor definition
                            debug.log("Parsing constructor definition", .{});
                            const ctor_result = try parseClassConstructor(alloc, toks[current_pos..], class_name, depth + 1);
                            try methods.append(ctor_result.method);
                            // Add auto-property field if present
                            if (ctor_result.field) |field| {
                                try fields.append(field);
                            }
                            current_pos += ctor_result.consumed;
                            debug.log("Added constructor", .{});
                        } else if (std.mem.eql(u8, method_type, "method")) {
                            // Method definition using .method syntax (same as .fn)
                            debug.log("Parsing .method definition", .{});
                            const method_result = try parseClassMethod(alloc, toks[current_pos..], depth + 1);
                            try methods.append(method_result.method);
                            current_pos += method_result.consumed;
                            debug.log("Added method: {s}", .{method_result.method.name});
                        } else {
                            // Unknown .method construct, skip it
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
                } else if (toks[current_pos + 1].kind == .Ident) {
                    const ident = toks[current_pos + 1].kind.Ident;
                    debug.log("Found ident in class body: {s}", .{ident});
                    if (std.mem.eql(u8, ident, "fn")) {
                        // Method definition without dot
                        debug.log("Parsing method definition", .{});
                        const method_result = try parseClassMethod(alloc, toks[current_pos..], depth + 1);
                        try methods.append(method_result.method);
                        current_pos += method_result.consumed;
                        debug.log("Added method: {s}", .{method_result.method.name});
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
                // } else if (toks[current_pos + 1].kind == .Var) {
                //     // Instance variable declaration (var /name) - REMOVED: Invalid syntax
                //     debug.log("Found var declaration in class body", .{});
                //     const var_result = try parseClassVar(alloc, toks[current_pos..], depth + 1);
                //     try fields.append(var_result.field);
                //     current_pos += var_result.consumed;
                //     debug.log("Added instance variable: {s}", .{var_result.field.name});
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
        // } else if (toks[current_pos].kind == .LBracket) {
        //     // Field definition with bracket syntax [name value] - REMOVED: Invalid syntax
        //     debug.log("Found bracket field definition in class body", .{});
        //     const field_result = try parseClassBracketField(alloc, toks[current_pos..], depth + 1);
        //     try fields.append(field_result.field);
        //     current_pos += field_result.consumed;
        //     debug.log("Added field: {s}", .{field_result.field.name});
        } else {
            // Skip non-paren tokens
            // std.debug.print("DEBUG: parseClass skipping token at pos={d}: {s}\n", .{current_pos, @tagName(toks[current_pos].kind)});
            current_pos += 1;
        }
    }

    if (current_pos >= toks.len or toks[current_pos].kind != .RParen) {
        // std.debug.print("DEBUG: parseClass ExpectedRParen at pos={d}, token={s}\n", .{current_pos, if (current_pos < toks.len) @tagName(toks[current_pos].kind) else "EOF"});
        return error.ExpectedRParen;
    }
    current_pos += 1;

    const fields_slice = try fields.toOwnedSlice();
    const methods_slice = try methods.toOwnedSlice();

    // std.debug.print("DEBUG: parseClass returning, consumed={d} tokens\n", .{current_pos});

    return ParseResult{
        .node = .{ 
            .Expression = .{ 
                .ClassDef = .{
                    .name = class_name,
                    .fields = fields_slice,
                    .methods = methods_slice,
                    .parent_class = parent_class,
                    .traits = &[_][]const u8{},
                }
            }
        },
        .consumed = current_pos,
    };
}


const PropertyParseResult = struct {
    field: ast.ClassDef.ClassField,
    consumed: usize,
};

fn parseClassBracketField(alloc: std.mem.Allocator, toks: []const Token, depth: usize) !PropertyParseResult {
    // Parse [name value] format
    if (toks.len < 3 or toks[0].kind != .LBracket) return error.UnexpectedToken;
    if (depth > MAX_RECURSION_DEPTH) return error.MaxRecursionDepthExceeded;
    
    var current_pos: usize = 1; // Skip LBracket
    
    // Get field name
    if (current_pos >= toks.len or toks[current_pos].kind != .Ident) return error.ExpectedFieldName;
    const field_name = try alloc.dupe(u8, toks[current_pos].kind.Ident);
    current_pos += 1;
    
    // Get default value (required in bracket syntax)
    var default_value: ?*ast.Expression = null;
    if (current_pos < toks.len and toks[current_pos].kind != .RBracket) {
        const value_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
        if (value_result.node == .Expression) {
            const value_ptr = try alloc.create(ast.Expression);
            value_ptr.* = value_result.node.Expression;
            default_value = value_ptr;
        }
        current_pos += value_result.consumed;
    }
    
    if (current_pos >= toks.len or toks[current_pos].kind != .RBracket) return error.ExpectedRBracket;
    current_pos += 1;
    
    return PropertyParseResult{
        .field = .{
            .name = field_name,
            .type_annotation = null,
            .default_value = default_value,
            .is_public = true, // Default to public
        },
        .consumed = current_pos,
    };
}

fn parseClassVar(alloc: std.mem.Allocator, toks: []const Token, depth: usize) !PropertyParseResult {
    // Parse (var /name) or (var /name initial-value) format
    if (toks.len < 4 or toks[0].kind != .LParen or toks[1].kind != .Var) return error.UnexpectedToken;
    if (depth > MAX_RECURSION_DEPTH) return error.MaxRecursionDepthExceeded;
    
    var current_pos: usize = 2; // Skip LParen and var
    
    // Get variable name - must start with /
    if (current_pos >= toks.len or toks[current_pos].kind != .Slash) return error.ExpectedSlashForInstanceVar;
    current_pos += 1;
    
    if (current_pos >= toks.len or toks[current_pos].kind != .Ident) return error.ExpectedInstanceVarName;
    const var_name = try alloc.dupe(u8, toks[current_pos].kind.Ident);
    current_pos += 1;
    
    // Check for optional initial value
    var default_value: ?*ast.Expression = null;
    if (current_pos < toks.len and toks[current_pos].kind != .RParen) {
        const value_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
        if (value_result.node == .Expression) {
            const value_ptr = try alloc.create(ast.Expression);
            value_ptr.* = value_result.node.Expression;
            default_value = value_ptr;
        }
        current_pos += value_result.consumed;
    }
    
    if (current_pos >= toks.len or toks[current_pos].kind != .RParen) return error.ExpectedRParen;
    current_pos += 1;
    
    return PropertyParseResult{
        .field = .{
            .name = var_name,
            .type_annotation = null,
            .default_value = default_value,
            .is_public = true, // Default to public for now
        },
        .consumed = current_pos,
    };
}

fn parseClassProperty(alloc: std.mem.Allocator, toks: []const Token, depth: usize) !PropertyParseResult {
    // Parse (.prop name Type) format
    if (toks.len < 5 or toks[0].kind != .LParen) return error.UnexpectedToken;
    if (depth > MAX_RECURSION_DEPTH) return error.MaxRecursionDepthExceeded;
    
    var current_pos: usize = 1; // Skip LParen
    
    // Skip dot if present
    if (current_pos < toks.len and toks[current_pos].kind == .Dot) {
        current_pos += 1;
    }
    
    // Skip prop keyword
    if (current_pos >= toks.len or toks[current_pos].kind != .Ident or 
        !std.mem.eql(u8, toks[current_pos].kind.Ident, "prop")) {
        return error.UnexpectedToken;
    }
    current_pos += 1;
    
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

const ConstructorParseResult = struct {
    method: ast.ClassDef.ClassMethod,
    field: ?ast.ClassDef.ClassField, // Optional field for auto-property
    consumed: usize,
};

fn parseClassMethod(alloc: std.mem.Allocator, toks: []const Token, depth: usize) !MethodParseResult {
    // Parse (.fn name [params] body) format
    if (toks.len < 5 or toks[0].kind != .LParen) return error.UnexpectedToken;
    if (depth > MAX_RECURSION_DEPTH) return error.MaxRecursionDepthExceeded;
    
    var current_pos: usize = 1; // Skip LParen
    
    // Skip dot if present
    if (current_pos < toks.len and toks[current_pos].kind == .Dot) {
        current_pos += 1;
    }
    
    // Skip fn/method keyword (can be either .Fn keyword or .Ident "fn"/"method")
    if (current_pos >= toks.len) return error.UnexpectedToken;
    
    if (toks[current_pos].kind == .Fn) {
        // It's the Fn keyword
        current_pos += 1;
    } else if (toks[current_pos].kind == .Ident) {
        const ident = toks[current_pos].kind.Ident;
        if (std.mem.eql(u8, ident, "fn") or std.mem.eql(u8, ident, "method")) {
            // It's an identifier "fn" or "method"
            current_pos += 1;
        } else {
            return error.UnexpectedToken;
        }
    } else {
        return error.UnexpectedToken;
    }
    
    // Get method name
    if (current_pos >= toks.len or toks[current_pos].kind != .Ident) return error.ExpectedMethodName;
    const method_name = try alloc.dupe(u8, toks[current_pos].kind.Ident);
    current_pos += 1;
    
    // Parse parameters
    var params = std.ArrayList(ast.ClassDef.Parameter).init(alloc);
    errdefer params.deinit();
    
    // Check for special _ parameter (ignore all args)
    if (current_pos < toks.len and toks[current_pos].kind == .Ident) {
        const ident = toks[current_pos].kind.Ident;
        if (std.mem.eql(u8, ident, "_")) {
            // _ means ignore all parameters, create a special parameter
            // For now, we'll treat it as having no parameters
            current_pos += 1; // Skip _
        } else {
            // Regular identifier parameter without brackets (single param)
            const param_name = try alloc.dupe(u8, ident);
            current_pos += 1;
            
            try params.append(.{
                .name = param_name,
                .type_annotation = null,
                .default_value = null,
            });
        }
    } else if (current_pos < toks.len and toks[current_pos].kind == .LBracket) {
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
    
    // Parse body - collect all expressions until RParen
    var body_exprs = std.ArrayList(*ast.Expression).init(alloc);
    errdefer body_exprs.deinit();
    
    while (current_pos < toks.len and toks[current_pos].kind != .RParen) {
        const expr_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
        current_pos += expr_result.consumed;
        
        const expr_ptr = try alloc.create(ast.Expression);
        expr_ptr.* = try expr_result.node.Expression.clone(alloc);
        try body_exprs.append(expr_ptr);
    }
    
    // Create body expression - wrap multiple expressions in DoBlock
    const body_ptr = try alloc.create(ast.Expression);
    if (body_exprs.items.len == 0) {
        // Empty body - default to nil
        body_ptr.* = .{ .Literal = .{ .value = .{ .Nil = {} } } };
        body_exprs.deinit();
    } else if (body_exprs.items.len == 1) {
        body_ptr.* = body_exprs.items[0].*;
        alloc.destroy(body_exprs.items[0]);
        body_exprs.deinit();
    } else {
        body_ptr.* = .{ .DoBlock = .{ .statements = try body_exprs.toOwnedSlice() } };
    }
    
    if (current_pos >= toks.len or toks[current_pos].kind != .RParen) return error.ExpectedRParen;
    current_pos += 1;
    
    return MethodParseResult{
        .method = .{
            .name = method_name,
            .params = try params.toOwnedSlice(),
            .return_type = null,
            .body = body_ptr,
            .visibility = .Public,
            .method_type = .Regular,
            .is_virtual = false,
            .is_abstract = false,
        },
        .consumed = current_pos,
    };
}

fn parseClassConstructor(alloc: std.mem.Allocator, toks: []const Token, class_name: []const u8, depth: usize) !ConstructorParseResult {
    // Parse (.ctor [params] body) or (.ctor /param body) format
    // Constructor is a special method named "init" or class_name_init
    if (toks.len < 4 or toks[0].kind != .LParen) return error.UnexpectedToken;
    if (depth > MAX_RECURSION_DEPTH) return error.MaxRecursionDepthExceeded;
    
    var current_pos: usize = 1; // Skip LParen
    
    // Skip dot if present
    if (current_pos < toks.len and toks[current_pos].kind == .Dot) {
        current_pos += 1;
    }
    
    // Skip ctor keyword
    if (current_pos >= toks.len or toks[current_pos].kind != .Ident or 
        !std.mem.eql(u8, toks[current_pos].kind.Ident, "ctor")) {
        return error.UnexpectedToken;
    }
    current_pos += 1;
    
    // Parse parameters
    var params = std.ArrayList(ast.ClassDef.Parameter).init(alloc);
    errdefer params.deinit();
    
    // Track auto-property parameter if present
    var auto_property_param: ?[]const u8 = null;
    
    // Check for special _ parameter (ignore all args)
    if (current_pos < toks.len and toks[current_pos].kind == .Ident) {
        const ident = toks[current_pos].kind.Ident;
        if (std.mem.eql(u8, ident, "_")) {
            // _ means ignore all parameters
            current_pos += 1; // Skip _
        } else {
            // Regular identifier parameter without brackets (should not happen for constructor)
            return error.UnexpectedToken;
        }
    } else if (current_pos < toks.len and toks[current_pos].kind == .Slash) {
        // Auto-property constructor syntax: (.ctor /param_name ...)
        current_pos += 1; // Skip '/'
        if (current_pos >= toks.len or toks[current_pos].kind != .Ident) return error.ExpectedParameterName;
        
        const param_name = try alloc.dupe(u8, toks[current_pos].kind.Ident);
        current_pos += 1;
        
        try params.append(.{
            .name = param_name,
            .type_annotation = null,
            .default_value = null,
        });
        
        // For auto-property constructor, we need to track this for later
        // so we can generate the assignment in the body
        auto_property_param = param_name;
    } else if (current_pos < toks.len and toks[current_pos].kind == .LBracket) {
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
    
    // Parse body - collect all expressions until RParen
    var body_exprs = std.ArrayList(*ast.Expression).init(alloc);
    errdefer body_exprs.deinit();
    
    while (current_pos < toks.len and toks[current_pos].kind != .RParen) {
        const expr_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
        current_pos += expr_result.consumed;
        
        const expr_ptr = try alloc.create(ast.Expression);
        expr_ptr.* = try expr_result.node.Expression.clone(alloc);
        try body_exprs.append(expr_ptr);
    }
    
    // Create body expression - wrap multiple expressions in DoBlock
    var body_ptr: *ast.Expression = undefined;
    
    // If this is an auto-property constructor, wrap the body with an assignment
    if (auto_property_param) |param| {
        // Create a do block with all statements:
        // 1. (= /param param) - assign parameter to field
        // 2. All the body expressions
        
        var statements = std.ArrayList(*ast.Expression).init(alloc);
        errdefer statements.deinit();
        
        // Create the field assignment: (= /param param)
        const field_access = try alloc.create(ast.Expression);
        field_access.* = .{ .FieldAccess = .{
            .object = null, // implicit self
            .field_name = try alloc.dupe(u8, param),
        }};
        
        const param_var = try alloc.create(ast.Expression);
        param_var.* = .{ .Variable = .{ .name = try alloc.dupe(u8, param) }};
        
        const assignment = try alloc.create(ast.Expression);
        assignment.* = .{ .FieldAssignment = .{
            .object = null, // implicit self
            .field_name = try alloc.dupe(u8, param),
            .value = param_var,
        }};
        
        try statements.append(assignment);
        
        // Add all the body expressions
        for (body_exprs.items) |expr| {
            try statements.append(expr);
        }
        body_exprs.deinit();
        
        // Create the do block
        body_ptr = try alloc.create(ast.Expression);
        body_ptr.* = .{ .DoBlock = .{
            .statements = try statements.toOwnedSlice(),
        }};
    } else {
        body_ptr = try alloc.create(ast.Expression);
        if (body_exprs.items.len == 0) {
            // Empty body - default to nil
            body_ptr.* = .{ .Literal = .{ .value = .{ .Nil = {} } } };
            body_exprs.deinit();
        } else if (body_exprs.items.len == 1) {
            body_ptr.* = body_exprs.items[0].*;
            alloc.destroy(body_exprs.items[0]);
            body_exprs.deinit();
        } else {
            body_ptr.* = .{ .DoBlock = .{ .statements = try body_exprs.toOwnedSlice() } };
        }
    }
    
    if (current_pos >= toks.len or toks[current_pos].kind != .RParen) return error.ExpectedRParen;
    current_pos += 1;
    
    // Constructor is named ClassName_init
    const ctor_name = try std.fmt.allocPrint(alloc, "{s}_init", .{class_name});
    
    // Create field if this is an auto-property constructor
    var field: ?ast.ClassDef.ClassField = null;
    if (auto_property_param) |param| {
        field = .{
            .name = try alloc.dupe(u8, param),
            .type_annotation = null, // TODO: could infer from constructor parameter type
            .default_value = null,
            .is_public = true,
        };
    }
    
    return ConstructorParseResult{
        .method = .{
            .name = ctor_name,
            .params = try params.toOwnedSlice(),
            .return_type = null,
            .body = body_ptr,
            .visibility = .Public,
            .method_type = .Regular,
            .is_virtual = false,
            .is_abstract = false,
        },
        .field = field,
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

fn parseMethodCall(alloc: std.mem.Allocator, toks: []const Token, depth: usize) !ParseResult {
    // Expects (obj .method arg1 arg2 ...)
    // Minimum: (obj .method) -> LParen, Ident, Dot, Ident, RParen (5 tokens)
    if (toks.len < 5 or toks[0].kind != .LParen) return error.UnexpectedToken;
    if (depth > MAX_RECURSION_DEPTH) return error.MaxRecursionDepthExceeded;

    var current_pos: usize = 1; // Skip LParen

    // Parse the object expression
    const obj_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
    current_pos += obj_result.consumed;

    // Expect a dot
    if (current_pos >= toks.len or toks[current_pos].kind != .Dot) return error.UnexpectedToken;
    current_pos += 1; // Skip dot

    // Expect method name (could be identifier or operator)
    if (current_pos >= toks.len) return error.ExpectedMethodName;
    
    const method_name = switch (toks[current_pos].kind) {
        .Ident => |name| try alloc.dupe(u8, name),
        .Slash => try alloc.dupe(u8, "/"),
        .Percent => try alloc.dupe(u8, "%"),
        else => return error.ExpectedMethodName,
    };
    current_pos += 1;

    // Parse arguments
    var args = std.ArrayList(*ast.Expression).init(alloc);
    errdefer args.deinit();

    while (current_pos < toks.len and toks[current_pos].kind != .RParen) {
        const arg_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
        const arg_ptr = try alloc.create(ast.Expression);
        arg_ptr.* = arg_result.node.Expression;
        try args.append(arg_ptr);
        current_pos += arg_result.consumed;
    }

    if (current_pos >= toks.len or toks[current_pos].kind != .RParen) return error.ExpectedRParen;
    current_pos += 1;

    const obj_ptr = try alloc.create(ast.Expression);
    obj_ptr.* = obj_result.node.Expression;

    return ParseResult{
        .node = .{
            .Expression = .{
                .MethodCall = .{
                    .object = obj_ptr,
                    .method_name = method_name,
                    .args = args,
                },
            },
        },
        .consumed = current_pos,
    };
}

fn parseMatch(alloc: std.mem.Allocator, toks: []const Token, depth: usize) !ParseResult {
    // Expects (match scrutinee (pattern1 expr1) (pattern2 expr2) ...)
    // Minimum: (match x (1 "one")) -> at least 7 tokens
    if (toks.len < 7 or toks[0].kind != .LParen or toks[1].kind != .Match) return error.UnexpectedToken;
    if (depth > MAX_RECURSION_DEPTH) return error.MaxRecursionDepthExceeded;

    var current_pos: usize = 2; // Skip LParen and Match

    // Parse scrutinee (the value being matched)
    if (current_pos >= toks.len) return error.UnexpectedEOF;
    const scrutinee_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
    current_pos += scrutinee_result.consumed;

    const scrutinee_ptr = try alloc.create(ast.Expression);
    scrutinee_ptr.* = try scrutinee_result.node.Expression.clone(alloc);

    // Parse match arms
    var arms = std.ArrayList(ast.MatchExpr.MatchArm).init(alloc);
    errdefer arms.deinit();

    while (current_pos < toks.len and toks[current_pos].kind != .RParen) {
        // Each arm should be (pattern body)
        if (toks[current_pos].kind != .LParen) return error.UnexpectedToken;
        current_pos += 1; // Skip LParen

        // Parse pattern
        const pattern = try parsePattern(alloc, toks[current_pos..], depth + 1);
        current_pos += pattern.consumed;

        // Parse body
        if (current_pos >= toks.len) return error.UnexpectedEOF;
        const body_result = try parseExpression(alloc, toks[current_pos..], depth + 1);
        current_pos += body_result.consumed;

        const body_ptr = try alloc.create(ast.Expression);
        body_ptr.* = try body_result.node.Expression.clone(alloc);

        // Expect closing paren for this arm
        if (current_pos >= toks.len or toks[current_pos].kind != .RParen) return error.ExpectedRParen;
        current_pos += 1;

        try arms.append(.{
            .pattern = pattern.pattern,
            .guard = null, // TODO: Add guard support later
            .body = body_ptr,
        });
    }

    if (current_pos >= toks.len or toks[current_pos].kind != .RParen) return error.ExpectedRParen;
    current_pos += 1;

    const arms_slice = try arms.toOwnedSlice();

    return ParseResult{
        .node = .{
            .Expression = .{
                .MatchExpr = .{
                    .scrutinee = scrutinee_ptr,
                    .arms = arms_slice,
                },
            },
        },
        .consumed = current_pos,
    };
}

const PatternParseResult = struct {
    pattern: ast.MatchExpr.Pattern,
    consumed: usize,
};

fn parsePattern(alloc: std.mem.Allocator, toks: []const Token, depth: usize) !PatternParseResult {
    if (depth > MAX_RECURSION_DEPTH) return error.MaxRecursionDepthExceeded;
    if (toks.len == 0) return error.EmptyExpression;

    const tok = toks[0];
    switch (tok.kind) {
        .Int, .Float, .Bool, .String => {
            // Literal pattern
            const expr_result = try parseExpression(alloc, toks, depth + 1);
            const value_ptr = try alloc.create(ast.Expression);
            value_ptr.* = try expr_result.node.Expression.clone(alloc);
            return PatternParseResult{
                .pattern = .{ .Literal = .{ .value = value_ptr } },
                .consumed = expr_result.consumed,
            };
        },
        .Ident => |ident| {
            if (std.mem.eql(u8, ident, "_")) {
                // Wildcard pattern
                return PatternParseResult{
                    .pattern = .{ .Wildcard = {} },
                    .consumed = 1,
                };
            } else {
                // Variable pattern
                const name_copy = try alloc.dupe(u8, ident);
                return PatternParseResult{
                    .pattern = .{ .Variable = .{ .name = name_copy, .type_annotation = null } },
                    .consumed = 1,
                };
            }
        },
        .LBracket => {
            // Array pattern
            var current_pos: usize = 1; // Skip LBracket
            var elements = std.ArrayList(ast.MatchExpr.Pattern).init(alloc);
            errdefer elements.deinit();

            while (current_pos < toks.len and toks[current_pos].kind != .RBracket) {
                const elem_result = try parsePattern(alloc, toks[current_pos..], depth + 1);
                try elements.append(elem_result.pattern);
                current_pos += elem_result.consumed;
            }

            if (current_pos >= toks.len or toks[current_pos].kind != .RBracket) {
                return error.ExpectedRBracket;
            }
            current_pos += 1;

            const elements_slice = try elements.toOwnedSlice();
            return PatternParseResult{
                .pattern = .{ .Array = .{ .elements = elements_slice, .rest = null } },
                .consumed = current_pos,
            };
        },
        .LBrace => {
            // Map pattern
            var current_pos: usize = 1; // Skip LBrace
            var fields = std.ArrayList(ast.MatchExpr.Pattern.MapPattern.MapFieldPattern).init(alloc);
            errdefer fields.deinit();

            while (current_pos < toks.len and toks[current_pos].kind != .RBrace) {
                // Parse key (must be an identifier starting with ^)
                if (current_pos >= toks.len) {
                    return error.UnexpectedEndOfInput;
                }
                
                const key_ident = switch (toks[current_pos].kind) {
                    .Ident => |ident| ident,
                    else => return error.ExpectedProperty,
                };
                
                if (!std.mem.startsWith(u8, key_ident, "^")) {
                    return error.ExpectedProperty;
                }
                
                // Remove the ^ prefix to get the actual key
                const key = try alloc.dupe(u8, key_ident[1..]);
                current_pos += 1;

                // Parse value pattern
                const value_result = try parsePattern(alloc, toks[current_pos..], depth + 1);
                try fields.append(.{
                    .key = key,
                    .pattern = value_result.pattern,
                });
                current_pos += value_result.consumed;
            }

            if (current_pos >= toks.len or toks[current_pos].kind != .RBrace) {
                return error.ExpectedRBrace;
            }
            current_pos += 1;

            const fields_slice = try fields.toOwnedSlice();
            return PatternParseResult{
                .pattern = .{ .Map = .{ .fields = fields_slice, .rest = null } },
                .consumed = current_pos,
            };
        },
        .LParen => {
            // Could be a constructor pattern or nested pattern
            // For now, parse as literal expression
            const expr_result = try parseExpression(alloc, toks, depth + 1);
            const value_ptr = try alloc.create(ast.Expression);
            value_ptr.* = try expr_result.node.Expression.clone(alloc);
            return PatternParseResult{
                .pattern = .{ .Literal = .{ .value = value_ptr } },
                .consumed = expr_result.consumed,
            };
        },
        else => {
            std.debug.print("ERROR: Unhandled token in parseExpression: {any} at location {}\n", .{tok.kind, tok.loc});
            return error.InvalidExpression;
        },
    }
}
