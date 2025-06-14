const std = @import("std");
const types = @import("core/types.zig");
const parser = @import("frontend/parser.zig");
const debug = @import("core/debug.zig");

/// Token types for data parser
pub const DataToken = struct {
    kind: DataTokenKind,
    loc: usize = 0,
};

pub const DataTokenKind = union(enum) {
    Int: i64,
    Float: f64,
    Bool: bool,
    String: []const u8,
    Symbol: []const u8,
    Keyword: []const u8, // Keywords starting with :
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
};

/// Tokenize Gene data format - more permissive than Gene language tokenizer
pub fn tokenizeData(allocator: std.mem.Allocator, source: []const u8) !std.ArrayList(DataToken) {
    var tokens = std.ArrayList(DataToken).init(allocator);
    errdefer {
        for (tokens.items) |*token| {
            switch (token.kind) {
                .String => |str| allocator.free(str),
                .Symbol => |sym| allocator.free(sym),
                .Keyword => |kw| allocator.free(kw),
                else => {},
            }
        }
        tokens.deinit();
    }

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

        // Handle delimiters - these cannot be part of symbols
        switch (c) {
            '(' => {
                try tokens.append(.{ .kind = .LParen, .loc = i });
                i += 1;
                continue;
            },
            ')' => {
                try tokens.append(.{ .kind = .RParen, .loc = i });
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
            else => {},
        }

        // Handle strings
        if (c == '"') {
            const start = i + 1;
            i += 1;
            while (i < source_to_parse.len and source_to_parse[i] != '"') {
                if (source_to_parse[i] == '\\' and i + 1 < source_to_parse.len) {
                    i += 2; // Skip escaped character
                } else {
                    i += 1;
                }
            }
            if (i >= source_to_parse.len) return error.UnterminatedString;
            const str = try allocator.dupe(u8, source_to_parse[start..i]);
            try tokens.append(.{ .kind = .{ .String = str }, .loc = start - 1 });
            i += 1;
            continue;
        }

        // Handle numbers
        if (std.ascii.isDigit(c) or ((c == '+' or c == '-') and i + 1 < source_to_parse.len and std.ascii.isDigit(source_to_parse[i + 1]))) {
            const start = i;
            var has_dot = false;

            // Handle optional sign
            if (c == '+' or c == '-') {
                i += 1;
            }

            // Parse integer part
            while (i < source_to_parse.len and std.ascii.isDigit(source_to_parse[i])) : (i += 1) {}

            // Check for decimal point
            if (i < source_to_parse.len and source_to_parse[i] == '.') {
                has_dot = true;
                i += 1;
                // Parse optional decimal part
                while (i < source_to_parse.len and std.ascii.isDigit(source_to_parse[i])) : (i += 1) {}
            }

            const num_str = source_to_parse[start..i];

            if (has_dot) {
                const float_val = try std.fmt.parseFloat(f64, num_str);
                try tokens.append(.{ .kind = .{ .Float = float_val }, .loc = start });
            } else {
                const int_val = try std.fmt.parseInt(i64, num_str, 10);
                try tokens.append(.{ .kind = .{ .Int = int_val }, .loc = start });
            }
            continue;
        }

        // Handle keywords (start with single :, not ::)
        if (c == ':' and (i + 1 >= source_to_parse.len or source_to_parse[i + 1] != ':')) {
            const start = i;
            i += 1;

            // Parse keyword name - can be any non-delimiter chars
            while (i < source_to_parse.len) {
                const ch = source_to_parse[i];
                if (std.ascii.isWhitespace(ch) or ch == '(' or ch == ')' or
                    ch == '[' or ch == ']' or ch == '{' or ch == '}')
                {
                    break;
                }
                i += 1;
            }

            if (i > start + 1) {
                const kw = try allocator.dupe(u8, source_to_parse[start + 1 .. i]);
                try tokens.append(.{ .kind = .{ .Keyword = kw }, .loc = start });
                continue;
            }
        }

        // Handle symbols - everything else
        // Symbols cannot start with digits unless escaped
        if (!std.ascii.isDigit(c)) {
            const start = i;

            // Handle escaped symbols starting with \
            if (c == '\\' and i + 1 < source_to_parse.len) {
                i += 1; // Skip the backslash
            }

            // Parse symbol - can contain almost anything except delimiters and whitespace
            while (i < source_to_parse.len) {
                const ch = source_to_parse[i];
                if (std.ascii.isWhitespace(ch) or ch == '(' or ch == ')' or
                    ch == '[' or ch == ']' or ch == '{' or ch == '}' or ch == '"')
                {
                    break;
                }

                // Handle escapes within symbol
                if (ch == '\\' and i + 1 < source_to_parse.len) {
                    i += 2; // Skip escape and next char
                } else {
                    i += 1;
                }
            }

            const sym_str = source_to_parse[start..i];

            // Check for special symbols
            if (std.mem.eql(u8, sym_str, "true")) {
                try tokens.append(.{ .kind = .{ .Bool = true }, .loc = start });
            } else if (std.mem.eql(u8, sym_str, "false")) {
                try tokens.append(.{ .kind = .{ .Bool = false }, .loc = start });
            } else if (std.mem.eql(u8, sym_str, "nil")) {
                // We'll handle nil as a special symbol in the parser
                const sym = try allocator.dupe(u8, sym_str);
                try tokens.append(.{ .kind = .{ .Symbol = sym }, .loc = start });
            } else {
                const sym = try allocator.dupe(u8, sym_str);
                try tokens.append(.{ .kind = .{ .Symbol = sym }, .loc = start });
            }
            continue;
        }

        // Skip unknown character
        i += 1;
    }

    return tokens;
}

/// Represents a parsed Gene data structure
pub const GeneData = struct {
    /// The head of the Gene expression (can be any expression)
    head: DataValue,

    /// Properties (^key value pairs)
    props: std.StringHashMap(DataValue),

    /// Positional children
    children: std.ArrayList(DataValue),

    /// Allocator used for this structure
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, head: DataValue) !*GeneData {
        const gene = try allocator.create(GeneData);
        gene.* = GeneData{
            .head = head,
            .props = std.StringHashMap(DataValue).init(allocator),
            .children = std.ArrayList(DataValue).init(allocator),
            .allocator = allocator,
        };
        return gene;
    }

    pub fn deinit(self: *GeneData) void {
        self.head.deinit(self.allocator);

        var prop_iter = self.props.iterator();
        while (prop_iter.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            entry.value_ptr.deinit(self.allocator);
        }
        self.props.deinit();

        for (self.children.items) |*child| {
            child.deinit(self.allocator);
        }
        self.children.deinit();

        self.allocator.destroy(self);
    }

    pub fn addProperty(self: *GeneData, key: []const u8, value: DataValue) !void {
        const key_copy = try self.allocator.dupe(u8, key);
        try self.props.put(key_copy, value);
    }

    pub fn addChild(self: *GeneData, child: DataValue) !void {
        try self.children.append(child);
    }
};

/// Represents a value in Gene data format
pub const DataValue = union(enum) {
    Nil,
    Bool: bool,
    Int: i64,
    Float: f64,
    String: []const u8,
    Symbol: []const u8,
    Array: []DataValue,
    Map: std.StringHashMap(DataValue),
    Gene: *GeneData,
    Document: *Document,

    pub fn deinit(self: *DataValue, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .String => |s| allocator.free(s),
            .Symbol => |s| allocator.free(s),
            .Array => |arr| {
                for (arr) |*item| {
                    item.deinit(allocator);
                }
                allocator.free(arr);
            },
            .Map => |*map| {
                var iter = map.iterator();
                while (iter.next()) |entry| {
                    allocator.free(entry.key_ptr.*);
                    entry.value_ptr.deinit(allocator);
                }
                map.deinit();
            },
            .Gene => |gene| gene.deinit(),
            .Document => |doc| {
                var mut_doc = doc;
                mut_doc.deinit();
                allocator.destroy(doc);
            },
            else => {},
        }
    }

    pub fn clone(self: DataValue, allocator: std.mem.Allocator) !DataValue {
        return switch (self) {
            .Nil => .Nil,
            .Bool => |b| .{ .Bool = b },
            .Int => |i| .{ .Int = i },
            .Float => |f| .{ .Float = f },
            .String => |s| .{ .String = try allocator.dupe(u8, s) },
            .Symbol => |s| .{ .Symbol = try allocator.dupe(u8, s) },
            .Array => |arr| blk: {
                var new_arr = try allocator.alloc(DataValue, arr.len);
                for (arr, 0..) |item, i| {
                    new_arr[i] = try item.clone(allocator);
                }
                break :blk .{ .Array = new_arr };
            },
            .Map => |map| blk: {
                var new_map = std.StringHashMap(DataValue).init(allocator);
                var iter = map.iterator();
                while (iter.next()) |entry| {
                    const key_copy = try allocator.dupe(u8, entry.key_ptr.*);
                    const val_copy = try entry.value_ptr.clone(allocator);
                    try new_map.put(key_copy, val_copy);
                }
                break :blk .{ .Map = new_map };
            },
            .Gene => |gene| blk: {
                const new_gene = try GeneData.init(allocator, try gene.head.clone(allocator));

                // Clone properties
                var prop_iter = gene.props.iterator();
                while (prop_iter.next()) |entry| {
                    const val_copy = try entry.value_ptr.clone(allocator);
                    try new_gene.addProperty(entry.key_ptr.*, val_copy);
                }

                // Clone children
                for (gene.children.items) |child| {
                    const child_copy = try child.clone(allocator);
                    try new_gene.addChild(child_copy);
                }

                break :blk .{ .Gene = new_gene };
            },
            .Document => |doc| blk: {
                const new_doc = try allocator.create(Document);
                var expressions = try allocator.alloc(DataValue, doc.expressions.len);
                for (doc.expressions, 0..) |expr, i| {
                    expressions[i] = try expr.clone(allocator);
                }
                new_doc.* = Document{
                    .expressions = expressions,
                    .allocator = allocator,
                };
                break :blk .{ .Document = new_doc };
            },
        };
    }
};

/// Result of parsing a Gene document
pub const Document = struct {
    expressions: []DataValue,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *Document) void {
        for (self.expressions) |*expr| {
            var mut_expr = expr;
            mut_expr.deinit(self.allocator);
        }
        self.allocator.free(self.expressions);
    }

    /// Convert to a GeneDocument data value
    pub fn toDataValue(self: *Document) !DataValue {
        // Create a GeneDocument
        const gene_doc = try GeneData.init(self.allocator, .{ .Symbol = try self.allocator.dupe(u8, "GeneDocument") });

        // Add all expressions as children
        for (self.expressions) |expr| {
            const expr_copy = try expr.clone(self.allocator);
            try gene_doc.addChild(expr_copy);
        }

        return .{ .Gene = gene_doc };
    }
};

/// Parse a Gene data string into a data structure
pub fn parseData(allocator: std.mem.Allocator, source: []const u8) !DataValue {
    // Tokenize the source using data tokenizer
    var tokens = try tokenizeData(allocator, source);
    defer {
        for (tokens.items) |*token| {
            switch (token.kind) {
                .String => |str| allocator.free(str),
                .Symbol => |sym| allocator.free(sym),
                .Keyword => |kw| allocator.free(kw),
                else => {},
            }
        }
        tokens.deinit();
    }

    if (tokens.items.len == 0) {
        return DataValue.Nil;
    }

    var pos: usize = 0;
    return try parseDataExpression(allocator, tokens.items, &pos);
}

/// Parse a Gene document (multiple top-level expressions)
pub fn parseDocument(allocator: std.mem.Allocator, source: []const u8) !Document {
    // Tokenize the source using data tokenizer (shebang handled in tokenizer)
    var tokens = try tokenizeData(allocator, source);
    defer {
        for (tokens.items) |*token| {
            switch (token.kind) {
                .String => |str| allocator.free(str),
                .Symbol => |sym| allocator.free(sym),
                .Keyword => |kw| allocator.free(kw),
                else => {},
            }
        }
        tokens.deinit();
    }

    var expressions = std.ArrayList(DataValue).init(allocator);
    defer expressions.deinit();

    var pos: usize = 0;
    while (pos < tokens.items.len) {
        const expr = try parseDataExpression(allocator, tokens.items, &pos);
        try expressions.append(expr);
    }

    return Document{
        .expressions = try expressions.toOwnedSlice(),
        .allocator = allocator,
    };
}

/// Parse a single expression using data tokens
fn parseDataExpression(allocator: std.mem.Allocator, tokens: []const DataToken, pos: *usize) !DataValue {
    if (pos.* >= tokens.len) {
        return error.UnexpectedEOF;
    }

    const token = tokens[pos.*];
    pos.* += 1;

    switch (token.kind) {
        .Int => |val| return .{ .Int = val },
        .Float => |val| return .{ .Float = val },
        .Bool => |val| return .{ .Bool = val },
        .String => |str| return .{ .String = try allocator.dupe(u8, str) },
        .Symbol => |sym| {
            // Check for special symbols
            if (std.mem.eql(u8, sym, "nil")) {
                return .Nil;
            } else {
                return .{ .Symbol = try allocator.dupe(u8, sym) };
            }
        },
        .Keyword => |kw| {
            // Keywords are treated as strings in maps
            return .{ .Symbol = try allocator.dupe(u8, kw) };
        },
        .LBracket => {
            // Parse array
            var items = std.ArrayList(DataValue).init(allocator);
            defer items.deinit();

            while (pos.* < tokens.len and tokens[pos.*].kind != .RBracket) {
                const item = try parseDataExpression(allocator, tokens, pos);
                try items.append(item);
            }

            if (pos.* >= tokens.len or tokens[pos.*].kind != .RBracket) {
                return error.ExpectedRBracket;
            }
            pos.* += 1; // Skip RBracket

            return .{ .Array = try items.toOwnedSlice() };
        },
        .LBrace => {
            // Parse map
            var map = std.StringHashMap(DataValue).init(allocator);
            errdefer {
                var iter = map.iterator();
                while (iter.next()) |entry| {
                    allocator.free(entry.key_ptr.*);
                    entry.value_ptr.deinit(allocator);
                }
                map.deinit();
            }

            while (pos.* < tokens.len and tokens[pos.*].kind != .RBrace) {
                // Parse key (must be a keyword)
                if (pos.* >= tokens.len) return error.UnexpectedEOF;

                const key_token = tokens[pos.*];
                const key = switch (key_token.kind) {
                    .Keyword => |kw| kw,
                    else => return error.MapKeyMustBeKeyword,
                };
                pos.* += 1;

                // Parse value
                const value = try parseDataExpression(allocator, tokens, pos);

                // Add to map
                const key_copy = try allocator.dupe(u8, key);
                try map.put(key_copy, value);
            }

            if (pos.* >= tokens.len or tokens[pos.*].kind != .RBrace) {
                return error.ExpectedRBrace;
            }
            pos.* += 1; // Skip RBrace

            return .{ .Map = map };
        },
        .LParen => {
            // Parse Gene expression
            if (pos.* >= tokens.len) return error.UnexpectedEOF;

            // Parse the head expression - can be any valid expression
            const head_expr = try parseDataExpression(allocator, tokens, pos);

            const gene = try GeneData.init(allocator, head_expr);
            errdefer gene.deinit();

            // Parse properties and children
            while (pos.* < tokens.len and tokens[pos.*].kind != .RParen) {
                // Check for property syntax (^prop value)
                if (pos.* < tokens.len and tokens[pos.*].kind == .Symbol) {
                    const sym = tokens[pos.*].kind.Symbol;
                    if (std.mem.startsWith(u8, sym, "^")) {
                        // This is a property
                        pos.* += 1;

                        // Parse property chain (e.g., ^a^^b^^c or ^^required)
                        var prop_parts = std.ArrayList([]const u8).init(allocator);
                        defer prop_parts.deinit();

                        var i: usize = 1; // Skip first ^
                        var start: usize = 1;

                        // Special case: if property starts with another ^, it's ^^prop syntax
                        if (i < sym.len and sym[i] == '^') {
                            // This is ^^prop, treat the whole thing as one property with ^ prefix
                            try prop_parts.append(sym[i..]); // Include the second ^
                        } else {
                            // Normal property parsing
                            while (i < sym.len) {
                                if (i + 1 < sym.len and sym[i] == '^' and sym[i + 1] == '^') {
                                    // Found ^^, this is a property separator
                                    try prop_parts.append(sym[start..i]);
                                    i += 2; // Skip ^^
                                    start = i;
                                } else {
                                    i += 1;
                                }
                            }
                            // Add the last part
                            try prop_parts.append(sym[start..]);
                        }

                        const parts = prop_parts.items;

                        // Parse the value (or use shorthand)
                        var prop_value: DataValue = undefined;

                        // Check if there's an explicit value or if it's shorthand
                        if (pos.* < tokens.len and tokens[pos.*].kind != .RParen) {
                            // Check if next token is another property
                            if (tokens[pos.*].kind == .Symbol) {
                                const next_sym = tokens[pos.*].kind.Symbol;
                                if (std.mem.startsWith(u8, next_sym, "^")) {
                                    // Next token is another property, so this is shorthand
                                    prop_value = .{ .Bool = true };
                                } else {
                                    // Parse property value
                                    prop_value = try parseDataExpression(allocator, tokens, pos);
                                }
                            } else {
                                // Parse property value
                                prop_value = try parseDataExpression(allocator, tokens, pos);
                            }
                        } else {
                            // At end of expression, shorthand property
                            prop_value = .{ .Bool = true };
                        }

                        // Now build the nested property structure from right to left
                        // For ^a^^b^^c value, we want ^a {^b {^c value}}
                        if (parts.len == 1) {
                            // Simple property
                            try gene.addProperty(parts[0], prop_value);
                        } else {
                            // Nested properties
                            var current_value = prop_value;
                            var idx = parts.len;
                            while (idx > 1) {
                                idx -= 1;
                                // Create a map with the property
                                var prop_map = std.StringHashMap(DataValue).init(allocator);
                                const key_copy = try allocator.dupe(u8, parts[idx]);
                                try prop_map.put(key_copy, current_value);
                                current_value = .{ .Map = prop_map };
                            }
                            // Add the top-level property
                            try gene.addProperty(parts[0], current_value);
                        }
                        continue;
                    }
                }

                // Regular child
                const child = try parseDataExpression(allocator, tokens, pos);
                try gene.addChild(child);
            }

            if (pos.* >= tokens.len or tokens[pos.*].kind != .RParen) {
                return error.ExpectedRParen;
            }
            pos.* += 1; // Skip RParen

            return .{ .Gene = gene };
        },
        else => {
            std.debug.print("Unexpected token: {any}\n", .{token.kind});
            return error.UnexpectedToken;
        },
    }
}

/// Pretty print a data value (reproduces original Gene syntax)
pub fn printDataValue(writer: anytype, value: DataValue, indent: usize) !void {
    switch (value) {
        .Nil => try writer.print("nil", .{}),
        .Bool => |b| try writer.print("{}", .{b}),
        .Int => |i| try writer.print("{}", .{i}),
        .Float => |f| {
            // Always show at least one decimal place for floats
            if (@floor(f) == f and @abs(f) < 1e10) {
                try writer.print("{d:.1}", .{f});
            } else {
                try writer.print("{d}", .{f});
            }
        },
        .String => |s| try writer.print("\"{s}\"", .{s}),
        .Symbol => |s| try writer.print("{s}", .{s}),
        .Array => |arr| {
            try writer.print("[", .{});
            for (arr, 0..) |item, i| {
                if (i > 0) try writer.print(" ", .{});
                try printDataValue(writer, item, indent);
            }
            try writer.print("]", .{});
        },
        .Map => |map| {
            try writer.print("{{", .{});
            var iter = map.iterator();
            var first = true;
            while (iter.next()) |entry| {
                if (!first) try writer.print(" ", .{});
                first = false;
                try writer.print(":{s} ", .{entry.key_ptr.*});
                try printDataValue(writer, entry.value_ptr.*, indent);
            }
            try writer.print("}}", .{});
        },
        .Gene => |gene| {
            try writer.print("(", .{});
            try printDataValue(writer, gene.head, indent);

            // Print properties
            var prop_iter = gene.props.iterator();
            while (prop_iter.next()) |entry| {
                try writer.print(" ^{s} ", .{entry.key_ptr.*});
                try printDataValue(writer, entry.value_ptr.*, indent);
            }

            // Print children
            for (gene.children.items) |child| {
                try writer.print(" ", .{});
                try printDataValue(writer, child, indent);
            }

            try writer.print(")", .{});
        },
        .Document => |doc| {
            // Print as GeneDocument
            try writer.print("(GeneDocument", .{});
            for (doc.expressions) |expr| {
                try writer.print(" ", .{});
                try printDataValue(writer, expr, indent);
            }
            try writer.print(")", .{});
        },
    }
}

/// Print parsed data structure (shows internal representation)
pub fn printParsedData(writer: anytype, value: DataValue, indent: usize) !void {
    try writeIndent(writer, indent);

    switch (value) {
        .Nil => try writer.print("Nil", .{}),
        .Bool => |b| try writer.print("Bool({})", .{b}),
        .Int => |i| try writer.print("Int({})", .{i}),
        .Float => |f| {
            // Always show at least one decimal place for floats in parsed output
            if (@floor(f) == f and @abs(f) < 1e10) {
                try writer.print("Float({d:.1})", .{f});
            } else {
                try writer.print("Float({d})", .{f});
            }
        },
        .String => |s| try writer.print("String(\"{s}\")", .{s}),
        .Symbol => |s| try writer.print("Symbol({s})", .{s}),
        .Array => |arr| {
            try writer.print("Array[{}]:\n", .{arr.len});
            for (arr, 0..) |item, i| {
                try writeIndent(writer, indent + 2);
                try writer.print("[{}] = ", .{i});
                try printParsedData(writer, item, indent + 2);
            }
        },
        .Map => |map| {
            try writer.print("Map[{}]:\n", .{map.count()});
            var iter = map.iterator();
            while (iter.next()) |entry| {
                try writeIndent(writer, indent + 2);
                try writer.print(":{s} => ", .{entry.key_ptr.*});
                try printParsedData(writer, entry.value_ptr.*, indent + 2);
            }
        },
        .Gene => |gene| {
            try writer.print("Gene(", .{});
            try printDataValue(writer, gene.head, 0);
            try writer.print("):\n", .{});

            // Print properties
            if (gene.props.count() > 0) {
                try writeIndent(writer, indent + 2);
                try writer.print("Properties[{}]:\n", .{gene.props.count()});
                var prop_iter = gene.props.iterator();
                while (prop_iter.next()) |entry| {
                    try writeIndent(writer, indent + 4);
                    try writer.print("^{s} = ", .{entry.key_ptr.*});
                    try printParsedData(writer, entry.value_ptr.*, indent + 4);
                }
            }

            // Print children
            if (gene.children.items.len > 0) {
                try writeIndent(writer, indent + 2);
                try writer.print("Children[{}]:\n", .{gene.children.items.len});
                for (gene.children.items, 0..) |child, i| {
                    try writeIndent(writer, indent + 4);
                    try writer.print("[{}] = ", .{i});
                    try printParsedData(writer, child, indent + 4);
                }
            }
        },
        .Document => |doc| {
            try writer.print("Document[{}]:\n", .{doc.expressions.len});
            for (doc.expressions, 0..) |expr, i| {
                try writeIndent(writer, indent + 2);
                try writer.print("Expression [{}]:\n", .{i});
                try printParsedData(writer, expr, indent + 4);
            }
        },
    }

    // Add newline for non-nested types
    switch (value) {
        .Array, .Map, .Gene, .Document => {},
        else => try writer.print("\n", .{}),
    }
}

fn writeIndent(writer: anytype, indent: usize) !void {
    for (0..indent) |_| {
        try writer.print(" ", .{});
    }
}

/// Convert a data value to JSON
pub fn toJson(writer: anytype, value: DataValue) !void {
    switch (value) {
        .Nil => try writer.print("null", .{}),
        .Bool => |b| try writer.print("{}", .{b}),
        .Int => |i| try writer.print("{}", .{i}),
        .Float => |f| try writer.print("{d}", .{f}),
        .String => |s| {
            // Properly escape JSON string
            try writer.print("\"", .{});
            for (s) |c| {
                switch (c) {
                    '"' => try writer.print("\\\"", .{}),
                    '\\' => try writer.print("\\\\", .{}),
                    '\n' => try writer.print("\\n", .{}),
                    '\r' => try writer.print("\\r", .{}),
                    '\t' => try writer.print("\\t", .{}),
                    else => try writer.print("{c}", .{c}),
                }
            }
            try writer.print("\"", .{});
        },
        .Symbol => |s| try writer.print("\"{s}\"", .{s}),
        .Array => |arr| {
            try writer.print("[", .{});
            for (arr, 0..) |item, i| {
                if (i > 0) try writer.print(",", .{});
                try toJson(writer, item);
            }
            try writer.print("]", .{});
        },
        .Map => |map| {
            try writer.print("{{", .{});
            var iter = map.iterator();
            var first = true;
            while (iter.next()) |entry| {
                if (!first) try writer.print(",", .{});
                first = false;
                try writer.print("\"{s}\":", .{entry.key_ptr.*});
                try toJson(writer, entry.value_ptr.*);
            }
            try writer.print("}}", .{});
        },
        .Gene => |gene| {
            try writer.print("{{", .{});
            try writer.print("\"_type\":\"gene\",", .{});
            try writer.print("\"_head\":", .{});
            try toJson(writer, gene.head);

            // Add properties
            if (gene.props.count() > 0) {
                try writer.print(",\"_props\":{{", .{});
                var prop_iter = gene.props.iterator();
                var first = true;
                while (prop_iter.next()) |entry| {
                    if (!first) try writer.print(",", .{});
                    first = false;
                    try writer.print("\"{s}\":", .{entry.key_ptr.*});
                    try toJson(writer, entry.value_ptr.*);
                }
                try writer.print("}}", .{});
            }

            // Add children
            if (gene.children.items.len > 0) {
                try writer.print(",\"_children\":[", .{});
                for (gene.children.items, 0..) |child, i| {
                    if (i > 0) try writer.print(",", .{});
                    try toJson(writer, child);
                }
                try writer.print("]", .{});
            }

            try writer.print("}}", .{});
        },
        .Document => |doc| {
            // Convert to GeneDocument JSON representation
            try writer.print("{{", .{});
            try writer.print("\"_type\":\"document\",", .{});
            try writer.print("\"_expressions\":[", .{});
            for (doc.expressions, 0..) |expr, i| {
                if (i > 0) try writer.print(",", .{});
                try toJson(writer, expr);
            }
            try writer.print("]", .{});
            try writer.print("}}", .{});
        },
    }
}
