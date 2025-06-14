const std = @import("std");
const types = @import("core/types.zig");
const parser = @import("frontend/parser.zig");
const debug = @import("core/debug.zig");

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
        };
    }
};

/// Result of parsing a Gene document
pub const Document = struct {
    expressions: []DataValue,
    allocator: std.mem.Allocator,
    
    pub fn deinit(self: *Document) void {
        for (self.expressions) |*expr| {
            expr.deinit(self.allocator);
        }
        self.allocator.free(self.expressions);
    }
};

/// Parse a Gene data string into a data structure
pub fn parseData(allocator: std.mem.Allocator, source: []const u8) !DataValue {
    // Tokenize the source
    var tokens = try parser.tokenize(allocator, source);
    defer {
        for (tokens.items) |*token| {
            switch (token.kind) {
                .String => |str| allocator.free(str),
                else => {},
            }
        }
        tokens.deinit();
    }
    
    if (tokens.items.len == 0) {
        return DataValue.Nil;
    }
    
    var pos: usize = 0;
    return try parseExpression(allocator, tokens.items, &pos);
}

/// Parse a Gene document (multiple top-level expressions)
pub fn parseDocument(allocator: std.mem.Allocator, source: []const u8) !Document {
    // Tokenize the source
    var tokens = try parser.tokenize(allocator, source);
    defer {
        for (tokens.items) |*token| {
            switch (token.kind) {
                .String => |str| allocator.free(str),
                else => {},
            }
        }
        tokens.deinit();
    }
    
    var expressions = std.ArrayList(DataValue).init(allocator);
    defer expressions.deinit();
    
    var pos: usize = 0;
    while (pos < tokens.items.len) {
        const expr = try parseExpression(allocator, tokens.items, &pos);
        try expressions.append(expr);
    }
    
    return Document{
        .expressions = try expressions.toOwnedSlice(),
        .allocator = allocator,
    };
}

/// Parse a single expression
fn parseExpression(allocator: std.mem.Allocator, tokens: []const parser.Token, pos: *usize) !DataValue {
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
        .Ident => |ident| {
            // Check for special identifiers
            if (std.mem.eql(u8, ident, "nil")) {
                return .Nil;
            } else if (std.mem.eql(u8, ident, "true")) {
                return .{ .Bool = true };
            } else if (std.mem.eql(u8, ident, "false")) {
                return .{ .Bool = false };
            } else {
                // Regular symbol
                return .{ .Symbol = try allocator.dupe(u8, ident) };
            }
        },
        // Handle keyword tokens that should be treated as symbols in data mode
        .Var, .Fn, .If, .Else, .Do, .Class, .New, .Match, .Macro, .Ns, .Import => |_| {
            // Get the keyword text
            const keyword_text = switch (token.kind) {
                .Var => "var",
                .Fn => "fn",
                .If => "if",
                .Else => "else",
                .Do => "do",
                .Class => "class",
                .New => "new",
                .Match => "match",
                .Macro => "macro",
                .Ns => "ns",
                .Import => "import",
                else => unreachable,
            };
            return .{ .Symbol = try allocator.dupe(u8, keyword_text) };
        },
        .Dot => return .{ .Symbol = try allocator.dupe(u8, ".") },
        .Slash => return .{ .Symbol = try allocator.dupe(u8, "/") },
        .Equals => return .{ .Symbol = try allocator.dupe(u8, "=") },
        .Percent => return .{ .Symbol = try allocator.dupe(u8, "%") },
        .LBracket => {
            // Parse array
            var items = std.ArrayList(DataValue).init(allocator);
            defer items.deinit();
            
            while (pos.* < tokens.len and tokens[pos.*].kind != .RBracket) {
                const item = try parseExpression(allocator, tokens, pos);
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
                // Parse key (must be a keyword symbol starting with :)
                if (pos.* >= tokens.len) return error.UnexpectedEOF;
                
                const key_token = tokens[pos.*];
                const key = switch (key_token.kind) {
                    .Ident => |ident| blk: {
                        if (!std.mem.startsWith(u8, ident, ":")) {
                            return error.MapKeyMustBeKeyword;
                        }
                        break :blk ident[1..]; // Skip the :
                    },
                    else => return error.MapKeyMustBeKeyword,
                };
                pos.* += 1;
                
                // Parse value
                const value = try parseExpression(allocator, tokens, pos);
                
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
            const head_expr = try parseExpression(allocator, tokens, pos);
            
            const gene = try GeneData.init(allocator, head_expr);
            errdefer gene.deinit();
            
            // Parse properties and children
            while (pos.* < tokens.len and tokens[pos.*].kind != .RParen) {
                // Check for property syntax (^prop value)
                if (pos.* < tokens.len and tokens[pos.*].kind == .Ident) {
                    const ident = tokens[pos.*].kind.Ident;
                    if (std.mem.startsWith(u8, ident, "^")) {
                        // This is a property
                        pos.* += 1;
                        const prop_name = ident[1..]; // Skip the ^
                        
                        // Parse property value
                        const prop_value = try parseExpression(allocator, tokens, pos);
                        try gene.addProperty(prop_name, prop_value);
                        continue;
                    }
                }
                
                // Regular child
                const child = try parseExpression(allocator, tokens, pos);
                try gene.addChild(child);
            }
            
            if (pos.* >= tokens.len or tokens[pos.*].kind != .RParen) {
                return error.ExpectedRParen;
            }
            pos.* += 1; // Skip RParen
            
            return .{ .Gene = gene };
        },
        else => return error.UnexpectedToken,
    }
}


/// Pretty print a data value (reproduces original Gene syntax)
pub fn printDataValue(writer: anytype, value: DataValue, indent: usize) !void {
    switch (value) {
        .Nil => try writer.print("nil", .{}),
        .Bool => |b| try writer.print("{}", .{b}),
        .Int => |i| try writer.print("{}", .{i}),
        .Float => |f| try writer.print("{d}", .{f}),
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
    }
}

/// Print parsed data structure (shows internal representation)
pub fn printParsedData(writer: anytype, value: DataValue, indent: usize) !void {
    try writeIndent(writer, indent);
    
    switch (value) {
        .Nil => try writer.print("Nil", .{}),
        .Bool => |b| try writer.print("Bool({})", .{b}),
        .Int => |i| try writer.print("Int({})", .{i}),
        .Float => |f| try writer.print("Float({d})", .{f}),
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
    }
    
    // Add newline for non-nested types
    switch (value) {
        .Array, .Map, .Gene => {},
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
    }
}