const std = @import("std");
const ast = @import("ast.zig");

pub const ParseError = error{
    UnexpectedToken,
    IncompleteInput,
    InvalidSyntax,
    InvalidStringLiteral,
    UnknownToken,
    MemoryAllocationFailed,
    OutOfMemory,
};

pub fn parseGeneSource(allocator: *std.mem.Allocator, source: []const u8) ![]ast.AstNode {
    var nodes = std.ArrayList(ast.AstNode).init(allocator.*);
    defer nodes.deinit();

    var in_string = false;
    var current_token = std.ArrayList(u8).init(allocator.*);
    defer current_token.deinit();

    var i: usize = 0;
    while (i < source.len) : (i += 1) {
        const c = source[i];

        // Skip whitespace
        if (std.mem.indexOfScalar(u8, " \n\r\t", c) != null) {
            if (current_token.items.len > 0) {
                const token = try current_token.toOwnedSlice();
                try handleToken(allocator, &nodes, token);
                current_token = std.ArrayList(u8).init(allocator.*);
            }
            continue;
        }

        // Handle strings
        if (c == '"') {
            if (in_string) {
                // End of string
                try current_token.append('"');
                const value = current_token.items[1 .. current_token.items.len - 1];
                const value_copy = try allocator.alloc(u8, value.len);
                var index: usize = 0;
                while (index < value.len) : (index += 1) {
                    value_copy[index] = value[index];
                }
                try nodes.append(ast.AstNode{
                    .Stmt = ast.Stmt{
                        .ExprStmt = ast.Expr{
                            .StrLit = value_copy,
                        },
                    },
                });
                current_token = std.ArrayList(u8).init(allocator.*);
                in_string = false;
            } else {
                // Start of string
                if (current_token.items.len > 0) {
                    const token = try current_token.toOwnedSlice();
                    try handleToken(allocator, &nodes, token);
                    current_token = std.ArrayList(u8).init(allocator.*);
                }
                try current_token.append('"');
                in_string = true;
            }
        } else if (in_string) {
            try current_token.append(c);
        } else if (c == '(') {
            // Start of expression
            if (current_token.items.len > 0) {
                const token = try current_token.toOwnedSlice();
                try handleToken(allocator, &nodes, token);
                current_token = std.ArrayList(u8).init(allocator.*);
            }
            try parseExpression(allocator, &nodes, source[i..]);
            // Skip past the expression
            while (i < source.len and source[i] != ')') : (i += 1) {}
        } else {
            try current_token.append(c);
        }
    }

    if (current_token.items.len > 0) {
        const token = try current_token.toOwnedSlice();
        try handleToken(allocator, &nodes, token);
    }

    if (nodes.items.len == 0) {
        return error.IncompleteInput;
    }

    return nodes.toOwnedSlice();
}

fn handleToken(allocator: *std.mem.Allocator, nodes: *std.ArrayList(ast.AstNode), token: []const u8) !void {
    if (std.mem.eql(u8, token, "Class")) {
        // Class definition will be handled in parseExpression
        return;
    } else if (std.mem.eql(u8, token, "fn")) {
        // Function definition will be handled in parseExpression
        return;
    } else if (std.mem.eql(u8, token, "print")) {
        const ident = try allocator.alloc(u8, 5);
        std.mem.copy(u8, ident, "print");
        try nodes.append(ast.AstNode{
            .Stmt = ast.Stmt{
                .ExprStmt = ast.Expr{
                    .Ident = ident,
                },
            },
        });
    } else {
        const ident = try allocator.alloc(u8, token.len);
        std.mem.copy(u8, ident, token);
        try nodes.append(ast.AstNode{
            .Stmt = ast.Stmt{
                .ExprStmt = ast.Expr{
                    .Ident = ident,
                },
            },
        });
    }
}

fn parseExpression(allocator: *std.mem.Allocator, nodes: *std.ArrayList(ast.AstNode), source: []const u8) !void {
    var i: usize = 1; // Skip opening '('
    var current_token = std.ArrayList(u8).init(allocator.*);
    defer current_token.deinit();

    while (i < source.len and source[i] != ')') {
        const c = source[i];

        if (std.mem.indexOfScalar(u8, " \n\r\t", c) != null) {
            if (current_token.items.len > 0) {
                const token = try current_token.toOwnedSlice();
                if (std.mem.eql(u8, token, "Class")) {
                    try parseClassDefinition(allocator, nodes, source[i..]);
                    return;
                }
                current_token = std.ArrayList(u8).init(allocator.*);
            }
            i += 1;
            continue;
        }

        try current_token.append(c);
        i += 1;
    }
}

fn parseClassDefinition(allocator: *std.mem.Allocator, nodes: *std.ArrayList(ast.AstNode), source: []const u8) !void {
    var i: usize = 0;
    var current_token = std.ArrayList(u8).init(allocator.*);
    defer current_token.deinit();

    // Parse class name
    while (i < source.len and !std.mem.indexOfScalar(u8, " \n\r\t{", source[i])) : (i += 1) {
        try current_token.append(source[i]);
    }
    const class_name = try current_token.toOwnedSlice();

    // Skip to start of properties
    while (i < source.len and source[i] != '{') : (i += 1) {}
    i += 1; // Skip '{'

    var props = std.ArrayList(ast.ClassProp).init(allocator.*);
    defer props.deinit();

    // Parse properties
    while (i < source.len and source[i] != '}') {
        // Skip whitespace
        while (i < source.len and std.mem.indexOfScalar(u8, " \n\r\t", source[i])) : (i += 1) {}

        // Parse property name
        current_token = std.ArrayList(u8).init(allocator.*);
        while (i < source.len and !std.mem.indexOfScalar(u8, " \n\r\t:", source[i])) : (i += 1) {
            try current_token.append(source[i]);
        }
        const prop_name = try current_token.toOwnedSlice();

        // Skip to type
        while (i < source.len and source[i] != ':') : (i += 1) {}
        i += 1; // Skip ':'

        // Parse type
        current_token = std.ArrayList(u8).init(allocator.*);
        while (i < source.len and !std.mem.indexOfScalar(u8, " \n\r\t)", source[i])) : (i += 1) {
            try current_token.append(source[i]);
        }
        const prop_type = try current_token.toOwnedSlice();

        // Check for required modifier
        var required = false;
        if (i + 1 < source.len and source[i] == '^' and source[i + 1] == '^') {
            if (std.mem.startsWith(u8, source[i..], "^^required")) {
                required = true;
                i += "^^required".len;
            }
        }

        // Add property
        const name_copy = try allocator.alloc(u8, prop_name.len);
        std.mem.copy(u8, name_copy, prop_name);

        const type_copy = try allocator.alloc(u8, prop_type.len);
        std.mem.copy(u8, type_copy, prop_type);

        try props.append(ast.ClassProp{
            .name = name_copy,
            .required = required,
            .typ = type_copy,
        });
    }

    // Create class node
    try nodes.append(ast.AstNode{
        .ClassDecl = ast.ClassDecl{
            .name = class_name,
            .props = try props.toOwnedSlice(),
        },
    });
}
