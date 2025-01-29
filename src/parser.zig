const std = @import("std");
const ast = @import("ast.zig");
const types = @import("types.zig");

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
    // Skip shebang line if present
    var i: usize = 0;
    if (source.len > 1 and source[0] == '#' and source[1] == '!') {
        while (i < source.len and source[i] != '\n') : (i += 1) {}
        i += 1; // Skip the newline
    }

    var nodes = std.ArrayList(ast.AstNode).init(allocator.*);
    defer nodes.deinit();

    var in_string = false;
    var current_token = std.ArrayList(u8).init(allocator.*);
    defer current_token.deinit();

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
                const literal = try allocator.create(ast.AstNode.Literal);
                literal.* = .{
                    .value = .{ .String = value_copy },
                };
                const expr = try allocator.create(ast.AstNode.Expression);
                expr.* = .{
                    .Literal = literal,
                };
                const stmt = try allocator.create(ast.AstNode.Statement);
                stmt.* = .{
                    .Expression = expr,
                };
                try nodes.append(ast.AstNode{
                    .Statement = stmt,
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
            const expr_start = i;
            var depth: i32 = 1;
            while (i < source.len and depth > @as(i32, 0)) : (i += 1) {
                if (source[i] == '(') depth += 1;
                if (source[i] == ')') depth -= 1;
            }
            try parseExpression(allocator, &nodes, source[expr_start..i]);
        } else {
            try current_token.append(c);
        }
    }

    if (current_token.items.len > 0) {
        const token = try current_token.toOwnedSlice();
        try handleToken(allocator, &nodes, token);
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
        std.mem.copyForwards(u8, ident, "print");
        const variable = try allocator.create(ast.AstNode.Variable);
        variable.* = .{
            .name = ident,
        };
        const expr = try allocator.create(ast.AstNode.Expression);
        expr.* = .{
            .Variable = variable,
        };
        const stmt = try allocator.create(ast.AstNode.Statement);
        stmt.* = .{
            .Expression = expr,
        };
        try nodes.append(ast.AstNode{
            .Statement = stmt,
        });
    } else {
        const ident = try allocator.alloc(u8, token.len);
        std.mem.copyForwards(u8, ident, token);
        const variable = try allocator.create(ast.AstNode.Variable);
        variable.* = .{
            .name = ident,
        };
        const expr = try allocator.create(ast.AstNode.Expression);
        expr.* = .{
            .Variable = variable,
        };
        const stmt = try allocator.create(ast.AstNode.Statement);
        stmt.* = .{
            .Expression = expr,
        };
        try nodes.append(ast.AstNode{
            .Statement = stmt,
        });
    }
}

fn parseExpression(allocator: *std.mem.Allocator, nodes: *std.ArrayList(ast.AstNode), source: []const u8) !void {
    var i: usize = 1; // Skip opening '('
    var current_token = std.ArrayList(u8).init(allocator.*);
    defer current_token.deinit();

    // Parse function name
    while (i < source.len and std.mem.indexOfScalar(u8, " \n\r\t)", source[i]) == null) : (i += 1) {
        try current_token.append(source[i]);
    }
    const func_name = try current_token.toOwnedSlice();

    // Create function call node
    const fcall = try allocator.create(ast.AstNode.Call);
    const func_var = try allocator.create(ast.AstNode.Variable);
    func_var.* = .{ .name = try allocator.dupe(u8, func_name) };
    const func_expr = try allocator.create(ast.AstNode.Expression);
    func_expr.* = .{ .Variable = func_var };

    fcall.* = .{
        .function = func_expr,
        .args = std.ArrayList(ast.AstNode).init(allocator.*),
    };

    // Parse arguments
    while (i < source.len and source[i] != ')') {
        // Skip whitespace
        while (i < source.len and std.mem.indexOfScalar(u8, " \n\r\t", source[i]) != null) : (i += 1) {}

        if (source[i] == '"') { // String literal
            const start = i;
            i += 1;
            while (i < source.len and source[i] != '"') : (i += 1) {}
            if (i >= source.len) return ParseError.InvalidStringLiteral;

            const str_val = try allocator.dupe(u8, source[start + 1 .. i]);
            const str_lit = try allocator.create(ast.AstNode.Literal);
            str_lit.* = .{ .value = .{ .String = str_val } };

            const expr = try allocator.create(ast.AstNode.Expression);
            expr.* = .{ .Literal = str_lit };

            try fcall.args.append(ast.AstNode{ .Expression = expr });
            i += 1; // Skip closing quote
        } else {
            // Parse other argument types
            const arg_start = i;
            while (i < source.len and std.mem.indexOfScalar(u8, " \n\r\t)", source[i]) == null) : (i += 1) {}
            const arg_val = try allocator.dupe(u8, source[arg_start..i]);

            const arg_var = try allocator.create(ast.AstNode.Variable);
            arg_var.* = .{ .name = arg_val };

            const expr = try allocator.create(ast.AstNode.Expression);
            expr.* = .{ .Variable = arg_var };

            try fcall.args.append(ast.AstNode{ .Expression = expr });
        }
    }

    const expr = try allocator.create(ast.AstNode.Expression);
    expr.* = .{ .Call = fcall };
    const stmt = try allocator.create(ast.AstNode.Statement);
    stmt.* = .{ .Expression = expr };
    try nodes.append(ast.AstNode{ .Statement = stmt });
}

fn parseClassDefinition(allocator: *std.mem.Allocator, nodes: *std.ArrayList(ast.AstNode), source: []const u8) !void {
    var i: usize = 0;
    var current_token = std.ArrayList(u8).init(allocator.*);
    defer current_token.deinit();

    // Parse class name
    while (i < source.len and std.mem.indexOfScalar(u8, " \n\r\t{", source[i]) == null) : (i += 1) {
        try current_token.append(source[i]);
    }
    const class_name = try current_token.toOwnedSlice();

    // Skip to start of properties
    while (i < source.len and source[i] != '{') : (i += 1) {}
    i += 1; // Skip '{'

    var props = std.ArrayList(ast.AstNode.Class.Field).init(allocator.*);
    defer props.deinit();

    // Parse properties
    while (i < source.len and source[i] != '}') {
        // Skip whitespace
        while (i < source.len and std.mem.indexOfScalar(u8, " \n\r\t", source[i]) != null) : (i += 1) {}

        // Parse property name
        current_token = std.ArrayList(u8).init(allocator.*);
        while (i < source.len and std.mem.indexOfScalar(u8, " \n\r\t:", source[i]) == null) : (i += 1) {
            try current_token.append(source[i]);
        }
        const prop_name = try current_token.toOwnedSlice();

        // Skip to type
        while (i < source.len and source[i] != ':') : (i += 1) {}
        i += 1; // Skip ':'

        // Parse type
        current_token = std.ArrayList(u8).init(allocator.*);
        while (i < source.len and std.mem.indexOfScalar(u8, " \n\r\t)", source[i]) == null) : (i += 1) {
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
        std.mem.copyForwards(u8, name_copy, prop_name);

        const type_node = try allocator.create(types.Type);
        if (std.mem.eql(u8, prop_type, "String")) {
            type_node.* = .String;
        } else if (std.mem.eql(u8, prop_type, "Int")) {
            type_node.* = .Int;
        } else if (std.mem.eql(u8, prop_type, "Float")) {
            type_node.* = .Float;
        } else if (std.mem.eql(u8, prop_type, "Bool")) {
            type_node.* = .Bool;
        } else {
            // For unknown types, default to Any
            type_node.* = .Any;
        }

        try props.append(ast.AstNode.Class.Field{
            .name = name_copy,
            .required = required,
            .type = type_node,
        });
    }

    // Create class node
    const class_node = try allocator.create(ast.AstNode.Class);
    class_node.* = .{
        .name = class_name,
        .fields = try props.toOwnedSlice(),
        .methods = &.{},
        .parent = null,
    };
    try nodes.append(ast.AstNode{
        .Class = class_node,
    });
}
