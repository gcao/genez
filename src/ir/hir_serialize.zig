const std = @import("std");
const hir = @import("hir.zig");
const types = @import("../core/types.zig");

/// Serialize an HIR module to Gene format
///
/// This function takes an HIR module and serializes it to a human-readable
/// Gene format, writing the result to the provided writer.
///
/// Parameters:
///   - writer: The writer to output the serialized HIR
///   - module: The HIR module to serialize
///   - indent: The current indentation level
pub fn serializeModule(writer: anytype, module: hir.HIR, indent: usize) !void {
    try writeIndent(writer, indent);
    try writer.writeAll("(hir-module\n");

    for (module.functions.items, 0..) |func, i| {
        try writeIndent(writer, indent + 1);
        try writer.print("(function {d}\n", .{i});

        // Serialize function body
        for (func.body.items) |stmt| {
            try writeIndent(writer, indent + 2);
            try serializeStatement(writer, stmt, indent + 2);
            try writer.writeAll("\n");
        }

        try writeIndent(writer, indent + 1);
        try writer.writeAll(")\n");
    }

    try writeIndent(writer, indent);
    try writer.writeAll(")");
}

/// Serialize an HIR statement to Gene format
fn serializeStatement(writer: anytype, stmt: hir.HIR.Statement, indent: usize) !void {
    switch (stmt) {
        .Expression => |expr| {
            try serializeExpression(writer, expr, indent);
        },
    }
}

/// Serialize an HIR expression to Gene format
fn serializeExpression(writer: anytype, expr: hir.HIR.Expression, indent: usize) !void {
    switch (expr) {
        .literal => |lit| {
            try serializeHirLiteral(writer, lit);
        },
        .variable => |var_expr| {
            try writer.print("(var-ref \"{s}\")", .{var_expr.name});
        },
        .func_call => |call| {
            try writer.writeAll("(call ");
            try serializeExpression(writer, call.func.*, indent);
            try writer.writeAll(" ");

            for (call.args.items, 0..) |arg, i| {
                if (i > 0) try writer.writeAll(" ");
                try serializeExpression(writer, arg.*, indent + 1);
            }

            try writer.writeAll(")");
        },
        .binary_op => |bin_op| {
            try writer.print("(binary-op \"{s}\" ", .{@tagName(bin_op.op)});
            try serializeExpression(writer, bin_op.left.*, indent + 1);
            try writer.writeAll(" ");
            try serializeExpression(writer, bin_op.right.*, indent + 1);
            try writer.writeAll(")");
        },
        .if_expr => |if_expr| {
            try writer.writeAll("(if ");
            try serializeExpression(writer, if_expr.condition.*, indent + 1);
            try writer.writeAll(" ");
            try serializeExpression(writer, if_expr.then_branch.*, indent + 1);

            if (if_expr.else_branch) |else_branch| {
                try writer.writeAll(" ");
                try serializeExpression(writer, else_branch.*, indent + 1);
            }

            try writer.writeAll(")");
        },
        .func_def => |func_def| {
            try writer.print("(fn \"{s}\" [", .{func_def.name});

            for (func_def.params, 0..) |param, i| {
                if (i > 0) try writer.writeAll(" ");
                try writer.print("\"{s}\"", .{param.name});
                if (param.param_type) |param_type| {
                    try writer.print(" : \"{s}\"", .{param_type});
                }
            }

            try writer.writeAll("] ");
            try serializeExpression(writer, func_def.body.*, indent + 1);
            try writer.writeAll(")");
        },
        .var_decl => |var_decl| {
            try writer.print("(var-decl \"{s}\" ", .{var_decl.name});
            try serializeExpression(writer, var_decl.value.*, indent + 1);
            try writer.writeAll(")");
        },
    }
}

/// Serialize an HIR literal to Gene format
fn serializeHirLiteral(writer: anytype, lit: hir.HIR.Literal) !void {
    switch (lit) {
        .nil => try writer.writeAll("nil"),
        .bool => |b| try writer.print("{}", .{b}),
        .int => |i| try writer.print("{d}", .{i}),
        .float => |f| try writer.print("{d}", .{f}),
        .string => |s| try writer.print("\"{s}\"", .{s}),
        .symbol => |s| try writer.print("'{s}", .{s}),
        .array => |_| try writer.writeAll("(array ...)"),
        .map => |_| try writer.writeAll("(map ...)"),
    }
}

/// Write indentation spaces
fn writeIndent(writer: anytype, indent: usize) !void {
    var i: usize = 0;
    while (i < indent * 2) : (i += 1) {
        try writer.writeByte(' ');
    }
}
