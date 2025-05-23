const std = @import("std");
const ast = @import("ast.zig");
const types = @import("types.zig");

/// Serialize an AST node to Gene format
/// 
/// This function takes an AST node and serializes it to a human-readable
/// Gene format, writing the result to the provided writer.
///
/// Parameters:
///   - writer: The writer to output the serialized AST
///   - node: The AST node to serialize
///   - indent: The current indentation level
pub fn serializeNode(writer: anytype, node: ast.AstNode, indent: usize) !void {
    try writeIndent(writer, indent);
    
    switch (node) {
        .Expression => |expr| {
            try serializeExpression(writer, expr, indent);
        },
    }
}

/// Serialize an AST expression to Gene format
fn serializeExpression(writer: anytype, expr: ast.Expression, indent: usize) !void {
    switch (expr) {
        .Literal => |lit| {
            try serializeValue(writer, lit.value);
        },
        .Variable => |var_expr| {
            try writer.print("(var-ref \"{s}\")", .{var_expr.name});
        },
        .FuncCall => |call| {
            try writer.writeAll("(call ");
            try serializeExpression(writer, call.func.*, indent);
            try writer.writeAll(" ");
            
            for (call.args.items, 0..) |arg, i| {
                if (i > 0) try writer.writeAll(" ");
                try serializeExpression(writer, arg.*, indent + 1);
            }
            
            try writer.writeAll(")");
        },
        .BinaryOp => |bin_op| {
            try writer.print("(binary-op \"{s}\" ", .{bin_op.op.Ident});
            try serializeExpression(writer, bin_op.left.*, indent + 1);
            try writer.writeAll(" ");
            try serializeExpression(writer, bin_op.right.*, indent + 1);
            try writer.writeAll(")");
        },
        .If => |if_expr| {
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
        .FuncDef => |func_def| {
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
        .VarDecl => |var_decl| {
            try writer.print("(var-decl \"{s}\" ", .{var_decl.name});
            try serializeExpression(writer, var_decl.value.*, indent + 1);
            try writer.writeAll(")");
        },
    }
}

/// Serialize a Gene value to Gene format
fn serializeValue(writer: anytype, value: types.Value) !void {
    switch (value) {
        .Int => |int_val| {
            try writer.print("{d}", .{int_val});
        },
        .Float => |float_val| {
            try writer.print("{d}", .{float_val});
        },
        .Bool => |bool_val| {
            try writer.print("{}", .{bool_val});
        },
        .String => |string_val| {
            try writer.print("\"{s}\"", .{string_val});
        },
        .Nil => {
            try writer.writeAll("nil");
        },
        .Variable => |var_val| {
            try writer.print("(var \"{s}\")", .{var_val.name});
        },
        else => {
            try writer.writeAll("(unknown-value)");
        },
    }
}

/// Write indentation spaces
fn writeIndent(writer: anytype, indent: usize) !void {
    var i: usize = 0;
    while (i < indent * 2) : (i += 1) {
        try writer.writeByte(' ');
    }
}
