const std = @import("std");
const ast = @import("ast.zig");
const hir = @import("hir.zig");
const mir = @import("mir.zig");
const bytecode = @import("bytecode.zig");
const types = @import("types.zig");

/// Serialize an AST node to Gene format
pub fn serializeAst(writer: anytype, node: ast.AstNode, indent: usize) !void {
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

/// Serialize an HIR module to Gene format
pub fn serializeHir(writer: anytype, module: hir.HIR, indent: usize) !void {
    try writeIndent(writer, indent);
    try writer.writeAll("(hir-module\n");

    for (module.functions.items, 0..) |func, i| {
        try writeIndent(writer, indent + 1);
        try writer.print("(function {d}\n", .{i});

        // Serialize function body
        for (func.body.items) |stmt| {
            try writeIndent(writer, indent + 2);
            try serializeHirStatement(writer, stmt, indent + 2);
            try writer.writeAll("\n");
        }

        try writeIndent(writer, indent + 1);
        try writer.writeAll(")\n");
    }

    try writeIndent(writer, indent);
    try writer.writeAll(")");
}

/// Serialize an HIR statement to Gene format
fn serializeHirStatement(writer: anytype, stmt: hir.HIR.Statement, indent: usize) !void {
    switch (stmt) {
        .Expression => |expr| {
            try serializeHirExpression(writer, expr, indent);
        },
    }
}

/// Serialize an HIR expression to Gene format
fn serializeHirExpression(writer: anytype, expr: hir.HIR.Expression, indent: usize) !void {
    switch (expr) {
        .literal => |lit| {
            try serializeHirLiteral(writer, lit);
        },
        .variable => |var_expr| {
            try writer.print("(var-ref \"{s}\")", .{var_expr.name});
        },
        .func_call => |call| {
            try writer.writeAll("(call ");
            try serializeHirExpression(writer, call.func.*, indent);
            try writer.writeAll(" ");

            for (call.args.items, 0..) |arg, i| {
                if (i > 0) try writer.writeAll(" ");
                try serializeHirExpression(writer, arg.*, indent + 1);
            }

            try writer.writeAll(")");
        },
        .binary_op => |bin_op| {
            try writer.print("(binary-op \"{s}\" ", .{@tagName(bin_op.op)});
            try serializeHirExpression(writer, bin_op.left.*, indent + 1);
            try writer.writeAll(" ");
            try serializeHirExpression(writer, bin_op.right.*, indent + 1);
            try writer.writeAll(")");
        },
        .if_expr => |if_expr| {
            try writer.writeAll("(if ");
            try serializeHirExpression(writer, if_expr.condition.*, indent + 1);
            try writer.writeAll(" ");
            try serializeHirExpression(writer, if_expr.then_branch.*, indent + 1);

            if (if_expr.else_branch) |else_branch| {
                try writer.writeAll(" ");
                try serializeHirExpression(writer, else_branch.*, indent + 1);
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
            try serializeHirExpression(writer, func_def.body.*, indent + 1);
            try writer.writeAll(")");
        },
        .var_decl => |var_decl| {
            try writer.print("(var-decl \"{s}\" ", .{var_decl.name});
            try serializeHirExpression(writer, var_decl.value.*, indent + 1);
            try writer.writeAll(")");
        },
    }
}

/// Serialize an MIR module to Gene format
pub fn serializeMir(writer: anytype, module: mir.MIR, indent: usize) !void {
    try writeIndent(writer, indent);
    try writer.writeAll("(mir-module\n");

    // Serialize functions
    for (module.functions.items, 0..) |func, i| {
        try writeIndent(writer, indent + 1);
        try writer.print("(function {d}\n", .{i});

        // Serialize blocks
        for (func.blocks.items, 0..) |block, j| {
            try writeIndent(writer, indent + 2);
            try writer.print("(block {d}\n", .{j});

            // Serialize instructions
            for (block.instructions.items, 0..) |instr, k| {
                try writeIndent(writer, indent + 3);
                try writer.print("({d} ", .{k});
                try serializeMirInstruction(writer, instr);
                try writer.writeAll(")\n");
            }

            try writeIndent(writer, indent + 2);
            try writer.writeAll(")\n");
        }

        try writeIndent(writer, indent + 1);
        try writer.writeAll(")\n");
    }

    try writeIndent(writer, indent);
    try writer.writeAll(")");
}

/// Serialize an MIR instruction to Gene format
fn serializeMirInstruction(writer: anytype, instr: mir.MIR.Instruction) !void {
    switch (instr) {
        .LoadInt => |val| {
            try writer.print("load-int {d}", .{val});
        },
        .LoadFloat => |val| {
            try writer.print("load-float {d}", .{val});
        },
        .LoadBool => |val| {
            try writer.print("load-bool {}", .{val});
        },
        .LoadString => |val| {
            try writer.print("load-string \"{s}\"", .{val});
        },
        .LoadNil => {
            try writer.writeAll("load-nil");
        },
        .LoadSymbol => |val| {
            try writer.print("load-symbol '{s}", .{val});
        },
        .LoadArray => |_| {
            try writer.writeAll("load-array [...]");
        },
        .LoadMap => |_| {
            try writer.writeAll("load-map {...}");
        },
        .LoadVariable => |name| {
            try writer.print("load-variable \"{s}\"", .{name});
        },
        .LoadFunction => |_| {
            try writer.writeAll("load-function");
        },
        .StoreVariable => |name| {
            try writer.print("store-variable \"{s}\"", .{name});
        },
        .Add => {
            try writer.writeAll("add");
        },
        .Sub => {
            try writer.writeAll("sub");
        },
        .LessThan => {
            try writer.writeAll("less-than");
        },
        .GreaterThan => {
            try writer.writeAll("greater-than");
        },
        .Jump => |target| {
            try writer.print("jump {d}", .{target});
        },
        .JumpIfFalse => |target| {
            try writer.print("jump-if-false {d}", .{target});
        },
        .Call => |arg_count| {
            try writer.print("call {d}", .{arg_count});
        },
        .Print => {
            try writer.writeAll("print");
        },
        .Return => {
            try writer.writeAll("return");
        },
    }
}

/// Serialize a bytecode function to Gene format
pub fn serializeBytecode(writer: anytype, func: bytecode.Function, indent: usize) !void {
    try writeIndent(writer, indent);
    try writer.writeAll("(bytecode-function\n");

    // Serialize function metadata
    try writeIndent(writer, indent + 1);
    try writer.print("(name \"{s}\")\n", .{func.name});

    try writeIndent(writer, indent + 1);
    try writer.print("(param-count {d})\n", .{func.param_count});

    // Serialize instructions
    try writeIndent(writer, indent + 1);
    try writer.writeAll("(instructions\n");
    for (func.instructions.items, 0..) |instr, i| {
        try writeIndent(writer, indent + 2);
        try writer.print("({d} ", .{i});
        try serializeBytecodeInstruction(writer, instr);
        try writer.writeAll(")\n");
    }
    try writeIndent(writer, indent + 1);
    try writer.writeAll(")\n");

    try writeIndent(writer, indent);
    try writer.writeAll(")");
}

/// Serialize a bytecode instruction to Gene format
fn serializeBytecodeInstruction(writer: anytype, instr: bytecode.Instruction) !void {
    try writer.print("{s}", .{@tagName(instr.op)});

    if (instr.operand) |operand| {
        try writer.writeAll(" ");
        try serializeValue(writer, operand);
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
