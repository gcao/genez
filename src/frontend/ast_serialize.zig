const std = @import("std");
const ast = @import("ast.zig");
const types = @import("../core/types.zig");

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
            
            // Handle args which is an ArrayList
            for (call.args.items) |arg| {
                try writer.writeAll(" ");
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
            try writer.writeAll(" ");
            try serializeExpression(writer, if_expr.else_branch.*, indent + 1);
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

            try writer.writeAll("]");
            
            // Add rest parameter if present
            if (func_def.rest_param) |rp| {
                try writer.print(" rest:\"{s}\"", .{rp});
            }
            
            try writer.writeAll(" ");
            try serializeExpression(writer, func_def.body.*, indent + 1);
            try writer.writeAll(")");
        },
        .VarDecl => |var_decl| {
            try writer.print("(var-decl \"{s}\" ", .{var_decl.name});
            try serializeExpression(writer, var_decl.value.*, indent + 1);
            try writer.writeAll(")");
        },
        .SimpleFuncDef => |func_def| {
            try writer.print("(simple-fn \"{s}\" {d})", .{ func_def.getName(), func_def.body_literal });
        },
        .ArrayLiteral => |arr_lit| {
            try writer.writeAll("(array ");
            for (arr_lit.elements, 0..) |element, i| {
                if (i > 0) try writer.writeAll(" ");
                try serializeExpression(writer, element.*, indent + 1);
            }
            try writer.writeAll(")");
        },
        .MapLiteral => |map_lit| {
            try writer.writeAll("(map ");
            for (map_lit.entries, 0..) |entry, i| {
                if (i > 0) try writer.writeAll(" ");
                try writer.writeAll("[");
                try serializeExpression(writer, entry.key.*, indent + 1);
                try writer.writeAll(" ");
                try serializeExpression(writer, entry.value.*, indent + 1);
                try writer.writeAll("]");
            }
            try writer.writeAll(")");
        },
        .DoBlock => |do_block| {
            try writer.writeAll("(do ");
            for (do_block.statements, 0..) |stmt, i| {
                if (i > 0) try writer.writeAll(" ");
                try serializeExpression(writer, stmt.*, indent + 1);
            }
            try writer.writeAll(")");
        },
        .ClassDef => |class_def| {
            try writer.print("(class \"{s}\"", .{class_def.name});
            
            // Add parent class if present
            if (class_def.parent_class) |parent| {
                try writer.print(" :extends \"{s}\"", .{parent});
            }
            
            // Add traits if present
            if (class_def.traits.len > 0) {
                try writer.writeAll(" :implements (");
                for (class_def.traits, 0..) |trait_name, i| {
                    if (i > 0) try writer.writeAll(" ");
                    try writer.print("\"{s}\"", .{trait_name});
                }
                try writer.writeAll(")");
            }
            
            // Add fields
            if (class_def.fields.len > 0) {
                try writer.writeAll(" :fields (");
                for (class_def.fields, 0..) |field, i| {
                    if (i > 0) try writer.writeAll(" ");
                    try writer.print("(\"{s}\"", .{field.name});
                    if (field.type_annotation) |type_ann| {
                        try writer.print(" :{s}", .{type_ann});
                    }
                    if (!field.is_public) try writer.writeAll(" :private");
                    if (field.default_value) |default_val| {
                        try writer.writeAll(" :default ");
                        try serializeExpression(writer, default_val.*, indent + 1);
                    }
                    try writer.writeAll(")");
                }
                try writer.writeAll(")");
            }
            
            // Add methods
            if (class_def.methods.len > 0) {
                try writer.writeAll(" :methods (");
                for (class_def.methods, 0..) |method, i| {
                    if (i > 0) try writer.writeAll(" ");
                    try writer.print("(\"{s}\" (", .{method.name});
                    
                    // Parameters
                    for (method.params, 0..) |param, j| {
                        if (j > 0) try writer.writeAll(" ");
                        try writer.print("\"{s}\"", .{param.name});
                        if (param.type_annotation) |type_ann| {
                            try writer.print(" :{s}", .{type_ann});
                        }
                    }
                    try writer.writeAll(") ");
                    
                    // Method body
                    try serializeExpression(writer, method.body.*, indent + 1);
                    
                    // Method flags
                    if (method.visibility != .Public) {
                        try writer.print(" :{s}", .{@tagName(method.visibility)});
                    }
                    if (method.is_virtual) try writer.writeAll(" :virtual");
                    if (method.is_abstract) try writer.writeAll(" :abstract");
                    if (method.method_type != .Regular) {
                        try writer.print(" :{s}", .{@tagName(method.method_type)});
                    }
                    
                    try writer.writeAll(")");
                }
                try writer.writeAll(")");
            }
            
            try writer.writeAll(")");
        },
        .InstanceCreation => |inst| {
            try writer.print("(new \"{s}\"", .{inst.class_name});
            
            // Add constructor arguments
            for (inst.args.items) |arg| {
                try writer.writeAll(" ");
                try serializeExpression(writer, arg.*, indent + 1);
            }
            
            try writer.writeAll(")");
        },
        .MatchExpr => |match_expr| {
            try writer.writeAll("(match ");
            try serializeExpression(writer, match_expr.scrutinee.*, indent + 1);
            
            for (match_expr.arms) |arm| {
                try writer.writeAll(" (");
                try serializePattern(writer, arm.pattern, indent + 1);
                
                if (arm.guard) |guard| {
                    try writer.writeAll(" :when ");
                    try serializeExpression(writer, guard.*, indent + 1);
                }
                
                try writer.writeAll(" ");
                try serializeExpression(writer, arm.body.*, indent + 1);
                try writer.writeAll(")");
            }
            
            try writer.writeAll(")");
        },
        .ModuleDef => |module_def| {
            try writer.print("(module \"{s}\"", .{module_def.name});
            
            // Add imports
            if (module_def.imports.len > 0) {
                try writer.writeAll(" :imports (");
                for (module_def.imports, 0..) |import_spec, i| {
                    if (i > 0) try writer.writeAll(" ");
                    try writer.print("(\"{s}\"", .{import_spec.module_path});
                    if (import_spec.alias) |alias| {
                        try writer.print(" :as \"{s}\"", .{alias});
                    }
                    if (import_spec.items) |items| {
                        try writer.writeAll(" :items (");
                        for (items, 0..) |item, j| {
                            if (j > 0) try writer.writeAll(" ");
                            try writer.print("\"{s}\"", .{item.name});
                            if (item.alias) |alias| {
                                try writer.print(" :as \"{s}\"", .{alias});
                            }
                        }
                        try writer.writeAll(")");
                    }
                    try writer.writeAll(")");
                }
                try writer.writeAll(")");
            }
            
            // Add exports
            if (module_def.exports.len > 0) {
                try writer.writeAll(" :exports (");
                for (module_def.exports, 0..) |export_name, i| {
                    if (i > 0) try writer.writeAll(" ");
                    try writer.print("\"{s}\"", .{export_name});
                }
                try writer.writeAll(")");
            }
            
            // Add body
            try writer.writeAll(" ");
            try serializeExpression(writer, module_def.body.*, indent + 1);
            
            try writer.writeAll(")");
        },
        .ImportStmt => |import_stmt| {
            try writer.print("(import \"{s}\"", .{import_stmt.module_path});
            
            if (import_stmt.alias) |alias| {
                try writer.print(" :as \"{s}\"", .{alias});
            }
            
            if (import_stmt.items) |items| {
                try writer.writeAll(" :items (");
                for (items, 0..) |item, i| {
                    if (i > 0) try writer.writeAll(" ");
                    try writer.print("\"{s}\"", .{item.name});
                    if (item.alias) |alias| {
                        try writer.print(" :as \"{s}\"", .{alias});
                    }
                }
                try writer.writeAll(")");
            }
            
            try writer.writeAll(")");
        },
        .ExportStmt => |export_stmt| {
            try writer.writeAll("(export ");
            for (export_stmt.items, 0..) |item, i| {
                if (i > 0) try writer.writeAll(" ");
                try writer.print("\"{s}\"", .{item.name});
                if (item.alias) |alias| {
                    try writer.print(" :as \"{s}\"", .{alias});
                }
            }
            try writer.writeAll(")");
        },
        .PathAccess => |path| {
            try serializeExpression(writer, path.object.*, indent);
            try writer.writeAll("/");
            try serializeExpression(writer, path.path.*, indent);
        },
        .PathAssignment => |assign| {
            try writer.writeAll("(= ");
            try serializeExpression(writer, assign.path.*, indent);
            try writer.writeAll(" ");
            try serializeExpression(writer, assign.value.*, indent);
            try writer.writeAll(")");
        },
        .MethodCall => |call| {
            try writer.writeAll("(");
            try serializeExpression(writer, call.object.*, indent);
            try writer.print(" .{s}", .{call.method_name});
            for (call.args.items) |arg| {
                try writer.writeAll(" ");
                try serializeExpression(writer, arg.*, indent);
            }
            try writer.writeAll(")");
        },
        .NamespaceDecl => |ns_decl| {
            try writer.print("(ns \"{s}\" ", .{ns_decl.name});
            try serializeExpression(writer, ns_decl.body.*, indent + 1);
            try writer.writeAll(")");
        },
        .Return => |ret| {
            try writer.writeAll("(return");
            if (ret.value) |value| {
                try writer.writeAll(" ");
                try serializeExpression(writer, value.*, indent);
            }
            try writer.writeAll(")");
        },
        .TryExpr => |try_expr| {
            try writer.writeAll("(try ");
            try serializeExpression(writer, try_expr.body.*, indent + 1);
            
            if (try_expr.catch_clauses.len > 0) {
                for (try_expr.catch_clauses) |catch_clause| {
                    try writer.writeAll(" (catch");
                    if (catch_clause.error_var) |error_var| {
                        try writer.print(" {s}", .{error_var});
                    }
                    try writer.writeAll(" ");
                    try serializeExpression(writer, catch_clause.body.*, indent + 1);
                    try writer.writeAll(")");
                }
            }
            
            if (try_expr.finally_block) |finally_block| {
                try writer.writeAll(" (finally ");
                try serializeExpression(writer, finally_block.*, indent + 1);
                try writer.writeAll(")");
            }
            
            try writer.writeAll(")");
        },
        .ThrowExpr => |throw_expr| {
            try writer.writeAll("(throw ");
            try serializeExpression(writer, throw_expr.value.*, indent);
            try writer.writeAll(")");
        },
        .ForLoop => |for_loop| {
            try writer.print("(for {s} in ", .{for_loop.iterator});
            try serializeExpression(writer, for_loop.iterable.*, indent);
            try writer.writeAll(" ");
            try serializeExpression(writer, for_loop.body.*, indent + 1);
            try writer.writeAll(")");
        },
        .CExternDecl => |extern_decl| {
            try writer.print("(c-extern {s} (", .{extern_decl.name});
            for (extern_decl.params, 0..) |param, i| {
                if (i > 0) try writer.writeAll(" ");
                try writer.print("[{s} \"{s}\"]", .{param.name, param.c_type});
            }
            try writer.writeAll(") ");
            if (extern_decl.return_type) |ret_type| {
                try writer.print("\"{s}\"", .{ret_type});
            } else {
                try writer.writeAll("nil");
            }
            try writer.print(" \"{s}\"", .{extern_decl.lib});
            if (extern_decl.symbol) |symbol| {
                try writer.print(" :symbol \"{s}\"", .{symbol});
            }
            if (extern_decl.calling_convention) |cc| {
                try writer.print(" :convention \"{s}\"", .{cc});
            }
            if (extern_decl.is_variadic) {
                try writer.writeAll(" :variadic true");
            }
            try writer.writeAll(")");
        },
        .CStructDecl => |struct_decl| {
            try writer.print("(c-struct {s} (", .{struct_decl.name});
            for (struct_decl.fields, 0..) |field, i| {
                if (i > 0) try writer.writeAll(" ");
                try writer.print("[{s} \"{s}\"", .{field.name, field.c_type});
                if (field.bit_size) |size| {
                    try writer.print(" {d}", .{size});
                }
                try writer.writeAll("]");
            }
            try writer.writeAll(")");
            if (struct_decl.is_packed) {
                try writer.writeAll(" :packed true");
            }
            if (struct_decl.alignment) |alignment| {
                try writer.print(" :align {d}", .{alignment});
            }
            try writer.writeAll(")");
        },
        .CTypeDecl => |type_decl| {
            try writer.print("(c-type {s} \"{s}\")", .{type_decl.name, type_decl.c_type});
        },
        .CCallback => |callback| {
            try writer.writeAll("(c-callback ");
            try serializeExpression(writer, callback.function.*, indent);
            if (callback.signature) |sig| {
                try writer.print(" \"{s}\"", .{sig});
            }
            try writer.writeAll(")");
        },
        else => |tag| {
            try writer.print("(; unsupported: {s} ;)", .{@tagName(tag)});
        },
    }
}

/// Serialize a pattern to Gene format
fn serializePattern(writer: anytype, pattern: ast.MatchExpr.Pattern, indent: usize) @TypeOf(writer).Error!void {
    switch (pattern) {
        .Literal => |lit| {
            try serializeExpression(writer, lit.value.*, indent);
        },
        .Variable => |var_pat| {
            try writer.print("${s}", .{var_pat.name});
            if (var_pat.type_annotation) |type_ann| {
                try writer.print(":{s}", .{type_ann});
            }
        },
        .Wildcard => {
            try writer.writeAll("_");
        },
        .Constructor => |ctor| {
            try writer.print("({s}", .{ctor.constructor});
            for (ctor.fields) |field| {
                try writer.writeAll(" ");
                try serializePattern(writer, field, indent + 1);
            }
            try writer.writeAll(")");
        },
        .Array => |arr| {
            try writer.writeAll("[");
            for (arr.elements, 0..) |elem, i| {
                if (i > 0) try writer.writeAll(" ");
                try serializePattern(writer, elem, indent + 1);
            }
            if (arr.rest) |rest| {
                if (arr.elements.len > 0) try writer.writeAll(" ");
                try writer.print("..{s}", .{rest});
            }
            try writer.writeAll("]");
        },
        .Map => |map| {
            try writer.writeAll("{");
            for (map.fields, 0..) |field, i| {
                if (i > 0) try writer.writeAll(" ");
                try writer.print("{s}: ", .{field.key});
                try serializePattern(writer, field.pattern, indent + 1);
            }
            if (map.rest) |rest| {
                if (map.fields.len > 0) try writer.writeAll(" ");
                try writer.print("..{s}", .{rest});
            }
            try writer.writeAll("}");
        },
        .Or => |or_pat| {
            try writer.writeAll("(");
            for (or_pat.patterns, 0..) |pat, i| {
                if (i > 0) try writer.writeAll(" | ");
                try serializePattern(writer, pat, indent + 1);
            }
            try writer.writeAll(")");
        },
        .Range => |range| {
            try serializeExpression(writer, range.start.*, indent);
            if (range.inclusive) {
                try writer.writeAll("..");
            } else {
                try writer.writeAll("...");
            }
            try serializeExpression(writer, range.end.*, indent);
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
            // Always print with decimal point to distinguish from integers
            if (@floor(float_val) == float_val and float_val == @trunc(float_val)) {
                // It's a whole number, print with .0
                try writer.print("{d:.1}", .{float_val});
            } else {
                try writer.print("{d}", .{float_val});
            }
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
