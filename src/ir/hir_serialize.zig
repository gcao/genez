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

    // Serialize imports
    if (module.imports.items.len > 0) {
        try writeIndent(writer, indent + 1);
        try writer.writeAll("(imports\n");
        for (module.imports.items) |import| {
            try writeIndent(writer, indent + 2);
            try writer.print("(import \"{s}\"", .{import.module_path});
            if (import.alias) |alias| {
                try writer.print(" :as \"{s}\"", .{alias});
            }
            if (import.items) |items| {
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
            try writer.writeAll(")\n");
        }
        try writeIndent(writer, indent + 1);
        try writer.writeAll(")\n");
    }

    // Serialize FFI functions
    if (module.ffi_functions.items.len > 0) {
        try writeIndent(writer, indent + 1);
        try writer.writeAll("(ffi-functions\n");
        for (module.ffi_functions.items) |ffi_func| {
            try writeIndent(writer, indent + 2);
            try writer.print("(c-extern {s} (", .{ffi_func.name});
            for (ffi_func.params, 0..) |param, i| {
                if (i > 0) try writer.writeAll(" ");
                try writer.print("[{s} \"{s}\"]", .{param.name, param.c_type});
            }
            try writer.writeAll(") ");
            if (ffi_func.return_type) |ret| {
                try writer.print("\"{s}\"", .{ret});
            } else {
                try writer.writeAll("nil");
            }
            try writer.print(" \"{s}\"", .{ffi_func.lib});
            if (ffi_func.symbol) |sym| {
                try writer.print(" :symbol \"{s}\"", .{sym});
            }
            if (ffi_func.calling_convention) |cc| {
                try writer.print(" :convention \"{s}\"", .{cc});
            }
            if (ffi_func.is_variadic) {
                try writer.writeAll(" :variadic true");
            }
            try writer.writeAll(")\n");
        }
        try writeIndent(writer, indent + 1);
        try writer.writeAll(")\n");
    }

    // Serialize FFI structs
    if (module.ffi_structs.items.len > 0) {
        try writeIndent(writer, indent + 1);
        try writer.writeAll("(ffi-structs\n");
        for (module.ffi_structs.items) |ffi_struct| {
            try writeIndent(writer, indent + 2);
            try writer.print("(c-struct {s} (", .{ffi_struct.name});
            for (ffi_struct.fields, 0..) |field, i| {
                if (i > 0) try writer.writeAll(" ");
                try writer.print("[{s} \"{s}\"", .{field.name, field.c_type});
                if (field.bit_size) |size| {
                    try writer.print(" {d}", .{size});
                }
                try writer.writeAll("]");
            }
            try writer.writeAll(")");
            if (ffi_struct.is_packed) {
                try writer.writeAll(" :packed true");
            }
            if (ffi_struct.alignment) |alignment| {
                try writer.print(" :align {d}", .{alignment});
            }
            try writer.writeAll(")\n");
        }
        try writeIndent(writer, indent + 1);
        try writer.writeAll(")\n");
    }

    // Serialize FFI types
    if (module.ffi_types.items.len > 0) {
        try writeIndent(writer, indent + 1);
        try writer.writeAll("(ffi-types\n");
        for (module.ffi_types.items) |ffi_type| {
            try writeIndent(writer, indent + 2);
            try writer.print("(c-type {s} \"{s}\")\n", .{ffi_type.name, ffi_type.c_type});
        }
        try writeIndent(writer, indent + 1);
        try writer.writeAll(")\n");
    }

    // Serialize functions
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
fn serializeStatement(writer: anytype, stmt: hir.HIR.Statement, indent: usize) anyerror!void {
    switch (stmt) {
        .Expression => |expr| {
            try serializeExpression(writer, expr, indent);
        },
    }
}

/// Serialize an HIR expression to Gene format
fn serializeExpression(writer: anytype, expr: hir.HIR.Expression, indent: usize) anyerror!void {
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
        .function => |func| {
            try writer.print("(fn \"{s}\" [", .{func.name});
            for (func.params, 0..) |param, i| {
                if (i > 0) try writer.writeAll(" ");
                try writer.print("\"{s}\"", .{param.name});
                if (param.param_type) |param_type| {
                    try writer.print(" : \"{s}\"", .{param_type});
                }
            }
            try writer.writeAll("] (do ");
            for (func.body.items, 0..) |stmt, i| {
                if (i > 0) try writer.writeAll(" ");
                try serializeStatement(writer, stmt, indent + 1);
            }
            try writer.writeAll("))");
        },
        .array_literal => |arr_lit| {
            try writer.writeAll("(array ");
            for (arr_lit.elements, 0..) |element, i| {
                if (i > 0) try writer.writeAll(" ");
                try serializeExpression(writer, element.*, indent + 1);
            }
            try writer.writeAll(")");
        },
        .map_literal => |map_lit| {
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
        .do_block => |do_block| {
            try writer.writeAll("(do ");
            for (do_block.statements, 0..) |stmt, i| {
                if (i > 0) try writer.writeAll(" ");
                try serializeExpression(writer, stmt.*, indent + 1);
            }
            try writer.writeAll(")");
        },
        .class_def => |class_def| {
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
                        if (param.param_type) |type_ann| {
                            try writer.print(" :{s}", .{type_ann});
                        }
                    }
                    try writer.writeAll(") ");
                    
                    // Method body
                    try serializeExpression(writer, method.body.*, indent + 1);
                    
                    // Method flags
                    if (!method.is_public) try writer.writeAll(" :private");
                    if (method.is_virtual) try writer.writeAll(" :virtual");
                    if (method.is_abstract) try writer.writeAll(" :abstract");
                    if (method.is_static) try writer.writeAll(" :static");
                    
                    try writer.writeAll(")");
                }
                try writer.writeAll(")");
            }
            
            try writer.writeAll(")");
        },
        .instance_creation => |inst_creation| {
            try writer.print("(new \"{s}\"", .{inst_creation.class_name});
            for (inst_creation.args.items) |arg| {
                try writer.writeAll(" ");
                try serializeExpression(writer, arg.*, indent + 1);
            }
            try writer.writeAll(")");
        },
        .method_call => |method_call| {
            try writer.writeAll("(method-call ");
            try serializeExpression(writer, method_call.object.*, indent + 1);
            try writer.print(" \"{s}\"", .{method_call.method_name});
            for (method_call.args.items) |arg| {
                try writer.writeAll(" ");
                try serializeExpression(writer, arg.*, indent + 1);
            }
            try writer.writeAll(")");
        },
        .field_access => |field_access| {
            if (field_access.object) |obj| {
                try serializeExpression(writer, obj.*, indent);
                try writer.print("/{s}", .{field_access.field_name});
            } else {
                try writer.print("/{s}", .{field_access.field_name});
            }
        },
        .path_assignment => |path_assign| {
            try writer.writeAll("(= ");
            try serializeExpression(writer, path_assign.path.*, indent);
            try writer.writeAll(" ");
            try serializeExpression(writer, path_assign.value.*, indent);
            try writer.writeAll(")");
        },
        .case_expr => |case_expr| {
            try writer.writeAll("(case ");
            try serializeExpression(writer, case_expr.scrutinee.*, indent + 1);
            
            for (case_expr.branches) |branch| {
                try writer.writeAll(" (when ");
                try serializeExpression(writer, branch.condition.*, indent + 1);
                try writer.writeAll(" ");
                try serializeExpression(writer, branch.body.*, indent + 1);
                try writer.writeAll(")");
            }
            
            if (case_expr.else_branch) |else_br| {
                try writer.writeAll(" (else ");
                try serializeExpression(writer, else_br.*, indent + 1);
                try writer.writeAll(")");
            }
            
            try writer.writeAll(")");
        },
        .match_expr => |match_expr| {
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
        .macro_def => |macro_ptr| {
            try writer.writeAll("(macro ");
            try writer.writeAll(macro_ptr.name);
            
            // Write parameters
            try writer.writeAll(" [");
            for (macro_ptr.params, 0..) |param, i| {
                if (i > 0) try writer.writeAll(" ");
                try writer.writeAll(param.name);
                if (param.is_variadic) try writer.writeAll("...");
            }
            try writer.writeAll("] ");
            
            // Write body
            try serializeExpression(writer, macro_ptr.body.*, indent + 1);
            try writer.writeAll(")");
        },
        .macro_call => |macro_call| {
            try writer.writeAll("(");
            try serializeExpression(writer, macro_call.macro.*, indent);
            
            // Write arguments
            for (macro_call.args) |arg| {
                try writer.writeAll(" ");
                try serializeExpression(writer, arg.*, indent);
            }
            try writer.writeAll(")");
        },
        .for_loop => |for_ptr| {
            try writer.print("(for {s} in ", .{for_ptr.iterator});
            try serializeExpression(writer, for_ptr.iterable.*, indent);
            try writer.writeAll(" ");
            try serializeExpression(writer, for_ptr.body.*, indent + 1);
            try writer.writeAll(")");
        },
        .while_loop => |while_ptr| {
            try writer.writeAll("(while ");
            try serializeExpression(writer, while_ptr.condition.*, indent);
            try writer.writeAll(" ");
            try serializeExpression(writer, while_ptr.body.*, indent + 1);
            try writer.writeAll(")");
        },
        .return_expr => |ret_ptr| {
            try writer.writeAll("(return");
            if (ret_ptr.value) |val| {
                try writer.writeAll(" ");
                try serializeExpression(writer, val.*, indent);
            }
            try writer.writeAll(")");
        },
        .import_stmt => |import_ptr| {
            try writer.print("(import \"{s}\"", .{import_ptr.module_path});
            if (import_ptr.alias) |alias| {
                try writer.print(" :as {s}", .{alias});
            }
            if (import_ptr.items) |items| {
                try writer.writeAll(" [");
                for (items, 0..) |item, i| {
                    if (i > 0) try writer.writeAll(" ");
                    if (item.alias) |alias| {
                        try writer.print("[{s} {s}]", .{item.name, alias});
                    } else {
                        try writer.writeAll(item.name);
                    }
                }
                try writer.writeAll("]");
            }
            try writer.writeAll(")");
        },
        .module_access => |mod_access| {
            try writer.print("{s}/{s}", .{mod_access.module, mod_access.member});
        },
        .namespace_decl => |ns_ptr| {
            try writer.print("(ns \"{s}\" ", .{ns_ptr.name});
            try serializeExpression(writer, ns_ptr.body.*, indent + 1);
            try writer.writeAll(")");
        },
        .try_expr => |try_ptr| {
            try writer.writeAll("(try ");
            try serializeExpression(writer, try_ptr.body.*, indent + 1);
            
            for (try_ptr.catch_clauses) |catch_clause| {
                try writer.writeAll(" (catch");
                if (catch_clause.error_var) |var_name| {
                    try writer.print(" {s}", .{var_name});
                }
                try writer.writeAll(" ");
                try serializeExpression(writer, catch_clause.body.*, indent + 1);
                try writer.writeAll(")");
            }
            
            if (try_ptr.finally_block) |finally| {
                try writer.writeAll(" (finally ");
                try serializeExpression(writer, finally.*, indent + 1);
                try writer.writeAll(")");
            }
            
            try writer.writeAll(")");
        },
        .throw_expr => |throw_ptr| {
            try writer.writeAll("(throw ");
            try serializeExpression(writer, throw_ptr.value.*, indent);
            try writer.writeAll(")");
        },
        .c_callback => |cb_ptr| {
            try writer.writeAll("(c-callback ");
            try serializeExpression(writer, cb_ptr.function.*, indent);
            if (cb_ptr.signature) |sig| {
                try writer.print(" \"{s}\"", .{sig});
            }
            try writer.writeAll(")");
        },
        .module_def => |mod_def| {
            try writer.print("(module \"{s}\"", .{mod_def.name});
            
            // Write exports if any
            if (mod_def.exports.len > 0) {
                try writer.writeAll(" :exports [");
                for (mod_def.exports, 0..) |export_name, i| {
                    if (i > 0) try writer.writeAll(" ");
                    try writer.print("\"{s}\"", .{export_name});
                }
                try writer.writeAll("]");
            }
            
            // Write body
            try writer.writeAll("\n");
            for (mod_def.body.items) |stmt| {
                try writeIndent(writer, indent + 1);
                try serializeStatement(writer, stmt, indent + 1);
                try writer.writeAll("\n");
            }
            try writeIndent(writer, indent);
            try writer.writeAll(")");
        },
        .export_stmt => |export_stmt| {
            try writer.writeAll("(export");
            for (export_stmt.items) |item| {
                try writer.writeAll(" ");
                if (item.alias) |alias| {
                    try writer.print("[\"{s}\" => \"{s}\"]", .{item.name, alias});
                } else {
                    try writer.print("\"{s}\"", .{item.name});
                }
            }
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

/// Serialize an HIR pattern to Gene format
fn serializePattern(writer: anytype, pattern: hir.HIR.Pattern, indent: usize) anyerror!void {
    switch (pattern) {
        .literal => |lit| {
            try serializeExpression(writer, lit.value.*, indent);
        },
        .variable => |var_pat| {
            try writer.print("${s}", .{var_pat.name});
            if (var_pat.type_annotation) |type_ann| {
                try writer.print(":{s}", .{type_ann});
            }
        },
        .wildcard => {
            try writer.writeAll("_");
        },
        .constructor => |ctor| {
            try writer.print("({s}", .{ctor.constructor});
            for (ctor.fields) |field| {
                try writer.writeAll(" ");
                try serializePattern(writer, field, indent + 1);
            }
            try writer.writeAll(")");
        },
        .array => |arr| {
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
        .map => |map| {
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
        .or_pattern => |or_pat| {
            try writer.writeAll("(");
            for (or_pat.patterns, 0..) |pat, i| {
                if (i > 0) try writer.writeAll(" | ");
                try serializePattern(writer, pat, indent + 1);
            }
            try writer.writeAll(")");
        },
        .range => |range| {
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

/// Write indentation spaces
fn writeIndent(writer: anytype, indent: usize) !void {
    var i: usize = 0;
    while (i < indent * 2) : (i += 1) {
        try writer.writeByte(' ');
    }
}
