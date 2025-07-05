const std = @import("std");
const mir = @import("mir.zig");
const types = @import("../core/types.zig");

/// Serialize an MIR module to Gene format
///
/// This function takes an MIR module and serializes it to a human-readable
/// Gene format, writing the result to the provided writer.
///
/// Parameters:
///   - writer: The writer to output the serialized MIR
///   - module: The MIR module to serialize
///   - indent: The current indentation level
pub fn serializeModule(writer: anytype, module: mir.MIR, indent: usize) !void {
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
                try serializeInstruction(writer, instr);
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
fn serializeInstruction(writer: anytype, instr: mir.MIR.Instruction) !void {
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
        .LoadVariable => |name| {
            try writer.print("load-variable \"{s}\"", .{name});
        },
        .LoadParameter => |index| {
            try writer.print("load-parameter {d}", .{index});
        },
        .LoadFunction => |_| {
            try writer.writeAll("load-function");
        },
        .LoadModule => |path| {
            try writer.print("load-module \"{s}\"", .{path});
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
        .Mul => {
            try writer.writeAll("mul");
        },
        .Div => {
            try writer.writeAll("div");
        },
        .LessThan => {
            try writer.writeAll("less-than");
        },
        .GreaterThan => {
            try writer.writeAll("greater-than");
        },
        .Equal => {
            try writer.writeAll("equal");
        },
        .NotEqual => {
            try writer.writeAll("not-equal");
        },
        .LessEqual => {
            try writer.writeAll("less-equal");
        },
        .GreaterEqual => {
            try writer.writeAll("greater-equal");
        },
        .LogicalAnd => {
            try writer.writeAll("logical-and");
        },
        .LogicalOr => {
            try writer.writeAll("logical-or");
        },
        .LogicalNot => {
            try writer.writeAll("logical-not");
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
        .DefineClass => |class_def| {
            try writer.print("define-class \"{s}\"", .{class_def.name});
        },
        .CreateInstance => |inst_creation| {
            try writer.print("create-instance \"{s}\" {}", .{ inst_creation.class_name, inst_creation.arg_count });
        },
        .Get => {
            try writer.writeAll("get");
        },
        .Set => {
            try writer.writeAll("set");
        },
        .CallMethod => |method_call| {
            try writer.print("call-method \"{s}\" {d}", .{ method_call.method_name, method_call.arg_count });
        },
        .Length => {
            try writer.writeAll("length");
        },
        .ArrayGet => {
            try writer.writeAll("array-get");
        },
        .MapGet => {
            try writer.writeAll("map-get");
        },
        .Duplicate => {
            try writer.writeAll("duplicate");
        },
        .Pop => {
            try writer.writeAll("pop");
        },
        .IsArray => {
            try writer.writeAll("is-array");
        },
        .IsMap => {
            try writer.writeAll("is-map");
        },
        .CreateNamespace => {
            try writer.writeAll("create-namespace");
        },
        .PushNamespace => {
            try writer.writeAll("push-namespace");
        },
        .PopNamespace => {
            try writer.writeAll("pop-namespace");
        },
        .CreateModule => {
            try writer.writeAll("create-module");
        },
        .PushModule => {
            try writer.writeAll("push-module");
        },
        .PopModule => {
            try writer.writeAll("pop-module");
        },
        .MarkExport => {
            try writer.writeAll("mark-export");
        },
        .Export => {
            try writer.writeAll("export");
        },
        .TryStart => |catch_target| {
            try writer.print("try-start {d}", .{catch_target});
        },
        .TryEnd => {
            try writer.writeAll("try-end");
        },
        .Throw => {
            try writer.writeAll("throw");
        },
        .LoadException => {
            try writer.writeAll("load-exception");
        },
        .ClearException => {
            try writer.writeAll("clear-exception");
        },
        .CreateCallback => {
            try writer.writeAll("create-callback");
        },
        .CreateArray => |count| {
            try writer.print("create-array {d}", .{count});
        },
        .CreateMap => |count| {
            try writer.print("create-map {d}", .{count});
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
