const std = @import("std");
const mir = @import("mir.zig");
const types = @import("types.zig");

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

/// Write indentation spaces
fn writeIndent(writer: anytype, indent: usize) !void {
    var i: usize = 0;
    while (i < indent * 2) : (i += 1) {
        try writer.writeByte(' ');
    }
}
