const std = @import("std");
const bytecode = @import("bytecode.zig");
const types = @import("../core/types.zig");

/// Serialize a bytecode function to Gene format
///
/// This function takes a bytecode function and serializes it to a human-readable
/// Gene format, writing the result to the provided writer.
///
/// Parameters:
///   - writer: The writer to output the serialized bytecode
///   - func: The bytecode function to serialize
///   - indent: The current indentation level
pub fn serializeFunction(writer: anytype, func: bytecode.Function, indent: usize) !void {
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
        try serializeInstruction(writer, instr);
        try writer.writeAll(")\n");
    }
    try writeIndent(writer, indent + 1);
    try writer.writeAll(")\n");
    
    try writeIndent(writer, indent);
    try writer.writeAll(")");
}

/// Serialize a bytecode instruction to Gene format
fn serializeInstruction(writer: anytype, instr: bytecode.Instruction) !void {
    try writer.print("{s}", .{@tagName(instr.op)});
    
    if (instr.immediate) |immediate| {
        try writer.writeAll(" ");
        try serializeValue(writer, immediate);
    }
    if (instr.var_name) |var_name| {
        try writer.print(" var:{s}", .{var_name});
    }
    if (instr.jump_target) |target| {
        try writer.print(" target:{d}", .{target});
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
