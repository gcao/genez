const std = @import("std");

// Global debug flag - set by the runtime
var debug_enabled: bool = false;

pub fn setDebugEnabled(enabled: bool) void {
    debug_enabled = enabled;
}

pub fn log(comptime format: []const u8, args: anytype) void {
    if (debug_enabled) {
        std.debug.print("[DEBUG] " ++ format ++ "\n", args);
    }
}

pub fn logValue(value: @import("types.zig").Value) void {
    switch (value) {
        .Nil => log("Nil", .{}),
        .Bool => |b| log("Bool: {}", .{b}),
        .Int => |i| log("Int: {}", .{i}),
        .Float => |f| log("Float: {}", .{f}),
        .String => |s| log("String: \"{s}\"", .{s}),
        .Symbol => |s| log("Symbol: {s}", .{s}),
        .Array => |arr| log("Array of length {}", .{arr.len}),
        .Map => |map| log("Map with {} entries", .{map.count()}),
        .Function => |func| log("Function: {any}", .{func}),
        .ReturnAddress => |addr| log("ReturnAddress: {any}", .{addr}),
        .Variable => |var_val| log("Variable: {s}", .{var_val.name}),
        .BuiltinOperator => |op| log("BuiltinOperator: {any}", .{op}),
        .Class => |class| log("Class: {s}", .{class.name}),
        .Object => |obj_id| log("Object ID: {}", .{obj_id}),
        .CPtr => |ptr| if (ptr) |p| log("CPtr: {*}", .{p}) else log("CPtr: null", .{}),
        .CFunction => |func| log("CFunction: {*}", .{func}),
        .CStruct => |ptr| log("CStruct: {*}", .{ptr}),
        .CArray => |arr| log("CArray[{} x {}]", .{ arr.len, arr.element_size }),
    }
}

// Format a token for debugging
pub fn formatToken(token: @import("../frontend/parser.zig").Token) []const u8 {
    return switch (token.kind) {
        .LParen => "(",
        .RParen => ")",
        .LBracket => "[",
        .RBracket => "]",
        .Equals => "=",
        .Ident => |val| val,
        .String => |val| std.fmt.allocPrint(std.heap.page_allocator, "\"{s}\"", .{val}) catch "<string>",
        .Int => |val| std.fmt.allocPrint(std.heap.page_allocator, "{}", .{val}) catch "<int>",
        .Bool => |val| if (val) "true" else "false",
        .If => "if",
        .Else => "else",
        .Var => "var",
        .Fn => "fn",
    };
}
