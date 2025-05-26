const std = @import("std");

pub fn log(comptime format: []const u8, args: anytype) void {
    std.debug.print("[DEBUG] " ++ format ++ "\n", args);
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
    }
}

// Format a token for debugging
pub fn formatToken(token: @import("parser.zig").Token) []const u8 {
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
