const std = @import("std");

pub fn log(comptime format: []const u8, args: anytype) void {
    if (@import("build_options").debug_mode) {
        std.debug.print("[DEBUG] " ++ format ++ "\n", args);
    }
}

pub fn logValue(value: @import("types.zig").Value) void {
    if (@import("build_options").debug_mode) {
        switch (value) {
            .Nil => log("Nil", .{}),
            .Bool => |b| log("Bool: {}", .{b}),
            .Int => |i| log("Int: {}", .{i}),
            .Float => |f| log("Float: {}", .{f}),
            .String => |s| log("String: {s}", .{s}),
            .Symbol => |s| log("Symbol: {s}", .{s}),
            .Array => |arr| log("Array of length {}", .{arr.len}),
            .Map => |map| log("Map with {} entries", .{map.count()}),
        }
    }
}
