const std = @import("std");

const DEBUG_MODE = true; // Or false, depending on default debug logging preference

pub fn log(comptime format: []const u8, args: anytype) void {
    if (DEBUG_MODE) {
        std.debug.print("[DEBUG] " ++ format ++ "\n", args);
    }
}

pub fn logValue(value: @import("types.zig").Value) void {
    if (DEBUG_MODE) {
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
