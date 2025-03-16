const std = @import("std");
const runtime = @import("runtime.zig");

pub const VERSION = "0.1.0";

fn printHelp(writer: anytype) !void {
    try writer.print("Gene Programming Language v{s}\n", .{VERSION});
    try writer.print("\nUsage: gene <command> [options] [arguments]\n", .{});
    try writer.print("\nAvailable Commands:\n", .{});
    try writer.print("  run       Run a Gene source file\n", .{});
    try writer.print("  compile   Compile a Gene source file\n", .{});
    try writer.print("  help      Show this help message\n", .{});
    try writer.print("  version   Show the current version\n", .{});
    try writer.print("\nOptions:\n", .{});
    try writer.print("  --debug   Enable debug output\n", .{});
}

pub fn main() !void {
    std.debug.print("Entering main function\n", .{});
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        std.debug.print("Usage: gene <command> [args...]\n", .{});
        return;
    }

    const command = args[1];
    if (std.mem.eql(u8, command, "run")) {
        if (args.len < 3) {
            std.debug.print("Usage: gene run <file>\n", .{});
            return;
        }

        const file = args[2];
        var rt = runtime.Runtime.init(allocator, false, std.io.getStdOut().writer());
        defer rt.deinit();

        try rt.runFile(file);
    } else if (std.mem.eql(u8, command, "compile")) {
        if (args.len < 3) {
            std.debug.print("Usage: gene compile <file>\n", .{});
            return;
        }

        const file = args[2];
        var rt = runtime.Runtime.init(allocator, false, std.io.getStdOut().writer());
        defer rt.deinit();

        try rt.compileFile(file);
    } else {
        std.debug.print("Unknown command: {s}\n", .{command});
        return;
    }
}
