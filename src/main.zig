const std = @import("std");
const runtime = @import("runtime.zig");

pub const VERSION = "0.1.0";

fn printHelp(writer: anytype) !void {
    try writer.print("Gene Programming Language CLI v{s}\n", .{VERSION});
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
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();

    // Skip program name
    _ = args.next();

    // Check for command
    const command = args.next() orelse {
        try printHelp(std.io.getStdOut().writer());
        return;
    };

    if (std.mem.eql(u8, command, "run")) {
        var debug_mode = false;

        // Check for --debug flag
        const maybe_flag = args.next();
        if (maybe_flag) |flag| {
            if (std.mem.eql(u8, flag, "--debug")) {
                debug_mode = true;
            } else {
                // If it's not --debug, treat it as the filename
                const rt = runtime.Runtime.init(allocator, debug_mode);
                try rt.runFile(flag);
                return;
            }
        }

        // Get filename after --debug
        const filename = args.next() orelse {
            std.debug.print("Error: No input file specified\n", .{});
            return error.NoInputFile;
        };

        const rt = runtime.Runtime.init(allocator, debug_mode);
        try rt.runFile(filename);
    } else if (std.mem.eql(u8, command, "compile")) {
        if (args.next()) |filename| {
            const rt = runtime.Runtime.init(allocator, false);
            try rt.compileFile(filename);
        } else {
            std.debug.print("Error: No file specified\n", .{});
            return error.NoFileSpecified;
        }
    } else if (std.mem.eql(u8, command, "help")) {
        try printHelp(std.io.getStdOut().writer());
    } else if (std.mem.eql(u8, command, "version")) {
        try std.io.getStdOut().writer().print("Gene v{s}\n", .{VERSION});
    } else {
        std.debug.print("Unknown command: {s}\n", .{command});
        return error.UnknownCommand;
    }
}
