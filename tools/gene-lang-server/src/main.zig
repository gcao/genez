const std = @import("std");
const lsp = @import("lsp.zig");
const Server = @import("server.zig").Server;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Parse command line arguments
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    // Check if --stdio flag is present (VS Code uses this)
    var use_stdio = false;
    for (args) |arg| {
        if (std.mem.eql(u8, arg, "--stdio")) {
            use_stdio = true;
            break;
        }
    }

    if (!use_stdio) {
        std.debug.print("Gene Language Server\n", .{});
        std.debug.print("Usage: gene-lang-server --stdio\n", .{});
        return;
    }

    // Initialize the language server
    var server = try Server.init(allocator);
    defer server.deinit();

    // Run the server on stdio
    try server.run();
}