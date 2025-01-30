const std = @import("std");
pub const ast = @import("ast.zig");
pub const vm = @import("vm.zig");
pub const parser = @import("parser.zig");
pub const bytecode = @import("bytecode.zig");
pub const hir = @import("hir.zig");
pub const mir = @import("mir.zig");

const VERSION = "0.1.0";

const Command = enum {
    run,
    compile,
    help,
    version,
};

const CommandInfo = struct {
    description: []const u8,
};

const CommandMap = std.StringHashMap(CommandInfo);

fn printHelp(writer: anytype) !void {
    try writer.print("Gene Programming Language CLI v{s}\n", .{VERSION});
    try writer.print("\nUsage: gene <command> [options]\n", .{});
    try writer.print("\nAvailable Commands:\n", .{});
    try writer.print("  run       Run a Gene source file\n", .{});
    try writer.print("  compile   Compile a Gene source file\n", .{});
    try writer.print("  help      Show this help message\n", .{});
    try writer.print("  version   Show the current version\n", .{});
}

fn astToHir(allocator: *std.mem.Allocator, nodes: []const ast.AstNode) !hir.HIR {
    return try hir.HIR.astToHir(allocator.*, nodes);
}

fn hirToMir(allocator: *std.mem.Allocator, hir_prog: hir.HIR) !mir.MIR {
    return try mir.MIR.hirToMir(allocator.*, hir_prog);
}

fn runFile(allocator: *std.mem.Allocator, file_path: []const u8) !void {
    // Initialize VM
    std.debug.print("1. Initializing VM...\n", .{});
    var my_vm = try vm.VM.init();
    std.debug.print("2. VM initialized successfully\n", .{});
    defer my_vm.deinit();

    // Read input file
    std.debug.print("3. Reading input file...\n", .{});
    const input = std.fs.cwd().readFileAlloc(allocator.*, file_path, std.math.maxInt(usize)) catch |err| {
        std.debug.print("Failed to read file: {any}\n", .{err});
        return err;
    };
    std.debug.print("4. File read successfully\n", .{});
    defer allocator.free(input);

    // Parse source
    const parsed = parser.parseGeneSource(allocator, input) catch |err| {
        std.debug.print("Parser error: {s}\n", .{@errorName(err)});
        return;
    };
    defer allocator.free(parsed);

    // Convert to bytecode
    var module = try bytecode.lowerToBytecode(allocator, parsed);
    defer module.deinit();

    // Run the module
    try my_vm.runModule(&module, std.io.getStdOut().writer());
}

fn compileFile(allocator: *std.mem.Allocator, file_path: []const u8) !void {
    // Read input file
    const input = std.fs.cwd().readFileAlloc(allocator.*, file_path, std.math.maxInt(usize)) catch |err| {
        std.debug.print("Failed to read file: {any}\n", .{err});
        return err;
    };
    defer allocator.free(input);

    // Parse source
    const parsed = try parser.parseGeneSource(allocator, input);
    defer allocator.free(parsed);

    // Convert to bytecode
    var module = try bytecode.lowerToBytecode(allocator, parsed);
    defer module.deinit();

    std.debug.print("Compilation successful. Generated {d} functions.\n", .{module.functions.len});
}

pub fn main() !void {
    std.debug.print("1. Entering main function\n", .{});

    // Create an allocator
    std.debug.print("2. Creating GPA allocator\n", .{});
    var gpa = std.heap.GeneralPurposeAllocator(.{
        .verbose_log = false,
        .safety = false,
        .never_unmap = false,
    }){};
    std.debug.print("3. GPA allocator created\n", .{});
    var allocator = gpa.allocator();
    defer _ = gpa.deinit();

    // Parse command-line arguments
    std.debug.print("4. Parsing command-line arguments\n", .{});
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    std.debug.print("5. Received {} arguments:\n", .{args.len});
    for (args, 0..) |arg, i| {
        std.debug.print("  args[{}] = {s}\n", .{ i, arg });
    }

    // Determine command
    const command: Command = blk: {
        if (args.len < 2) {
            std.debug.print("6. No command specified, showing help\n", .{});
            break :blk .help;
        }

        const cmd_str = args[1];
        std.debug.print("7. Parsing command: {s}\n", .{cmd_str});

        if (std.mem.eql(u8, cmd_str, "run")) {
            std.debug.print("8. Command is 'run'\n", .{});
            break :blk .run;
        }
        if (std.mem.eql(u8, cmd_str, "compile")) {
            std.debug.print("8. Command is 'compile'\n", .{});
            break :blk .compile;
        }
        if (std.mem.eql(u8, cmd_str, "help")) {
            std.debug.print("8. Command is 'help'\n", .{});
            break :blk .help;
        }
        if (std.mem.eql(u8, cmd_str, "version")) {
            std.debug.print("8. Command is 'version'\n", .{});
            break :blk .version;
        }

        std.debug.print("9. Unknown command: {s}\n", .{cmd_str});
        break :blk .help;
    };

    // Execute command
    switch (command) {
        .run => {
            if (args.len < 3) {
                std.debug.print("Error: Please provide a file to run.\n", .{});
                try printHelp(std.io.getStdErr().writer());
                return error.InvalidArguments;
            }
            std.debug.print("Running file: {s}\n", .{args[2]});
            try runFile(&allocator, args[2]);
        },
        .compile => {
            if (args.len < 3) {
                std.debug.print("Error: Please provide a file to compile.\n", .{});
                try printHelp(std.io.getStdErr().writer());
                return error.InvalidArguments;
            }
            try compileFile(&allocator, args[2]);
        },
        .help => try printHelp(std.io.getStdOut().writer()),
        .version => std.debug.print("Gene Programming Language v{s}\n", .{VERSION}),
    }
}
