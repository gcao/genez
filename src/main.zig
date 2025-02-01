const std = @import("std");
pub const ast = @import("ast.zig");
pub const vm = @import("vm.zig");
pub const parser = @import("parser.zig");
pub const bytecode = @import("bytecode.zig");
pub const hir = @import("hir.zig");
pub const mir = @import("mir.zig");

const VERSION = "0.1.0";

var debug_enabled = false;

fn debugPrint(comptime format: []const u8, args: anytype) void {
    if (debug_enabled) {
        std.debug.print("[DEBUG] ", .{});
        std.debug.print(format, args);
    }
}

fn debugSection(comptime title: []const u8) void {
    if (debug_enabled) {
        std.debug.print("\n[DEBUG] === {s} ===\n", .{title});
    }
}

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
    try writer.print("\nUsage: gene <command> [options] [arguments]\n", .{});
    try writer.print("\nAvailable Commands:\n", .{});
    try writer.print("  run       Run a Gene source file\n", .{});
    try writer.print("  compile   Compile a Gene source file\n", .{});
    try writer.print("  help      Show this help message\n", .{});
    try writer.print("  version   Show the current version\n", .{});
    try writer.print("\nOptions:\n", .{});
    try writer.print("  --debug   Enable debug output\n", .{});
}

fn astToHir(allocator: *std.mem.Allocator, nodes: []const ast.AstNode) !hir.HIR {
    return try hir.HIR.astToHir(allocator.*, nodes);
}

fn hirToMir(allocator: *std.mem.Allocator, hir_prog: hir.HIR) !mir.MIR {
    return try mir.MIR.hirToMir(allocator.*, hir_prog);
}

fn runFile(allocator: *std.mem.Allocator, file_path: []const u8) !u8 {
    // Initialize VM
    debugPrint("Initializing virtual machine...\n", .{});
    var my_vm = try vm.VM.init();
    debugPrint("Virtual machine initialized\n", .{});
    defer my_vm.deinit();

    // Read input file
    debugPrint("Reading source file...\n", .{});
    const input = std.fs.cwd().readFileAlloc(allocator.*, file_path, std.math.maxInt(usize)) catch |err| {
        std.debug.print("Failed to read file: {any}\n", .{err});
        return err;
    };
    debugPrint("Source file loaded successfully\n", .{});
    defer allocator.free(input);

    // Parse source
    debugSection("Compilation");
    debugPrint("Parsing source code...\n", .{});
    const parsed = parser.parseGeneSource(allocator, input) catch |err| {
        std.debug.print("Parser error: {s}\n", .{@errorName(err)});
        return err;
    };
    defer allocator.free(parsed);

    // Convert to bytecode
    debugPrint("Generating bytecode...\n", .{});
    var module = try bytecode.lowerToBytecode(allocator, parsed);
    debugPrint("Bytecode generation complete\n", .{});
    defer module.deinit();

    // Run the module and print the result
    debugSection("Execution");
    debugPrint("Running bytecode...\n", .{});
    const result = try my_vm.runModule(&module, std.io.getStdOut().writer());
    debugPrint("Execution complete\n", .{});
    
    if (debug_enabled) {
        switch (result) {
            .int => |value| {
                if (value != 0) {
                    try std.io.getStdOut().writer().print("\nResult: {d}\n", .{value});
                }
                debugPrint("Return value: {d}\n", .{value});
            },
            .string => |value| {
                try std.io.getStdOut().writer().print("\nResult: {s}\n", .{value});
                debugPrint("Return value: {s}\n", .{value});
            },
        }
    }

    // Return 0 for successful execution
    return 0;
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

pub fn main() !u8 {
    if (debug_enabled) {
        debugSection("Gene Language Interpreter");
    }
    
    debugPrint("Creating memory allocator...\n", .{});
    var gpa = std.heap.GeneralPurposeAllocator(.{
        .verbose_log = false,
        .safety = false,
        .never_unmap = false,
    }){};
    var allocator = gpa.allocator();
    defer _ = gpa.deinit();

    // Parse command-line arguments
    debugSection("Command Processing");
    debugPrint("Parsing command-line arguments...\n", .{});
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (debug_enabled) {
        debugPrint("Arguments received:\n", .{});
        for (args, 0..) |arg, i| {
            debugPrint("  [{d}] {s}\n", .{ i, arg });
        }
    }

    // Check for debug flag
    var file_arg_index: usize = 2;
    if (args.len > 2 and std.mem.eql(u8, args[2], "--debug")) {
        debug_enabled = true;
        file_arg_index = 3;
    }

    // Determine command
    const command: Command = blk: {
        if (args.len < 2) {
            debugPrint("No command specified, showing help\n", .{});
            break :blk .help;
        }

        const cmd_str = args[1];
        debugPrint("Processing command: {s}\n", .{cmd_str});

        if (std.mem.eql(u8, cmd_str, "run")) {
            break :blk .run;
        }
        if (std.mem.eql(u8, cmd_str, "compile")) {
            break :blk .compile;
        }
        if (std.mem.eql(u8, cmd_str, "help")) {
            break :blk .help;
        }
        if (std.mem.eql(u8, cmd_str, "version")) {
            break :blk .version;
        }

        debugPrint("Unknown command encountered\n", .{});
        break :blk .help;
    };

    // Execute command
    return switch (command) {
        .run => {
            if (args.len <= file_arg_index) {
                std.debug.print("Error: Please provide a file to run.\n", .{});
                try printHelp(std.io.getStdErr().writer());
                return 1;
            }
            debugSection("File Execution");
            debugPrint("Executing file: {s}\n", .{args[file_arg_index]});
            return try runFile(&allocator, args[file_arg_index]);
        },
        .compile => {
            if (args.len < 3) {
                std.debug.print("Error: Please provide a file to compile.\n", .{});
                try printHelp(std.io.getStdErr().writer());
                return 1;
            }
            try compileFile(&allocator, args[2]);
            return 0;
        },
        .help => {
            try printHelp(std.io.getStdOut().writer());
            return 0;
        },
        .version => {
            std.debug.print("Gene Programming Language v{s}\n", .{VERSION});
            return 0;
        },
    };
}
