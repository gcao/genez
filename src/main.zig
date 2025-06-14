const std = @import("std");
const runtime = @import("runtime.zig");
const data_parser = @import("data_parser.zig");

pub const VERSION = "0.1.0";

fn printHelp(writer: anytype) !void {
    try writer.print("Gene Programming Language v{s}\n", .{VERSION});
    try writer.print("\nUsage: gene <command> [options] [arguments]\n", .{});
    try writer.print("\nAvailable Commands:\n", .{});
    try writer.print("  run       Run a Gene source file\n", .{});
    try writer.print("  compile   Compile a Gene source file\n", .{});
    try writer.print("  repl      Start the Gene REPL\n", .{});
    try writer.print("  eval      Evaluate a Gene expression string\n", .{});
    try writer.print("  parse     Parse a Gene data file\n", .{});
    try writer.print("  help      Show this help message\n", .{});
    try writer.print("  version   Show the current version\n", .{});
    try writer.print("\nOptions:\n", .{});
    try writer.print("  --debug   Enable debug output\n", .{});
}

pub fn main() !void {
    // Revert back to GeneralPurposeAllocator
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        std.debug.print("Usage: gene <command> [args...]\n", .{});
        return;
    }

    // Check for debug flag
    var debug_mode = false;
    for (args) |arg| {
        if (std.mem.eql(u8, arg, "--debug")) {
            debug_mode = true;
            break;
        }
    }

    const command = args[1];
    if (std.mem.eql(u8, command, "run")) {
        if (args.len < 3) {
            std.debug.print("Usage: gene run <file> [--debug]\n", .{});
            return;
        }

        const file = args[2];
        var rt = runtime.Runtime.init(allocator, debug_mode, std.io.getStdOut().writer());
        defer rt.deinit();

        try rt.runFile(file);
    } else if (std.mem.eql(u8, command, "compile")) {
        if (args.len < 3) {
            std.debug.print("Usage: gene compile <file> [--debug]\n", .{});
            return;
        }

        const file = args[2];
        var rt = runtime.Runtime.init(allocator, debug_mode, std.io.getStdOut().writer());
        defer rt.deinit();

        try rt.compileFile(file);
    } else if (std.mem.eql(u8, command, "eval")) {
        if (args.len < 3) {
            std.debug.print("Usage: gene eval <source> [--debug]\n", .{});
            return;
        }

        const source = args[2];
        var rt = runtime.Runtime.init(allocator, debug_mode, std.io.getStdOut().writer());
        defer rt.deinit();

        try rt.eval(source);
    } else if (std.mem.eql(u8, command, "repl")) {
        var rt = runtime.Runtime.init(allocator, debug_mode, std.io.getStdOut().writer());
        defer rt.deinit();

        try rt.runRepl();
    } else if (std.mem.eql(u8, command, "help")) {
        try printHelp(std.io.getStdOut().writer());
    } else if (std.mem.eql(u8, command, "version")) {
        std.debug.print("Gene Programming Language v{s}\n", .{VERSION});
    } else if (std.mem.eql(u8, command, "parse")) {
        if (args.len < 3) {
            std.debug.print("Usage: gene parse <file> [--json|--raw|--parsed]\n", .{});
            std.debug.print("  --json    Output as JSON\n", .{});
            std.debug.print("  --raw     Output raw Gene syntax (default)\n", .{});
            std.debug.print("  --parsed  Show internal parsed data structure\n", .{});
            return;
        }

        const file = args[2];
        
        // Parse output format flags
        const OutputFormat = enum { raw, json, parsed };
        var output_format = OutputFormat.raw;
        
        for (args) |arg| {
            if (std.mem.eql(u8, arg, "--json")) {
                output_format = .json;
            } else if (std.mem.eql(u8, arg, "--parsed")) {
                output_format = .parsed;
            } else if (std.mem.eql(u8, arg, "--raw")) {
                output_format = .raw;
            }
        }
        
        // Handle stdin
        const stdout = std.io.getStdOut().writer();
        if (std.mem.eql(u8, file, "-")) {
            // Read from stdin
            const stdin = std.io.getStdIn();
            const source = try stdin.readToEndAlloc(allocator, std.math.maxInt(usize));
            defer allocator.free(source);
            
            const doc = data_parser.parseDocument(allocator, source) catch |err| {
                if (err == error.GeneHeadMustBeSymbol) {
                    std.debug.print("\nHint: For Gene code files, use 'gene run -'\n", .{});
                    std.debug.print("      For Gene data files, use 'gene parse -'\n", .{});
                    std.debug.print("\nExample data file: examples/data_example.gene\n", .{});
                }
                return err;
            };
            defer {
                var doc_mut = doc;
                doc_mut.deinit();
            }
            
            switch (output_format) {
                .json => {
                    try stdout.print("[", .{});
                    for (doc.expressions, 0..) |expr, i| {
                        if (i > 0) try stdout.print(",", .{});
                        try data_parser.toJson(stdout, expr);
                    }
                    try stdout.print("]\n", .{});
                },
                .raw => {
                    for (doc.expressions) |expr| {
                        try data_parser.printDataValue(stdout, expr, 0);
                        try stdout.print("\n", .{});
                    }
                },
                .parsed => {
                    try stdout.print("=== Parsed Data Structure ===\n", .{});
                    try stdout.print("Document with {} expression(s):\n\n", .{doc.expressions.len});
                    for (doc.expressions, 0..) |expr, i| {
                        try stdout.print("Expression [{}]:\n", .{i});
                        try data_parser.printParsedData(stdout, expr, 2);
                        if (i < doc.expressions.len - 1) {
                            try stdout.print("\n", .{});
                        }
                    }
                },
            }
        } else {
            // Read from file
            const source_file = try std.fs.cwd().openFile(file, .{});
            defer source_file.close();
            
            const source = try source_file.readToEndAlloc(allocator, std.math.maxInt(usize));
            defer allocator.free(source);
            
            const doc = data_parser.parseDocument(allocator, source) catch |err| {
                if (err == error.GeneHeadMustBeSymbol) {
                    std.debug.print("\nHint: For Gene code files, use 'gene run {s}'\n", .{file});
                    std.debug.print("      For Gene data files, use 'gene parse {s}'\n", .{file});
                    std.debug.print("\nExample data file: examples/data_example.gene\n", .{});
                }
                return err;
            };
            defer {
                var doc_mut = doc;
                doc_mut.deinit();
            }
            
            switch (output_format) {
                .json => {
                    try stdout.print("[", .{});
                    for (doc.expressions, 0..) |expr, i| {
                        if (i > 0) try stdout.print(",", .{});
                        try data_parser.toJson(stdout, expr);
                    }
                    try stdout.print("]\n", .{});
                },
                .raw => {
                    for (doc.expressions) |expr| {
                        try data_parser.printDataValue(stdout, expr, 0);
                        try stdout.print("\n", .{});
                    }
                },
                .parsed => {
                    try stdout.print("=== Parsed Data Structure ===\n", .{});
                    try stdout.print("Document with {} expression(s):\n\n", .{doc.expressions.len});
                    for (doc.expressions, 0..) |expr, i| {
                        try stdout.print("Expression [{}]:\n", .{i});
                        try data_parser.printParsedData(stdout, expr, 2);
                        if (i < doc.expressions.len - 1) {
                            try stdout.print("\n", .{});
                        }
                    }
                },
            }
        }
    } else {
        std.debug.print("Unknown command: {s}\n", .{command});
        return;
    }
}
