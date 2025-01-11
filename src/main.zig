const std = @import("std");
pub const vm = @import("vm.zig");
pub const parser = @import("parser.zig");
pub const bytecode = @import("bytecode.zig");

pub fn main() !void {
    std.debug.print("DEBUG: Entering main function\n", .{});
    std.debug.print("Gene VM starting...\n", .{});
    std.debug.print("", .{}); // Force flush

    // Initialize VM
    var my_vm = vm.VM.init();

    // Create an allocator for parser
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var allocator = gpa.allocator();

    // Read input file and debug print arguments
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    std.debug.print("Arguments: {any}\n", .{args});

    if (args.len < 2) {
        std.debug.print("Usage: gene <file.gene>\n", .{});
        return error.InvalidArguments;
    }

    std.debug.print("Attempting to read file: {s}\n", .{args[1]});
    const input = std.fs.cwd().readFileAlloc(allocator, args[1], std.math.maxInt(usize)) catch |err| {
        std.debug.print("Failed to read file: {any}\n", .{err});
        return err;
    };
    defer allocator.free(input);

    std.debug.print("Successfully read file content:\n{s}\n", .{input});
    std.debug.print("", .{}); // Force flush
    std.debug.print("Input length: {d}\n", .{input.len});
    std.debug.print("First 10 bytes: {x}\n", .{input[0..@min(10, input.len)]});
    std.debug.print("Last 10 bytes: {x}\n", .{input[@max(0, input.len - 10)..input.len]});

    std.debug.print("\nStarting parsing...\n", .{});
    std.debug.print("", .{}); // Force flush
    const parsed = try parser.parseGeneSource(&allocator, input);
    defer allocator.free(parsed);

    std.debug.print("\nParsed {d} nodes:\n", .{parsed.len});
    std.debug.print("", .{}); // Force flush
    for (parsed) |node| {
        switch (node) {
            .Stmt => |stmt| switch (stmt) {
                .ExprStmt => |expr| switch (expr) {
                    .StrLit => |value| std.debug.print("  String literal: {s}\n", .{value}),
                    .Ident => |value| std.debug.print("  Identifier: {s}\n", .{value}),
                    else => std.debug.print("  Unknown expression type\n", .{}),
                },
                else => std.debug.print("  Unknown statement type\n", .{}),
            },
            else => std.debug.print("  Unknown node type\n", .{}),
        }
    }

    // Convert AST to bytecode
    std.debug.print("\nStarting bytecode generation...\n", .{});
    std.debug.print("", .{}); // Force flush
    std.debug.print("AST before bytecode generation:\n", .{});
    var idx: usize = 0;
    for (parsed) |node| {
        switch (node) {
            .Stmt => |stmt| switch (stmt) {
                .ExprStmt => |expr| switch (expr) {
                    .StrLit => |value| std.debug.print("  [{}] String literal: {s}\n", .{ idx, value }),
                    .Ident => |value| std.debug.print("  [{}] Identifier: {s}\n", .{ idx, value }),
                    else => std.debug.print("  [{}] Unknown expression type\n", .{idx}),
                },
                else => std.debug.print("  [{}] Unknown statement type\n", .{idx}),
            },
            else => std.debug.print("  [{}] Unknown node type\n", .{idx}),
        }
        idx += 1;
    }

    const module = try bytecode.lowerToBytecode(&allocator, parsed);

    std.debug.print("Generated {d} functions:\n", .{module.functions.len});
    for (module.functions) |func| {
        std.debug.print("  Function with {d} instructions:\n", .{func.instructions.len});
        for (func.instructions) |instr| {
            switch (instr.code) {
                .LoadString => |load| std.debug.print("    LOAD_STRING {s}\n", .{load.value}),
                .Print => std.debug.print("    PRINT\n", .{}),
            }
        }
    }

    if (module.functions.len == 0) {
        return error.NoFunctionsGenerated;
    }

    // Run the first function
    try my_vm.runFunction(module.functions[0]);

    std.debug.print("Program completed.\n", .{});
}
