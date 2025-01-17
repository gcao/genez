const std = @import("std");
pub const ast = @import("ast.zig");
pub const vm = @import("vm.zig");
pub const parser = @import("parser.zig");
pub const bytecode = @import("bytecode.zig");
pub const hir = @import("hir.zig");
pub const mir = @import("mir.zig");

fn astToHir(allocator: *std.mem.Allocator, nodes: []const ast.AstNode) !hir.HIR {
    return try hir.HIR.astToHir(allocator.*, nodes);
}

fn hirToMir(allocator: *std.mem.Allocator, hir_prog: hir.HIR) !mir.MIR {
    return try mir.MIR.hirToMir(allocator.*, hir_prog);
}

pub fn main() !void {
    std.debug.print("DEBUG: Entering main function\n", .{});
    std.debug.print("Gene VM starting...\n", .{});
    std.debug.print("", .{}); // Force flush

    // Initialize VM with error handling
    var my_vm = try vm.VM.init();
    defer my_vm.deinit();

    // Create an allocator for parser
    var gpa = std.heap.GeneralPurposeAllocator(.{
        .verbose_log = false,
        .safety = false,
        .never_unmap = false,
    }){};
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

    // Convert AST to HIR
    std.debug.print("\nLowering AST to HIR...\n", .{});
    var hir_prog = try astToHir(&allocator, parsed);
    defer hir_prog.deinit();

    // Convert HIR to MIR
    std.debug.print("\nLowering HIR to MIR...\n", .{});
    var mir_prog = try hirToMir(&allocator, hir_prog);
    defer mir_prog.deinit();

    // Convert MIR to bytecode
    std.debug.print("\nGenerating bytecode from MIR...\n", .{});
    var module = try bytecode.lowerToBytecode(&allocator, parsed);
    defer module.deinit();

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

    try my_vm.runModule(&module);
    std.debug.print("Program completed.\n", .{});
}
