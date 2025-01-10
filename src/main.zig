const std = @import("std");
pub const parser = @import("parser.zig");
pub const ast = @import("ast.zig");
pub const typechecker = @import("typechecker.zig");
pub const bytecode = @import("bytecode.zig");
pub const vm = @import("vm.zig");
pub const builtin = @import("builtin");

const wasm = struct {
    extern "env" fn log(n: i32) void;
    extern "env" fn log_str(ptr: [*]const u8, len: usize) void;
};

// WASM exports
pub export fn initInterpreter() void {
    wasm.log(42); // Test logging
}

pub export fn runGeneCode(code_ptr: [*:0]const u8, code_len: usize) void {
    wasm.log_str(code_ptr, code_len);
    // TODO: Implement actual code running
}

// Native entry point
pub fn main() !void {
    if (comptime builtin.target.cpu.arch == .wasm32) {
        return; // Exit early for WASM builds
    }

    // Only import these for non-WASM builds
    const process = std.process;
    const fs = std.fs;

    // Get allocator pointer instead of the allocator itself
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var allocator = gpa.allocator();

    // parse arguments
    const args = try process.argsAlloc(allocator);
    defer process.argsFree(allocator, args);

    if (args.len < 2) {
        std.debug.print("Usage: {s} <file.gene>\n", .{args[0]});
        return;
    }

    const file_name = args[1];
    const source = try fs.cwd().readFileAlloc(allocator, file_name, 4096);
    defer allocator.free(source);

    try runSource(&allocator, source);
}

// Separate function to handle the source code processing
fn runSource(allocator: *std.mem.Allocator, source: []const u8) !void {
    // 1) parse
    const ast_nodes = try parser.parseGeneSource(allocator, source);

    // 2) typecheck
    try typechecker.checkProgram(ast_nodes);

    // 3) to bytecode
    const module = try bytecode.lowerToBytecode(allocator);

    // 4) create VM and run
    var my_vm = vm.VM.init();
    if (module.functions.len > 0) {
        try my_vm.runFunction(module.functions[0]);
    }

    std.debug.print("Done.\n", .{});
}
