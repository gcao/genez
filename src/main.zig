const std = @import("std");
const parser = @import("parser.zig");
const typechecker = @import("typechecker.zig");
const bytecode = @import("bytecode.zig");
const vm_mod = @import("vm.zig");

// main.zig
pub export fn initInterpreter() void {
    // Possibly create global data, or do some init
}

pub export fn runGeneCode(code_ptr: [*:0]const u8, code_len: usize) void {
    // “Interpret” the code string from JavaScript
    // parse -> typecheck -> bytecode -> run
}

pub fn main() !void {
    var alloc = std.heap.page_allocator;

    // parse arguments
    const args = std.process.argsAlloc(alloc) catch return;
    defer std.process.argsFree(alloc, args) catch {};

    if (args.len < 2) {
        std.debug.print("Usage: {s} <file.gene>\n", .{args[0]});
        return;
    }

    const file_name = args[1];
    const source = try std.fs.cwd().readFileAlloc(alloc, file_name, 4096);
    defer alloc.free(source);

    // 1) parse
    const ast_nodes = try parser.parseGeneSource(alloc, source);

    // 2) typecheck
    try typechecker.checkProgram(ast_nodes);

    // 3) to bytecode
    const module = try bytecode.lowerToBytecode(alloc);

    // 4) create VM and run
    var my_vm = vm_mod.VM.init();
    if (module.functions.len > 0) {
        try my_vm.runFunction(module.functions[0]);
    }

    std.debug.print("Done.\n", .{});
}
