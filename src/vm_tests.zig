const std = @import("std");
const main = @import("main.zig");
const vm = main.vm;
const bytecode = main.bytecode;

test "vm basic run function" {
    var my_vm = try vm.VM.init();

    var instrs: [2]bytecode.BytecodeInstr = .{
        .{ .code = .{ .LoadInt = .{ .value = 10 } } },
        .{ .code = .Print },
    };
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const func = bytecode.Function{
        .instructions = try arena.allocator().dupe(bytecode.BytecodeInstr, instrs[0..]),
        .allocator = arena.allocator(),
    };
    try my_vm.runFunction(&func);
    // This should print "VM Print: 10"
    // Possibly also test the stack behavior or other
    // instructions if you had them
}
