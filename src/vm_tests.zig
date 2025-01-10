const std = @import("std");
const main = @import("main.zig");
const vm = main.vm;
const bytecode = main.bytecode;

test "vm basic run function" {
    var my_vm = vm.VM.init();

    var instrs: [2]bytecode.BytecodeInstr = .{
        .{ .code = .{ .LoadInt = .{ .value = 10 } } },
        .{ .code = .Print },
    };
    const func = bytecode.Function{
        .instructions = instrs[0..],
    };
    try my_vm.runFunction(func);
    // This should print "VM Print: 10"
    // Possibly also test the stack behavior or other
    // instructions if you had them
}
