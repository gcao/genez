const std = @import("std");
const vm_mod = @import("../src/vm.zig");
const bc = @import("../src/bytecode.zig");

test "vm basic run function" {
    var my_vm = vm_mod.VM.init();

    var instrs: [2]bc.BytecodeInstr = .{
        .{ .code = .LoadInt = .{ .value = 10 } },
        .{ .code = .Print },
    };
    const func = bc.BytecodeFunction{
        .instructions = instrs[0..],
    };
    try my_vm.runFunction(func);
    // This should print "VM Print: 10"
    // Possibly also test the stack behavior or other
    // instructions if you had them
}
