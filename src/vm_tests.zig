const std = @import("std");
const vm = @import("vm.zig");
const bytecode = @import("bytecode.zig");

test "integer literal evaluates to correct value" {
    // Initialize VM
    var vm_instance = try vm.VM.init();
    defer vm_instance.deinit();

    // Create test function with LoadInt(1) and Return instructions
    const instructions = &.{
        bytecode.BytecodeInstr{
            .code = .{
                .LoadInt = .{ .value = 1 },
            },
        },
        bytecode.BytecodeInstr{
            .code = .Return,
        },
    };

    const test_func = bytecode.Function{
        .instructions = instructions,
        .allocator = std.testing.allocator,
    };

    // Execute the function
    const result = try vm_instance.runFunction(&test_func, std.io.getStdOut().writer());

    // Verify the result
    try std.testing.expectEqual(@as(i64, 1), result.int);
}
