const std = @import("std");
const bc = @import("bytecode.zig");

pub fn VM() type {
    return struct {
        // Example: a stack, registers, etc.
        stack: [256]i64, // naive stack
        sp: usize,

        pub fn init() VM {
            return VM{
                .stack = [_]i64{0} ** 256,
                .sp = 0,
            };
        }

        pub fn runFunction(self: *VM, func: bc.BytecodeFunction) !void {
            var ip: usize = 0;
            while (ip < func.instructions.len) {
                const instr = func.instructions[ip];
                switch (instr.code) {
                    .LoadInt => |li| {
                        self.stack[self.sp] = li.value;
                        self.sp += 1;
                    },
                    .AddInt => {
                        const b = self.stack[self.sp - 1];
                        const a = self.stack[self.sp - 2];
                        self.sp -= 2;
                        self.stack[self.sp] = a + b;
                        self.sp += 1;
                    },
                    .Print => {
                        const top = self.stack[self.sp - 1];
                        self.sp -= 1;
                        std.debug.print("VM Print: {d}\n", .{top});
                    },
                }
                ip += 1;
            }
        }
    };
}
