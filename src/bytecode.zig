pub const std = @import("std");

pub const OpCode = union(enum) {
    LoadInt: LoadIntOp,
    AddInt,
    Print,
    // etc. specialized ops
};

pub const LoadIntOp = struct {
    value: i64,
};

pub const BytecodeInstr = struct {
    code: OpCode,
};

pub const BytecodeFunction = struct {
    instructions: []BytecodeInstr,
};

pub const BytecodeModule = struct {
    functions: []BytecodeFunction,
};

pub fn lowerToBytecode(allocator: *std.mem.Allocator, /* typedProg or something */) !BytecodeModule {
    // In a real scenario, you'd traverse the typed AST,
    // produce instructions. We'll do a dummy function:
    var func_mem = try allocator.alloc(BytecodeInstr, 2);

    func_mem[0] = .{ .code = .LoadInt = .{ .value = 42 } };
    func_mem[1] = .{ .code = .Print };

    var fn_mem = try allocator.alloc(BytecodeFunction, 1);
    fn_mem[0] = BytecodeFunction{
        .instructions = func_mem,
    };

    return BytecodeModule{
        .functions = fn_mem,
    };
}
