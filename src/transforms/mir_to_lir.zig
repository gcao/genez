const std = @import("std");
const mir = @import("../ir/mir.zig");
const lir = @import("../ir/lir.zig");
const types = @import("../core/types.zig");

/// Convert MIR to LIR
/// This stage performs register allocation and converts to register-based instructions
pub fn convert(allocator: std.mem.Allocator, mir_program: *mir.MIR) !lir.LIR {
    var lir_program = lir.LIR.init(allocator);
    errdefer lir_program.deinit();

    // Convert each function
    for (mir_program.functions.items) |*mir_func| {
        const lir_func = try convertFunction(allocator, mir_func);
        try lir_program.functions.append(lir_func);
    }

    return lir_program;
}

fn convertFunction(allocator: std.mem.Allocator, mir_func: *mir.MIR.Function) !lir.LIR.Function {
    var lir_func = lir.LIR.Function.init(allocator);
    lir_func.name = try allocator.dupe(u8, mir_func.name);
    lir_func.param_count = mir_func.param_count;

    // Simple register allocation: each MIR value gets a unique register
    var register_allocator = RegisterAllocator.init(allocator);
    defer register_allocator.deinit();

    // Convert each basic block
    for (mir_func.blocks.items) |*block| {
        try convertBlock(allocator, &register_allocator, block, &lir_func);
    }

    lir_func.register_count = register_allocator.next_register;
    return lir_func;
}

fn convertBlock(allocator: std.mem.Allocator, reg_alloc: *RegisterAllocator, mir_block: *mir.MIR.Block, lir_func: *lir.LIR.Function) !void {
    // Convert each instruction in the block
    for (mir_block.instructions.items) |instr| {
        try convertInstruction(allocator, reg_alloc, instr, lir_func);
    }
}

fn convertInstruction(allocator: std.mem.Allocator, reg_alloc: *RegisterAllocator, mir_instr: mir.MIR.Instruction, lir_func: *lir.LIR.Function) !void {
    switch (mir_instr) {
        .LoadInt => |value| {
            const dest_reg = reg_alloc.allocateRegister();
            const lir_value = types.Value{ .Int = value };
            try lir_func.instructions.append(.{
                .LoadConst = .{ .dest = dest_reg, .val = lir_value },
            });
        },
        .LoadFloat => |value| {
            const dest_reg = reg_alloc.allocateRegister();
            const lir_value = types.Value{ .Float = value };
            try lir_func.instructions.append(.{
                .LoadConst = .{ .dest = dest_reg, .val = lir_value },
            });
        },
        .LoadBool => |value| {
            const dest_reg = reg_alloc.allocateRegister();
            const lir_value = types.Value{ .Bool = value };
            try lir_func.instructions.append(.{
                .LoadConst = .{ .dest = dest_reg, .val = lir_value },
            });
        },
        .LoadString => |value| {
            const dest_reg = reg_alloc.allocateRegister();
            const string_copy = try allocator.dupe(u8, value);
            const lir_value = types.Value{ .String = string_copy };
            try lir_func.instructions.append(.{
                .LoadConst = .{ .dest = dest_reg, .val = lir_value },
            });
        },
        .LoadNil => {
            const dest_reg = reg_alloc.allocateRegister();
            try lir_func.instructions.append(.{
                .LoadNil = .{ .dest = dest_reg },
            });
        },
        .LoadSymbol => |value| {
            const dest_reg = reg_alloc.allocateRegister();
            const symbol_copy = try allocator.dupe(u8, value);
            const lir_value = types.Value{ .Symbol = symbol_copy };
            try lir_func.instructions.append(.{
                .LoadConst = .{ .dest = dest_reg, .val = lir_value },
            });
        },
        .LoadVariable => |name| {
            // Load variable by name - this will be converted to LoadVar in bytecode
            const dest_reg = reg_alloc.allocateRegister();
            const name_copy = try allocator.dupe(u8, name);
            const lir_value = types.Value{ .Symbol = name_copy };
            // Use a special LoadVariable instruction that will map to LoadVar
            try lir_func.instructions.append(.{
                .LoadVariable = .{ .dest = dest_reg, .name = lir_value },
            });
        },
        .LoadParameter => |index| {
            // Parameters are stored in the first registers
            const dest_reg = reg_alloc.allocateRegister();
            const param_reg: lir.LIR.Reg = @intCast(index);
            try lir_func.instructions.append(.{
                .Move = .{ .dest = dest_reg, .src = param_reg },
            });
        },
        .Add => {
            // Pop two operands and push result
            const right_reg = reg_alloc.getLastRegister();
            const left_reg = reg_alloc.getSecondLastRegister();
            const dest_reg = reg_alloc.allocateRegister();
            try lir_func.instructions.append(.{
                .Add = .{ .dest = dest_reg, .left = left_reg, .right = right_reg },
            });
        },
        .Sub => {
            const right_reg = reg_alloc.getLastRegister();
            const left_reg = reg_alloc.getSecondLastRegister();
            const dest_reg = reg_alloc.allocateRegister();
            try lir_func.instructions.append(.{
                .Sub = .{ .dest = dest_reg, .left = left_reg, .right = right_reg },
            });
        },
        .Mul => {
            const right_reg = reg_alloc.getLastRegister();
            const left_reg = reg_alloc.getSecondLastRegister();
            const dest_reg = reg_alloc.allocateRegister();
            try lir_func.instructions.append(.{
                .Mul = .{ .dest = dest_reg, .left = left_reg, .right = right_reg },
            });
        },
        .Div => {
            const right_reg = reg_alloc.getLastRegister();
            const left_reg = reg_alloc.getSecondLastRegister();
            const dest_reg = reg_alloc.allocateRegister();
            try lir_func.instructions.append(.{
                .Div = .{ .dest = dest_reg, .left = left_reg, .right = right_reg },
            });
        },
        .LessThan => {
            const right_reg = reg_alloc.getLastRegister();
            const left_reg = reg_alloc.getSecondLastRegister();
            const dest_reg = reg_alloc.allocateRegister();
            try lir_func.instructions.append(.{
                .Lt = .{ .dest = dest_reg, .left = left_reg, .right = right_reg },
            });
        },
        .GreaterThan => {
            const right_reg = reg_alloc.getLastRegister();
            const left_reg = reg_alloc.getSecondLastRegister();
            const dest_reg = reg_alloc.allocateRegister();
            try lir_func.instructions.append(.{
                .Gt = .{ .dest = dest_reg, .left = left_reg, .right = right_reg },
            });
        },
        .Equal => {
            const right_reg = reg_alloc.getLastRegister();
            const left_reg = reg_alloc.getSecondLastRegister();
            const dest_reg = reg_alloc.allocateRegister();
            try lir_func.instructions.append(.{
                .Eq = .{ .dest = dest_reg, .left = left_reg, .right = right_reg },
            });
        },
        .Call => |arg_count| {
            // Function is on the stack, followed by arguments
            const func_reg = reg_alloc.getRegisterAt(reg_alloc.stack_depth - arg_count - 1);
            
            var args = std.ArrayList(lir.LIR.Reg).init(allocator);
            var i: usize = 0;
            while (i < arg_count) : (i += 1) {
                const arg_reg = reg_alloc.getRegisterAt(reg_alloc.stack_depth - arg_count + i);
                try args.append(arg_reg);
            }
            
            const dest_reg = reg_alloc.allocateRegister();
            try lir_func.instructions.append(.{
                .Call = .{ .dest = dest_reg, .func = func_reg, .args = args },
            });
        },
        .Return => {
            if (reg_alloc.stack_depth > 0) {
                const value_reg = reg_alloc.getLastRegister();
                try lir_func.instructions.append(.{
                    .Return = .{ .value = value_reg },
                });
            } else {
                try lir_func.instructions.append(.{
                    .Return = .{ .value = null },
                });
            }
        },
        else => {
            // Handle other instructions as needed
            // For now, we'll skip unsupported instructions
        },
    }
}

/// Simple register allocator for MIR to LIR conversion
const RegisterAllocator = struct {
    allocator: std.mem.Allocator,
    next_register: lir.LIR.Reg,
    stack_depth: usize, // Track virtual stack depth for conversions
    register_stack: std.ArrayList(lir.LIR.Reg), // Track register assignments

    fn init(allocator: std.mem.Allocator) RegisterAllocator {
        return RegisterAllocator{
            .allocator = allocator,
            .next_register = 0,
            .stack_depth = 0,
            .register_stack = std.ArrayList(lir.LIR.Reg).init(allocator),
        };
    }

    fn deinit(self: *RegisterAllocator) void {
        self.register_stack.deinit();
    }

    fn allocateRegister(self: *RegisterAllocator) lir.LIR.Reg {
        const reg = self.next_register;
        self.next_register += 1;
        self.stack_depth += 1;
        self.register_stack.append(reg) catch @panic("Failed to track register");
        return reg;
    }

    fn getLastRegister(self: *RegisterAllocator) lir.LIR.Reg {
        if (self.register_stack.items.len == 0) return 0;
        return self.register_stack.items[self.register_stack.items.len - 1];
    }

    fn getSecondLastRegister(self: *RegisterAllocator) lir.LIR.Reg {
        if (self.register_stack.items.len < 2) return 0;
        return self.register_stack.items[self.register_stack.items.len - 2];
    }

    fn getRegisterAt(self: *RegisterAllocator, index: usize) lir.LIR.Reg {
        if (index >= self.register_stack.items.len) return 0;
        return self.register_stack.items[index];
    }
};