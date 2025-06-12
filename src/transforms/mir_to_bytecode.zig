const std = @import("std");
const mir = @import("../ir/mir.zig");
const bytecode = @import("../backend/bytecode.zig");
const types = @import("../core/types.zig");

pub const ConversionResult = struct {
    main_func: bytecode.Function,
    created_functions: std.ArrayList(*bytecode.Function),
    allocator: std.mem.Allocator,

    pub fn deinit(self: *ConversionResult) void {
        // Clean up created functions
        for (self.created_functions.items) |func| {
            func.deinit();
            self.allocator.destroy(func);
        }
        self.created_functions.deinit();

        // Clean up main function
        self.main_func.deinit();
    }
};

pub fn convert(allocator: std.mem.Allocator, mir_prog: *mir.MIR) !ConversionResult {
    var main_func = bytecode.Function.init(allocator);
    errdefer main_func.deinit();

    var created_functions = std.ArrayList(*bytecode.Function).init(allocator);
    errdefer {
        for (created_functions.items) |f| {
            f.deinit();
            allocator.destroy(f);
        }
        created_functions.deinit();
    }

    // Convert all MIR functions except the last one to separate bytecode functions
    // The last MIR function becomes the main function

    const num_functions = mir_prog.functions.items.len;
    if (num_functions == 0) {
        return ConversionResult{
            .main_func = main_func,
            .created_functions = created_functions,
            .allocator = allocator,
        };
    }

    // Convert user-defined functions (all except the last one)
    for (mir_prog.functions.items[0 .. num_functions - 1]) |*mir_func| {
        const bytecode_func = try convertMirFunction(allocator, mir_func);
        const func_ptr = try allocator.create(bytecode.Function);
        func_ptr.* = bytecode_func;
        try created_functions.append(func_ptr);
    }

    // Convert the main function (last MIR function)
    var next_reg: u16 = 0;
    var stack = StackTracker.init(allocator);
    defer stack.deinit();

    const main_mir_func = &mir_prog.functions.items[num_functions - 1];
    for (main_mir_func.blocks.items) |*block| {
        for (block.instructions.items) |*instr| {
            try convertInstructionWithStack(&main_func, instr, &created_functions, &next_reg, &stack);
        }
    }

    // Add a return instruction at the end of the main function if not present
    const needs_return = if (main_func.instructions.items.len == 0) true else switch (main_func.instructions.items[main_func.instructions.items.len - 1].op) {
        .Return => false,
        else => true,
    };

    if (needs_return) {
        try main_func.instructions.append(.{
            .op = bytecode.OpCode.Return,
            .immediate = null,
        });
    }

    return ConversionResult{
        .main_func = main_func,
        .created_functions = created_functions,
        .allocator = allocator,
    };
}

fn convertMirFunction(allocator: std.mem.Allocator, mir_func: *mir.MIR.Function) !bytecode.Function {
    var bytecode_func = bytecode.Function.init(allocator);
    errdefer bytecode_func.deinit();

    // Copy function metadata
    bytecode_func.name = try allocator.dupe(u8, mir_func.name);
    bytecode_func.param_count = mir_func.param_count;
    bytecode_func.register_count = 16; // Start with reasonable default

    // Initialize stack tracking for this function
    var next_reg: u16 = @as(u16, @intCast(mir_func.param_count)); // Start after parameters
    var stack = StackTracker.init(allocator);
    defer stack.deinit();

    // Since this is a user-defined function, we don't have access to created_functions here
    // We'll pass an empty list (user-defined functions shouldn't create more functions)
    var empty_functions = std.ArrayList(*bytecode.Function).init(allocator);
    defer empty_functions.deinit();

    // Convert all blocks
    for (mir_func.blocks.items) |*block| {
        for (block.instructions.items) |*instr| {
            try convertInstructionWithStack(&bytecode_func, instr, &empty_functions, &next_reg, &stack);
        }
    }

    // Add a return instruction if not present
    const needs_return = if (bytecode_func.instructions.items.len == 0) true else switch (bytecode_func.instructions.items[bytecode_func.instructions.items.len - 1].op) {
        .Return => false,
        else => true,
    };

    if (needs_return) {
        try bytecode_func.instructions.append(.{
            .op = bytecode.OpCode.Return,
            .immediate = null,
        });
    }

    return bytecode_func;
}

// Simple stack tracker to map MIR stack positions to registers
const StackTracker = struct {
    registers: std.ArrayList(u16),
    allocator: std.mem.Allocator,

    fn init(allocator: std.mem.Allocator) StackTracker {
        return StackTracker{
            .registers = std.ArrayList(u16).init(allocator),
            .allocator = allocator,
        };
    }

    fn deinit(self: *StackTracker) void {
        self.registers.deinit();
    }

    fn push(self: *StackTracker, reg: u16) !void {
        try self.registers.append(reg);
    }

    fn pop(self: *StackTracker) u16 {
        return self.registers.pop() orelse 0;
    }

    fn peek(self: *StackTracker, offset: usize) u16 {
        const idx = self.registers.items.len - 1 - offset;
        return self.registers.items[idx];
    }

    fn len(self: *StackTracker) usize {
        return self.registers.items.len;
    }
};

fn convertInstructionWithStack(func: *bytecode.Function, instr: *mir.MIR.Instruction, created_functions: *std.ArrayList(*bytecode.Function), next_reg: *u16, stack: *StackTracker) !void {
    switch (instr.*) {
        .LoadInt => |val| {
            const dst_reg = next_reg.*;
            next_reg.* += 1;
            try stack.push(dst_reg); // Track this value on the stack
            try func.instructions.append(.{
                .op = bytecode.OpCode.LoadConst,
                .dst = dst_reg,
                .immediate = types.Value{ .Int = val },
            });
        },
        .LoadFloat => |val| {
            const dst_reg = next_reg.*;
            next_reg.* += 1;
            try stack.push(dst_reg); // Track this value on the stack
            try func.instructions.append(.{
                .op = bytecode.OpCode.LoadConst,
                .dst = dst_reg,
                .immediate = types.Value{ .Float = val },
            });
        },
        .LoadBool => |val| {
            const dst_reg = next_reg.*;
            next_reg.* += 1;
            try stack.push(dst_reg); // Track this value on the stack
            try func.instructions.append(.{
                .op = bytecode.OpCode.LoadConst,
                .dst = dst_reg,
                .immediate = types.Value{ .Bool = val },
            });
        },
        .LoadString => |val| {
            // Duplicate the string for the bytecode operand
            const str_copy = try func.allocator.dupe(u8, val);
            errdefer func.allocator.free(str_copy); // Free if append fails
            const dst_reg = next_reg.*;
            next_reg.* += 1;
            try stack.push(dst_reg); // Track this value on the stack
            try func.instructions.append(.{
                .op = bytecode.OpCode.LoadConst,
                .dst = dst_reg,
                .immediate = types.Value{ .String = str_copy },
            });
            // No need to clear MIR instr if we're duplicating
            // instr.* = .LoadNil; // REMOVED
        },
        .LoadNil => {
            const dst_reg = next_reg.*;
            next_reg.* += 1;
            try stack.push(dst_reg); // Track this value on the stack
            try func.instructions.append(.{
                .op = bytecode.OpCode.LoadConst,
                .dst = dst_reg,
                .immediate = types.Value{ .Nil = {} },
            });
        },
        .LoadSymbol => |val| {
            // Duplicate the symbol for the bytecode operand
            const sym_copy = try func.allocator.dupe(u8, val);
            errdefer func.allocator.free(sym_copy); // Free if append fails
            const dst_reg = next_reg.*;
            next_reg.* += 1;
            try stack.push(dst_reg); // Track this value on the stack
            try func.instructions.append(.{
                .op = bytecode.OpCode.LoadConst,
                .dst = dst_reg,
                .immediate = types.Value{ .Symbol = sym_copy },
            });
            // No need to clear MIR instr if we're duplicating
            // instr.* = .LoadNil; // REMOVED
        },
        .LoadArray => |val| {
            // Take ownership of the array
            const dst_reg = next_reg.*;
            next_reg.* += 1;
            try stack.push(dst_reg); // Track this value on the stack
            try func.instructions.append(.{
                .op = bytecode.OpCode.LoadConst,
                .dst = dst_reg,
                .immediate = types.Value{ .Array = val },
            });
            // Clear the MIR instruction so it won't be freed
            instr.* = .LoadNil;
        },
        .LoadMap => |val| {
            // Take ownership of the map
            const dst_reg = next_reg.*;
            next_reg.* += 1;
            try stack.push(dst_reg); // Track this value on the stack
            try func.instructions.append(.{
                .op = bytecode.OpCode.LoadConst,
                .dst = dst_reg,
                .immediate = types.Value{ .Map = val },
            });
            // Clear the MIR instruction so it won't be freed
            instr.* = .LoadNil;
        },
        .LoadVariable => |val| {
            // Duplicate the variable name (symbol) for the bytecode operand
            const name_copy = try func.allocator.dupe(u8, val);
            errdefer func.allocator.free(name_copy); // Free if append fails
            const dst_reg = next_reg.*;
            next_reg.* += 1;
            try stack.push(dst_reg); // Track this value on the stack
            try func.instructions.append(.{
                .op = bytecode.OpCode.LoadVar,
                .dst = dst_reg,
                .var_name = name_copy,
            });
            // No need to clear MIR instr if we're duplicating
            // instr.* = .LoadNil; // REMOVED
        },
        .LoadParameter => |param_index| {
            // Load parameter from the stack frame
            const dst_reg = next_reg.*;
            next_reg.* += 1;
            try stack.push(dst_reg); // Track this value on the stack
            try func.instructions.append(.{
                .op = bytecode.OpCode.LoadParam,
                .dst = dst_reg,
                .immediate = types.Value{ .Int = @as(i64, @intCast(param_index)) },
            });
        },
        .Add => {
            if (stack.len() < 2) {
                std.debug.print("ERROR: Add needs 2 operands but only {} on stack\n", .{stack.len()});
                return error.OutOfMemory;
            }

            const right_reg = stack.pop();
            const left_reg = stack.pop();
            const dst_reg = next_reg.*;
            next_reg.* += 1;
            try stack.push(dst_reg);

            try func.instructions.append(.{
                .op = bytecode.OpCode.Add,
                .src1 = left_reg,
                .src2 = right_reg,
                .dst = dst_reg,
            });
        },
        .Sub => {
            if (stack.len() < 2) {
                std.debug.print("ERROR: Sub needs 2 operands but only {} on stack\n", .{stack.len()});
                return error.OutOfMemory;
            }

            const right_reg = stack.pop();
            const left_reg = stack.pop();
            const dst_reg = next_reg.*;
            next_reg.* += 1;
            try stack.push(dst_reg);

            try func.instructions.append(.{
                .op = bytecode.OpCode.Sub,
                .src1 = left_reg,
                .src2 = right_reg,
                .dst = dst_reg,
            });
        },
        .Mul => {
            // Use register-based instruction
            if (stack.len() < 2) {
                std.debug.print("ERROR: Mul needs 2 operands but only {} on stack\n", .{stack.len()});
                return error.OutOfMemory;
            }

            const right_reg = stack.pop();
            const left_reg = stack.pop();
            const dst_reg = next_reg.*;
            next_reg.* += 1;
            try stack.push(dst_reg);

            try func.instructions.append(.{
                .op = bytecode.OpCode.Mul,
                .src1 = left_reg,
                .src2 = right_reg,
                .dst = dst_reg,
            });
        },
        .Div => {
            // Use register-based instruction
            if (stack.len() < 2) {
                std.debug.print("ERROR: Div needs 2 operands but only {} on stack\n", .{stack.len()});
                return error.OutOfMemory;
            }

            const right_reg = stack.pop();
            const left_reg = stack.pop();
            const dst_reg = next_reg.*;
            next_reg.* += 1;
            try stack.push(dst_reg);

            try func.instructions.append(.{
                .op = bytecode.OpCode.Div,
                .src1 = left_reg,
                .src2 = right_reg,
                .dst = dst_reg,
            });
        },
        .LessThan => {
            // Use register-based instruction
            if (stack.len() < 2) {
                std.debug.print("ERROR: LessThan needs 2 operands but only {} on stack\n", .{stack.len()});
                return error.OutOfMemory;
            }

            const right_reg = stack.pop();
            const left_reg = stack.pop();
            const dst_reg = next_reg.*;
            next_reg.* += 1;
            try stack.push(dst_reg);

            try func.instructions.append(.{
                .op = bytecode.OpCode.Lt,
                .src1 = left_reg,
                .src2 = right_reg,
                .dst = dst_reg,
            });
        },
        .GreaterThan => {
            // Use register-based instruction
            if (stack.len() < 2) {
                std.debug.print("ERROR: GreaterThan needs 2 operands but only {} on stack\n", .{stack.len()});
                return error.OutOfMemory;
            }

            const right_reg = stack.pop();
            const left_reg = stack.pop();
            const dst_reg = next_reg.*;
            next_reg.* += 1;
            try stack.push(dst_reg);

            try func.instructions.append(.{
                .op = bytecode.OpCode.Gt,
                .src1 = left_reg,
                .src2 = right_reg,
                .dst = dst_reg,
            });
        },
        .Equal => {
            // Use register-based instruction
            if (stack.len() < 2) {
                std.debug.print("ERROR: Equal needs 2 operands but only {} on stack\n", .{stack.len()});
                return error.OutOfMemory;
            }

            const right_reg = stack.pop();
            const left_reg = stack.pop();
            const dst_reg = next_reg.*;
            next_reg.* += 1;
            try stack.push(dst_reg);

            try func.instructions.append(.{
                .op = bytecode.OpCode.Eq,
                .src1 = left_reg,
                .src2 = right_reg,
                .dst = dst_reg,
            });
        },
        .Jump => |target| try func.instructions.append(.{
            .op = bytecode.OpCode.Jump,
            .immediate = types.Value{ .Int = @as(i64, @intCast(target)) },
        }),
        .JumpIfFalse => |target| {
            if (stack.len() < 1) {
                std.debug.print("ERROR: JumpIfFalse needs a condition but stack is empty\n", .{});
                return error.OutOfMemory;
            }

            const condition_reg = stack.pop();

            try func.instructions.append(.{
                .op = bytecode.OpCode.JumpIfFalse,
                .src1 = condition_reg,
                .immediate = types.Value{ .Int = @as(i64, @intCast(target)) },
            });
        },
        .Call => |arg_count| {
            // Use stack tracker to find function and arguments
            // In MIR stack: [other, function, arg1, arg2, ..., argN] before call
            // We need to pop N args and then the function

            if (stack.len() < arg_count + 1) {
                std.debug.print("ERROR: Call with {} args but only {} items on stack\n", .{ arg_count, stack.len() });
                return error.OutOfMemory;
            }

            // Pop arguments from stack (they are on top)
            var arg_regs = std.ArrayList(u16).init(func.allocator);
            defer arg_regs.deinit();

            var i: usize = 0;
            while (i < arg_count) : (i += 1) {
                try arg_regs.insert(0, stack.pop()); // Insert at front to preserve order
            }

            // Now pop the function
            const func_reg = stack.pop();

            const dst_reg = next_reg.*;
            next_reg.* += 1;
            try stack.push(dst_reg); // Push result onto stack

            try func.instructions.append(.{
                .op = bytecode.OpCode.Call,
                .src1 = func_reg,
                .dst = dst_reg,
                .immediate = types.Value{ .Int = @as(i64, @intCast(arg_count)) },
            });
        },
        .Print => {
            // Use register-based instruction
            if (stack.len() < 1) {
                std.debug.print("ERROR: Print needs 1 operand but stack is empty\n", .{});
                return error.OutOfMemory;
            }

            const src_reg = stack.pop();

            try func.instructions.append(.{
                .op = bytecode.OpCode.Print,
                .src1 = src_reg,
            });
        },
        .Return => {
            // Use register-based instruction
            var return_reg: ?u16 = null;
            if (stack.len() > 0) {
                return_reg = stack.pop();
            }

            try func.instructions.append(.{
                .op = bytecode.OpCode.Return,
                .src1 = return_reg,
            });
        },
        .LoadFunction => |mir_func_ptr| {
            // Find the function by name in the already-created functions
            var func_index: ?usize = null;
            for (created_functions.items, 0..) |bytecode_func_ptr, i| {
                if (std.mem.eql(u8, bytecode_func_ptr.name, mir_func_ptr.name)) {
                    func_index = i;
                    break;
                }
            }

            if (func_index == null) {
                std.debug.print("ERROR: Function '{s}' not found in created functions\n", .{mir_func_ptr.name});
                return error.FunctionNotFound;
            }

            // Load the function by index
            const dst_reg = next_reg.*;
            next_reg.* += 1;
            try stack.push(dst_reg); // Track this value on the stack
            try func.instructions.append(.{
                .op = bytecode.OpCode.LoadConst,
                .dst = dst_reg,
                .immediate = types.Value{ .Function = created_functions.items[func_index.?] },
            });

            std.debug.print("INFO: Loading function '{s}' (index {}) with {} parameters\n", .{ mir_func_ptr.name, func_index.?, mir_func_ptr.param_count });
        },
        .StoreVariable => |name| {
            // Duplicate the variable name (string) for the bytecode operand
            const name_copy = try func.allocator.dupe(u8, name);
            errdefer func.allocator.free(name_copy); // Free if append fails

            if (stack.len() < 1) {
                std.debug.print("ERROR: StoreVariable needs a value but stack is empty\n", .{});
                return error.OutOfMemory;
            }

            const src_reg = stack.pop();

            try func.instructions.append(.{
                .op = bytecode.OpCode.StoreVar,
                .src1 = src_reg,
                .var_name = name_copy,
            });
            // No need to clear MIR instr if we're duplicating
            // instr.* = .LoadNil; // REMOVED
        },
    }
}
