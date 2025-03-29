const std = @import("std");
const mir = @import("mir.zig");
const bytecode = @import("bytecode.zig");
const types = @import("types.zig");

pub fn convert(allocator: std.mem.Allocator, mir_prog: *mir.MIR) !bytecode.Function {
    var func = bytecode.Function.init(allocator);
    errdefer func.deinit();

    // Convert each MIR function to bytecode
    for (mir_prog.functions.items) |*mir_func| {
        // Convert each block to bytecode instructions
        for (mir_func.blocks.items) |*block| {
            // Convert each instruction to bytecode
            for (block.instructions.items) |*instr| {
                try convertInstruction(&func, instr);
            }
        }
    }

    return func;
}

fn convertInstruction(func: *bytecode.Function, instr: *mir.MIR.Instruction) !void {
    switch (instr.*) {
        .LoadInt => |val| try func.instructions.append(.{
            .op = bytecode.OpCode.LoadConst,
            .operand = types.Value{ .Int = val },
        }),
        .LoadFloat => |val| try func.instructions.append(.{
            .op = bytecode.OpCode.LoadConst,
            .operand = types.Value{ .Float = val },
        }),
        .LoadBool => |val| try func.instructions.append(.{
            .op = bytecode.OpCode.LoadConst,
            .operand = types.Value{ .Bool = val },
        }),
        .LoadString => |val| {
            // Take ownership of the string
            try func.instructions.append(.{
                .op = bytecode.OpCode.LoadConst,
                .operand = types.Value{ .String = val },
            });
            // Clear the MIR instruction so it won't be freed
            instr.* = .LoadNil;
        },
        .LoadNil => try func.instructions.append(.{
            .op = bytecode.OpCode.LoadConst,
            .operand = types.Value{ .Nil = {} },
        }),
        .LoadSymbol => |val| {
            // Take ownership of the symbol
            try func.instructions.append(.{
                .op = bytecode.OpCode.LoadConst,
                .operand = types.Value{ .Symbol = val },
            });
            // Clear the MIR instruction so it won't be freed
            instr.* = .LoadNil;
        },
        .LoadArray => |val| {
            // Take ownership of the array
            try func.instructions.append(.{
                .op = bytecode.OpCode.LoadConst,
                .operand = types.Value{ .Array = val },
            });
            // Clear the MIR instruction so it won't be freed
            instr.* = .LoadNil;
        },
        .LoadMap => |val| {
            // Take ownership of the map
            try func.instructions.append(.{
                .op = bytecode.OpCode.LoadConst,
                .operand = types.Value{ .Map = val },
            });
            // Clear the MIR instruction so it won't be freed
            instr.* = .LoadNil;
        },
        .LoadVariable => |val| {
            // Take ownership of the variable name
            try func.instructions.append(.{
                .op = bytecode.OpCode.LoadVar,
                .operand = types.Value{ .Symbol = val },
            });
            // Clear the MIR instruction so it won't be freed
            instr.* = .LoadNil;
        },
        .Add => try func.instructions.append(.{
            .op = bytecode.OpCode.Add,
            .operand = null,
        }),
        .Sub => try func.instructions.append(.{
            .op = bytecode.OpCode.Sub,
            .operand = null,
        }),
        .LessThan => try func.instructions.append(.{
            .op = bytecode.OpCode.Lt,
            .operand = null,
        }),
        .GreaterThan => try func.instructions.append(.{
            .op = bytecode.OpCode.Gt,
            .operand = null,
        }),
        .Jump => |target| try func.instructions.append(.{
            .op = bytecode.OpCode.LoadConst,
            .operand = types.Value{ .Int = @as(i64, @intCast(target)) },
        }),
        .JumpIfFalse => |target| try func.instructions.append(.{
            .op = bytecode.OpCode.LoadConst,
            .operand = types.Value{ .Int = @as(i64, @intCast(target)) },
        }),
        .Call => |arg_count| try func.instructions.append(.{
            .op = bytecode.OpCode.Call,
            .operand = types.Value{ .Int = @as(i64, @intCast(arg_count)) },
        }),
        .Print => try func.instructions.append(.{
            .op = bytecode.OpCode.Print,
            .operand = null,
        }),
        .Return => try func.instructions.append(.{
            .op = bytecode.OpCode.Return,
            .operand = null,
        }),
        .LoadFunction => |func_ptr| {
            // Convert the function's blocks to bytecode
            var bc_func = bytecode.Function.init(func.allocator);

            for (func_ptr.blocks.items) |*block| {
                for (block.instructions.items) |*block_instr| {
                    try convertInstruction(&bc_func, block_instr);
                }
            }

            // Create a function value
            const temp_func = try func.allocator.create(bytecode.Function);
            temp_func.* = .{
                .instructions = bc_func.instructions,
                .allocator = func.allocator,
                .name = try func.allocator.dupe(u8, func_ptr.name),
                .param_count = 0, // TODO: Update when MIR function params are implemented
            };

            const func_value = types.Value{ .Function = temp_func };

            // Load it as a constant
            try func.instructions.append(.{
                .op = bytecode.OpCode.LoadConst,
                .operand = func_value,
            });
        },
        .StoreVariable => |name| {
            try func.instructions.append(.{
                .op = bytecode.OpCode.StoreVar,
                .operand = types.Value{ .String = name },
            });
            // Clear the MIR instruction so it won't be freed
            instr.* = .LoadNil;
        },
    }
}
