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
        .Print => try func.instructions.append(.{
            .op = bytecode.OpCode.Print,
            .operand = null,
        }),
        .Return => try func.instructions.append(.{
            .op = bytecode.OpCode.Return,
            .operand = null,
        }),
    }
}
