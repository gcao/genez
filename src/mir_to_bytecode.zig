const std = @import("std");
const mir = @import("mir.zig");
const bytecode = @import("bytecode.zig");
const ast = @import("ast.zig");
const AstNode = ast.AstNode;

pub fn convert(allocator: std.mem.Allocator, mir_prog: *mir.MIR) !bytecode.Function {
    var func = bytecode.Function.init(allocator);
    errdefer func.deinit();

    // Convert each MIR function to bytecode
    for (mir_prog.functions.items) |*mir_func| {
        // Convert each block in the function
        for (mir_func.blocks.items) |*block| {
            // Convert each instruction to bytecode
            for (block.instructions.items) |*instr| {
                try convertInstruction(allocator, &func.instructions, instr);
            }
        }
    }
    return func;
}

fn convertInstruction(allocator: std.mem.Allocator, instructions: *std.ArrayList(bytecode.Instruction), instr: *const mir.MIR.Instruction) !void {
    switch (instr.*) {
        .LoadInt => |val| try instructions.append(.{
            .op = .LoadConst,
            .operand = .{ .Int = val },
        }),
        .LoadFloat => |val| try instructions.append(.{
            .op = .LoadConst,
            .operand = .{ .Float = val },
        }),
        .LoadBool => |val| try instructions.append(.{
            .op = .LoadConst,
            .operand = .{ .Bool = val },
        }),
        .LoadString => |val| try instructions.append(.{
            .op = .LoadConst,
            .operand = .{ .String = try allocator.dupe(u8, val) },
        }),
        .LoadNil => try instructions.append(.{
            .op = .LoadConst,
            .operand = .{ .Nil = {} },
        }),
        .LoadSymbol => |val| try instructions.append(.{
            .op = .LoadConst,
            .operand = .{ .Symbol = try allocator.dupe(u8, val) },
        }),
        .LoadArray => |val| try instructions.append(.{
            .op = .LoadConst,
            .operand = .{ .Array = try allocator.dupe(AstNode.Value, val) },
        }),
        .LoadMap => |val| {
            var new_map = std.StringHashMap(AstNode.Value).init(allocator);
            var it = val.iterator();
            while (it.next()) |entry| {
                try new_map.put(try allocator.dupe(u8, entry.key_ptr.*), entry.value_ptr.*);
            }
            try instructions.append(.{
                .op = .LoadConst,
                .operand = .{ .Map = new_map },
            });
        },
        .LoadVariable => |val| try instructions.append(.{
            .op = .LoadVar,
            .operand = .{ .String = try allocator.dupe(u8, val) },
        }),
        .Add => try instructions.append(.{ .op = .Add }),
        .Sub => try instructions.append(.{ .op = .Sub }),
        .Mul => try instructions.append(.{ .op = .Mul }),
        .Div => try instructions.append(.{ .op = .Div }),
        .Lt => try instructions.append(.{ .op = .Lt }),
        .Gt => try instructions.append(.{ .op = .Gt }),
        .Eq => try instructions.append(.{ .op = .Eq }),
        .Print => try instructions.append(.{ .op = .Print }),
        .Return => try instructions.append(.{ .op = .Return }),
        .DefineFunction => |func| {
            // Create a new function
            var new_func = bytecode.Function.init(allocator);

            // Convert all instructions in the function's blocks
            for (func.blocks.items) |block| {
                for (block.instructions.items) |*block_instr| {
                    try convertInstruction(allocator, &new_func.instructions, block_instr);
                }
            }

            // Add the function to the module's functions list
            try instructions.append(.{
                .op = .DefineFunction,
                .operand = .{ .String = try allocator.dupe(u8, func.name) },
            });
        },
        .Call => |call| {
            // First convert the function expression
            try instructions.append(.{ .op = .LoadVar, .operand = .{ .String = try allocator.dupe(u8, call.func) } });

            // Call the function
            try instructions.append(.{
                .op = .Call,
                .operand = .{ .Int = @as(i64, @intCast(call.arg_count)) },
            });
        },
        .JumpIfFalse => |jump| try instructions.append(.{
            .op = .JumpIfFalse,
            .operand = .{ .Int = @as(i64, @intCast(jump.offset)) },
        }),
        .Jump => |jump| try instructions.append(.{
            .op = .Jump,
            .operand = .{ .Int = @as(i64, @intCast(jump.offset)) },
        }),
        .Store => |expr| {
            try convertInstruction(allocator, instructions, &.{ .LoadVariable = expr.LoadVariable });
        },
    }
}
