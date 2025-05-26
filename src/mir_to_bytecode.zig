const std = @import("std");
const mir = @import("mir.zig");
const bytecode = @import("bytecode.zig");
const types = @import("types.zig");

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

    // The main function should only contain top-level statements
    // For now, we'll assume all MIR functions are function definitions that should be
    // loaded and stored as variables in the main function

    // Convert each MIR function to bytecode
    for (mir_prog.functions.items) |*mir_func| {
        // Each MIR function's blocks contain the main program code
        // (This is because the parser currently puts everything in functions)
        for (mir_func.blocks.items) |*block| {
            // Convert each instruction to bytecode
            for (block.instructions.items) |*instr| {
                try convertInstruction(&main_func, instr, &created_functions);
            }
        }
    }

    return ConversionResult{
        .main_func = main_func,
        .created_functions = created_functions,
        .allocator = allocator,
    };
}

fn convertInstruction(func: *bytecode.Function, instr: *mir.MIR.Instruction, created_functions: *std.ArrayList(*bytecode.Function)) !void {
    _ = created_functions;
    std.debug.print("[MIR->BC] Converting instruction: {s} to function with {} instructions\n", .{ @tagName(instr.*), func.instructions.items.len });
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
            // Duplicate the string for the bytecode operand
            const str_copy = try func.allocator.dupe(u8, val);
            errdefer func.allocator.free(str_copy); // Free if append fails
            try func.instructions.append(.{
                .op = bytecode.OpCode.LoadConst,
                .operand = types.Value{ .String = str_copy },
            });
            // No need to clear MIR instr if we're duplicating
            // instr.* = .LoadNil; // REMOVED
        },
        .LoadNil => try func.instructions.append(.{
            .op = bytecode.OpCode.LoadConst,
            .operand = types.Value{ .Nil = {} },
        }),
        .LoadSymbol => |val| {
            // Duplicate the symbol for the bytecode operand
            const sym_copy = try func.allocator.dupe(u8, val);
            errdefer func.allocator.free(sym_copy); // Free if append fails
            try func.instructions.append(.{
                .op = bytecode.OpCode.LoadConst,
                .operand = types.Value{ .Symbol = sym_copy },
            });
            // No need to clear MIR instr if we're duplicating
            // instr.* = .LoadNil; // REMOVED
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
            // Duplicate the variable name (symbol) for the bytecode operand
            const name_copy = try func.allocator.dupe(u8, val);
            errdefer func.allocator.free(name_copy); // Free if append fails
            try func.instructions.append(.{
                .op = bytecode.OpCode.LoadVar,
                .operand = types.Value{ .Symbol = name_copy },
            });
            // No need to clear MIR instr if we're duplicating
            // instr.* = .LoadNil; // REMOVED
        },
        .LoadParameter => |param_index| {
            // Load parameter from the stack frame
            try func.instructions.append(.{
                .op = bytecode.OpCode.LoadParam,
                .operand = types.Value{ .Int = @as(i64, @intCast(param_index)) },
            });
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
        .Equal => try func.instructions.append(.{
            .op = bytecode.OpCode.Eq,
            .operand = null,
        }),
        .Jump => |target| try func.instructions.append(.{
            .op = bytecode.OpCode.Jump,
            .operand = types.Value{ .Int = @as(i64, @intCast(target)) },
        }),
        .JumpIfFalse => |target| try func.instructions.append(.{
            .op = bytecode.OpCode.JumpIfFalse,
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
            // Instead of loading the function object directly, store it in a variable by name
            // and load the variable when needed
            // Store the function in a variable
            try func.instructions.append(.{
                .op = bytecode.OpCode.StoreVar,
                .operand = types.Value{ .String = try func.allocator.dupe(u8, func_ptr.name) },
            });
            // When the function is referenced, it should be loaded by name (LoadVar)
            // So we do not push the function object directly onto the stack here
        },
        .StoreVariable => |name| {
            // Duplicate the variable name (string) for the bytecode operand
            const name_copy = try func.allocator.dupe(u8, name);
            errdefer func.allocator.free(name_copy); // Free if append fails
            try func.instructions.append(.{
                .op = bytecode.OpCode.StoreVar,
                // StoreVar operand should probably be Symbol, not String, like LoadVar
                // but keeping as String for now to match existing code.
                .operand = types.Value{ .String = name_copy },
            });
            // No need to clear MIR instr if we're duplicating
            // instr.* = .LoadNil; // REMOVED
        },
    }
}
