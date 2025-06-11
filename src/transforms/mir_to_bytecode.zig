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

    // The main function should only contain top-level statements
    // For now, we'll assume all MIR functions are function definitions that should be
    // loaded and stored as variables in the main function

    // Convert each MIR function to bytecode
    var next_reg: u16 = 0;
    for (mir_prog.functions.items) |*mir_func| {
        // Each MIR function's blocks contain the main program code
        // (This is because the parser currently puts everything in functions)
        for (mir_func.blocks.items) |*block| {
            // Convert each instruction to bytecode
            for (block.instructions.items) |*instr| {
                try convertInstruction(&main_func, instr, &created_functions, &next_reg);
            }
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

fn convertInstruction(func: *bytecode.Function, instr: *mir.MIR.Instruction, created_functions: *std.ArrayList(*bytecode.Function), next_reg: *u16) !void {
    switch (instr.*) {
        .LoadInt => |val| {
            const dst_reg = next_reg.*;
            next_reg.* += 1;
            try func.instructions.append(.{
                .op = bytecode.OpCode.LoadConst,
                .dst = dst_reg,
                .immediate = types.Value{ .Int = val },
            });
        },
        .LoadFloat => |val| {
            const dst_reg = next_reg.*;
            next_reg.* += 1;
            try func.instructions.append(.{
                .op = bytecode.OpCode.LoadConst,
                .dst = dst_reg,
                .immediate = types.Value{ .Float = val },
            });
        },
        .LoadBool => |val| {
            const dst_reg = next_reg.*;
            next_reg.* += 1;
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
            try func.instructions.append(.{
                .op = bytecode.OpCode.LoadConst,
                .immediate = types.Value{ .Symbol = sym_copy },
            });
            // No need to clear MIR instr if we're duplicating
            // instr.* = .LoadNil; // REMOVED
        },
        .LoadArray => |val| {
            // Take ownership of the array
            try func.instructions.append(.{
                .op = bytecode.OpCode.LoadConst,
                .immediate = types.Value{ .Array = val },
            });
            // Clear the MIR instruction so it won't be freed
            instr.* = .LoadNil;
        },
        .LoadMap => |val| {
            // Take ownership of the map
            try func.instructions.append(.{
                .op = bytecode.OpCode.LoadConst,
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
            try func.instructions.append(.{
                .op = bytecode.OpCode.LoadParam,
                .immediate = types.Value{ .Int = @as(i64, @intCast(param_index)) },
            });
        },
        .Add => {
            // Binary operations in Gene are function calls, so we need to:
            // 1. Load the operator as a variable
            // 2. Load the operands
            // 3. Call the function with the operands

            const op_copy = try func.allocator.dupe(u8, "+");
            errdefer func.allocator.free(op_copy);

            // Load the operator
            try func.instructions.append(.{
                .op = bytecode.OpCode.LoadVar,
                .immediate = types.Value{ .String = op_copy },
            });

            // Operands should already be on stack from previous instructions
            // Just need to add the Call instruction
            try func.instructions.append(.{
                .op = bytecode.OpCode.Call,
                .immediate = types.Value{ .Int = 2 }, // 2 args
            });
        },
        .Sub => {
            const op_copy = try func.allocator.dupe(u8, "-");
            errdefer func.allocator.free(op_copy);

            try func.instructions.append(.{
                .op = bytecode.OpCode.LoadVar,
                .immediate = types.Value{ .String = op_copy },
            });

            try func.instructions.append(.{
                .op = bytecode.OpCode.Call,
                .immediate = types.Value{ .Int = 2 }, // 2 args
            });
        },
        .Mul => try func.instructions.append(.{
            .op = bytecode.OpCode.Mul,
            .immediate = null,
        }),
        .Div => try func.instructions.append(.{
            .op = bytecode.OpCode.Div,
            .immediate = null,
        }),
        .LessThan => try func.instructions.append(.{
            .op = bytecode.OpCode.Lt,
            .immediate = null,
        }),
        .GreaterThan => try func.instructions.append(.{
            .op = bytecode.OpCode.Gt,
            .immediate = null,
        }),
        .Equal => try func.instructions.append(.{
            .op = bytecode.OpCode.Eq,
            .immediate = null,
        }),
        .Jump => |target| try func.instructions.append(.{
            .op = bytecode.OpCode.Jump,
            .immediate = types.Value{ .Int = @as(i64, @intCast(target)) },
        }),
        .JumpIfFalse => |target| try func.instructions.append(.{
            .op = bytecode.OpCode.JumpIfFalse,
            .immediate = types.Value{ .Int = @as(i64, @intCast(target)) },
        }),
        .Call => |arg_count| {
            // For register-based calls, we need to find the function register
            // In a stack-based MIR, the function and arguments are loaded consecutively
            // But in our register allocation, we need to account for nested calls
            
            if (next_reg.* < arg_count + 1) {
                return error.OutOfMemory;
            }
            
            // Simple heuristic: scan backwards through recent instructions to find
            // the function that was loaded for this call
            var func_reg: u16 = 0;
            
            // Look at the pattern of recent instructions to determine the function register
            const recent_instrs = func.instructions.items;
            if (recent_instrs.len >= 2) {
                const last_instr = recent_instrs[recent_instrs.len - 1];
                
                // If the last instruction was also a Call, this might be a nested call
                // In the pattern: print(+(1,2)), the function for the outer call 
                // would have been loaded before the inner call sequence
                if (last_instr.op == .Call and arg_count == 1) {
                    // Look for a LoadVar instruction that loaded a print-like function
                    // Scan backwards to find it
                    var i: usize = recent_instrs.len;
                    while (i > 0) {
                        i -= 1;
                        const instr_check = recent_instrs[i];
                        if (instr_check.op == .LoadVar and instr_check.dst != null) {
                            // Check if this was before the inner call sequence
                            if (instr_check.dst.? < last_instr.src1.?) {
                                func_reg = instr_check.dst.?;
                                break;
                            }
                        }
                    }
                } else {
                    // Normal case: function is loaded just before arguments
                    func_reg = @as(u16, @intCast(next_reg.* - arg_count - 1));
                }
            } else {
                // Fallback to simple calculation
                func_reg = @as(u16, @intCast(next_reg.* - arg_count - 1));
            }
            
            const dst_reg = next_reg.*;
            next_reg.* += 1;
            
            try func.instructions.append(.{
                .op = bytecode.OpCode.Call,
                .src1 = func_reg,
                .dst = dst_reg,
                .immediate = types.Value{ .Int = @as(i64, @intCast(arg_count)) },
            });
        },
        .Print => try func.instructions.append(.{
            .op = bytecode.OpCode.Print,
            .immediate = null,
        }),
        .Return => try func.instructions.append(.{
            .op = bytecode.OpCode.Return,
            .immediate = null,
        }),
        .LoadFunction => |func_ptr| {
            // Create a proper function object and convert the MIR function body to bytecode
            const new_func = try func.allocator.create(bytecode.Function);
            new_func.* = bytecode.Function{
                .instructions = std.ArrayList(bytecode.Instruction).init(func.allocator),
                .allocator = func.allocator,
                .name = try func.allocator.dupe(u8, func_ptr.name),
                .param_count = func_ptr.param_count,
                .register_count = 0,  // TODO: Implement proper register allocation
                .local_count = 0,
                .temp_count = 0,
            };

            // Convert the MIR function body to bytecode instructions
            var func_next_reg: u16 = 0;
            for (func_ptr.blocks.items) |*block| {
                for (block.instructions.items) |*mir_instr| {
                    try convertInstruction(new_func, mir_instr, created_functions, &func_next_reg);
                }
            }

            // Add a return instruction at the end of the function if not present
            const needs_return = if (new_func.instructions.items.len == 0) true else switch (new_func.instructions.items[new_func.instructions.items.len - 1].op) {
                .Return => false,
                else => true,
            };

            if (needs_return) {
                try new_func.instructions.append(.{
                    .op = bytecode.OpCode.Return,
                    .immediate = null,
                });
            }

            // Track the function for cleanup
            try created_functions.append(new_func);

            // Load the function object onto the stack
            try func.instructions.append(.{
                .op = bytecode.OpCode.LoadConst,
                .immediate = types.Value{ .Function = new_func },
            });
        },
        .StoreVariable => |name| {
            // Duplicate the variable name (string) for the bytecode operand
            const name_copy = try func.allocator.dupe(u8, name);
            errdefer func.allocator.free(name_copy); // Free if append fails
            try func.instructions.append(.{
                .op = bytecode.OpCode.StoreVar,
                // StoreVar operand should probably be Symbol, not String, like LoadVar
                // but keeping as String for now to match existing code.
                .immediate = types.Value{ .String = name_copy },
            });
            // No need to clear MIR instr if we're duplicating
            // instr.* = .LoadNil; // REMOVED
        },
    }
}
