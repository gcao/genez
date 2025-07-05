const std = @import("std");
const mir = @import("../ir/mir.zig");
const bytecode = @import("../backend/bytecode.zig");
const types = @import("../core/types.zig");

pub const ConversionResult = struct {
    main_func: bytecode.Function,
    created_functions: std.ArrayList(*bytecode.Function),
    ffi_functions: std.ArrayList(*@import("../ir/hir.zig").HIR.FFIFunction),
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
        
        // FFI functions are not owned by this result
        self.ffi_functions.deinit();
    }
};

pub fn convert(allocator: std.mem.Allocator, mir_prog: *mir.MIR, ffi_functions: []const *@import("../ir/hir.zig").HIR.FFIFunction) !ConversionResult {
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
        // Copy FFI function pointers to a new list
        var ffi_funcs = std.ArrayList(*@import("../ir/hir.zig").HIR.FFIFunction).init(allocator);
        for (ffi_functions) |ffi_func| {
            try ffi_funcs.append(ffi_func);
        }
        
        return ConversionResult{
            .main_func = main_func,
            .created_functions = created_functions,
            .ffi_functions = ffi_funcs,
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

    // Build address mapping for the main function
    var mir_to_bytecode_addr = std.AutoHashMap(usize, usize).init(allocator);
    defer mir_to_bytecode_addr.deinit();

    const main_mir_func = &mir_prog.functions.items[num_functions - 1];

    // First pass: Convert instructions and build address mapping
    for (main_mir_func.blocks.items) |*block| {
        for (block.instructions.items, 0..) |*instr, mir_addr| {
            // Record the mapping from MIR address to current bytecode address
            const bytecode_addr = main_func.instructions.items.len;
            try mir_to_bytecode_addr.put(mir_addr, bytecode_addr);

            try convertInstructionWithStack(&main_func, instr, &created_functions, &next_reg, &stack);
        }
    }

    // Second pass: Fix jump targets using the address mapping
    for (main_func.instructions.items) |*bytecode_instr| {
        switch (bytecode_instr.op) {
            .Jump => {
                if (bytecode_instr.immediate) |*imm| {
                    if (imm.* == .Int) {
                        const mir_target = @as(usize, @intCast(imm.Int));
                        if (mir_to_bytecode_addr.get(mir_target)) |bytecode_target| {
                            imm.Int = @as(i64, @intCast(bytecode_target));
                        }
                    }
                }
            },
            .JumpIfFalse => {
                if (bytecode_instr.immediate) |*imm| {
                    if (imm.* == .Int) {
                        const mir_target = @as(usize, @intCast(imm.Int));
                        if (mir_to_bytecode_addr.get(mir_target)) |bytecode_target| {
                            imm.Int = @as(i64, @intCast(bytecode_target));
                        }
                    }
                }
            },
            .TryStart => {
                if (bytecode_instr.jump_target) |mir_target| {
                    if (mir_to_bytecode_addr.get(mir_target)) |bytecode_target| {
                        bytecode_instr.jump_target = bytecode_target;
                    }
                }
            },
            else => {},
        }
    }

    // Add a return instruction at the end of the main function if not present
    const needs_return = if (main_func.instructions.items.len == 0) true else switch (main_func.instructions.items[main_func.instructions.items.len - 1].op) {
        .Return => false,
        else => true,
    };

    if (needs_return) {
        // Check if there's a value on the stack to return
        var return_reg: ?u16 = null;
        if (stack.len() > 0) {
            return_reg = stack.pop();
        }

        try main_func.instructions.append(.{
            .op = bytecode.OpCode.Return,
            .src1 = return_reg,
        });
    }

    // Record register usage for the main function
    main_func.register_count = next_reg;

    // Copy FFI function pointers to a new list
    var ffi_funcs = std.ArrayList(*@import("../ir/hir.zig").HIR.FFIFunction).init(allocator);
    for (ffi_functions) |ffi_func| {
        try ffi_funcs.append(ffi_func);
    }
    
    return ConversionResult{
        .main_func = main_func,
        .created_functions = created_functions,
        .ffi_functions = ffi_funcs,
        .allocator = allocator,
    };
}

fn convertMirFunction(allocator: std.mem.Allocator, mir_func: *mir.MIR.Function) anyerror!bytecode.Function {
    var bytecode_func = bytecode.Function.init(allocator);
    errdefer bytecode_func.deinit();

    // Copy function metadata
    bytecode_func.name = try allocator.dupe(u8, mir_func.name);
    bytecode_func.param_count = mir_func.param_count;
    bytecode_func.rest_param = if (mir_func.rest_param) |rp| try allocator.dupe(u8, rp) else null;
    // We'll compute the actual register usage during conversion
    bytecode_func.register_count = 0;

    // std.debug.print("\n=== Converting MIR function '{s}' to bytecode ===\n", .{mir_func.name});
    // std.debug.print("Parameters: {}\n", .{mir_func.param_count});

    // Initialize stack tracking for this function
    var next_reg: u16 = @as(u16, @intCast(mir_func.param_count)); // Start after parameters
    var stack = StackTracker.init(allocator);
    defer stack.deinit();

    // Since this is a user-defined function, we don't have access to created_functions here
    // We'll pass an empty list (user-defined functions shouldn't create more functions)
    var empty_functions = std.ArrayList(*bytecode.Function).init(allocator);
    defer empty_functions.deinit();

    // First pass: Convert all blocks and build address mapping
    var mir_to_bytecode_addr = std.AutoHashMap(usize, usize).init(allocator);
    defer mir_to_bytecode_addr.deinit();

    for (mir_func.blocks.items) |*block| {
        for (block.instructions.items, 0..) |*instr, mir_addr| {
            // Record the mapping from MIR address to current bytecode address
            const bytecode_addr = bytecode_func.instructions.items.len;
            try mir_to_bytecode_addr.put(mir_addr, bytecode_addr);

            try convertInstructionWithStack(&bytecode_func, instr, &empty_functions, &next_reg, &stack);
        }
    }

    // Second pass: Fix jump targets using the address mapping
    for (bytecode_func.instructions.items) |*bytecode_instr| {
        switch (bytecode_instr.op) {
            .Jump => {
                if (bytecode_instr.immediate) |*imm| {
                    if (imm.* == .Int) {
                        const mir_target = @as(usize, @intCast(imm.Int));
                        if (mir_to_bytecode_addr.get(mir_target)) |bytecode_target| {
                            imm.Int = @as(i64, @intCast(bytecode_target));
                        }
                    }
                }
            },
            .JumpIfFalse => {
                if (bytecode_instr.immediate) |*imm| {
                    if (imm.* == .Int) {
                        const mir_target = @as(usize, @intCast(imm.Int));
                        if (mir_to_bytecode_addr.get(mir_target)) |bytecode_target| {
                            imm.Int = @as(i64, @intCast(bytecode_target));
                        }
                    }
                }
            },
            .TryStart => {
                if (bytecode_instr.jump_target) |mir_target| {
                    if (mir_to_bytecode_addr.get(mir_target)) |bytecode_target| {
                        bytecode_instr.jump_target = bytecode_target;
                    }
                }
            },
            else => {},
        }
    }

    // Add a return instruction if not present
    const needs_return = if (bytecode_func.instructions.items.len == 0) true else switch (bytecode_func.instructions.items[bytecode_func.instructions.items.len - 1].op) {
        .Return => false,
        else => true,
    };

    if (needs_return) {
        // Check if there's a value on the stack to return
        var return_reg: ?u16 = null;
        if (stack.len() > 0) {
            return_reg = stack.pop();
        }

        try bytecode_func.instructions.append(.{
            .op = bytecode.OpCode.Return,
            .src1 = return_reg,
        });
    }

    // Record the number of registers actually used
    bytecode_func.register_count = next_reg;

    // Debug: Print summary of generated bytecode
    // std.debug.print("\n=== Bytecode summary for function '{s}' ===\n", .{bytecode_func.name});
    // std.debug.print("Total instructions: {}\n", .{bytecode_func.instructions.items.len});
    // std.debug.print("Register count: {}\n", .{bytecode_func.register_count});
    // std.debug.print("Parameter count: {}\n", .{bytecode_func.param_count});
    // std.debug.print("\nGenerated bytecode:\n", .{});
    // for (bytecode_func.instructions.items, 0..) |instr, i| {
    //     std.debug.print("  [{:3}] ", .{i});
    //     switch (instr.op) {
    //         .LoadConst => {
    //             if (instr.immediate) |imm| {
    //                 switch (imm) {
    //                     .Int => |val| std.debug.print("LoadConst r{} = {}", .{ instr.dst.?, val }),
    //                     .Function => std.debug.print("LoadConst r{} = Function", .{instr.dst.?}),
    //                     else => std.debug.print("LoadConst r{} = {any}", .{ instr.dst.?, imm }),
    //                 }
    //             } else {
    //                 std.debug.print("LoadConst r{}", .{instr.dst.?});
    //             }
    //         },
    //         .LoadParam => std.debug.print("LoadParam r{} = param[{}]", .{ instr.dst.?, instr.immediate.?.Int }),
    //         .LoadVar => std.debug.print("LoadVar r{} = var({s})", .{ instr.dst.?, instr.var_name.? }),
    //         .Add => std.debug.print("Add r{} = r{} + r{}", .{ instr.dst.?, instr.src1.?, instr.src2.? }),
    //         .Sub => std.debug.print("Sub r{} = r{} - r{}", .{ instr.dst.?, instr.src1.?, instr.src2.? }),
    //         .Lt => std.debug.print("Lt r{} = r{} < r{}", .{ instr.dst.?, instr.src1.?, instr.src2.? }),
    //         .JumpIfFalse => std.debug.print("JumpIfFalse r{} to {}", .{ instr.src1.?, instr.immediate.?.Int }),
    //         .Jump => std.debug.print("Jump to {}", .{instr.immediate.?.Int}),
    //         .Call => std.debug.print("Call r{} = r{}({} args)", .{ instr.dst.?, instr.src1.?, instr.immediate.?.Int }),
    //         .Return => {
    //             if (instr.src1) |reg| {
    //                 std.debug.print("Return r{}", .{reg});
    //             } else {
    //                 std.debug.print("Return", .{});
    //             }
    //         },
    //         .Move => std.debug.print("Move r{} = r{}", .{ instr.dst.?, instr.src1.? }),
    //         else => std.debug.print("{any}", .{instr.op}),
    //     }
    //     std.debug.print("\n", .{});
    // }
    // std.debug.print("=== End of function '{s}' ===\n\n", .{bytecode_func.name});

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
    // Debug: Print the MIR instruction being converted
    // std.debug.print("\n[MIR->Bytecode] Converting MIR instruction: ", .{});
    // switch (instr.*) {
    //     .LoadInt => |val| std.debug.print("LoadInt({})", .{val}),
    //     .LoadFloat => |val| std.debug.print("LoadFloat({})", .{val}),
    //     .LoadBool => |val| std.debug.print("LoadBool({})", .{val}),
    //     .LoadString => |val| std.debug.print("LoadString(\"{s}\")", .{val}),
    //     .LoadNil => std.debug.print("LoadNil", .{}),
    //     .LoadSymbol => |val| std.debug.print("LoadSymbol(:{s})", .{val}),
    //     .LoadArray => std.debug.print("LoadArray", .{}),
    //     .LoadMap => std.debug.print("LoadMap", .{}),
    //     .LoadVariable => |val| std.debug.print("LoadVariable({s})", .{val}),
    //     .LoadParameter => |idx| std.debug.print("LoadParameter({})", .{idx}),
    //     .Add => std.debug.print("Add", .{}),
    //     .Sub => std.debug.print("Sub", .{}),
    //     .Mul => std.debug.print("Mul", .{}),
    //     .Div => std.debug.print("Div", .{}),
    //     .LessThan => std.debug.print("LessThan", .{}),
    //     .GreaterThan => std.debug.print("GreaterThan", .{}),
    //     .Equal => std.debug.print("Equal", .{}),
    //     .Jump => |target| std.debug.print("Jump({})", .{target}),
    //     .JumpIfFalse => |target| std.debug.print("JumpIfFalse({})", .{target}),
    //     .Call => |count| std.debug.print("Call(args={})", .{count}),
    //     .Print => std.debug.print("Print", .{}),
    //     .Return => std.debug.print("Return", .{}),
    //     .LoadFunction => |f| std.debug.print("LoadFunction({s})", .{f.name}),
    //     .StoreVariable => |name| std.debug.print("StoreVariable({s})", .{name}),
    //     .DefineClass => |class_def| std.debug.print("DefineClass({s})", .{class_def.name}),
    //     .CreateInstance => |class_name| std.debug.print("CreateInstance({s})", .{class_name}),
    //     .GetField => |field_name| std.debug.print("GetField({s})", .{field_name}),
    //     .SetField => |field_name| std.debug.print("SetField({s})", .{field_name}),
    //     .CallMethod => |method_call| std.debug.print("CallMethod({s}, {} args)", .{method_call.method_name, method_call.arg_count}),
    // }
    // std.debug.print(" | Bytecode pos: {} | Stack depth: {} | Next reg: {}\n", .{ func.instructions.items.len, stack.len(), next_reg.* });

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
            // std.debug.print("  -> Generated: LoadConst r{} = {}\n", .{ dst_reg, val });
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
        .CreateArray => |count| {
            // Pop N elements from stack and create array
            const dst_reg = next_reg.*;
            next_reg.* += 1;

            // The CreateArray instruction expects the elements to be in consecutive registers
            // starting from src1. We'll collect the registers that hold the elements.
            var src_regs = try func.allocator.alloc(u16, count);
            defer func.allocator.free(src_regs);

            // Pop in reverse order so first array element is first popped
            var i: usize = count;
            while (i > 0) {
                i -= 1;
                src_regs[i] = stack.pop();
            }

            // Generate CreateArray instruction
            // src1 will be the first element register (if any)
            // immediate will contain the count
            try func.instructions.append(.{
                .op = bytecode.OpCode.CreateArray,
                .dst = dst_reg,
                .src1 = if (count > 0) src_regs[0] else null,
                .immediate = types.Value{ .Int = @intCast(count) },
            });

            try stack.push(dst_reg);
        },
        .CreateMap => |count| {
            // Pop N*2 elements from stack (key-value pairs) and create map
            const dst_reg = next_reg.*;
            next_reg.* += 1;

            // The CreateMap instruction expects key-value pairs in consecutive registers
            var src_regs = try func.allocator.alloc(u16, count * 2);
            defer func.allocator.free(src_regs);

            // Pop in reverse order so first key-value pair is first popped
            var i: usize = count * 2;
            while (i > 0) {
                i -= 1;
                src_regs[i] = stack.pop();
            }

            // Generate CreateMap instruction
            // src1 will be the first key register (if any)
            // immediate will contain the count of key-value pairs
            try func.instructions.append(.{
                .op = bytecode.OpCode.CreateMap,
                .dst = dst_reg,
                .src1 = if (count > 0) src_regs[0] else null,
                .immediate = types.Value{ .Int = @intCast(count) },
            });

            try stack.push(dst_reg);
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
            // std.debug.print("  -> Generated: LoadVar r{} = var({s})\n", .{ dst_reg, val });
            // No need to clear MIR instr if we're duplicating
            // instr.* = .LoadNil; // REMOVED
        },
        .LoadModule => |path| {
            // Duplicate the module path for the bytecode operand
            const path_copy = try func.allocator.dupe(u8, path);
            errdefer func.allocator.free(path_copy);
            const dst_reg = next_reg.*;
            next_reg.* += 1;
            try stack.push(dst_reg); // Track this value on the stack
            try func.instructions.append(.{
                .op = bytecode.OpCode.LoadModule,
                .dst = dst_reg,
                .var_name = path_copy,
            });
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
            // std.debug.print("  -> Generated: LoadParam r{} = param[{}]\n", .{ dst_reg, param_index });
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
            // std.debug.print("  -> Generated: Add r{} = r{} + r{}\n", .{ dst_reg, left_reg, right_reg });
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
            // std.debug.print("  -> Generated: Sub r{} = r{} - r{}\n", .{ dst_reg, left_reg, right_reg });
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
            // std.debug.print("  -> Generated: Lt r{} = r{} < r{}\n", .{ dst_reg, left_reg, right_reg });
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
        .NotEqual => {
            // Use register-based instruction
            if (stack.len() < 2) {
                std.debug.print("ERROR: NotEqual needs 2 operands but only {} on stack\n", .{stack.len()});
                return error.OutOfMemory;
            }

            const right_reg = stack.pop();
            const left_reg = stack.pop();
            const dst_reg = next_reg.*;
            next_reg.* += 1;
            try stack.push(dst_reg);

            try func.instructions.append(.{
                .op = bytecode.OpCode.Ne,
                .src1 = left_reg,
                .src2 = right_reg,
                .dst = dst_reg,
            });
        },
        .LessEqual => {
            // Use register-based instruction
            if (stack.len() < 2) {
                std.debug.print("ERROR: LessEqual needs 2 operands but only {} on stack\n", .{stack.len()});
                return error.OutOfMemory;
            }

            const right_reg = stack.pop();
            const left_reg = stack.pop();
            const dst_reg = next_reg.*;
            next_reg.* += 1;
            try stack.push(dst_reg);

            try func.instructions.append(.{
                .op = bytecode.OpCode.Le,
                .src1 = left_reg,
                .src2 = right_reg,
                .dst = dst_reg,
            });
        },
        .GreaterEqual => {
            // Use register-based instruction
            if (stack.len() < 2) {
                std.debug.print("ERROR: GreaterEqual needs 2 operands but only {} on stack\n", .{stack.len()});
                return error.OutOfMemory;
            }

            const right_reg = stack.pop();
            const left_reg = stack.pop();
            const dst_reg = next_reg.*;
            next_reg.* += 1;
            try stack.push(dst_reg);

            try func.instructions.append(.{
                .op = bytecode.OpCode.Ge,
                .src1 = left_reg,
                .src2 = right_reg,
                .dst = dst_reg,
            });
        },
        .LogicalAnd => {
            // For && we need short-circuit evaluation, but for now we'll do simple evaluation
            if (stack.len() < 2) {
                std.debug.print("ERROR: LogicalAnd needs 2 operands but only {} on stack\n", .{stack.len()});
                return error.OutOfMemory;
            }

            const right_reg = stack.pop();
            const left_reg = stack.pop();
            const dst_reg = next_reg.*;
            next_reg.* += 3; // Reserve space for function and 2 args
            try stack.push(dst_reg);

            // For now, just evaluate both sides (no short-circuit)
            // TODO: Implement proper short-circuit evaluation
            // Since we don't have And opcode, use Call to && builtin
            const and_op = try func.allocator.dupe(u8, "&&");
            try func.instructions.append(.{
                .op = bytecode.OpCode.LoadVar,
                .dst = dst_reg,
                .var_name = and_op,
            });
            // Move operands to correct positions for call
            // Arguments must be in consecutive registers after the function register
            try func.instructions.append(.{
                .op = bytecode.OpCode.Move,
                .dst = dst_reg + 1,
                .src1 = left_reg,
            });
            try func.instructions.append(.{
                .op = bytecode.OpCode.Move,
                .dst = dst_reg + 2,
                .src1 = right_reg,
            });
            try func.instructions.append(.{
                .op = bytecode.OpCode.Call,
                .dst = dst_reg,
                .src1 = dst_reg,
                .immediate = types.Value{ .Int = 2 },
            });
        },
        .LogicalOr => {
            // For || we need short-circuit evaluation, but for now we'll do simple evaluation
            if (stack.len() < 2) {
                std.debug.print("ERROR: LogicalOr needs 2 operands but only {} on stack\n", .{stack.len()});
                return error.OutOfMemory;
            }

            const right_reg = stack.pop();
            const left_reg = stack.pop();
            const dst_reg = next_reg.*;
            next_reg.* += 3; // Reserve space for function and 2 args
            try stack.push(dst_reg);

            // For now, just evaluate both sides (no short-circuit)
            // TODO: Implement proper short-circuit evaluation
            // Since we don't have Or opcode, use Call to || builtin
            const or_op = try func.allocator.dupe(u8, "||");
            try func.instructions.append(.{
                .op = bytecode.OpCode.LoadVar,
                .dst = dst_reg,
                .var_name = or_op,
            });
            // Move operands to correct positions for call
            // Arguments must be in consecutive registers after the function register
            try func.instructions.append(.{
                .op = bytecode.OpCode.Move,
                .dst = dst_reg + 1,
                .src1 = left_reg,
            });
            try func.instructions.append(.{
                .op = bytecode.OpCode.Move,
                .dst = dst_reg + 2,
                .src1 = right_reg,
            });
            try func.instructions.append(.{
                .op = bytecode.OpCode.Call,
                .dst = dst_reg,
                .src1 = dst_reg,
                .immediate = types.Value{ .Int = 2 },
            });
        },
        .LogicalNot => {
            // Unary operator
            if (stack.len() < 1) {
                std.debug.print("ERROR: LogicalNot needs 1 operand but only {} on stack\n", .{stack.len()});
                return error.OutOfMemory;
            }

            const operand_reg = stack.pop();
            const dst_reg = next_reg.*;
            next_reg.* += 1;
            try stack.push(dst_reg);

            // We DO have Not opcode!
            try func.instructions.append(.{
                .op = bytecode.OpCode.Not,
                .src1 = operand_reg,
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
            // std.debug.print("  -> Generated: JumpIfFalse r{} to MIR:{} (will be fixed to bytecode addr)\n", .{ condition_reg, target });
        },
        .Call => |arg_count| {
            // Use stack tracker to find function and arguments
            // In MIR stack: [other, function, arg1, arg2, ..., argN] before call
            // We need to pop N args and then the function

            if (stack.len() < arg_count + 1) {
                std.debug.print("ERROR: Call with {} args but only {} items on stack\n", .{ arg_count, stack.len() });
                return error.OutOfMemory;
            }
            
            // Debug: print stack state
            // std.debug.print("DEBUG Call: arg_count={}, stack.len={}\n", .{ arg_count, stack.len() });

            // Pop arguments from stack (they are on top)
            var arg_regs = std.ArrayList(u16).init(func.allocator);
            defer arg_regs.deinit();

            var i: usize = 0;
            while (i < arg_count) : (i += 1) {
                try arg_regs.insert(0, stack.pop()); // Insert at front to preserve order
            }

            // Now pop the function
            const func_reg = stack.pop();

            // Move arguments to registers immediately following the function register
            for (arg_regs.items, 0..) |reg, idx| {
                const target = func_reg + 1 + @as(u16, @intCast(idx));
                try func.instructions.append(.{
                    .op = bytecode.OpCode.Move,
                    .dst = target,
                    .src1 = reg,
                });
                // std.debug.print("  -> Generated: Move r{} = r{} (arg {})\n", .{ target, reg, idx });
                if (target >= next_reg.*) next_reg.* = target + 1;
            }

            const dst_reg = next_reg.*;
            next_reg.* += 1;
            try stack.push(dst_reg); // Push result onto stack

            try func.instructions.append(.{
                .op = bytecode.OpCode.Call,
                .src1 = func_reg,
                .dst = dst_reg,
                .immediate = types.Value{ .Int = @as(i64, @intCast(arg_count)) },
            });
            // std.debug.print("  -> Generated: Call r{} = r{}({} args)\n", .{ dst_reg, func_reg, arg_count });
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
            } else if (next_reg.* > 0) {
                // If stack is empty but we have allocated registers,
                // return the last allocated register (for control flow cases)
                return_reg = next_reg.* - 1;
            }

            try func.instructions.append(.{
                .op = bytecode.OpCode.Return,
                .src1 = return_reg,
            });
            if (return_reg) |_| {
                // std.debug.print("  -> Generated: Return r{}\n", .{reg});
            } else {
                // std.debug.print("  -> Generated: Return (no value)\n", .{});
            }
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
                // Function not found - this might be an anonymous function
                // Convert it to bytecode and add it to created_functions
                const bytecode_func = try convertMirFunction(func.allocator, mir_func_ptr);
                const func_ptr = try func.allocator.create(bytecode.Function);
                func_ptr.* = bytecode_func;
                try created_functions.append(func_ptr);
                func_index = created_functions.items.len - 1;

                // Debug output
                // std.debug.print("INFO: Converted anonymous function '{s}' on demand\n", .{mir_func_ptr.name});
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
            // std.debug.print("  -> Generated: LoadConst r{} = Function({s})\n", .{ dst_reg, mir_func_ptr.name });
            // std.debug.print("INFO: Loading function '{s}' (index {}) with {} parameters\n", .{ mir_func_ptr.name, func_index.?, mir_func_ptr.param_count });
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
        .DefineClass => |class_def| {
            // Create a class definition value
            const class_ptr = try func.allocator.create(types.ClassDefinition);
            class_ptr.* = types.ClassDefinition.init(func.allocator, try func.allocator.dupe(u8, class_def.name));

            // Store parent name for runtime lookup
            // For now, store the parent_name in the immediate value
            // The VM will need to resolve it when StoreVar is executed
            var parent_name_copy: ?[]const u8 = null;
            if (class_def.parent_name) |parent_name| {
                parent_name_copy = try func.allocator.dupe(u8, parent_name);
            }

            // Copy fields
            for (class_def.fields) |field_name| {
                const duped_name = try func.allocator.dupe(u8, field_name);
                const field_def = types.ClassDefinition.FieldDefinition{
                    .name = duped_name,
                    .type_name = null, // TODO: Add type information
                    .default_value = null,
                    .is_public = true,
                };
                // Use a separate duped key for the map
                const key_name = try func.allocator.dupe(u8, field_name);
                try class_ptr.fields.put(key_name, field_def);
            }

            // Copy methods - they've already been converted to bytecode functions
            // std.debug.print("Converting {} methods for class {s}\n", .{class_def.methods.count(), class_def.name});
            var method_iter = class_def.methods.iterator();
            while (method_iter.next()) |entry| {
                const method_name = entry.key_ptr.*;
                const mir_method = entry.value_ptr.*;
                // std.debug.print("Converting method {s}\n", .{method_name});

                // Convert MIR function to bytecode function
                const method_bytecode = try convertMirFunction(func.allocator, mir_method);
                const method_func_ptr = try func.allocator.create(bytecode.Function);
                method_func_ptr.* = method_bytecode;

                // Store the method in the class
                try class_ptr.methods.put(try func.allocator.dupe(u8, method_name), method_func_ptr);
                // std.debug.print("Added method {s} to class {s}\n", .{method_name, class_def.name});
            }

            // Load the class as a constant
            const dst_reg = next_reg.*;
            next_reg.* += 1;
            try stack.push(dst_reg);

            try func.instructions.append(.{
                .op = bytecode.OpCode.LoadConst,
                .dst = dst_reg,
                .immediate = types.Value{ .Class = class_ptr },
            });
            // std.debug.print("  -> Generated: LoadConst r{} = Class({s})\n", .{ dst_reg, class_def.name });
            
            // If there's a parent class, generate code to set it
            if (parent_name_copy) |parent_name| {
                // Load the parent class by name
                const parent_reg = next_reg.*;
                next_reg.* += 1;
                
                try func.instructions.append(.{
                    .op = bytecode.OpCode.LoadVar,
                    .dst = parent_reg,
                    .var_name = parent_name,
                });
                
                // Set the parent on the class using ClassParent opcode
                // We'll need to add a SetClassParent opcode or use a different approach
                // For now, let's use a special instruction that the VM will handle
                try func.instructions.append(.{
                    .op = bytecode.OpCode.ClassParent,
                    .dst = dst_reg,  // class register
                    .src1 = parent_reg,  // parent register
                });
            }
        },
        .CreateInstance => |inst_creation| {
            // Pop constructor arguments from stack (they are on top)
            const arg_count = inst_creation.arg_count;
            if (stack.len() < arg_count + 1) { // +1 for the class
                std.debug.print("ERROR: CreateInstance with {} args but only {} items on stack\n", .{ arg_count, stack.len() });
                return error.StackUnderflow;
            }

            // Pop arguments
            var arg_regs = std.ArrayList(u16).init(func.allocator);
            defer arg_regs.deinit();

            var i: usize = 0;
            while (i < arg_count) : (i += 1) {
                try arg_regs.insert(0, stack.pop()); // Insert at front to preserve order
            }

            // Pop the class from stack
            const class_reg = stack.pop();

            // Move arguments to registers immediately following the class register
            for (arg_regs.items, 0..) |reg, idx| {
                const target = class_reg + 1 + @as(u16, @intCast(idx));
                try func.instructions.append(.{
                    .op = bytecode.OpCode.Move,
                    .dst = target,
                    .src1 = reg,
                });
                if (target >= next_reg.*) next_reg.* = target + 1;
            }

            // Use the New opcode
            const dst_reg = next_reg.*;
            next_reg.* += 1;
            try stack.push(dst_reg);

            try func.instructions.append(.{
                .op = bytecode.OpCode.New,
                .dst = dst_reg,
                .src1 = class_reg,
                .immediate = types.Value{ .Int = @as(i64, @intCast(arg_count)) },
            });
            // std.debug.print("  -> Generated: New r{} = new r{}({} args)\n", .{ dst_reg, class_reg, arg_count });
        },
        .Get => {
            if (stack.len() < 2) {
                std.debug.print("ERROR: Get needs object and key but stack has {} items\n", .{stack.len()});
                return error.StackUnderflow;
            }
            const key_reg = stack.pop();
            const obj_reg = stack.pop();
            const dst_reg = next_reg.*;
            next_reg.* += 1;
            try stack.push(dst_reg);

            try func.instructions.append(.{
                .op = bytecode.OpCode.Get,
                .dst = dst_reg,
                .src1 = obj_reg,
                .src2 = key_reg,
            });
        },
        .Set => {
            // Pop value, key, and object from stack
            if (stack.len() < 3) {
                std.debug.print("ERROR: Set needs object, key, and value but stack has {} items\n", .{stack.len()});
                return error.StackUnderflow;
            }
            const value_reg = stack.pop();
            const key_reg = stack.pop();
            const obj_reg = stack.pop();

            try func.instructions.append(.{
                .op = bytecode.OpCode.Set,
                .src1 = obj_reg,
                .src2 = key_reg,
                .dst = value_reg, // Using dst as value reg
            });
        },
        .Length => {
            // Pop value from stack and push its length
            if (stack.len() < 1) {
                std.debug.print("ERROR: Length needs a value but stack is empty\n", .{});
                return error.StackUnderflow;
            }
            const value_reg = stack.pop();
            const dst_reg = next_reg.*;
            next_reg.* += 1;
            try stack.push(dst_reg);
            
            try func.instructions.append(.{
                .op = bytecode.OpCode.Length,
                .src1 = value_reg,
                .dst = dst_reg,
            });
        },
        .ArrayGet => {
            // Pop index and array from stack
            if (stack.len() < 2) {
                std.debug.print("ERROR: ArrayGet needs array and index but stack has {} items\n", .{stack.len()});
                return error.StackUnderflow;
            }
            const index_reg = stack.pop();
            const array_reg = stack.pop();
            const dst_reg = next_reg.*;
            next_reg.* += 1;
            try stack.push(dst_reg);
            
            try func.instructions.append(.{
                .op = bytecode.OpCode.Get,
                .dst = dst_reg,
                .src1 = array_reg,
                .src2 = index_reg,
            });
        },
        .MapGet => {
            // Pop key and map from stack
            if (stack.len() < 2) {
                std.debug.print("ERROR: MapGet needs map and key but stack has {} items\n", .{stack.len()});
                return error.StackUnderflow;
            }
            const key_reg = stack.pop();
            const map_reg = stack.pop();
            const dst_reg = next_reg.*;
            next_reg.* += 1;
            try stack.push(dst_reg);
            
            try func.instructions.append(.{
                .op = bytecode.OpCode.Get,
                .dst = dst_reg,
                .src1 = map_reg,
                .src2 = key_reg,
            });
        },
        .Duplicate => {
            // Duplicate top of stack
            if (stack.len() < 1) {
                std.debug.print("ERROR: Duplicate needs a value but stack is empty\n", .{});
                return error.StackUnderflow;
            }
            const src_reg = stack.peek(0); // Peek at top
            const dst_reg = next_reg.*;
            next_reg.* += 1;
            try stack.push(dst_reg);
            
            try func.instructions.append(.{
                .op = bytecode.OpCode.Move,
                .dst = dst_reg,
                .src1 = src_reg,
            });
        },
        .Pop => {
            // Pop and discard top of stack
            if (stack.len() < 1) {
                std.debug.print("ERROR: Pop needs a value but stack is empty\n", .{});
                return error.StackUnderflow;
            }
            _ = stack.pop();
            // No bytecode needed - just removed from compile-time stack
        },
        // Commented out unimplemented instructions for now
        // .IsArray, .IsMap
        // will be implemented when VM support is added
        .CallMethod => |method_call| {
            // Pop arguments and instance from stack
            const total_args = method_call.arg_count + 1; // +1 for instance
            if (stack.len() < total_args) {
                std.debug.print("ERROR: CallMethod needs {} items but stack has {}\n", .{ total_args, stack.len() });
                return error.StackUnderflow;
            }

            // Pop arguments
            var arg_regs = try func.allocator.alloc(u16, method_call.arg_count);
            defer func.allocator.free(arg_regs);

            var i: usize = method_call.arg_count;
            while (i > 0) : (i -= 1) {
                arg_regs[i - 1] = stack.pop();
            }

            // Pop instance
            const instance_reg = stack.pop();

            const dst_reg = next_reg.*;
            next_reg.* += 1;
            try stack.push(dst_reg);

            const opcode = if (method_call.is_super) bytecode.OpCode.CallSuperMethod else bytecode.OpCode.CallMethod;
            try func.instructions.append(.{
                .op = opcode,
                .dst = dst_reg,
                .src1 = instance_reg,
                .var_name = try func.allocator.dupe(u8, method_call.method_name),
                .immediate = types.Value{ .Int = @intCast(method_call.arg_count) },
            });
            // std.debug.print("  -> Generated: CallMethod r{} = r{}.{s}({} args)\n", .{ dst_reg, instance_reg, method_call.method_name, method_call.arg_count });
        },
        .CreateNamespace => {
            // Pop namespace name from stack
            const name_reg = stack.pop();
            const dst_reg = next_reg.*;
            next_reg.* += 1;
            
            try func.instructions.append(.{
                .op = bytecode.OpCode.CreateNamespace,
                .dst = dst_reg,
                .src1 = name_reg,
            });
            
            try stack.push(dst_reg);
        },
        .PushNamespace => {
            // Pop namespace from stack and push to namespace context
            const ns_reg = stack.pop();
            
            try func.instructions.append(.{
                .op = bytecode.OpCode.PushNamespace,
                .src1 = ns_reg,
            });
        },
        .PopNamespace => {
            // Pop namespace from context and push to stack
            const dst_reg = next_reg.*;
            next_reg.* += 1;
            
            try func.instructions.append(.{
                .op = bytecode.OpCode.PopNamespace,
                .dst = dst_reg,
            });
            
            try stack.push(dst_reg);
        },
        .TryStart => |catch_target| {
            // Start of try block with catch target
            try func.instructions.append(.{
                .op = bytecode.OpCode.TryStart,
                .jump_target = @intCast(catch_target),
            });
        },
        .TryEnd => {
            // End of try/catch/finally block
            try func.instructions.append(.{
                .op = bytecode.OpCode.TryEnd,
            });
        },
        .Throw => {
            // Throw exception from stack
            if (stack.len() < 1) {
                std.debug.print("ERROR: Throw needs 1 operand but stack is empty\n", .{});
                return error.StackUnderflow;
            }
            
            const error_reg = stack.pop();
            try func.instructions.append(.{
                .op = bytecode.OpCode.Throw,
                .src1 = error_reg,
            });
        },
        .LoadException => {
            // Load current exception onto stack
            const dst_reg = next_reg.*;
            next_reg.* += 1;
            
            try func.instructions.append(.{
                .op = bytecode.OpCode.LoadException,
                .dst = dst_reg,
            });
            
            try stack.push(dst_reg);
        },
        .ClearException => {
            // Clear current exception
            try func.instructions.append(.{
                .op = bytecode.OpCode.ClearException,
            });
        },
        .CreateCallback => {
            // Create a C callback wrapper from function and signature on stack
            if (stack.len() < 2) {
                std.debug.print("ERROR: CreateCallback needs 2 operands but stack has {}\n", .{stack.len()});
                return error.StackUnderflow;
            }
            
            const sig_reg = stack.pop(); // Signature (string or nil)
            const func_reg = stack.pop(); // Function
            const dst_reg = next_reg.*;
            next_reg.* += 1;
            
            try func.instructions.append(.{
                .op = bytecode.OpCode.CreateCallback,
                .dst = dst_reg,
                .src1 = func_reg,
                .src2 = sig_reg,
            });
            
            try stack.push(dst_reg);
        },

        .IsArray => {
            if (stack.len() < 1) {
                std.debug.print("ERROR: IsArray needs 1 operand but stack is empty\n", .{});
                return error.StackUnderflow;
            }

            const src_reg = stack.pop();
            const dst_reg = next_reg.*;
            next_reg.* += 1;
            try stack.push(dst_reg);

            try func.instructions.append(.{
                .op = bytecode.OpCode.IsArray,
                .dst = dst_reg,
                .src1 = src_reg,
            });
        },

        .IsMap => {
            if (stack.len() < 1) {
                std.debug.print("ERROR: IsMap needs 1 operand but stack is empty\n", .{});
                return error.StackUnderflow;
            }

            const src_reg = stack.pop();
            const dst_reg = next_reg.*;
            next_reg.* += 1;
            try stack.push(dst_reg);

            try func.instructions.append(.{
                .op = bytecode.OpCode.IsMap,
                .dst = dst_reg,
                .src1 = src_reg,
            });
        },
        
        .CreateModule => {
            // Pop module name from stack
            const name_reg = stack.pop();
            const dst_reg = next_reg.*;
            next_reg.* += 1;
            
            try func.instructions.append(.{
                .op = bytecode.OpCode.CreateModule,
                .dst = dst_reg,
                .src1 = name_reg,
            });
            
            try stack.push(dst_reg);
        },
        
        .PushModule => {
            // Push module onto context stack
            const module_reg = stack.pop();
            try func.instructions.append(.{
                .op = bytecode.OpCode.PushModule,
                .src1 = module_reg,
            });
        },
        
        .PopModule => {
            // Pop module from context stack
            const dst_reg = next_reg.*;
            next_reg.* += 1;
            
            try func.instructions.append(.{
                .op = bytecode.OpCode.PopModule,
                .dst = dst_reg,
            });
            
            try stack.push(dst_reg);
        },
        
        .MarkExport => {
            // Mark name for export (name is on stack)
            const name_reg = stack.pop();
            try func.instructions.append(.{
                .op = bytecode.OpCode.MarkExport,
                .src1 = name_reg,
            });
        },
        
        .Export => {
            // Export value with name (both on stack)
            if (stack.len() < 2) {
                std.debug.print("ERROR: Export needs 2 operands but stack has {}\n", .{stack.len()});
                return error.StackUnderflow;
            }
            
            const name_reg = stack.pop();
            const value_reg = stack.pop();
            
            try func.instructions.append(.{
                .op = bytecode.OpCode.Export,
                .src1 = value_reg,
                .src2 = name_reg,
            });
        },
    }
}
