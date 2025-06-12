const std = @import("std");
const types = @import("../core/types.zig");
const debug = @import("../core/debug.zig");
const bytecode = @import("bytecode.zig");

pub const VMError = error{
    StackOverflow,
    StackUnderflow,
    TypeMismatch,
    UndefinedVariable,
    UnsupportedInstruction,
    ExpectedLParen,
    ExpectedRParen,
    NoReturnAddress,
    ArgumentCountMismatch,
    UnknownFunction,
    UnsupportedFunction,
    NotImplemented,
    DivisionByZero,
} || std.mem.Allocator.Error || std.fs.File.WriteError;

pub const CallFrame = struct {
    caller_func: ?*const bytecode.Function, // Calling function to restore to
    pc: usize, // Program counter within the function
    register_base: u16, // Base register index for this frame
    prev_register_base: u16, // Previous frame's register base
    return_addr: usize, // Return address (instruction index to return to)
    return_reg: u16, // Register where the return value should be stored

    pub fn init(caller_func: ?*const bytecode.Function, register_base: u16, prev_register_base: u16, return_addr: usize, return_reg: u16) CallFrame {
        return CallFrame{
            .caller_func = caller_func,
            .pc = 0,
            .register_base = register_base,
            .prev_register_base = prev_register_base,
            .return_addr = return_addr,
            .return_reg = return_reg,
        };
    }
};

pub const VM = struct {
    allocator: std.mem.Allocator,
    stdout: std.fs.File.Writer,
    registers: std.ArrayList(types.Value), // Register file
    variables: std.StringArrayHashMap(types.Value), // Global variables
    call_frames: std.ArrayList(CallFrame),
    current_func: ?*const bytecode.Function, // Current function being executed
    pc: usize, // Program counter
    current_register_base: u16, // Current frame's register base
    next_free_register: u16, // Next available register for allocation
    allocated_functions: std.ArrayList(*bytecode.Function), // Track allocated function objects for cleanup
    function_called: bool, // Flag to indicate if a function was just called

    pub fn init(allocator: std.mem.Allocator, stdout: std.fs.File.Writer) VM {

        // Initialize variables with builtins
        var variables = std.StringArrayHashMap(types.Value).init(allocator);

        // Store the print function as a built-in operator
        variables.put("print", .{ .BuiltinOperator = .Print }) catch unreachable;

        // Store built-in operators
        // Arithmetic operators
        variables.put("+", .{ .BuiltinOperator = .Add }) catch unreachable;
        variables.put("-", .{ .BuiltinOperator = .Sub }) catch unreachable;
        variables.put("*", .{ .BuiltinOperator = .Mul }) catch unreachable;
        variables.put("/", .{ .BuiltinOperator = .Div }) catch unreachable;
        variables.put("%", .{ .BuiltinOperator = .Mod }) catch unreachable;
        variables.put("**", .{ .BuiltinOperator = .Pow }) catch unreachable;

        // Comparison operators
        variables.put("==", .{ .BuiltinOperator = .Eq }) catch unreachable;
        variables.put("!=", .{ .BuiltinOperator = .Ne }) catch unreachable;
        variables.put("<", .{ .BuiltinOperator = .LessThan }) catch unreachable;
        variables.put(">", .{ .BuiltinOperator = .GreaterThan }) catch unreachable;
        variables.put("<=", .{ .BuiltinOperator = .LessEqual }) catch unreachable;
        variables.put(">=", .{ .BuiltinOperator = .GreaterEqual }) catch unreachable;

        // Logical operators
        variables.put("&&", .{ .BuiltinOperator = .And }) catch unreachable;
        variables.put("||", .{ .BuiltinOperator = .Or }) catch unreachable;
        variables.put("!", .{ .BuiltinOperator = .Not }) catch unreachable;

        // Bitwise operators
        variables.put("&", .{ .BuiltinOperator = .BitAnd }) catch unreachable;
        variables.put("|", .{ .BuiltinOperator = .BitOr }) catch unreachable;
        variables.put("^", .{ .BuiltinOperator = .BitXor }) catch unreachable;
        variables.put("~", .{ .BuiltinOperator = .BitNot }) catch unreachable;
        variables.put("<<", .{ .BuiltinOperator = .Shl }) catch unreachable;
        variables.put(">>", .{ .BuiltinOperator = .Shr }) catch unreachable;

        // String operators
        variables.put("++", .{ .BuiltinOperator = .Concat }) catch unreachable;

        // Built-in functions
        variables.put("len", .{ .BuiltinOperator = .Len }) catch unreachable;
        variables.put("type", .{ .BuiltinOperator = .Type }) catch unreachable;

        return VM{
            .allocator = allocator,
            .stdout = stdout,
            .registers = std.ArrayList(types.Value).init(allocator),
            .variables = variables,
            .call_frames = std.ArrayList(CallFrame).init(allocator),
            .current_func = null,
            .pc = 0,
            .current_register_base = 0,
            .next_free_register = 0,
            .allocated_functions = std.ArrayList(*bytecode.Function).init(allocator),
            .function_called = false,
        };
    }

    pub fn deinit(self: *VM) void {
        self.call_frames.deinit();

        for (self.allocated_functions.items) |func| {
            func.deinit();
            self.allocator.destroy(func);
        }
        self.allocated_functions.deinit();

        for (self.registers.items) |*value| {
            value.deinit(self.allocator);
        }
        self.registers.deinit();

        var it = self.variables.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.deinit(self.allocator);
            // Don't free the key here, as it's owned by the Variable value
            // self.allocator.free(entry.key_ptr.*);
        }
        self.variables.deinit();

        // Clean up current function if it exists
        // Note: We don't deinit the function itself as it's owned by the caller
        // or has already been cleaned up as part of the stack or variables
        self.current_func = null;
    }

    pub fn setVariable(self: *VM, name: []const u8, value: types.Value) !void {
        try self.variables.put(name, value);
    }

    pub fn execute(self: *VM, func: *const bytecode.Function) VMError!void {
        debug.log("Executing function: {s}", .{func.name});
        self.current_func = func;
        self.pc = 0;

        // Print all instructions before execution
        debug.log("Instructions:", .{});
        for (func.instructions.items, 0..) |instr, i| {
            debug.log("  [{d}] {s}", .{ i, @tagName(instr.op) });
            if (instr.immediate) |immediate| {
                switch (immediate) {
                    .Int => |val| debug.log("    Immediate: Int {d}", .{val}),
                    .String => |val| debug.log("    Immediate: String \"{s}\"", .{val}),
                    else => debug.log("    Immediate: Other", .{}),
                }
            }
            if (instr.var_name) |var_name| {
                debug.log("    Variable: {s}", .{var_name});
            }
        }

        while (true) {
            // Use current_func if available, otherwise use the original func
            const executing_func = self.current_func orelse func;

            // Check if we're done with the current function
            debug.log("VM loop: pc={}, instruction_count={}", .{self.pc, executing_func.instructions.items.len});
            if (self.pc >= executing_func.instructions.items.len) {
                debug.log("VM: PC {} >= instruction count {}, checking call frames", .{self.pc, executing_func.instructions.items.len});
                // If we have no call frames, we're done with the program
                if (self.call_frames.items.len == 0) {
                    break;
                }

                // Otherwise, return to the caller
                const frame = self.call_frames.items[self.call_frames.items.len - 1];
                _ = self.call_frames.pop();
                self.current_func = frame.caller_func;
                self.pc = frame.return_addr;
                self.current_register_base = frame.prev_register_base;
                continue;
            }

            // Execute the current instruction
            const instruction = executing_func.instructions.items[self.pc];
            self.function_called = false; // Reset the flag
            try self.executeInstruction(instruction);

            // Only increment PC if a function wasn't called
            if (!self.function_called) {
                self.pc += 1;
            }
        }
    }

    // Register management functions
    fn getRegister(self: *VM, reg: bytecode.Reg) VMError!types.Value {
        const absolute_reg = self.current_register_base + reg;
        if (absolute_reg >= self.registers.items.len) {
            return error.StackUnderflow; // Reusing this error for register bounds
        }
        return try self.registers.items[absolute_reg].clone(self.allocator);
    }

    fn setRegister(self: *VM, reg: bytecode.Reg, value: types.Value) VMError!void {
        const absolute_reg = self.current_register_base + reg;

        // Extend register file if needed
        while (absolute_reg >= self.registers.items.len) {
            try self.registers.append(.{ .Nil = {} });
        }

        // Clean up old value
        self.registers.items[absolute_reg].deinit(self.allocator);
        self.registers.items[absolute_reg] = value;
    }

    fn allocateRegisters(self: *VM, count: u16) VMError!u16 {
        const base = self.next_free_register;

        debug.log("Allocating {} registers at base {} (current len {})", .{ count, base, self.registers.items.len });

        // Extend register file if needed
        while (self.next_free_register + count > self.registers.items.len) {
            try self.registers.append(.{ .Nil = {} });
        }

        self.next_free_register += count;
        debug.log("New register count: {}", .{self.registers.items.len});
        return base;
    }

    fn executeInstruction(self: *VM, instruction: bytecode.Instruction) VMError!void {
        switch (instruction.op) {
            .Add => {
                // Register-based addition: Add Rs1, Rs2 -> Rd
                const src1_reg = instruction.src1 orelse return error.UnsupportedInstruction;
                const src2_reg = instruction.src2 orelse return error.UnsupportedInstruction;
                const dst_reg = instruction.dst orelse return error.UnsupportedInstruction;

                debug.log("Add instruction: R{} = R{} + R{}", .{ dst_reg, src1_reg, src2_reg });

                var left = try self.getRegister(src1_reg);
                defer left.deinit(self.allocator);
                var right = try self.getRegister(src2_reg);
                defer right.deinit(self.allocator);

                // Handle addition based on types
                const result = switch (left) {
                    .Int => |left_val| switch (right) {
                        .Int => |right_val| types.Value{ .Int = left_val + right_val },
                        .Float => |right_val| types.Value{ .Float = @as(f64, @floatFromInt(left_val)) + right_val },
                        else => {
                            debug.log("TypeMismatch in Add: left={}, right={}", .{ left, right });
                            return error.TypeMismatch;
                        },
                    },
                    .Float => |left_val| switch (right) {
                        .Int => |right_val| types.Value{ .Float = left_val + @as(f64, @floatFromInt(right_val)) },
                        .Float => |right_val| types.Value{ .Float = left_val + right_val },
                        else => {
                            debug.log("TypeMismatch in Add: left={}, right={}", .{ left, right });
                            return error.TypeMismatch;
                        },
                    },
                    .String => |left_val| switch (right) {
                        .String => |right_val| blk: {
                            const concat_result = try std.fmt.allocPrint(self.allocator, "{s}{s}", .{ left_val, right_val });
                            break :blk types.Value{ .String = concat_result };
                        },
                        else => {
                            debug.log("TypeMismatch in Add: left={}, right={}", .{ left, right });
                            return error.TypeMismatch;
                        },
                    },
                    else => {
                        debug.log("TypeMismatch in Add: left={}, right={}", .{ left, right });
                        return error.TypeMismatch;
                    },
                };

                try self.setRegister(dst_reg, result);
            },
            .LoadConst => {
                // Register-based LoadConst: LoadConst Rd, #constant
                const dst_reg = instruction.dst orelse return error.UnsupportedInstruction;
                const value = instruction.immediate orelse return error.UnsupportedInstruction;

                debug.log("LoadConst: R{} = {any}", .{ dst_reg, value });

                // For functions, don't clone - just use the reference directly
                const reg_value = if (value == .Function)
                    value
                else
                    try value.clone(self.allocator);

                try self.setRegister(dst_reg, reg_value);
            },
            .LoadVar => {
                // Register-based LoadVar: LoadVar Rd, var_name
                const dst_reg = instruction.dst orelse return error.UnsupportedInstruction;
                const name = instruction.var_name orelse return error.UnsupportedInstruction;

                debug.log("LoadVar: R{} = {s}", .{ dst_reg, name });

                // First check if it's a function parameter
                if (self.current_func) |current_func| {
                    debug.log("Checking parameters in current function: {s}", .{current_func.name});
                    debug.log("Current function has {} parameters", .{current_func.param_count});

                    // For simplicity, we'll handle the first parameter as 'n' for now
                    // In a real implementation, we'd have a mapping of parameter names
                    if ((std.mem.eql(u8, name, "n") or std.mem.eql(u8, name, "x")) and current_func.param_count > 0) {
                        // Parameters are stored in the first registers of the frame
                        const param_reg: u16 = 0; // First parameter in register 0 of current frame
                        const param_value = try self.getRegister(param_reg);
                        debug.log("Found parameter '{s}' in R{}: {any}", .{ name, param_reg, param_value });
                        try self.setRegister(dst_reg, param_value);
                        return;
                    }
                }

                // Check for built-in operators and global variables
                if (self.variables.get(name)) |value| {
                    debug.log("Found variable in global scope: {any}", .{value});
                    try self.setRegister(dst_reg, try value.clone(self.allocator));
                } else {
                    debug.log("ERROR: Variable {s} not found", .{name});
                    return error.UndefinedVariable;
                }
            },
            .LoadParam => {
                // Register-based LoadParam: LoadParam Rd, #param_index
                const dst_reg = instruction.dst orelse return error.UnsupportedInstruction;
                const param_index = if (instruction.immediate) |imm| switch (imm) {
                    .Int => |idx| @as(u16, @intCast(idx)),
                    else => return error.TypeMismatch,
                } else return error.UnsupportedInstruction;

                debug.log("LoadParam: R{} = param[{}]", .{ dst_reg, param_index });

                // Parameters are stored in the first registers of the current frame
                const param_reg = param_index;
                debug.log("Fetching parameter register {} (base {} + index {})", .{ param_reg, self.current_register_base, param_index });
                const param_value = try self.getRegister(param_reg);
                debug.log("Loaded parameter from R{}: {any}", .{ self.current_register_base + param_reg, param_value });

                try self.setRegister(dst_reg, param_value);
            },
            .StoreVar => {
                // Register-based StoreVar: StoreVar var_name, Rs
                const src_reg = instruction.src1 orelse return error.UnsupportedInstruction;
                const name = instruction.var_name orelse return error.UnsupportedInstruction;

                debug.log("StoreVar: {s} = R{}", .{ name, src_reg });

                // Get the value from the source register
                const value = try self.getRegister(src_reg);

                // Store the value in the variables map
                // Check if the variable already exists
                if (self.variables.getIndex(name)) |idx| {
                    // Free the old value
                    var old_value_copy = self.variables.values()[idx];
                    old_value_copy.deinit(self.allocator);

                    // Update the value in place
                    self.variables.values()[idx] = value;
                } else {
                    // No existing key, add a new entry
                    try self.variables.put(name, value);
                }

                debug.log("Stored variable {s} = {any}", .{ name, value });
            },
            .StoreGlobal => {
                // Register-based StoreGlobal: StoreGlobal global_name, Rs
                const src_reg = instruction.src1 orelse return error.UnsupportedInstruction;
                const name = instruction.var_name orelse return error.UnsupportedInstruction;

                debug.log("StoreGlobal: {s} = R{}", .{ name, src_reg });

                // Get the value from the source register
                const value = try self.getRegister(src_reg);

                // Store the value in the variables map (same as StoreVar)
                if (self.variables.getIndex(name)) |idx| {
                    // Free the old value
                    var old_value_copy = self.variables.values()[idx];
                    old_value_copy.deinit(self.allocator);

                    // Update the value in place
                    self.variables.values()[idx] = value;
                } else {
                    // No existing key, add a new entry
                    try self.variables.put(name, value);
                }

                debug.log("Stored global variable {s} = {any}", .{ name, value });
            },
            .Move => {
                // Register-based Move: Move Rd, Rs
                const dst_reg = instruction.dst orelse return error.UnsupportedInstruction;
                const src_reg = instruction.src1 orelse return error.UnsupportedInstruction;

                debug.log("Move: R{} = R{}", .{ dst_reg, src_reg });

                const value = try self.getRegister(src_reg);
                try self.setRegister(dst_reg, value);
            },
            .Sub => {
                // Register-based subtraction: Sub Rs1, Rs2 -> Rd
                const src1_reg = instruction.src1 orelse return error.UnsupportedInstruction;
                const src2_reg = instruction.src2 orelse return error.UnsupportedInstruction;
                const dst_reg = instruction.dst orelse return error.UnsupportedInstruction;

                debug.log("Sub instruction: R{} = R{} - R{}", .{ dst_reg, src1_reg, src2_reg });

                var left = try self.getRegister(src1_reg);
                defer left.deinit(self.allocator);
                var right = try self.getRegister(src2_reg);
                defer right.deinit(self.allocator);

                // Handle subtraction based on types
                const result = switch (left) {
                    .Int => |left_val| switch (right) {
                        .Int => |right_val| types.Value{ .Int = left_val - right_val },
                        .Float => |right_val| types.Value{ .Float = @as(f64, @floatFromInt(left_val)) - right_val },
                        else => {
                            debug.log("TypeMismatch in Sub: left={}, right={}", .{ left, right });
                            return error.TypeMismatch;
                        },
                    },
                    .Float => |left_val| switch (right) {
                        .Int => |right_val| types.Value{ .Float = left_val - @as(f64, @floatFromInt(right_val)) },
                        .Float => |right_val| types.Value{ .Float = left_val - right_val },
                        else => {
                            debug.log("TypeMismatch in Sub: left={}, right={}", .{ left, right });
                            return error.TypeMismatch;
                        },
                    },
                    else => {
                        debug.log("TypeMismatch in Sub: left={}, right={}", .{ left, right });
                        return error.TypeMismatch;
                    },
                };

                try self.setRegister(dst_reg, result);
            },
            .Mul => {
                // Register-based multiplication: Mul Rs1, Rs2 -> Rd
                const src1_reg = instruction.src1 orelse return error.UnsupportedInstruction;
                const src2_reg = instruction.src2 orelse return error.UnsupportedInstruction;
                const dst_reg = instruction.dst orelse return error.UnsupportedInstruction;

                debug.log("Mul instruction: R{} = R{} * R{}", .{ dst_reg, src1_reg, src2_reg });

                var left = try self.getRegister(src1_reg);
                defer left.deinit(self.allocator);
                var right = try self.getRegister(src2_reg);
                defer right.deinit(self.allocator);

                // Handle multiplication based on types
                const result = switch (left) {
                    .Int => |left_val| switch (right) {
                        .Int => |right_val| types.Value{ .Int = left_val * right_val },
                        .Float => |right_val| types.Value{ .Float = @as(f64, @floatFromInt(left_val)) * right_val },
                        else => {
                            debug.log("TypeMismatch in Mul: left={}, right={}", .{ left, right });
                            return error.TypeMismatch;
                        },
                    },
                    .Float => |left_val| switch (right) {
                        .Int => |right_val| types.Value{ .Float = left_val * @as(f64, @floatFromInt(right_val)) },
                        .Float => |right_val| types.Value{ .Float = left_val * right_val },
                        else => {
                            debug.log("TypeMismatch in Mul: left={}, right={}", .{ left, right });
                            return error.TypeMismatch;
                        },
                    },
                    else => {
                        debug.log("TypeMismatch in Mul: left={}, right={}", .{ left, right });
                        return error.TypeMismatch;
                    },
                };

                try self.setRegister(dst_reg, result);
            },
            .Div => {
                // Register-based division: Div Rs1, Rs2 -> Rd
                const src1_reg = instruction.src1 orelse return error.UnsupportedInstruction;
                const src2_reg = instruction.src2 orelse return error.UnsupportedInstruction;
                const dst_reg = instruction.dst orelse return error.UnsupportedInstruction;

                debug.log("Div instruction: R{} = R{} / R{}", .{ dst_reg, src1_reg, src2_reg });

                var left = try self.getRegister(src1_reg);
                defer left.deinit(self.allocator);
                var right = try self.getRegister(src2_reg);
                defer right.deinit(self.allocator);

                // Handle division based on types
                const result = switch (left) {
                    .Int => |left_val| switch (right) {
                        .Int => |right_val| blk: {
                            if (right_val == 0) {
                                debug.log("Division by zero in Div instruction", .{});
                                return error.DivisionByZero;
                            }
                            break :blk types.Value{ .Int = @divTrunc(left_val, right_val) };
                        },
                        .Float => |right_val| blk: {
                            if (right_val == 0.0) {
                                debug.log("Division by zero in Div instruction", .{});
                                return error.DivisionByZero;
                            }
                            break :blk types.Value{ .Float = @as(f64, @floatFromInt(left_val)) / right_val };
                        },
                        else => {
                            debug.log("TypeMismatch in Div: left={}, right={}", .{ left, right });
                            return error.TypeMismatch;
                        },
                    },
                    .Float => |left_val| switch (right) {
                        .Int => |right_val| blk: {
                            if (right_val == 0) {
                                debug.log("Division by zero in Div instruction", .{});
                                return error.DivisionByZero;
                            }
                            break :blk types.Value{ .Float = left_val / @as(f64, @floatFromInt(right_val)) };
                        },
                        .Float => |right_val| blk: {
                            if (right_val == 0.0) {
                                debug.log("Division by zero in Div instruction", .{});
                                return error.DivisionByZero;
                            }
                            break :blk types.Value{ .Float = left_val / right_val };
                        },
                        else => {
                            debug.log("TypeMismatch in Div: left={}, right={}", .{ left, right });
                            return error.TypeMismatch;
                        },
                    },
                    else => {
                        debug.log("TypeMismatch in Div: left={}, right={}", .{ left, right });
                        return error.TypeMismatch;
                    },
                };

                try self.setRegister(dst_reg, result);
            },
            .Lt => {
                // Register-based less than: Lt Rs1, Rs2 -> Rd
                const src1_reg = instruction.src1 orelse return error.UnsupportedInstruction;
                const src2_reg = instruction.src2 orelse return error.UnsupportedInstruction;
                const dst_reg = instruction.dst orelse return error.UnsupportedInstruction;

                debug.log("Lt instruction: R{} = R{} < R{}", .{ dst_reg, src1_reg, src2_reg });

                var left = try self.getRegister(src1_reg);
                defer left.deinit(self.allocator);
                var right = try self.getRegister(src2_reg);
                defer right.deinit(self.allocator);

                // Handle comparison based on types
                const result = switch (left) {
                    .Int => |left_val| switch (right) {
                        .Int => |right_val| left_val < right_val,
                        .Float => |right_val| @as(f64, @floatFromInt(left_val)) < right_val,
                        else => {
                            debug.log("TypeMismatch in Lt: left={}, right={}", .{ left, right });
                            return error.TypeMismatch;
                        },
                    },
                    .Float => |left_val| switch (right) {
                        .Int => |right_val| left_val < @as(f64, @floatFromInt(right_val)),
                        .Float => |right_val| left_val < right_val,
                        else => {
                            debug.log("TypeMismatch in Lt: left={}, right={}", .{ left, right });
                            return error.TypeMismatch;
                        },
                    },
                    else => {
                        debug.log("TypeMismatch in Lt: left={}, right={}", .{ left, right });
                        return error.TypeMismatch;
                    },
                };

                try self.setRegister(dst_reg, .{ .Bool = result });
            },
            .Gt => {
                // Register-based greater than: Gt Rs1, Rs2 -> Rd
                const src1_reg = instruction.src1 orelse return error.UnsupportedInstruction;
                const src2_reg = instruction.src2 orelse return error.UnsupportedInstruction;
                const dst_reg = instruction.dst orelse return error.UnsupportedInstruction;

                debug.log("Gt instruction: R{} = R{} > R{}", .{ dst_reg, src1_reg, src2_reg });

                var left = try self.getRegister(src1_reg);
                defer left.deinit(self.allocator);
                var right = try self.getRegister(src2_reg);
                defer right.deinit(self.allocator);

                // Handle comparison based on types
                const result = switch (left) {
                    .Int => |left_val| switch (right) {
                        .Int => |right_val| left_val > right_val,
                        .Float => |right_val| @as(f64, @floatFromInt(left_val)) > right_val,
                        else => {
                            debug.log("TypeMismatch in Gt: left={}, right={}", .{ left, right });
                            return error.TypeMismatch;
                        },
                    },
                    .Float => |left_val| switch (right) {
                        .Int => |right_val| left_val > @as(f64, @floatFromInt(right_val)),
                        .Float => |right_val| left_val > right_val,
                        else => {
                            debug.log("TypeMismatch in Gt: left={}, right={}", .{ left, right });
                            return error.TypeMismatch;
                        },
                    },
                    else => {
                        debug.log("TypeMismatch in Gt: left={}, right={}", .{ left, right });
                        return error.TypeMismatch;
                    },
                };

                try self.setRegister(dst_reg, .{ .Bool = result });
            },
            .Eq => {
                // Register-based equality: Eq Rs1, Rs2 -> Rd
                const src1_reg = instruction.src1 orelse return error.UnsupportedInstruction;
                const src2_reg = instruction.src2 orelse return error.UnsupportedInstruction;
                const dst_reg = instruction.dst orelse return error.UnsupportedInstruction;

                debug.log("Eq instruction: R{} = R{} == R{}", .{ dst_reg, src1_reg, src2_reg });

                var left = try self.getRegister(src1_reg);
                defer left.deinit(self.allocator);
                var right = try self.getRegister(src2_reg);
                defer right.deinit(self.allocator);

                // Handle equality comparison based on types
                const result = switch (left) {
                    .Int => |left_val| switch (right) {
                        .Int => |right_val| left_val == right_val,
                        .Float => |right_val| @as(f64, @floatFromInt(left_val)) == right_val,
                        else => false, // Different types are never equal
                    },
                    .Float => |left_val| switch (right) {
                        .Int => |right_val| left_val == @as(f64, @floatFromInt(right_val)),
                        .Float => |right_val| left_val == right_val,
                        else => false, // Different types are never equal
                    },
                    .String => |left_val| switch (right) {
                        .String => |right_val| std.mem.eql(u8, left_val, right_val),
                        else => false, // Different types are never equal
                    },
                    .Bool => |left_val| switch (right) {
                        .Bool => |right_val| left_val == right_val,
                        else => false, // Different types are never equal
                    },
                    .Nil => switch (right) {
                        .Nil => true, // nil == nil
                        else => false, // nil is not equal to any other type
                    },
                    else => false, // Other types not supported for equality comparison yet
                };

                try self.setRegister(dst_reg, .{ .Bool = result });
            },
            .Print => {
                // Register-based print: Print Rs
                const src_reg = instruction.src1 orelse return error.UnsupportedInstruction;

                debug.log("Print instruction: print R{}", .{src_reg});

                var value = try self.getRegister(src_reg);
                defer value.deinit(self.allocator);

                switch (value) {
                    .Int => |val| try self.stdout.print("{d}\n", .{val}),
                    .String => |str| try self.stdout.print("{s}\n", .{str}),
                    .Symbol => |sym| try self.stdout.print("{s}\n", .{sym}),
                    .Bool => |b| try self.stdout.print("{}\n", .{b}),
                    .Float => |f| try self.stdout.print("{d}\n", .{f}),
                    .Nil => try self.stdout.print("nil\n", .{}),
                    .Array => |arr| try self.stdout.print("{any}\n", .{arr}),
                    .Map => |map| try self.stdout.print("{any}\n", .{map}),
                    .ReturnAddress => |addr| try self.stdout.print("ReturnAddress: {any}\n", .{addr}),
                    .Function => |func| try self.stdout.print("Function: {any}\n", .{func}),
                    .Variable => |var_val| try self.stdout.print("Variable: {s}\n", .{var_val.name}),
                    .BuiltinOperator => |op| try self.stdout.print("BuiltinOperator: {any}\n", .{op}),
                }
            },
            .Call => {
                // Register-based function call: Call function_reg, [R0, R1, ...] -> Rd
                debug.log("Call operation", .{});
                const func_reg = instruction.src1 orelse return error.UnsupportedInstruction;
                const dst_reg = instruction.dst orelse return error.UnsupportedInstruction;
                const arg_count = if (instruction.immediate) |imm| switch (imm) {
                    .Int => @as(usize, @intCast(imm.Int)),
                    else => return error.TypeMismatch,
                } else return error.UnsupportedInstruction;

                debug.log("Register-based call: R{} = call R{} with {} args -> R{}", .{ dst_reg, func_reg, arg_count, dst_reg });

                // Get function from register
                var func_val = try self.getRegister(func_reg);
                defer func_val.deinit(self.allocator);
                debug.logValue(func_val);

                // Handle built-in functions first
                if (func_val == .BuiltinOperator) {
                    const builtin_op = func_val.BuiltinOperator;
                    
                    // Handle print function
                    if (builtin_op == .Print) {
                        // Print can take any number of arguments
                        for (0..arg_count) |i| {
                            // For single argument calls, the argument might be in the previous register
                            // (result of a previous operation)
                            const arg_reg = if (arg_count == 1) 
                                dst_reg - 1  // Previous register (result of previous call)
                            else 
                                func_reg + 1 + @as(u16, @intCast(i)); // Arguments follow function register
                            var arg = try self.getRegister(arg_reg);
                            defer arg.deinit(self.allocator);

                            // Print without newline except for the last argument
                            switch (arg) {
                                .Int => |val| try self.stdout.print("{d}", .{val}),
                                .String => |str| try self.stdout.print("{s}", .{str}),
                                .Symbol => |sym| try self.stdout.print("{s}", .{sym}),
                                .Bool => |b| try self.stdout.print("{}", .{b}),
                                .Float => |f| try self.stdout.print("{d}", .{f}),
                                .Nil => try self.stdout.print("nil", .{}),
                                .Array => |arr| try self.stdout.print("{any}", .{arr}),
                                .Map => |map| try self.stdout.print("{any}", .{map}),
                                .ReturnAddress => |addr| try self.stdout.print("ReturnAddress: {any}", .{addr}),
                                .Function => |func| try self.stdout.print("Function: {s}", .{func.name}),
                                .Variable => |var_name| try self.stdout.print("{s}", .{var_name.name}),
                                .BuiltinOperator => |op| try self.stdout.print("BuiltinOperator: {any}", .{op}),
                            }

                            // Add space between arguments except for the last one
                            if (i < arg_count - 1) {
                                try self.stdout.print(" ", .{});
                            }
                        }

                        // Add newline after all arguments
                        try self.stdout.print("\n", .{});

                        // Print function returns nil
                        try self.setRegister(dst_reg, .{ .Nil = {} });
                        return;
                    }
                    
                    // Handle add operator
                    if (builtin_op == .Add) {
                        if (arg_count != 2) {
                            debug.log("Add operator requires exactly 2 arguments, got {}", .{arg_count});
                            return error.ArgumentCountMismatch;
                        }
                        
                        // Get the two arguments
                        var left = try self.getRegister(func_reg + 1);
                        defer left.deinit(self.allocator);
                        var right = try self.getRegister(func_reg + 2);
                        defer right.deinit(self.allocator);
                        
                        // Perform addition based on types
                        const result = switch (left) {
                            .Int => |left_val| switch (right) {
                                .Int => |right_val| types.Value{ .Int = left_val + right_val },
                                .Float => |right_val| types.Value{ .Float = @as(f64, @floatFromInt(left_val)) + right_val },
                                else => {
                                    debug.log("TypeMismatch in Add: left={}, right={}", .{ left, right });
                                    return error.TypeMismatch;
                                },
                            },
                            .Float => |left_val| switch (right) {
                                .Int => |right_val| types.Value{ .Float = left_val + @as(f64, @floatFromInt(right_val)) },
                                .Float => |right_val| types.Value{ .Float = left_val + right_val },
                                else => {
                                    debug.log("TypeMismatch in Add: left={}, right={}", .{ left, right });
                                    return error.TypeMismatch;
                                },
                            },
                            .String => |left_val| switch (right) {
                                .String => |right_val| blk: {
                                    const concat_result = try std.fmt.allocPrint(self.allocator, "{s}{s}", .{ left_val, right_val });
                                    break :blk types.Value{ .String = concat_result };
                                },
                                else => {
                                    debug.log("TypeMismatch in Add: left={}, right={}", .{ left, right });
                                    return error.TypeMismatch;
                                },
                            },
                            else => {
                                debug.log("TypeMismatch in Add: left={}, right={}", .{ left, right });
                                return error.TypeMismatch;
                            },
                        };
                        
                        try self.setRegister(dst_reg, result);
                        return;
                    }
                    
                    // Handle subtract operator
                    if (builtin_op == .Sub) {
                        if (arg_count != 2) {
                            debug.log("Sub operator requires exactly 2 arguments, got {}", .{arg_count});
                            return error.ArgumentCountMismatch;
                        }
                        
                        // Get the two arguments
                        var left = try self.getRegister(func_reg + 1);
                        defer left.deinit(self.allocator);
                        var right = try self.getRegister(func_reg + 2);
                        defer right.deinit(self.allocator);
                        
                        // Perform subtraction based on types
                        const result = switch (left) {
                            .Int => |left_val| switch (right) {
                                .Int => |right_val| types.Value{ .Int = left_val - right_val },
                                .Float => |right_val| types.Value{ .Float = @as(f64, @floatFromInt(left_val)) - right_val },
                                else => {
                                    debug.log("TypeMismatch in Sub: left={}, right={}", .{ left, right });
                                    return error.TypeMismatch;
                                },
                            },
                            .Float => |left_val| switch (right) {
                                .Int => |right_val| types.Value{ .Float = left_val - @as(f64, @floatFromInt(right_val)) },
                                .Float => |right_val| types.Value{ .Float = left_val - right_val },
                                else => {
                                    debug.log("TypeMismatch in Sub: left={}, right={}", .{ left, right });
                                    return error.TypeMismatch;
                                },
                            },
                            else => {
                                debug.log("TypeMismatch in Sub: left={}, right={}", .{ left, right });
                                return error.TypeMismatch;
                            },
                        };
                        
                        try self.setRegister(dst_reg, result);
                        return;
                    }
                    
                    // Handle less than operator
                    if (builtin_op == .LessThan) {
                        if (arg_count != 2) {
                            debug.log("LessThan operator requires exactly 2 arguments, got {}", .{arg_count});
                            return error.ArgumentCountMismatch;
                        }
                        
                        // Get the two arguments
                        var left = try self.getRegister(func_reg + 1);
                        defer left.deinit(self.allocator);
                        var right = try self.getRegister(func_reg + 2);
                        defer right.deinit(self.allocator);
                        
                        // Perform comparison based on types
                        const result = switch (left) {
                            .Int => |left_val| switch (right) {
                                .Int => |right_val| left_val < right_val,
                                .Float => |right_val| @as(f64, @floatFromInt(left_val)) < right_val,
                                else => {
                                    debug.log("TypeMismatch in LessThan: left={}, right={}", .{ left, right });
                                    return error.TypeMismatch;
                                },
                            },
                            .Float => |left_val| switch (right) {
                                .Int => |right_val| left_val < @as(f64, @floatFromInt(right_val)),
                                .Float => |right_val| left_val < right_val,
                                else => {
                                    debug.log("TypeMismatch in LessThan: left={}, right={}", .{ left, right });
                                    return error.TypeMismatch;
                                },
                            },
                            else => {
                                debug.log("TypeMismatch in LessThan: left={}, right={}", .{ left, right });
                                return error.TypeMismatch;
                            },
                        };
                        
                        try self.setRegister(dst_reg, .{ .Bool = result });
                        return;
                    }

                    // Handle other built-in operators here...
                    debug.log("Unsupported built-in operator: {any}", .{builtin_op});
                    return error.UnsupportedFunction;
                }
                
                if (func_val == .Variable) {
                    const func_name = func_val.Variable.name;

                    // Handle print function by name (for backward compatibility)
                    if (std.mem.eql(u8, func_name, "print")) {
                        // Print can take any number of arguments
                        for (0..arg_count) |i| {
                            const arg_reg = func_reg + 1 + @as(u16, @intCast(i)); // Arguments follow function register
                            var arg = try self.getRegister(arg_reg);
                            defer arg.deinit(self.allocator);

                            // Print without newline except for the last argument
                            switch (arg) {
                                .Int => |val| try self.stdout.print("{d}", .{val}),
                                .String => |str| try self.stdout.print("{s}", .{str}),
                                .Symbol => |sym| try self.stdout.print("{s}", .{sym}),
                                .Bool => |b| try self.stdout.print("{}", .{b}),
                                .Float => |f| try self.stdout.print("{d}", .{f}),
                                .Nil => try self.stdout.print("nil", .{}),
                                .Array => |arr| try self.stdout.print("{any}", .{arr}),
                                .Map => |map| try self.stdout.print("{any}", .{map}),
                                .ReturnAddress => |addr| try self.stdout.print("ReturnAddress: {any}", .{addr}),
                                .Function => |func| try self.stdout.print("Function: {s}", .{func.name}),
                                .Variable => |var_name| try self.stdout.print("{s}", .{var_name.name}),
                                .BuiltinOperator => |op| try self.stdout.print("BuiltinOperator: {any}", .{op}),
                            }
                            
                            // Add space between arguments except for the last one
                            if (i < arg_count - 1) {
                                try self.stdout.print(" ", .{});
                            }
                        }
                        
                        // Add newline after all arguments
                        try self.stdout.print("\n", .{});
                        
                        // Print function returns nil
                        try self.setRegister(dst_reg, .{ .Nil = {} });
                        return;
                    }
                    
                    // Handle other built-in functions here...
                    debug.log("Unsupported built-in function: {s}", .{func_name});
                    return error.UnsupportedFunction;
                }

                // Handle user-defined functions
                if (func_val == .Function) {
                    const user_func = func_val.Function;
                    debug.log("Calling user-defined function: {s}", .{user_func.name});
                    
                    // Set up new call frame for the function
                    const new_register_base = self.next_free_register;
                    const frame = CallFrame.init(self.current_func, new_register_base, self.current_register_base, self.pc + 1, dst_reg);
                    try self.call_frames.append(frame);
                    
                    // Allocate registers for the function (parameters + locals)
                    const needed_regs = @as(u16, @intCast(user_func.param_count + user_func.register_count));
                    const allocated_base = try self.allocateRegisters(needed_regs);
                    
                    // Copy arguments to parameter registers
                    for (0..arg_count) |i| {
                        // Arguments should be in registers following the function register
                        const arg_reg = func_reg + 1 + @as(u16, @intCast(i));
                        const arg = try self.getRegister(arg_reg);
                        debug.log("Loading argument {} from R{}: {any}", .{ i, arg_reg, arg });

                        const dest_reg = allocated_base + @as(u16, @intCast(i));
                        // Directly store argument into the absolute register of the new frame
                        while (dest_reg >= self.registers.items.len) {
                            try self.registers.append(.{ .Nil = {} });
                        }
                        self.registers.items[dest_reg].deinit(self.allocator);
                        self.registers.items[dest_reg] = arg;

                        debug.log("Stored argument {} in R{} (base + {})", .{ i, dest_reg, i });
                    }
                    
                    // Switch to the new function
                    self.current_func = user_func;
                    self.pc = 0;  // Start at beginning of function
                    self.current_register_base = allocated_base;
                    self.function_called = true;  // Don't increment PC
                    
                    return;
                }
                
                debug.log("User-defined function calls not yet implemented for this function type", .{});
                return error.NotImplemented;
            },
            .Return => {
                // Register-based return: Return [Rs] (optional return value)
                debug.log("Return operation", .{});
                debug.log("Current register count: {}", .{self.registers.items.len});

                // Get return value if specified
                var return_value: ?types.Value = null;
                if (instruction.src1) |return_reg| {
                    return_value = try self.getRegister(return_reg);
                    debug.log("Return value from R{}: {any}", .{ return_reg, return_value.? });
                } else {
                    debug.log("Return with no value", .{});
                    return_value = .{ .Nil = {} };
                }
                
                // If we have call frames, restore the previous one
                debug.log("Return: call_frames.len = {}", .{self.call_frames.items.len});
                if (self.call_frames.items.len > 0) {
                    const frame = self.call_frames.items[self.call_frames.items.len - 1];
                    _ = self.call_frames.pop();
                    
                    // Restore the previous function context first
                    self.current_func = frame.caller_func;
                    self.pc = frame.return_addr;
                    self.current_register_base = frame.prev_register_base;
                    
                    // Store return value in the destination register of the calling context
                    if (return_value) |ret_val| {
                        debug.log("Storing return value in R{}: {any}", .{ frame.return_reg, ret_val });
                        try self.setRegister(frame.return_reg, ret_val);
                    }
                    
                    debug.log("Restored call frame: pc={}, base={}", .{ self.pc, self.current_register_base });
                } else {
                    // No call frames - this is the main function returning
                    debug.log("Main function returning", .{});
                    if (return_value) |*ret_val| {
                        ret_val.deinit(self.allocator);
                    }
                    return;  // Exit the VM
                }
            },
            .Jump => {
                // Register-based jump: Jump #target_address
                const target = if (instruction.immediate) |imm| switch (imm) {
                    .Int => @as(usize, @intCast(imm.Int)),
                    else => return error.TypeMismatch,
                } else return error.UnsupportedInstruction;
                
                debug.log("Jump instruction: jump to {}", .{target});
                
                // Set PC to target address (minus 1 because it will be incremented)
                self.pc = target - 1;
            },
            .JumpIfFalse => {
                // Register-based conditional jump: JumpIfFalse Rs, #target_address
                const condition_reg = instruction.src1 orelse return error.UnsupportedInstruction;
                const target = if (instruction.immediate) |imm| switch (imm) {
                    .Int => @as(usize, @intCast(imm.Int)),
                    else => return error.TypeMismatch,
                } else return error.UnsupportedInstruction;
                
                debug.log("JumpIfFalse instruction: if !R{} then jump to {}", .{ condition_reg, target });
                
                var condition_value = try self.getRegister(condition_reg);
                defer condition_value.deinit(self.allocator);
                
                // Check if condition is false
                const should_jump = switch (condition_value) {
                    .Bool => |b| !b,
                    .Nil => true, // nil is considered false
                    .Int => |i| i == 0, // 0 is considered false
                    else => false, // other values are considered true
                };
                
                if (should_jump) {
                    // Set PC to target address (minus 1 because it will be incremented)
                    self.pc = target - 1;
                }
            },
            .Array => {
                // Register-based array creation: Array Rs1, Rs2, ... -> Rd
                debug.log("Array instruction: create array", .{});
                
                // For now, just create an empty array slice
                // TODO: Implement proper array creation with elements from registers
                const empty_array = try self.allocator.alloc(types.Value, 0);
                const dst_reg = instruction.dst orelse return error.UnsupportedInstruction;
                
                try self.setRegister(dst_reg, .{ .Array = empty_array });
            },
            .Map => {
                // Register-based map creation: Map Rs1, Rs2, ... -> Rd
                debug.log("Map instruction: create map", .{});
                
                // For now, just create an empty map
                // TODO: Implement proper map creation with key-value pairs from registers
                const empty_map = std.StringHashMap(types.Value).init(self.allocator);
                const dst_reg = instruction.dst orelse return error.UnsupportedInstruction;
                
                try self.setRegister(dst_reg, .{ .Map = empty_map });
            },
        }
    }

    fn executeBuiltinOperator(self: *VM, op: types.BuiltinOperatorType, arg1: types.Value, arg2: types.Value) !types.Value {
        _ = self; // Suppress unused parameter warning

        return switch (op) {
            .Add => {
                // Handle + operator
                if (arg1 == .Int and arg2 == .Int) {
                    return types.Value{ .Int = arg1.Int + arg2.Int };
                } else {
                    return error.TypeMismatch;
                }
            },
            .Sub => {
                // Handle - operator
                if (arg1 == .Int and arg2 == .Int) {
                    return types.Value{ .Int = arg1.Int - arg2.Int };
                } else {
                    return error.TypeMismatch;
                }
            },
            .Mul => {
                // Handle * operator
                if (arg1 == .Int and arg2 == .Int) {
                    return types.Value{ .Int = arg1.Int * arg2.Int };
                } else {
                    return error.TypeMismatch;
                }
            },
            .Div => {
                // Handle / operator
                if (arg1 == .Int and arg2 == .Int) {
                    if (arg2.Int == 0) {
                        return error.DivisionByZero;
                    }
                    return types.Value{ .Int = @divTrunc(arg1.Int, arg2.Int) };
                } else {
                    return error.TypeMismatch;
                }
            },
            else => {
                return error.TypeMismatch;
            },
        };
    }
};
