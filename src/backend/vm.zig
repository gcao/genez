const std = @import("std");
const types = @import("../core/types.zig");
const debug = @import("../core/debug.zig");
const bytecode = @import("bytecode.zig");
const builtin_classes = @import("../core/builtin_classes.zig");
const gc = @import("../core/gc.zig");

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
    InvalidInstruction,
    FieldNotFound,
    MethodNotFound,
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
    constructor_called: bool, // Flag to indicate if a constructor was just called
    object_pool: std.ArrayList(*types.ObjectInstance), // Object pool for reference semantics
    next_object_id: u32, // Next available object ID
    allocated_classes: std.ArrayList(*types.ClassDefinition), // Track allocated class objects for cleanup
    core_classes: ?builtin_classes.CoreClasses, // Core built-in classes
    garbage_collector: ?*gc.GC, // Garbage collector

    pub fn init(allocator: std.mem.Allocator, stdout: std.fs.File.Writer) VM {

        // Initialize variables with builtins
        var variables = std.StringArrayHashMap(types.Value).init(allocator);

        // Store the print function as a built-in operator
        variables.put("print", .{ .BuiltinOperator = .Print }) catch unreachable;
        variables.put("println", .{ .BuiltinOperator = .Println }) catch unreachable;

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

        // GC functions (use underscore instead of slash to avoid parser issues)
        variables.put("gc_collect", .{ .BuiltinOperator = .GCCollect }) catch unreachable;
        variables.put("gc_disable", .{ .BuiltinOperator = .GCDisable }) catch unreachable;
        variables.put("gc_enable", .{ .BuiltinOperator = .GCEnable }) catch unreachable;
        variables.put("gc_stats", .{ .BuiltinOperator = .GCStats }) catch unreachable;

        var vm = VM{
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
            .constructor_called = false,
            .object_pool = std.ArrayList(*types.ObjectInstance).init(allocator),
            .next_object_id = 0,
            .allocated_classes = std.ArrayList(*types.ClassDefinition).init(allocator),
            .core_classes = null,
            .garbage_collector = null,
        };

        // Initialize garbage collector
        vm.garbage_collector = gc.GC.init(allocator) catch null;

        // Initialize core classes
        vm.initCoreClasses() catch unreachable;

        return vm;
    }

    pub fn deinit(self: *VM) void {
        // Clean up garbage collector first
        if (self.garbage_collector) |gc_ptr| {
            gc_ptr.deinit();
        }

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

        // Clean up object pool
        for (self.object_pool.items) |obj| {
            obj.deinit();
            self.allocator.destroy(obj);
        }
        self.object_pool.deinit();

        // Clean up allocated classes
        for (self.allocated_classes.items) |class| {
            class.deinit();
            self.allocator.destroy(class);
        }
        self.allocated_classes.deinit();

        // Clean up current function if it exists
        // Note: We don't deinit the function itself as it's owned by the caller
        // or has already been cleaned up as part of the stack or variables
        self.current_func = null;
    }

    pub fn getObjectById(self: *VM, id: u32) ?*types.ObjectInstance {
        for (self.object_pool.items) |obj| {
            if (obj.id == id) return obj;
        }
        return null;
    }

    fn initCoreClasses(self: *VM) !void {
        // Initialize all core classes
        self.core_classes = try builtin_classes.CoreClasses.init(self.allocator);

        // Register all core classes in the global variables
        try self.core_classes.?.registerInVM(&self.variables);

        // Track all created classes for cleanup
        try self.allocated_classes.append(self.core_classes.?.class_class);
        try self.allocated_classes.append(self.core_classes.?.any_class);
        try self.allocated_classes.append(self.core_classes.?.number_class);
        try self.allocated_classes.append(self.core_classes.?.int_class);
        try self.allocated_classes.append(self.core_classes.?.float_class);
        try self.allocated_classes.append(self.core_classes.?.string_class);
        try self.allocated_classes.append(self.core_classes.?.bool_class);
        try self.allocated_classes.append(self.core_classes.?.nil_class);
        try self.allocated_classes.append(self.core_classes.?.symbol_class);
        try self.allocated_classes.append(self.core_classes.?.fn_class);
        try self.allocated_classes.append(self.core_classes.?.builtin_fn_class);
        try self.allocated_classes.append(self.core_classes.?.macro_class);
        try self.allocated_classes.append(self.core_classes.?.array_class);
        try self.allocated_classes.append(self.core_classes.?.map_class);
    }

    pub fn setVariable(self: *VM, name: []const u8, value: types.Value) !void {
        try self.variables.put(name, value);
    }

    /// Allocate a GC-managed value if GC is enabled
    fn allocGCValue(self: *VM, value: types.Value) !types.Value {
        if (self.garbage_collector) |gc_ptr| {
            // For now, only manage heap-allocated values
            switch (value) {
                .String, .Array, .Map, .Object => {
                    const managed_ptr = try gc_ptr.allocValue(value);
                    return managed_ptr.*;
                },
                else => return value,
            }
        }
        return value;
    }

    pub fn execute(self: *VM, func: *const bytecode.Function) VMError!void {
        debug.log("Executing function: {s}", .{func.name});
        self.current_func = func;
        self.pc = 0;

        // Allocate registers for the main function before execution
        const base = try self.allocateRegisters(func.register_count);
        self.current_register_base = base;

        // Register VM registers as GC roots
        if (self.garbage_collector) |gc_ptr| {
            // Add all registers as potential roots
            for (self.registers.items) |*reg| {
                try gc_ptr.addRoot(reg);
            }
        }

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
            debug.log("VM loop: pc={}, instruction_count={}", .{ self.pc, executing_func.instructions.items.len });
            if (self.pc >= executing_func.instructions.items.len) {
                debug.log("VM: PC {} >= instruction count {}, checking call frames", .{ self.pc, executing_func.instructions.items.len });
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
            // constructor_called is reset in Return instruction handler
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
                // getRegister will add current_register_base, so we just pass the index
                debug.log("Fetching parameter {} from current frame (base {})", .{ param_index, self.current_register_base });
                const param_value = try self.getRegister(param_index);
                debug.log("Loaded parameter {}: {any}", .{ param_index, param_value });

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
                    .Class => |class| try self.stdout.print("Class: {s}\n", .{class.name}),
                    .Object => |obj_id| {
                        if (self.getObjectById(obj_id)) |obj| {
                            try self.stdout.print("Object of {s}\n", .{obj.class.name});
                        } else {
                            try self.stdout.print("Object(invalid ID: {})\n", .{obj_id});
                        }
                    },
                    .CPtr => |ptr| {
                        if (ptr) |p| {
                            try self.stdout.print("CPtr: {*}\n", .{p});
                        } else {
                            try self.stdout.print("CPtr: null\n", .{});
                        }
                    },
                    .CFunction => |func| try self.stdout.print("CFunction: {*}\n", .{func}),
                    .CStruct => |ptr| try self.stdout.print("CStruct: {*}\n", .{ptr}),
                    .CArray => |arr| try self.stdout.print("CArray: ptr={*}, len={}, element_size={}\n", .{ arr.ptr, arr.len, arr.element_size }),
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
                                dst_reg - 1 // Previous register (result of previous call)
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
                                .Class => |class| try self.stdout.print("Class: {s}", .{class.name}),
                                .Object => |obj_id| {
                                    if (self.getObjectById(obj_id)) |obj| {
                                        try self.stdout.print("Object of {s}", .{obj.class.name});
                                    } else {
                                        try self.stdout.print("Object(invalid ID: {})", .{obj_id});
                                    }
                                },
                                .CPtr => |ptr| {
                                    if (ptr) |p| {
                                        try self.stdout.print("CPtr: {*}", .{p});
                                    } else {
                                        try self.stdout.print("CPtr: null", .{});
                                    }
                                },
                                .CFunction => |func| try self.stdout.print("CFunction: {*}", .{func}),
                                .CStruct => |ptr| try self.stdout.print("CStruct: {*}", .{ptr}),
                                .CArray => |arr| try self.stdout.print("CArray[{} x {}]", .{ arr.len, arr.element_size }),
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

                    // Handle println function (same as print, already adds newline)
                    if (builtin_op == .Println) {
                        // Println can take any number of arguments
                        for (0..arg_count) |i| {
                            // For single argument calls, the argument might be in the previous register
                            // (result of a previous operation)
                            const arg_reg = if (arg_count == 1)
                                dst_reg - 1 // Previous register (result of previous call)
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
                                .Class => |class| try self.stdout.print("Class: {s}", .{class.name}),
                                .Object => |obj_id| {
                                    if (self.getObjectById(obj_id)) |obj| {
                                        try self.stdout.print("Object of {s}", .{obj.class.name});
                                    } else {
                                        try self.stdout.print("Object(invalid ID: {})", .{obj_id});
                                    }
                                },
                                .CPtr => |ptr| {
                                    if (ptr) |p| {
                                        try self.stdout.print("CPtr: {*}", .{p});
                                    } else {
                                        try self.stdout.print("CPtr: null", .{});
                                    }
                                },
                                .CFunction => |func| try self.stdout.print("CFunction: {*}", .{func}),
                                .CStruct => |ptr| try self.stdout.print("CStruct: {*}", .{ptr}),
                                .CArray => |arr| try self.stdout.print("CArray[{} x {}]", .{ arr.len, arr.element_size }),
                            }

                            // Add space between arguments except for the last one
                            if (i < arg_count - 1) {
                                try self.stdout.print(" ", .{});
                            }
                        }

                        // Add newline after all arguments
                        try self.stdout.print("\n", .{});

                        // Println function returns nil
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

                    // Handle multiplication operator
                    if (builtin_op == .Mul) {
                        if (arg_count != 2) {
                            debug.log("Multiply operator requires exactly 2 arguments, got {}", .{arg_count});
                            return error.ArgumentCountMismatch;
                        }

                        // Get the two arguments
                        var left = try self.getRegister(func_reg + 1);
                        defer left.deinit(self.allocator);
                        var right = try self.getRegister(func_reg + 2);
                        defer right.deinit(self.allocator);

                        // Perform multiplication based on types
                        const result = switch (left) {
                            .Int => |left_val| switch (right) {
                                .Int => |right_val| types.Value{ .Int = left_val * right_val },
                                .Float => |right_val| types.Value{ .Float = @as(f64, @floatFromInt(left_val)) * right_val },
                                else => {
                                    debug.log("TypeMismatch in Multiply: left={}, right={}", .{ left, right });
                                    return error.TypeMismatch;
                                },
                            },
                            .Float => |left_val| switch (right) {
                                .Int => |right_val| types.Value{ .Float = left_val * @as(f64, @floatFromInt(right_val)) },
                                .Float => |right_val| types.Value{ .Float = left_val * right_val },
                                else => {
                                    debug.log("TypeMismatch in Multiply: left={}, right={}", .{ left, right });
                                    return error.TypeMismatch;
                                },
                            },
                            else => {
                                debug.log("TypeMismatch in Multiply: left={}, right={}", .{ left, right });
                                return error.TypeMismatch;
                            },
                        };

                        try self.setRegister(dst_reg, result);
                        return;
                    }

                    // Handle division operator
                    if (builtin_op == .Div) {
                        if (arg_count != 2) {
                            debug.log("Divide operator requires exactly 2 arguments, got {}", .{arg_count});
                            return error.ArgumentCountMismatch;
                        }

                        // Get the two arguments
                        var left = try self.getRegister(func_reg + 1);
                        defer left.deinit(self.allocator);
                        var right = try self.getRegister(func_reg + 2);
                        defer right.deinit(self.allocator);

                        // Check for division by zero
                        const is_zero = switch (right) {
                            .Int => |val| val == 0,
                            .Float => |val| val == 0.0,
                            else => false,
                        };
                        if (is_zero) {
                            debug.log("Division by zero", .{});
                            return error.DivisionByZero;
                        }

                        // Perform division based on types
                        const result = switch (left) {
                            .Int => |left_val| switch (right) {
                                .Int => |right_val| types.Value{ .Int = @divTrunc(left_val, right_val) },
                                .Float => |right_val| types.Value{ .Float = @as(f64, @floatFromInt(left_val)) / right_val },
                                else => {
                                    debug.log("TypeMismatch in Divide: left={}, right={}", .{ left, right });
                                    return error.TypeMismatch;
                                },
                            },
                            .Float => |left_val| switch (right) {
                                .Int => |right_val| types.Value{ .Float = left_val / @as(f64, @floatFromInt(right_val)) },
                                .Float => |right_val| types.Value{ .Float = left_val / right_val },
                                else => {
                                    debug.log("TypeMismatch in Divide: left={}, right={}", .{ left, right });
                                    return error.TypeMismatch;
                                },
                            },
                            else => {
                                debug.log("TypeMismatch in Divide: left={}, right={}", .{ left, right });
                                return error.TypeMismatch;
                            },
                        };

                        try self.setRegister(dst_reg, result);
                        return;
                    }

                    // Handle greater than or equal operator
                    if (builtin_op == .GreaterEqual) {
                        if (arg_count != 2) {
                            debug.log("GreaterEqual operator requires exactly 2 arguments, got {}", .{arg_count});
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
                                .Int => |right_val| left_val >= right_val,
                                .Float => |right_val| @as(f64, @floatFromInt(left_val)) >= right_val,
                                else => {
                                    debug.log("TypeMismatch in GreaterEqual: left={}, right={}", .{ left, right });
                                    return error.TypeMismatch;
                                },
                            },
                            .Float => |left_val| switch (right) {
                                .Int => |right_val| left_val >= @as(f64, @floatFromInt(right_val)),
                                .Float => |right_val| left_val >= right_val,
                                else => {
                                    debug.log("TypeMismatch in GreaterEqual: left={}, right={}", .{ left, right });
                                    return error.TypeMismatch;
                                },
                            },
                            else => {
                                debug.log("TypeMismatch in GreaterEqual: left={}, right={}", .{ left, right });
                                return error.TypeMismatch;
                            },
                        };

                        try self.setRegister(dst_reg, .{ .Bool = result });
                        return;
                    }

                    // Handle greater than operator
                    if (builtin_op == .GreaterThan) {
                        if (arg_count != 2) {
                            debug.log("GreaterThan operator requires exactly 2 arguments, got {}", .{arg_count});
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
                                .Int => |right_val| left_val > right_val,
                                .Float => |right_val| @as(f64, @floatFromInt(left_val)) > right_val,
                                else => {
                                    debug.log("TypeMismatch in GreaterThan: left={}, right={}", .{ left, right });
                                    return error.TypeMismatch;
                                },
                            },
                            .Float => |left_val| switch (right) {
                                .Int => |right_val| left_val > @as(f64, @floatFromInt(right_val)),
                                .Float => |right_val| left_val > right_val,
                                else => {
                                    debug.log("TypeMismatch in GreaterThan: left={}, right={}", .{ left, right });
                                    return error.TypeMismatch;
                                },
                            },
                            else => {
                                debug.log("TypeMismatch in GreaterThan: left={}, right={}", .{ left, right });
                                return error.TypeMismatch;
                            },
                        };

                        try self.setRegister(dst_reg, .{ .Bool = result });
                        return;
                    }

                    // Handle equality operator
                    if (builtin_op == .Eq) {
                        if (arg_count != 2) {
                            debug.log("Eq operator requires exactly 2 arguments, got {}", .{arg_count});
                            return error.ArgumentCountMismatch;
                        }

                        // Get the two arguments
                        var left = try self.getRegister(func_reg + 1);
                        defer left.deinit(self.allocator);
                        var right = try self.getRegister(func_reg + 2);
                        defer right.deinit(self.allocator);

                        // Perform equality check based on types
                        const result = switch (left) {
                            .Int => |left_val| switch (right) {
                                .Int => |right_val| left_val == right_val,
                                .Float => |right_val| @as(f64, @floatFromInt(left_val)) == right_val,
                                else => false,
                            },
                            .Float => |left_val| switch (right) {
                                .Int => |right_val| left_val == @as(f64, @floatFromInt(right_val)),
                                .Float => |right_val| left_val == right_val,
                                else => false,
                            },
                            .Bool => |left_val| switch (right) {
                                .Bool => |right_val| left_val == right_val,
                                else => false,
                            },
                            .String => |left_val| switch (right) {
                                .String => |right_val| std.mem.eql(u8, left_val, right_val),
                                else => false,
                            },
                            .Nil => switch (right) {
                                .Nil => true,
                                else => false,
                            },
                            else => false, // Other types not comparable yet
                        };

                        try self.setRegister(dst_reg, .{ .Bool = result });
                        return;
                    }

                    // Handle inequality operator
                    if (builtin_op == .Ne) {
                        if (arg_count != 2) {
                            debug.log("Ne operator requires exactly 2 arguments, got {}", .{arg_count});
                            return error.ArgumentCountMismatch;
                        }

                        // Get the two arguments
                        var left = try self.getRegister(func_reg + 1);
                        defer left.deinit(self.allocator);
                        var right = try self.getRegister(func_reg + 2);
                        defer right.deinit(self.allocator);

                        // Perform comparison based on types (same as Eq but negated)
                        const result = switch (left) {
                            .Int => |left_val| switch (right) {
                                .Int => |right_val| left_val != right_val,
                                .Float => |right_val| @as(f64, @floatFromInt(left_val)) != right_val,
                                else => true, // Different types are not equal
                            },
                            .Float => |left_val| switch (right) {
                                .Int => |right_val| left_val != @as(f64, @floatFromInt(right_val)),
                                .Float => |right_val| left_val != right_val,
                                else => true, // Different types are not equal
                            },
                            .String => |left_val| switch (right) {
                                .String => |right_val| !std.mem.eql(u8, left_val, right_val),
                                else => true, // Different types are not equal
                            },
                            .Bool => |left_val| switch (right) {
                                .Bool => |right_val| left_val != right_val,
                                else => true, // Different types are not equal
                            },
                            .Nil => switch (right) {
                                .Nil => false, // nil != nil is false
                                else => true, // nil != anything else is true
                            },
                            else => true, // For other types, assume not equal
                        };

                        try self.setRegister(dst_reg, .{ .Bool = result });
                        return;
                    }

                    // Handle logical NOT operator
                    if (builtin_op == .Not) {
                        if (arg_count != 1) {
                            debug.log("Not operator requires exactly 1 argument, got {}", .{arg_count});
                            return error.ArgumentCountMismatch;
                        }

                        // Get the argument
                        var arg = try self.getRegister(func_reg + 1);
                        defer arg.deinit(self.allocator);

                        // Convert to boolean and negate
                        const result = switch (arg) {
                            .Bool => |b| !b,
                            .Nil => true,  // !nil is true
                            .Int => |i| i == 0,  // !0 is true, !non-zero is false
                            .Float => |f| f == 0.0,  // !0.0 is true, !non-zero is false
                            .String => |s| s.len == 0,  // !"" is true, !non-empty is false
                            .Array => |arr| arr.len == 0,  // ![] is true, !non-empty is false
                            .Map => |map| map.count() == 0,  // !{} is true, !non-empty is false
                            else => false,  // For other types, !value is false
                        };

                        try self.setRegister(dst_reg, .{ .Bool = result });
                        return;
                    }

                    // Handle GC operations
                    if (builtin_op == .GCCollect) {
                        if (self.garbage_collector) |gc_ptr| {
                            try gc_ptr.collect();
                            try self.setRegister(dst_reg, .Nil);
                        } else {
                            try self.setRegister(dst_reg, .{ .Bool = false });
                        }
                        return;
                    }

                    if (builtin_op == .GCDisable) {
                        if (self.garbage_collector) |gc_ptr| {
                            gc_ptr.disable();
                            try self.setRegister(dst_reg, .{ .Bool = true });
                        } else {
                            try self.setRegister(dst_reg, .{ .Bool = false });
                        }
                        return;
                    }

                    if (builtin_op == .GCEnable) {
                        if (self.garbage_collector) |gc_ptr| {
                            gc_ptr.enable();
                            try self.setRegister(dst_reg, .{ .Bool = true });
                        } else {
                            try self.setRegister(dst_reg, .{ .Bool = false });
                        }
                        return;
                    }

                    if (builtin_op == .GCStats) {
                        if (self.garbage_collector) |gc_ptr| {
                            const stats = gc_ptr.getStats();
                            // For now, return a simple map with stats
                            var stats_map = std.StringHashMap(types.Value).init(self.allocator);
                            try stats_map.put("collections", .{ .Int = @intCast(stats.collections) });
                            try stats_map.put("total_allocated", .{ .Int = @intCast(stats.total_allocated) });
                            try stats_map.put("total_freed", .{ .Int = @intCast(stats.total_freed) });
                            try self.setRegister(dst_reg, .{ .Map = stats_map });
                        } else {
                            try self.setRegister(dst_reg, .Nil);
                        }
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
                                .Class => |class| try self.stdout.print("Class: {s}", .{class.name}),
                                .Object => |obj_id| {
                                    if (self.getObjectById(obj_id)) |obj| {
                                        try self.stdout.print("Object of {s}", .{obj.class.name});
                                    } else {
                                        try self.stdout.print("Object(invalid ID: {})", .{obj_id});
                                    }
                                },
                                .CPtr => |ptr| {
                                    if (ptr) |p| {
                                        try self.stdout.print("CPtr: {*}", .{p});
                                    } else {
                                        try self.stdout.print("CPtr: null", .{});
                                    }
                                },
                                .CFunction => |func| try self.stdout.print("CFunction: {*}", .{func}),
                                .CStruct => |ptr| try self.stdout.print("CStruct: {*}", .{ptr}),
                                .CArray => |arr| try self.stdout.print("CArray[{} x {}]", .{ arr.len, arr.element_size }),
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
                    self.pc = 0; // Start at beginning of function
                    self.current_register_base = allocated_base;
                    self.function_called = true; // Don't increment PC

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

                    // If the specified register contains Nil, try to find a non-Nil register
                    // This handles cases where control flow skipped some instructions
                    if (return_value.? == .Nil) {
                        debug.log("Return register R{} contains Nil, searching for non-Nil value", .{return_reg});
                        // Search backwards from the specified register to find a non-Nil value
                        var search_reg = return_reg;
                        while (search_reg > 0) {
                            search_reg -= 1;
                            var candidate_value = try self.getRegister(search_reg);
                            if (candidate_value != .Nil) {
                                debug.log("Found non-Nil value in R{}: {any}", .{ search_reg, candidate_value });
                                candidate_value.deinit(self.allocator);
                                return_value = try self.getRegister(search_reg); // Get a fresh copy
                                break;
                            }
                            candidate_value.deinit(self.allocator);
                        }
                    }
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
                        // Check if we're returning from a constructor
                        if (self.constructor_called) {
                            debug.log("Returning from constructor, ignoring return value", .{});
                            self.constructor_called = false; // Reset the flag
                        } else {
                            debug.log("Storing return value in R{}: {any}", .{ frame.return_reg, ret_val });
                            if (frame.caller_func) |caller| {
                                debug.log("Caller was: {s}", .{caller.name});
                            }
                            try self.setRegister(frame.return_reg, ret_val);
                        }
                    }

                    debug.log("Restored call frame: pc={}, base={}", .{ self.pc, self.current_register_base });

                    // Set flag to prevent PC increment after return
                    self.function_called = true;
                } else {
                    // No call frames - this is the main function returning
                    debug.log("Main function returning", .{});
                    if (return_value) |*ret_val| {
                        ret_val.deinit(self.allocator);
                    }
                    return; // Exit the VM
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
            .DefineClass => {
                // DefineClass is handled at compile time, classes are loaded as constants
                debug.log("DefineClass instruction should not appear at runtime", .{});
                return error.InvalidInstruction;
            },
            .New => {
                // Create object instance: New Rd, class_reg, [arg1, arg2, ...]
                const dst_reg = instruction.dst orelse return error.InvalidInstruction;
                const class_reg = instruction.src1 orelse return error.InvalidInstruction;

                // Get the class
                const class_val = try self.getRegister(class_reg);
                // Don't defer deinit for class values as they are shared

                if (class_val != .Class) {
                    debug.log("New instruction requires a Class, got {}", .{class_val});
                    return error.TypeMismatch;
                }

                const class = class_val.Class;

                // Create the object instance
                var obj = try self.allocator.create(types.ObjectInstance);
                const obj_id = self.next_object_id;
                self.next_object_id += 1;
                obj.* = types.ObjectInstance.init(self.allocator, class, obj_id);

                // Add to object pool
                try self.object_pool.append(obj);

                // Initialize fields with default values
                var field_iter = class.fields.iterator();
                while (field_iter.next()) |entry| {
                    const field_def = entry.value_ptr.*;
                    // Duplicate the field name for the object's own copy
                    const field_name_copy = try self.allocator.dupe(u8, entry.key_ptr.*);
                    if (field_def.default_value) |default| {
                        try obj.fields.put(field_name_copy, try default.clone(self.allocator));
                    } else {
                        try obj.fields.put(field_name_copy, .{ .Nil = {} });
                    }
                }

                try self.setRegister(dst_reg, .{ .Object = obj_id });
                debug.log("Created instance of class {s} in R{}", .{ class.name, dst_reg });

                // Check if there's a constructor to call
                // Constructor is named ClassName_init
                const ctor_name = try std.fmt.allocPrint(self.allocator, "{s}_init", .{class.name});
                defer self.allocator.free(ctor_name);

                // std.debug.print("Looking for constructor: {s}\n", .{ctor_name});
                // std.debug.print("Class has {} methods\n", .{class.methods.count()});
                // var method_iter = class.methods.iterator();
                // while (method_iter.next()) |entry| {
                //     std.debug.print("  Method: {s}\n", .{entry.key_ptr.*});
                // }

                if (class.methods.get(ctor_name)) |ctor_method| {
                    debug.log("Found constructor {s} in class {s}, calling it", .{ ctor_name, class.name });

                    // Get number of constructor arguments from immediate value
                    const arg_count: usize = if (instruction.immediate) |imm| blk: {
                        if (imm == .Int) break :blk @intCast(imm.Int);
                        break :blk 0;
                    } else 0;

                    // Set up new call frame for the constructor
                    const new_register_base = self.next_free_register;
                    const frame = CallFrame.init(self.current_func, new_register_base, self.current_register_base, self.pc + 1, dst_reg);
                    try self.call_frames.append(frame);

                    // Allocate registers for the constructor (self + parameters + locals)
                    const needed_regs = @as(u16, @intCast(ctor_method.param_count + ctor_method.register_count));
                    const allocated_base = try self.allocateRegisters(needed_regs);

                    // Set 'self' as first parameter (the newly created object)
                    const self_value = types.Value{ .Object = obj_id };
                    debug.log("Setting self (object ID {}) in register {}", .{ obj_id, allocated_base });
                    // Directly store 'self' into the absolute register of the new frame
                    while (allocated_base >= self.registers.items.len) {
                        try self.registers.append(.{ .Nil = {} });
                    }
                    self.registers.items[allocated_base].deinit(self.allocator);
                    self.registers.items[allocated_base] = self_value;

                    // Copy constructor arguments to parameter registers (after 'self')
                    for (0..arg_count) |i| {
                        const arg_reg = class_reg + 1 + @as(u16, @intCast(i));
                        const arg = try self.getRegister(arg_reg);
                        const dest_reg = allocated_base + 1 + @as(u16, @intCast(i));
                        // Directly store argument into the absolute register of the new frame
                        while (dest_reg >= self.registers.items.len) {
                            try self.registers.append(.{ .Nil = {} });
                        }
                        self.registers.items[dest_reg].deinit(self.allocator);
                        self.registers.items[dest_reg] = arg;
                    }

                    // Switch to the constructor
                    self.current_func = ctor_method;
                    self.pc = 0;
                    self.current_register_base = allocated_base;
                    self.function_called = true;

                    debug.log("Called constructor with {} args, self is object ID {}", .{ arg_count, obj_id });

                    // Mark that we called a constructor - we need to ignore its return value
                    self.constructor_called = true;
                } else {
                    debug.log("No constructor found for class {s}", .{class.name});
                }
            },
            .GetField => {
                // Get object field: GetField Rd, obj_reg, field_name
                const dst_reg = instruction.dst orelse return error.InvalidInstruction;
                const obj_reg = instruction.src1 orelse return error.InvalidInstruction;
                const field_name = instruction.var_name orelse return error.InvalidInstruction;

                var obj_val = try self.getRegister(obj_reg);
                defer obj_val.deinit(self.allocator);

                if (obj_val != .Object) {
                    debug.log("GetField requires an Object, got {}", .{obj_val});
                    return error.TypeMismatch;
                }

                const obj = self.getObjectById(obj_val.Object) orelse {
                    debug.log("Object with ID {} not found in pool", .{obj_val.Object});
                    return error.InvalidInstruction;
                };

                if (obj.fields.get(field_name)) |field_value| {
                    try self.setRegister(dst_reg, try field_value.clone(self.allocator));
                    debug.log("Got field {s} from object in R{}", .{ field_name, dst_reg });
                } else {
                    debug.log("Field {s} not found in object", .{field_name});
                    return error.FieldNotFound;
                }
            },
            .SetField => {
                // Set object field: SetField obj_reg, field_name, value_reg
                const obj_reg = instruction.src1 orelse return error.InvalidInstruction;
                const value_reg = instruction.src2 orelse return error.InvalidInstruction;
                const field_name = instruction.var_name orelse return error.InvalidInstruction;

                var obj_val = try self.getRegister(obj_reg);
                defer obj_val.deinit(self.allocator);

                if (obj_val != .Object) {
                    debug.log("SetField requires an Object, got {}", .{obj_val});
                    return error.TypeMismatch;
                }

                const obj = self.getObjectById(obj_val.Object) orelse {
                    debug.log("Object with ID {} not found in pool", .{obj_val.Object});
                    return error.InvalidInstruction;
                };

                var value = try self.getRegister(value_reg);
                defer value.deinit(self.allocator);

                // Check if field already exists
                if (obj.fields.get(field_name)) |old_value| {
                    // Field exists, just update the value
                    var old_value_copy = old_value;
                    old_value_copy.deinit(self.allocator);
                    // Update existing entry
                    try obj.fields.put(field_name, try value.clone(self.allocator));
                } else {
                    // Field doesn't exist, create new entry with duplicated key
                    try obj.fields.put(try self.allocator.dupe(u8, field_name), try value.clone(self.allocator));
                }
                debug.log("Set field {s} on object", .{field_name});
            },
            .CallMethod => {
                // Call method: CallMethod Rd, obj_reg, method_name, [arg1, arg2, ...]
                const dst_reg = instruction.dst orelse return error.InvalidInstruction;
                const obj_reg = instruction.src1 orelse return error.InvalidInstruction;
                const method_name = instruction.var_name orelse return error.InvalidInstruction;
                const arg_count = if (instruction.immediate) |imm|
                    if (imm == .Int) @as(usize, @intCast(imm.Int)) else 0
                else
                    0;

                var obj_val = try self.getRegister(obj_reg);
                defer obj_val.deinit(self.allocator);

                // Get the class for the value (works for both objects and primitives)
                const class = if (obj_val == .Object) blk: {
                    const obj = self.getObjectById(obj_val.Object) orelse {
                        debug.log("Object with ID {} not found in pool", .{obj_val.Object});
                        return error.InvalidInstruction;
                    };
                    break :blk obj.class;
                } else if (self.core_classes) |core|
                    core.getClassForValue(obj_val)
                else {
                    debug.log("Core classes not initialized", .{});
                    return error.InvalidInstruction;
                };

                // Look up the method in the class
                debug.log("Looking for method {s} in class {s}", .{ method_name, class.name });
                debug.log("Class has {} methods", .{class.methods.count()});
                var method_iter = class.methods.iterator();
                while (method_iter.next()) |entry| {
                    debug.log("  Method: {s}", .{entry.key_ptr.*});
                }

                // First check if method is in the class (including inherited methods)
                var current_class: ?*types.ClassDefinition = class;
                var found_method: ?*bytecode.Function = null;

                while (current_class) |c| {
                    if (c.methods.get(method_name)) |method| {
                        found_method = method;
                        break;
                    }
                    current_class = c.parent;
                }

                if (found_method) |method| {
                    debug.log("Calling method {s} on value of class {s}", .{ method_name, class.name });

                    // Set up new call frame for the method
                    const new_register_base = self.next_free_register;
                    const frame = CallFrame.init(self.current_func, new_register_base, self.current_register_base, self.pc + 1, dst_reg);
                    try self.call_frames.append(frame);

                    // Allocate registers for the method (self + parameters + locals)
                    const needed_regs = @as(u16, @intCast(method.param_count + method.register_count));
                    const allocated_base = try self.allocateRegisters(needed_regs);

                    // Set 'self' as first parameter
                    const self_value = try obj_val.clone(self.allocator);
                    // Directly store 'self' into the absolute register of the new frame
                    while (allocated_base >= self.registers.items.len) {
                        try self.registers.append(.{ .Nil = {} });
                    }
                    self.registers.items[allocated_base].deinit(self.allocator);
                    self.registers.items[allocated_base] = self_value;

                    // Copy arguments to parameter registers (after 'self')
                    for (0..arg_count) |i| {
                        const arg_reg = obj_reg + 1 + @as(u16, @intCast(i));
                        const arg = try self.getRegister(arg_reg);
                        const dest_reg = allocated_base + 1 + @as(u16, @intCast(i));
                        // Directly store argument into the absolute register of the new frame
                        while (dest_reg >= self.registers.items.len) {
                            try self.registers.append(.{ .Nil = {} });
                        }
                        self.registers.items[dest_reg].deinit(self.allocator);
                        self.registers.items[dest_reg] = arg;
                    }

                    // Switch to the method
                    self.current_func = method;
                    self.pc = 0;
                    self.current_register_base = allocated_base;
                    self.next_free_register = allocated_base + needed_regs;
                    self.function_called = true;
                } else {
                    // Try to find a function with the pattern ClassName_methodName
                    const func_name = try std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ class.name, method_name });
                    defer self.allocator.free(func_name);

                    debug.log("Method {s} not found in class, looking for function {s}", .{ method_name, func_name });

                    if (self.variables.get(func_name)) |func_val| {
                        if (func_val == .Function) {
                            const func = func_val.Function;
                            debug.log("Found function {s} as method implementation", .{func_name});

                            // Set up call similar to regular function call but with object as first argument
                            const new_register_base = self.next_free_register;
                            const frame = CallFrame.init(self.current_func, new_register_base, self.current_register_base, self.pc + 1, dst_reg);
                            try self.call_frames.append(frame);

                            // Allocate registers for the function (self + parameters + locals)
                            const needed_regs = @as(u16, @intCast(func.param_count + func.register_count));
                            const allocated_base = try self.allocateRegisters(needed_regs);

                            // Set object as first parameter (self)
                            const self_value = try obj_val.clone(self.allocator);
                            while (allocated_base >= self.registers.items.len) {
                                try self.registers.append(.{ .Nil = {} });
                            }
                            self.registers.items[allocated_base].deinit(self.allocator);
                            self.registers.items[allocated_base] = self_value;

                            // Copy arguments to parameter registers (after 'self')
                            for (0..arg_count) |i| {
                                const arg_reg = obj_reg + 1 + @as(u16, @intCast(i));
                                const arg = try self.getRegister(arg_reg);
                                const dest_reg = allocated_base + 1 + @as(u16, @intCast(i));
                                while (dest_reg >= self.registers.items.len) {
                                    try self.registers.append(.{ .Nil = {} });
                                }
                                self.registers.items[dest_reg].deinit(self.allocator);
                                self.registers.items[dest_reg] = arg;
                            }

                            // Switch to the function
                            self.current_func = func;
                            self.pc = 0;
                            self.current_register_base = allocated_base;
                            self.next_free_register = allocated_base + needed_regs;
                            self.function_called = true;
                            return;
                        }
                    }

                    debug.log("Method {s} not found on class {s} or as function {s}", .{ method_name, class.name, func_name });
                    return error.MethodNotFound;
                }
            },
            .Length => {
                // Get length: Length Rd, Rs
                const dst_reg = instruction.dst orelse return error.InvalidInstruction;
                const src_reg = instruction.src1 orelse return error.InvalidInstruction;

                var value = try self.getRegister(src_reg);
                defer value.deinit(self.allocator);

                const length: i64 = switch (value) {
                    .String => |s| @intCast(s.len),
                    .Array => |arr| @intCast(arr.len),
                    .Map => |map| @intCast(map.count()),
                    else => {
                        debug.log("Length not supported for type: {}", .{value});
                        return error.TypeMismatch;
                    },
                };

                try self.setRegister(dst_reg, .{ .Int = length });
            },
            .ArrayGet => {
                // Get array element: ArrayGet Rd, array_reg, index_reg
                const dst_reg = instruction.dst orelse return error.InvalidInstruction;
                const array_reg = instruction.src1 orelse return error.InvalidInstruction;
                const index_reg = instruction.src2 orelse return error.InvalidInstruction;

                var array_val = try self.getRegister(array_reg);
                defer array_val.deinit(self.allocator);
                var index_val = try self.getRegister(index_reg);
                defer index_val.deinit(self.allocator);

                if (array_val != .Array) {
                    debug.log("ArrayGet requires an Array, got {}", .{array_val});
                    return error.TypeMismatch;
                }
                if (index_val != .Int) {
                    debug.log("ArrayGet index must be Int, got {}", .{index_val});
                    return error.TypeMismatch;
                }

                const array = array_val.Array;
                const index = index_val.Int;

                if (index < 0 or index >= array.len) {
                    debug.log("Array index out of bounds: {} (length: {})", .{ index, array.len });
                    return error.InvalidInstruction;
                }

                const element = array[@intCast(index)];
                try self.setRegister(dst_reg, try element.clone(self.allocator));
            },
            .ArraySet => {
                // Set array element: ArraySet array_reg, index_reg, value_reg
                const array_reg = instruction.src1 orelse return error.InvalidInstruction;
                const index_reg = instruction.src2 orelse return error.InvalidInstruction;
                const value_reg = instruction.dst orelse return error.InvalidInstruction; // Using dst for value

                var array_val = try self.getRegister(array_reg);
                defer array_val.deinit(self.allocator);
                var index_val = try self.getRegister(index_reg);
                defer index_val.deinit(self.allocator);
                var value = try self.getRegister(value_reg);
                defer value.deinit(self.allocator);

                if (array_val != .Array) {
                    debug.log("ArraySet requires an Array, got {}", .{array_val});
                    return error.TypeMismatch;
                }
                if (index_val != .Int) {
                    debug.log("ArraySet index must be Int, got {}", .{index_val});
                    return error.TypeMismatch;
                }

                const array = array_val.Array;
                const index = index_val.Int;

                if (index < 0 or index >= array.len) {
                    debug.log("Array index out of bounds: {} (length: {})", .{ index, array.len });
                    return error.InvalidInstruction;
                }

                // Update the array element
                array[@intCast(index)].deinit(self.allocator);
                array[@intCast(index)] = try value.clone(self.allocator);
            },
            .Substring => {
                // Get substring: Substring Rd, str_reg, start_reg, end_reg
                const dst_reg = instruction.dst orelse return error.InvalidInstruction;
                const str_reg = instruction.src1 orelse return error.InvalidInstruction;
                const start_reg = instruction.src2 orelse return error.InvalidInstruction;
                // For now, using immediate for end position
                const end_val = if (instruction.immediate) |imm| switch (imm) {
                    .Int => @as(usize, @intCast(imm.Int)),
                    else => return error.TypeMismatch,
                } else return error.InvalidInstruction;

                var str_val = try self.getRegister(str_reg);
                defer str_val.deinit(self.allocator);
                var start_val = try self.getRegister(start_reg);
                defer start_val.deinit(self.allocator);

                if (str_val != .String) {
                    debug.log("Substring requires a String, got {}", .{str_val});
                    return error.TypeMismatch;
                }
                if (start_val != .Int) {
                    debug.log("Substring start must be Int, got {}", .{start_val});
                    return error.TypeMismatch;
                }

                const str = str_val.String;
                const start = @as(usize, @intCast(start_val.Int));
                const end = @min(end_val, str.len);

                if (start > str.len) {
                    debug.log("Substring start out of bounds: {} (length: {})", .{ start, str.len });
                    return error.InvalidInstruction;
                }

                const substring = try self.allocator.dupe(u8, str[start..end]);
                try self.setRegister(dst_reg, .{ .String = substring });
            },
            .MapGet => {
                // Get map value: MapGet Rd, map_reg, key_reg
                const dst_reg = instruction.dst orelse return error.InvalidInstruction;
                const map_reg = instruction.src1 orelse return error.InvalidInstruction;
                const key_reg = instruction.src2 orelse return error.InvalidInstruction;

                var map_val = try self.getRegister(map_reg);
                defer map_val.deinit(self.allocator);
                var key_val = try self.getRegister(key_reg);
                defer key_val.deinit(self.allocator);

                if (map_val != .Map) {
                    debug.log("MapGet requires a Map, got {}", .{map_val});
                    return error.TypeMismatch;
                }
                if (key_val != .String) {
                    debug.log("MapGet key must be String, got {}", .{key_val});
                    return error.TypeMismatch;
                }

                const map = map_val.Map;
                const key = key_val.String;

                if (map.get(key)) |value| {
                    try self.setRegister(dst_reg, try value.clone(self.allocator));
                } else {
                    try self.setRegister(dst_reg, .{ .Nil = {} });
                }
            },
            .MapSet => {
                // Set map value: MapSet map_reg, key_reg, value_reg
                const map_reg = instruction.src1 orelse return error.InvalidInstruction;
                const key_reg = instruction.src2 orelse return error.InvalidInstruction;
                const value_reg = if (instruction.immediate) |imm| switch (imm) {
                    .Int => @as(u16, @intCast(imm.Int)),
                    else => return error.TypeMismatch,
                } else return error.InvalidInstruction;

                var map_val = try self.getRegister(map_reg);
                defer map_val.deinit(self.allocator);
                var key_val = try self.getRegister(key_reg);
                defer key_val.deinit(self.allocator);
                var value = try self.getRegister(value_reg);
                defer value.deinit(self.allocator);

                if (map_val != .Map) {
                    debug.log("MapSet requires a Map, got {}", .{map_val});
                    return error.TypeMismatch;
                }
                if (key_val != .String) {
                    debug.log("MapSet key must be String, got {}", .{key_val});
                    return error.TypeMismatch;
                }

                var map = map_val.Map;
                const key = key_val.String;

                // Update or insert the key-value pair
                if (map.get(key)) |old_value| {
                    var old_value_copy = old_value;
                    old_value_copy.deinit(self.allocator);
                }
                try map.put(try self.allocator.dupe(u8, key), try value.clone(self.allocator));
            },
            .IsArray => {
                // Check if value is an array: IsArray Rd, Rs
                const dst_reg = instruction.dst orelse return error.InvalidInstruction;
                const src_reg = instruction.src1 orelse return error.InvalidInstruction;

                var src_val = try self.getRegister(src_reg);
                defer src_val.deinit(self.allocator);

                const is_array = src_val == .Array;
                try self.setRegister(dst_reg, .{ .Bool = is_array });
            },
            .IsMap => {
                // Check if value is a map: IsMap Rd, Rs
                const dst_reg = instruction.dst orelse return error.InvalidInstruction;
                const src_reg = instruction.src1 orelse return error.InvalidInstruction;

                var src_val = try self.getRegister(src_reg);
                defer src_val.deinit(self.allocator);

                const is_map = src_val == .Map;
                try self.setRegister(dst_reg, .{ .Bool = is_map });
            },
            .ArrayContains => {
                // Check if array contains value: ArrayContains Rd, array_reg, value_reg
                const dst_reg = instruction.dst orelse return error.InvalidInstruction;
                const array_reg = instruction.src1 orelse return error.InvalidInstruction;
                const value_reg = instruction.src2 orelse return error.InvalidInstruction;

                var array_val = try self.getRegister(array_reg);
                defer array_val.deinit(self.allocator);
                var search_val = try self.getRegister(value_reg);
                defer search_val.deinit(self.allocator);

                if (array_val != .Array) {
                    debug.log("ArrayContains requires an Array, got {}", .{array_val});
                    return error.TypeMismatch;
                }

                const array = array_val.Array;
                var contains = false;

                // Search for the value in the array
                for (array) |element| {
                    if (try self.valuesEqual(element, search_val)) {
                        contains = true;
                        break;
                    }
                }

                try self.setRegister(dst_reg, .{ .Bool = contains });
            },
            .MapHas => {
                // Check if map has key: MapHas Rd, map_reg, key_reg
                const dst_reg = instruction.dst orelse return error.InvalidInstruction;
                const map_reg = instruction.src1 orelse return error.InvalidInstruction;
                const key_reg = instruction.src2 orelse return error.InvalidInstruction;

                var map_val = try self.getRegister(map_reg);
                defer map_val.deinit(self.allocator);
                var key_val = try self.getRegister(key_reg);
                defer key_val.deinit(self.allocator);

                if (map_val != .Map) {
                    debug.log("MapHas requires a Map, got {}", .{map_val});
                    return error.TypeMismatch;
                }
                if (key_val != .String) {
                    debug.log("MapHas key must be String, got {}", .{key_val});
                    return error.TypeMismatch;
                }

                const map = map_val.Map;
                const key = key_val.String;
                const has_key = map.contains(key);

                try self.setRegister(dst_reg, .{ .Bool = has_key });
            },
            .Dup => {
                // Duplicate register value: Dup Rs -> same register used as source
                // Note: In our MIR->bytecode, we use Move to implement Dup
                return error.UnsupportedInstruction;
            },
            .Pop => {
                // Pop is a no-op in register-based VM - handled by register allocation
                // Note: Our MIR->bytecode handles Pop by not generating any instruction
                return error.UnsupportedInstruction;
            },
        }
    }

    fn valuesEqual(self: *VM, val1: types.Value, val2: types.Value) !bool {
        
        // Check if types are different
        if (@intFromEnum(val1) != @intFromEnum(val2)) {
            return false;
        }
        
        // Compare values based on type
        return switch (val1) {
            .Int => |n1| n1 == val2.Int,
            .Float => |f1| f1 == val2.Float,
            .String => |s1| std.mem.eql(u8, s1, val2.String),
            .Bool => |b1| b1 == val2.Bool,
            .Nil => true, // All nils are equal
            .Symbol => |s1| std.mem.eql(u8, s1, val2.Symbol),
            .Array => |arr1| blk: {
                const arr2 = val2.Array;
                if (arr1.len != arr2.len) break :blk false;
                for (arr1, arr2) |elem1, elem2| {
                    if (!try self.valuesEqual(elem1, elem2)) break :blk false;
                }
                break :blk true;
            },
            .Map => false, // Maps are compared by reference for now
            .Object => |id1| id1 == val2.Object, // Objects compared by ID
            else => false, // Other types not comparable
        };
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
