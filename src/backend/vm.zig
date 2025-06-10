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
    DivisionByZero,
} || std.mem.Allocator.Error || std.fs.File.WriteError;

pub const CallFrame = struct {
    func: *const bytecode.Function, // Function being executed
    pc: usize, // Program counter within the function
    bp: usize, // Base pointer (stack frame base)
    prev_bp: usize, // Previous base pointer
    return_addr: usize, // Return address (instruction index to return to)

    pub fn init(func: *const bytecode.Function, bp: usize, prev_bp: usize, return_addr: usize) CallFrame {
        return CallFrame{
            .func = func,
            .pc = 0,
            .bp = bp,
            .prev_bp = prev_bp,
            .return_addr = return_addr,
        };
    }
};

pub const VM = struct {
    allocator: std.mem.Allocator,
    stdout: std.fs.File.Writer,
    stack: std.ArrayList(types.Value),
    variables: std.StringArrayHashMap(types.Value),
    call_frames: std.ArrayList(CallFrame),
    current_func: ?*const bytecode.Function, // Current function being executed
    pc: usize, // Program counter
    bp: usize, // Base pointer (stack frame base)
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
            .stack = std.ArrayList(types.Value).init(allocator),
            .variables = variables,
            .call_frames = std.ArrayList(CallFrame).init(allocator),
            .current_func = null,
            .pc = 0,
            .bp = 0,
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

        for (self.stack.items) |*value| {
            value.deinit(self.allocator);
        }
        self.stack.deinit();

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

    pub fn execute(self: *VM, func: *const bytecode.Function) VMError!void {
        debug.log("Executing function: {s}", .{func.name});
        self.current_func = func;
        self.pc = 0;

        // Print all instructions before execution
        debug.log("Instructions:", .{});
        for (func.instructions.items, 0..) |instr, i| {
            debug.log("  [{d}] {s}", .{ i, @tagName(instr.op) });
            if (instr.operand) |operand| {
                switch (operand) {
                    .Int => |val| debug.log("    Operand: Int {d}", .{val}),
                    .String => |val| debug.log("    Operand: String \"{s}\"", .{val}),
                    else => debug.log("    Operand: Other", .{}),
                }
            }
        }

        while (true) {
            // Use current_func if available, otherwise use the original func
            const executing_func = self.current_func orelse func;

            // Check if we're done with the current function
            if (self.pc >= executing_func.instructions.items.len) {
                // If we have no call frames, we're done with the program
                if (self.call_frames.items.len == 0) {
                    break;
                }

                // Otherwise, return to the caller
                if (self.call_frames.pop()) |frame| {
                    self.current_func = frame.func;
                    self.pc = frame.return_addr;
                    self.bp = frame.prev_bp;
                }
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

    fn executeInstruction(self: *VM, instruction: bytecode.Instruction) VMError!void {
        switch (instruction.op) {
            .Add => {
                debug.log("Add instruction - stack before:", .{});
                for (0..self.stack.items.len) |i| {
                    debug.log("  Stack[{}] = {any}", .{ i, self.stack.items[i] });
                }

                if (self.stack.items.len < 2) {
                    return error.StackUnderflow;
                }

                // Get values from the stack (without removing them yet)
                const right_ref = self.stack.items[self.stack.items.len - 1];
                const left_ref = self.stack.items[self.stack.items.len - 2];

                // Clone the values to avoid use-after-free issues
                var right = try right_ref.clone(self.allocator);
                var left = try left_ref.clone(self.allocator);

                // Now remove the items from the stack
                self.stack.items.len -= 2;

                // Handle addition based on types
                switch (left) {
                    .Int => |left_val| switch (right) {
                        .Int => |right_val| try self.stack.append(.{ .Int = left_val + right_val }),
                        .Float => |right_val| try self.stack.append(.{ .Float = @as(f64, @floatFromInt(left_val)) + right_val }),
                        else => {
                            debug.log("TypeMismatch in Add: left={}, right={}", .{ left, right });
                            return error.TypeMismatch;
                        },
                    },
                    .Float => |left_val| switch (right) {
                        .Int => |right_val| try self.stack.append(.{ .Float = left_val + @as(f64, @floatFromInt(right_val)) }),
                        .Float => |right_val| try self.stack.append(.{ .Float = left_val + right_val }),
                        else => {
                            debug.log("TypeMismatch in Add: left={}, right={}", .{ left, right });
                            return error.TypeMismatch;
                        },
                    },
                    .String => |left_val| switch (right) {
                        .String => |right_val| {
                            const result = try std.fmt.allocPrint(self.allocator, "{s}{s}", .{ left_val, right_val });
                            try self.stack.append(.{ .String = result });
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
                }

                // Clean up operands
                left.deinit(self.allocator);
                right.deinit(self.allocator);
            },
            .LoadConst => {
                debug.logValue(instruction.operand.?);
                // For functions, don't clone - just use the reference directly
                const value = if (instruction.operand.? == .Function)
                    instruction.operand.?
                else
                    try instruction.operand.?.clone(self.allocator);
                try self.stack.append(value);
            },
            .LoadVar => {
                const name = switch (instruction.operand.?) {
                    .String => |str| str,
                    .Symbol => |sym| sym,
                    else => {
                        debug.log("LoadVar: unexpected operand type: {any}", .{instruction.operand.?});
                        return error.TypeMismatch;
                    },
                };
                debug.log("LoadVar: {s}", .{name});
                debug.log("Current stack size: {}", .{self.stack.items.len});
                debug.log("Current base pointer: {}", .{self.bp});
                debug.log("Current call frames: {}", .{self.call_frames.items.len});

                // First check if it's a function parameter
                if (self.current_func) |current_func| {
                    debug.log("Checking parameters in current function: {s}", .{current_func.name});
                    debug.log("Current function has {} parameters", .{current_func.param_count});

                    // For simplicity, we'll handle the first parameter as 'n' for now
                    // In a real implementation, we'd have a mapping of parameter names  
                    if ((std.mem.eql(u8, name, "n") or std.mem.eql(u8, name, "x")) and current_func.param_count > 0) {
                        if (self.bp < self.stack.items.len) {
                            debug.log("Checking for parameter {s} at bp={}", .{ name, self.bp });
                            const param_value = self.stack.items[self.bp];
                            debug.log("Found parameter '{s}' at bp={}: {any}", .{ name, self.bp, param_value });
                            try self.stack.append(try param_value.clone(self.allocator));
                            return;
                        }
                    }
                }

                // Check for built-in operators
                if (std.mem.eql(u8, name, "==")) {
                    // Handle == as a built-in operator by pushing a special function onto the stack
                    try self.stack.append(.{ .BuiltinOperator = .Eq });
                    // No need to free the name since we didn't allocate it
                } else if (std.mem.eql(u8, name, "+")) {
                    // Handle + as a built-in operator
                    try self.stack.append(.{ .BuiltinOperator = .Add });
                } else {
                    // If not a parameter, check global scope
                    debug.log("Checking global scope for variable: {s}", .{name});
                    debug.log("Variables in scope: {}", .{self.variables.count()});
                    var it = self.variables.iterator();
                    while (it.next()) |entry| {
                        debug.log("  Variable: {s}", .{entry.key_ptr.*});
                    }

                    if (self.variables.get(name)) |value| {
                        debug.log("Found variable in global scope: {any}", .{value});
                        try self.stack.append(try value.clone(self.allocator));
                    } else {
                        debug.log("ERROR: Variable {s} not found", .{name});
                        return error.UndefinedVariable;
                    }
                }
            },
            .LoadParam => {
                debug.log("LoadParam instruction", .{});
                if (instruction.operand == null) return error.UnsupportedInstruction;
                if (instruction.operand.? != .Int) return error.TypeMismatch;
                const param_index = @as(usize, @intCast(instruction.operand.?.Int));
                debug.log("LoadParam: {}", .{param_index});

                // Debug: print stack contents around bp
                debug.log("Stack contents around bp={}", .{self.bp});
                const start = if (self.bp > 2) self.bp - 2 else 0;
                const end = if (self.bp + 3 < self.stack.items.len) self.bp + 3 else self.stack.items.len;
                for (start..end) |i| {
                    debug.log("  Stack[{}] = {any}", .{ i, self.stack.items[i] });
                }

                // Calculate the position of the parameter on the stack
                // Parameters are stored at bp, bp+1, bp+2, etc.
                const param_pos = self.bp + param_index;

                if (param_pos >= self.stack.items.len) {
                    debug.log("Parameter index out of bounds: {} (stack size: {})", .{ param_pos, self.stack.items.len });
                    return error.StackUnderflow;
                }

                // Clone the parameter value and push it onto the stack
                const param_value = try self.stack.items[param_pos].clone(self.allocator);
                try self.stack.append(param_value);
                debug.log("Loaded parameter: {any}", .{param_value});
            },
            .StoreVar => {
                debug.log("StoreVar instruction", .{});
                if (self.stack.items.len < 1) {
                    return error.StackUnderflow;
                }

                const name = switch (instruction.operand.?) {
                    .String => |str| str,
                    .Symbol => |sym| sym,
                    else => {
                        debug.log("StoreVar: unexpected operand type: {any}", .{instruction.operand.?});
                        return error.TypeMismatch;
                    },
                };
                debug.log("StoreVar: {s}", .{name});

                // Get the value from the stack (without removing it yet)
                const value_ref = self.stack.items[self.stack.items.len - 1];

                // Clone the value to avoid use-after-free issues
                const value = try value_ref.clone(self.allocator);

                // Now remove the item from the stack
                self.stack.items.len -= 1;

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
                debug.log("StoreGlobal instruction", .{});
                if (self.stack.items.len < 1) {
                    return error.StackUnderflow;
                }

                const name = switch (instruction.operand.?) {
                    .String => |str| str,
                    .Symbol => |sym| sym,
                    else => {
                        debug.log("StoreGlobal: unexpected operand type: {any}", .{instruction.operand.?});
                        return error.TypeMismatch;
                    },
                };
                debug.log("StoreGlobal: {s}", .{name});

                // Get the value from the stack (without removing it yet)
                const value_ref = self.stack.items[self.stack.items.len - 1];

                // Clone the value to avoid use-after-free issues
                const value = try value_ref.clone(self.allocator);

                // Now remove the item from the stack
                self.stack.items.len -= 1;

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

                debug.log("Stored global variable {s} = {any}", .{ name, value });
            },
            .Sub => {
                debug.log("Sub instruction - stack before:", .{});
                for (0..self.stack.items.len) |i| {
                    debug.log("  Stack[{}] = {any}", .{ i, self.stack.items[i] });
                }

                if (self.stack.items.len < 2) {
                    return error.StackUnderflow;
                }

                // Get values from the stack (without removing them yet)
                const right_ref = self.stack.items[self.stack.items.len - 1];
                const left_ref = self.stack.items[self.stack.items.len - 2];

                // Clone the values to avoid use-after-free issues
                var right = try right_ref.clone(self.allocator);
                var left = try left_ref.clone(self.allocator);

                // Now remove the items from the stack
                self.stack.items.len -= 2;

                // Handle subtraction based on types
                switch (left) {
                    .Int => |left_val| switch (right) {
                        .Int => |right_val| try self.stack.append(.{ .Int = left_val - right_val }),
                        .Float => |right_val| try self.stack.append(.{ .Float = @as(f64, @floatFromInt(left_val)) - right_val }),
                        else => {
                            debug.log("TypeMismatch in Sub: left={}, right={}", .{ left, right });
                            return error.TypeMismatch;
                        },
                    },
                    .Float => |left_val| switch (right) {
                        .Int => |right_val| try self.stack.append(.{ .Float = left_val - @as(f64, @floatFromInt(right_val)) }),
                        .Float => |right_val| try self.stack.append(.{ .Float = left_val - right_val }),
                        else => {
                            debug.log("TypeMismatch in Sub: left={}, right={}", .{ left, right });
                            return error.TypeMismatch;
                        },
                    },
                    else => {
                        debug.log("TypeMismatch in Sub: left={}, right={}", .{ left, right });
                        return error.TypeMismatch;
                    },
                }

                // Clean up operands
                left.deinit(self.allocator);
                right.deinit(self.allocator);
            },
            .Mul => {
                if (self.stack.items.len < 2) {
                    return error.StackUnderflow;
                }

                const right_ref = self.stack.items[self.stack.items.len - 1];
                const left_ref = self.stack.items[self.stack.items.len - 2];

                var right = try right_ref.clone(self.allocator);
                var left = try left_ref.clone(self.allocator);

                self.stack.items.len -= 2;

                switch (left) {
                    .Int => |left_val| switch (right) {
                        .Int => |right_val| try self.stack.append(.{ .Int = left_val * right_val }),
                        else => {
                            // Restore stack for proper deinit
                            try self.stack.append(left);
                            try self.stack.append(right);
                            left.deinit(self.allocator); // left was already cloned
                            right.deinit(self.allocator); // right was already cloned
                            return error.TypeMismatch;
                        },
                    },
                    else => {
                        // Restore stack for proper deinit
                        try self.stack.append(left);
                        try self.stack.append(right);
                        left.deinit(self.allocator);
                        right.deinit(self.allocator);
                        return error.TypeMismatch;
                    },
                }

                left.deinit(self.allocator);
                right.deinit(self.allocator);
            },
            .Div => {
                if (self.stack.items.len < 2) {
                    return error.StackUnderflow;
                }

                const right_ref = self.stack.items[self.stack.items.len - 1];
                const left_ref = self.stack.items[self.stack.items.len - 2];

                var right = try right_ref.clone(self.allocator);
                var left = try left_ref.clone(self.allocator);

                self.stack.items.len -= 2;

                switch (left) {
                    .Int => |left_val| switch (right) {
                        .Int => |right_val| {
                            if (right_val == 0) {
                                // Restore stack for proper deinit
                                try self.stack.append(left);
                                try self.stack.append(right);
                                left.deinit(self.allocator);
                                right.deinit(self.allocator);
                                return error.DivisionByZero;
                            }
                            try self.stack.append(.{ .Int = @divTrunc(left_val, right_val) });
                        },
                        else => {
                            // Restore stack for proper deinit
                            try self.stack.append(left);
                            try self.stack.append(right);
                            left.deinit(self.allocator);
                            right.deinit(self.allocator);
                            return error.TypeMismatch;
                        },
                    },
                    else => {
                        // Restore stack for proper deinit
                        try self.stack.append(left);
                        try self.stack.append(right);
                        left.deinit(self.allocator);
                        right.deinit(self.allocator);
                        return error.TypeMismatch;
                    },
                }

                left.deinit(self.allocator);
                right.deinit(self.allocator);
            },
            .Lt => {
                debug.log("Lt instruction - stack before:", .{});
                for (0..self.stack.items.len) |i| {
                    debug.log("  Stack[{}] = {any}", .{ i, self.stack.items[i] });
                }

                if (self.stack.items.len < 2) {
                    return error.StackUnderflow;
                }

                // Get values from the stack (without removing them yet)
                const right_ref = self.stack.items[self.stack.items.len - 1];
                const left_ref = self.stack.items[self.stack.items.len - 2];

                // Clone the values to avoid use-after-free issues
                var right = try right_ref.clone(self.allocator);
                var left = try left_ref.clone(self.allocator);

                // Now remove the items from the stack
                self.stack.items.len -= 2;

                // Handle comparison based on types
                const result = switch (left) {
                    .Int => |left_val| switch (right) {
                        .Int => |right_val| left_val < right_val,
                        .Float => |right_val| @as(f64, @floatFromInt(left_val)) < right_val,
                        else => return error.TypeMismatch,
                    },
                    .Float => |left_val| switch (right) {
                        .Int => |right_val| left_val < @as(f64, @floatFromInt(right_val)),
                        .Float => |right_val| left_val < right_val,
                        else => return error.TypeMismatch,
                    },
                    else => return error.TypeMismatch,
                };

                // Clean up operands
                left.deinit(self.allocator);
                right.deinit(self.allocator);

                // Push result onto stack
                try self.stack.append(.{ .Bool = result });
            },
            .Gt => {
                if (self.stack.items.len < 2) {
                    return error.StackUnderflow;
                }

                // Get values from the stack (without removing them yet)
                const right_ref = self.stack.items[self.stack.items.len - 1];
                const left_ref = self.stack.items[self.stack.items.len - 2];

                // Clone the values to avoid use-after-free issues
                var right = try right_ref.clone(self.allocator);
                var left = try left_ref.clone(self.allocator);

                self.stack.items.len -= 2;

                // Handle comparison based on types
                const result = switch (left) {
                    .Int => |left_val| switch (right) {
                        .Int => |right_val| left_val > right_val,
                        .Float => |right_val| @as(f64, @floatFromInt(left_val)) > right_val,
                        else => return error.TypeMismatch,
                    },
                    .Float => |left_val| switch (right) {
                        .Int => |right_val| left_val > @as(f64, @floatFromInt(right_val)),
                        .Float => |right_val| left_val > right_val,
                        else => return error.TypeMismatch,
                    },
                    else => return error.TypeMismatch,
                };

                // Clean up operands
                left.deinit(self.allocator);
                right.deinit(self.allocator);

                // Push result onto stack
                try self.stack.append(.{ .Bool = result });
            },
            .Eq => {
                if (self.stack.items.len < 2) {
                    return error.StackUnderflow;
                }

                // Get values from the stack (without removing them yet)
                const right_ref = self.stack.items[self.stack.items.len - 1];
                const left_ref = self.stack.items[self.stack.items.len - 2];

                // Clone the values to avoid use-after-free issues
                var right = try right_ref.clone(self.allocator);
                var left = try left_ref.clone(self.allocator);

                self.stack.items.len -= 2;

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

                // Clean up operands
                left.deinit(self.allocator);
                right.deinit(self.allocator);

                // Push result onto stack
                try self.stack.append(.{ .Bool = result });
            },
            .Print => {
                if (self.stack.items.len < 1) {
                    return error.StackUnderflow;
                }
                // Clone the value before removing it from the stack
                var value = try self.stack.items[self.stack.items.len - 1].clone(self.allocator);
                self.stack.items.len -= 1; // Remove the item
                switch (value) {
                    .Int => |val| try self.stdout.print("{d}\n", .{val}),
                    .String => |str| try self.stdout.print("{s}\n", .{str}),
                    .Symbol => |sym| try self.stdout.print("{s}\n", .{sym}),
                    .Bool => |b| try self.stdout.print("{}", .{b}),
                    .Float => |f| try self.stdout.print("{d}\n", .{f}),
                    .Nil => try self.stdout.print("nil\n", .{}),
                    .Array => |arr| try self.stdout.print("{any}\n", .{arr}),
                    .Map => |map| try self.stdout.print("{any}\n", .{map}),
                    .ReturnAddress => |addr| try self.stdout.print("ReturnAddress: {any}", .{addr}),
                    .Function => |func| try self.stdout.print("Function: {any}\n", .{func}),
                    .Variable => |var_val| try self.stdout.print("Variable: {s}\n", .{var_val.name}),
                    .BuiltinOperator => |op| try self.stdout.print("BuiltinOperator: {any}\n", .{op}),
                }
                value.deinit(self.allocator);
            },
            .Call => {
                debug.log("Call operation", .{});
                // Get function and arguments
                const arg_count = instruction.operand.?.Int;
                const arg_count_usize = @as(usize, @intCast(arg_count));

                if (self.stack.items.len < arg_count_usize + 1) {
                    return error.StackUnderflow;
                }

                // Debug: print stack before function call
                debug.log("Stack before function call (arg_count={}):", .{arg_count});
                const start = if (self.stack.items.len > 5) self.stack.items.len - 5 else 0;
                for (start..self.stack.items.len) |i| {
                    debug.log("  Stack[{}] = {any}", .{ i, self.stack.items[i] });
                }

                // Get function from stack
                // Check if we have a variable with the name 'print' in our variables map
                const func_val = self.stack.items[self.stack.items.len - arg_count_usize - 1];
                debug.logValue(func_val);

                // Handle built-in functions
                if (func_val == .Variable) {
                    const func_name = func_val.Variable.name;

                    // Handle print function
                    if (std.mem.eql(u8, func_name, "print")) {
                        // Print can take any number of arguments
                        for (0..arg_count_usize) |i| {
                            const arg_index = self.stack.items.len - arg_count_usize + i;
                            const arg = self.stack.items[arg_index];

                            // Clone the argument before printing
                            var arg_clone = try arg.clone(self.allocator);
                            errdefer arg_clone.deinit(self.allocator);

                            // Print without newline except for the last argument
                            switch (arg_clone) {
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

                            arg_clone.deinit(self.allocator);
                        }

                        // Add a newline at the end
                        try self.stdout.print("\n", .{});

                        // Clean up the stack - remove function AND argument(s)
                        // Deinit values before shrinking
                        for (self.stack.items[self.stack.items.len - arg_count_usize - 1 ..]) |*v| {
                            v.deinit(self.allocator);
                        }
                        self.stack.items.len -= (arg_count_usize + 1); // Remove function and arguments

                        // Push nil as the return value
                        try self.stack.append(.{ .Nil = {} });

                        return;
                    }
                }

                // Handle built-in operators
                if (func_val == .BuiltinOperator) {
                    debug.log("Executing built-in operator: {any}", .{func_val.BuiltinOperator});

                    switch (func_val.BuiltinOperator) {
                        .Eq => {
                            // Check if we have two arguments
                            if (arg_count_usize != 2) {
                                return error.ArgumentCountMismatch;
                            }

                            // Get the arguments
                            const right_ref = self.stack.items[self.stack.items.len - 1];
                            const left_ref = self.stack.items[self.stack.items.len - 2];

                            // Clone the values to avoid use-after-free issues
                            var right = try right_ref.clone(self.allocator);
                            var left = try left_ref.clone(self.allocator);

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

                            // Clean up the stack - remove operator AND arguments
                            // Deinit values before shrinking
                            for (self.stack.items[self.stack.items.len - arg_count_usize - 1 ..]) |*v| {
                                v.deinit(self.allocator);
                            }
                            self.stack.items.len -= (arg_count_usize + 1); // Remove operator and arguments

                            // Clean up operands
                            left.deinit(self.allocator);
                            right.deinit(self.allocator);

                            // Push result onto stack
                            try self.stack.append(.{ .Bool = result });

                            return;
                        },
                        .Add => {
                            // Check if we have two arguments
                            if (arg_count_usize != 2) {
                                return error.ArgumentCountMismatch;
                            }

                            // Get the arguments
                            const right_ref = self.stack.items[self.stack.items.len - 1];
                            const left_ref = self.stack.items[self.stack.items.len - 2];

                            // Clone the values to avoid use-after-free issues
                            var right = try right_ref.clone(self.allocator);
                            var left = try left_ref.clone(self.allocator);

                            // Handle addition based on types and store result temporarily
                            const result_value = switch (left) {
                                .Int => |left_val| switch (right) {
                                    .Int => |right_val| types.Value{ .Int = left_val + right_val },
                                    .Float => |right_val| types.Value{ .Float = @as(f64, @floatFromInt(left_val)) + right_val },
                                    else => return error.TypeMismatch,
                                },
                                .Float => |left_val| switch (right) {
                                    .Int => |right_val| types.Value{ .Float = left_val + @as(f64, @floatFromInt(right_val)) },
                                    .Float => |right_val| types.Value{ .Float = left_val + right_val },
                                    else => return error.TypeMismatch,
                                },
                                .String => |left_val| switch (right) {
                                    .String => |right_val| types.Value{ .String = try std.fmt.allocPrint(self.allocator, "{s}{s}", .{ left_val, right_val }) },
                                    else => return error.TypeMismatch,
                                },
                                else => return error.TypeMismatch,
                            };

                            // Clean up the stack - remove operator AND arguments
                            // Deinit values before shrinking
                            for (self.stack.items[self.stack.items.len - arg_count_usize - 1 ..]) |*v| {
                                v.deinit(self.allocator);
                            }
                            self.stack.items.len -= (arg_count_usize + 1); // Remove operator and arguments

                            // Clean up operands
                            left.deinit(self.allocator);
                            right.deinit(self.allocator);

                            // Push result onto stack
                            try self.stack.append(result_value);

                            return;
                        },
                        .Sub => {
                            // Check if we have two arguments
                            if (arg_count_usize != 2) {
                                return error.ArgumentCountMismatch;
                            }

                            // Get the arguments
                            const right_ref = self.stack.items[self.stack.items.len - 1];
                            const left_ref = self.stack.items[self.stack.items.len - 2];

                            // Clone the values to avoid use-after-free issues
                            var right = try right_ref.clone(self.allocator);
                            var left = try left_ref.clone(self.allocator);

                            // Handle subtraction based on types
                            const result_value = switch (left) {
                                .Int => |left_val| switch (right) {
                                    .Int => |right_val| types.Value{ .Int = left_val - right_val },
                                    .Float => |right_val| types.Value{ .Float = @as(f64, @floatFromInt(left_val)) - right_val },
                                    else => types.Value{ .Nil = {} }, // Type error, return nil
                                },
                                .Float => |left_val| switch (right) {
                                    .Int => |right_val| types.Value{ .Float = left_val - @as(f64, @floatFromInt(right_val)) },
                                    .Float => |right_val| types.Value{ .Float = left_val - right_val },
                                    else => types.Value{ .Nil = {} }, // Type error, return nil
                                },
                                else => types.Value{ .Nil = {} }, // Type error, return nil
                            };

                            // Clean up the stack - remove operator AND arguments
                            for (self.stack.items[self.stack.items.len - arg_count_usize - 1 ..]) |*v| {
                                v.deinit(self.allocator);
                            }
                            self.stack.items.len -= (arg_count_usize + 1);

                            // Clean up operands
                            left.deinit(self.allocator);
                            right.deinit(self.allocator);

                            // Push result onto stack
                            try self.stack.append(result_value);

                            return;
                        },
                        .Mul => {
                            // Check if we have two arguments
                            if (arg_count_usize != 2) {
                                return error.ArgumentCountMismatch;
                            }

                            // Get the arguments
                            const right_ref = self.stack.items[self.stack.items.len - 1];
                            const left_ref = self.stack.items[self.stack.items.len - 2];

                            // Clone the values to avoid use-after-free issues
                            var right = try right_ref.clone(self.allocator);
                            var left = try left_ref.clone(self.allocator);

                            // Handle multiplication based on types
                            const result_value = switch (left) {
                                .Int => |left_val| switch (right) {
                                    .Int => |right_val| types.Value{ .Int = left_val * right_val },
                                    .Float => |right_val| types.Value{ .Float = @as(f64, @floatFromInt(left_val)) * right_val },
                                    else => types.Value{ .Nil = {} }, // Type error, return nil
                                },
                                .Float => |left_val| switch (right) {
                                    .Int => |right_val| types.Value{ .Float = left_val * @as(f64, @floatFromInt(right_val)) },
                                    .Float => |right_val| types.Value{ .Float = left_val * right_val },
                                    else => types.Value{ .Nil = {} }, // Type error, return nil
                                },
                                else => types.Value{ .Nil = {} }, // Type error, return nil
                            };

                            // Clean up the stack - remove operator AND arguments
                            for (self.stack.items[self.stack.items.len - arg_count_usize - 1 ..]) |*v| {
                                v.deinit(self.allocator);
                            }
                            self.stack.items.len -= (arg_count_usize + 1);

                            // Clean up operands
                            left.deinit(self.allocator);
                            right.deinit(self.allocator);

                            // Push result onto stack
                            try self.stack.append(result_value);

                            return;
                        },
                        .Div => {
                            // Check if we have two arguments
                            if (arg_count_usize != 2) {
                                return error.ArgumentCountMismatch;
                            }

                            // Get the arguments
                            const right_ref = self.stack.items[self.stack.items.len - 1];
                            const left_ref = self.stack.items[self.stack.items.len - 2];

                            // Clone the values to avoid use-after-free issues
                            var right = try right_ref.clone(self.allocator);
                            var left = try left_ref.clone(self.allocator);

                            // Handle division based on types
                            const result_value = switch (left) {
                                .Int => |left_val| switch (right) {
                                    .Int => |right_val| blk: {
                                        if (right_val == 0) break :blk types.Value{ .Nil = {} }; // Division by zero
                                        break :blk types.Value{ .Int = @divTrunc(left_val, right_val) };
                                    },
                                    .Float => |right_val| blk: {
                                        if (right_val == 0.0) break :blk types.Value{ .Nil = {} }; // Division by zero
                                        break :blk types.Value{ .Float = @as(f64, @floatFromInt(left_val)) / right_val };
                                    },
                                    else => types.Value{ .Nil = {} }, // Type error, return nil
                                },
                                .Float => |left_val| switch (right) {
                                    .Int => |right_val| blk: {
                                        if (right_val == 0) break :blk types.Value{ .Nil = {} }; // Division by zero
                                        break :blk types.Value{ .Float = left_val / @as(f64, @floatFromInt(right_val)) };
                                    },
                                    .Float => |right_val| blk: {
                                        if (right_val == 0.0) break :blk types.Value{ .Nil = {} }; // Division by zero
                                        break :blk types.Value{ .Float = left_val / right_val };
                                    },
                                    else => types.Value{ .Nil = {} }, // Type error, return nil
                                },
                                else => types.Value{ .Nil = {} }, // Type error, return nil
                            };

                            // Clean up the stack - remove operator AND arguments
                            for (self.stack.items[self.stack.items.len - arg_count_usize - 1 ..]) |*v| {
                                v.deinit(self.allocator);
                            }
                            self.stack.items.len -= (arg_count_usize + 1);

                            // Clean up operands
                            left.deinit(self.allocator);
                            right.deinit(self.allocator);

                            // Push result onto stack
                            try self.stack.append(result_value);

                            return;
                        },
                        .LessThan => {
                            // Check if we have two arguments
                            if (arg_count_usize != 2) {
                                return error.ArgumentCountMismatch;
                            }

                            // Get the arguments
                            const right_ref = self.stack.items[self.stack.items.len - 1];
                            const left_ref = self.stack.items[self.stack.items.len - 2];

                            // Clone the values to avoid use-after-free issues
                            var right = try right_ref.clone(self.allocator);
                            var left = try left_ref.clone(self.allocator);

                            // Handle less than comparison based on types
                            const result = switch (left) {
                                .Int => |left_val| switch (right) {
                                    .Int => |right_val| left_val < right_val,
                                    .Float => |right_val| @as(f64, @floatFromInt(left_val)) < right_val,
                                    else => false, // Type error, return false
                                },
                                .Float => |left_val| switch (right) {
                                    .Int => |right_val| left_val < @as(f64, @floatFromInt(right_val)),
                                    .Float => |right_val| left_val < right_val,
                                    else => false, // Type error, return false
                                },
                                else => false, // Type error, return false
                            };

                            // Clean up the stack - remove operator AND arguments
                            for (self.stack.items[self.stack.items.len - arg_count_usize - 1 ..]) |*v| {
                                v.deinit(self.allocator);
                            }
                            self.stack.items.len -= (arg_count_usize + 1);

                            // Clean up operands
                            left.deinit(self.allocator);
                            right.deinit(self.allocator);

                            // Push result onto stack
                            try self.stack.append(.{ .Bool = result });

                            return;
                        },
                        .GreaterThan => {
                            // Check if we have two arguments
                            if (arg_count_usize != 2) {
                                return error.ArgumentCountMismatch;
                            }

                            // Get the arguments
                            const right_ref = self.stack.items[self.stack.items.len - 1];
                            const left_ref = self.stack.items[self.stack.items.len - 2];

                            // Clone the values to avoid use-after-free issues
                            var right = try right_ref.clone(self.allocator);
                            var left = try left_ref.clone(self.allocator);

                            // Handle greater than comparison based on types
                            const result = switch (left) {
                                .Int => |left_val| switch (right) {
                                    .Int => |right_val| left_val > right_val,
                                    .Float => |right_val| @as(f64, @floatFromInt(left_val)) > right_val,
                                    else => false, // Type error, return false
                                },
                                .Float => |left_val| switch (right) {
                                    .Int => |right_val| left_val > @as(f64, @floatFromInt(right_val)),
                                    .Float => |right_val| left_val > right_val,
                                    else => false, // Type error, return false
                                },
                                else => false, // Type error, return false
                            };

                            // Clean up the stack - remove operator AND arguments
                            for (self.stack.items[self.stack.items.len - arg_count_usize - 1 ..]) |*v| {
                                v.deinit(self.allocator);
                            }
                            self.stack.items.len -= (arg_count_usize + 1);

                            // Clean up operands
                            left.deinit(self.allocator);
                            right.deinit(self.allocator);

                            // Push result onto stack
                            try self.stack.append(.{ .Bool = result });

                            return;
                        },
                        .Ne => {
                            // Not equal operator
                            if (arg_count_usize != 2) {
                                return error.ArgumentCountMismatch;
                            }
                            
                            const right_ref = self.stack.items[self.stack.items.len - 1];
                            const left_ref = self.stack.items[self.stack.items.len - 2];
                            
                            var right = try right_ref.clone(self.allocator);
                            var left = try left_ref.clone(self.allocator);
                            
                            // Reuse equality logic but negate result
                            const equal = switch (left) {
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
                                .String => |left_val| switch (right) {
                                    .String => |right_val| std.mem.eql(u8, left_val, right_val),
                                    else => false,
                                },
                                .Bool => |left_val| switch (right) {
                                    .Bool => |right_val| left_val == right_val,
                                    else => false,
                                },
                                .Nil => switch (right) {
                                    .Nil => true,
                                    else => false,
                                },
                                else => false,
                            };
                            
                            // Clean up the stack
                            for (self.stack.items[self.stack.items.len - arg_count_usize - 1 ..]) |*v| {
                                v.deinit(self.allocator);
                            }
                            self.stack.items.len -= (arg_count_usize + 1);
                            
                            left.deinit(self.allocator);
                            right.deinit(self.allocator);
                            
                            try self.stack.append(.{ .Bool = !equal });
                            return;
                        },
                        .Mod => {
                            // Modulo operator
                            if (arg_count_usize != 2) {
                                return error.ArgumentCountMismatch;
                            }
                            
                            const right_ref = self.stack.items[self.stack.items.len - 1];
                            const left_ref = self.stack.items[self.stack.items.len - 2];
                            
                            var right = try right_ref.clone(self.allocator);
                            var left = try left_ref.clone(self.allocator);
                            
                            const result_value = switch (left) {
                                .Int => |left_val| switch (right) {
                                    .Int => |right_val| blk: {
                                        if (right_val == 0) return error.DivisionByZero;
                                        break :blk types.Value{ .Int = @mod(left_val, right_val) };
                                    },
                                    else => return error.TypeMismatch,
                                },
                                else => return error.TypeMismatch,
                            };
                            
                            // Clean up the stack
                            for (self.stack.items[self.stack.items.len - arg_count_usize - 1 ..]) |*v| {
                                v.deinit(self.allocator);
                            }
                            self.stack.items.len -= (arg_count_usize + 1);
                            
                            left.deinit(self.allocator);
                            right.deinit(self.allocator);
                            
                            try self.stack.append(result_value);
                            return;
                        },
                        .Pow => {
                            // Power operator
                            if (arg_count_usize != 2) {
                                return error.ArgumentCountMismatch;
                            }
                            
                            const right_ref = self.stack.items[self.stack.items.len - 1];
                            const left_ref = self.stack.items[self.stack.items.len - 2];
                            
                            var right = try right_ref.clone(self.allocator);
                            var left = try left_ref.clone(self.allocator);
                            
                            const result_value = switch (left) {
                                .Int => |left_val| switch (right) {
                                    .Int => |right_val| blk: {
                                        if (right_val < 0) {
                                            // Convert to float for negative exponents
                                            const result = std.math.pow(f64, @as(f64, @floatFromInt(left_val)), @as(f64, @floatFromInt(right_val)));
                                            break :blk types.Value{ .Float = result };
                                        } else {
                                            const result = std.math.pow(i64, left_val, @as(u32, @intCast(right_val)));
                                            break :blk types.Value{ .Int = result };
                                        }
                                    },
                                    .Float => |right_val| blk: {
                                        const result = std.math.pow(f64, @as(f64, @floatFromInt(left_val)), right_val);
                                        break :blk types.Value{ .Float = result };
                                    },
                                    else => return error.TypeMismatch,
                                },
                                .Float => |left_val| switch (right) {
                                    .Int => |right_val| blk: {
                                        const result = std.math.pow(f64, left_val, @as(f64, @floatFromInt(right_val)));
                                        break :blk types.Value{ .Float = result };
                                    },
                                    .Float => |right_val| blk: {
                                        const result = std.math.pow(f64, left_val, right_val);
                                        break :blk types.Value{ .Float = result };
                                    },
                                    else => return error.TypeMismatch,
                                },
                                else => return error.TypeMismatch,
                            };
                            
                            // Clean up the stack
                            for (self.stack.items[self.stack.items.len - arg_count_usize - 1 ..]) |*v| {
                                v.deinit(self.allocator);
                            }
                            self.stack.items.len -= (arg_count_usize + 1);
                            
                            left.deinit(self.allocator);
                            right.deinit(self.allocator);
                            
                            try self.stack.append(result_value);
                            return;
                        },
                        .LessEqual => {
                            // Less than or equal operator
                            if (arg_count_usize != 2) {
                                return error.ArgumentCountMismatch;
                            }
                            
                            const right_ref = self.stack.items[self.stack.items.len - 1];
                            const left_ref = self.stack.items[self.stack.items.len - 2];
                            
                            var right = try right_ref.clone(self.allocator);
                            var left = try left_ref.clone(self.allocator);
                            
                            const result = switch (left) {
                                .Int => |left_val| switch (right) {
                                    .Int => |right_val| left_val <= right_val,
                                    .Float => |right_val| @as(f64, @floatFromInt(left_val)) <= right_val,
                                    else => false,
                                },
                                .Float => |left_val| switch (right) {
                                    .Int => |right_val| left_val <= @as(f64, @floatFromInt(right_val)),
                                    .Float => |right_val| left_val <= right_val,
                                    else => false,
                                },
                                else => false,
                            };
                            
                            // Clean up the stack
                            for (self.stack.items[self.stack.items.len - arg_count_usize - 1 ..]) |*v| {
                                v.deinit(self.allocator);
                            }
                            self.stack.items.len -= (arg_count_usize + 1);
                            
                            left.deinit(self.allocator);
                            right.deinit(self.allocator);
                            
                            try self.stack.append(.{ .Bool = result });
                            return;
                        },
                        .GreaterEqual => {
                            // Greater than or equal operator
                            if (arg_count_usize != 2) {
                                return error.ArgumentCountMismatch;
                            }
                            
                            const right_ref = self.stack.items[self.stack.items.len - 1];
                            const left_ref = self.stack.items[self.stack.items.len - 2];
                            
                            var right = try right_ref.clone(self.allocator);
                            var left = try left_ref.clone(self.allocator);
                            
                            const result = switch (left) {
                                .Int => |left_val| switch (right) {
                                    .Int => |right_val| left_val >= right_val,
                                    .Float => |right_val| @as(f64, @floatFromInt(left_val)) >= right_val,
                                    else => false,
                                },
                                .Float => |left_val| switch (right) {
                                    .Int => |right_val| left_val >= @as(f64, @floatFromInt(right_val)),
                                    .Float => |right_val| left_val >= right_val,
                                    else => false,
                                },
                                else => false,
                            };
                            
                            // Clean up the stack
                            for (self.stack.items[self.stack.items.len - arg_count_usize - 1 ..]) |*v| {
                                v.deinit(self.allocator);
                            }
                            self.stack.items.len -= (arg_count_usize + 1);
                            
                            left.deinit(self.allocator);
                            right.deinit(self.allocator);
                            
                            try self.stack.append(.{ .Bool = result });
                            return;
                        },
                        .And => {
                            // Logical and operator
                            if (arg_count_usize != 2) {
                                return error.ArgumentCountMismatch;
                            }
                            
                            const right_ref = self.stack.items[self.stack.items.len - 1];
                            const left_ref = self.stack.items[self.stack.items.len - 2];
                            
                            var right = try right_ref.clone(self.allocator);
                            var left = try left_ref.clone(self.allocator);
                            
                            // Convert values to boolean (truthy evaluation)
                            const left_bool = switch (left) {
                                .Bool => |b| b,
                                .Nil => false,
                                .Int => |i| i != 0,
                                .Float => |f| f != 0.0,
                                .String => |s| s.len > 0,
                                else => true, // Other values are truthy
                            };
                            
                            const right_bool = switch (right) {
                                .Bool => |b| b,
                                .Nil => false,
                                .Int => |i| i != 0,
                                .Float => |f| f != 0.0,
                                .String => |s| s.len > 0,
                                else => true, // Other values are truthy
                            };
                            
                            const result = left_bool and right_bool;
                            
                            // Clean up the stack
                            for (self.stack.items[self.stack.items.len - arg_count_usize - 1 ..]) |*v| {
                                v.deinit(self.allocator);
                            }
                            self.stack.items.len -= (arg_count_usize + 1);
                            
                            left.deinit(self.allocator);
                            right.deinit(self.allocator);
                            
                            try self.stack.append(.{ .Bool = result });
                            return;
                        },
                        .Or => {
                            // Logical or operator
                            if (arg_count_usize != 2) {
                                return error.ArgumentCountMismatch;
                            }
                            
                            const right_ref = self.stack.items[self.stack.items.len - 1];
                            const left_ref = self.stack.items[self.stack.items.len - 2];
                            
                            var right = try right_ref.clone(self.allocator);
                            var left = try left_ref.clone(self.allocator);
                            
                            // Convert values to boolean (truthy evaluation)
                            const left_bool = switch (left) {
                                .Bool => |b| b,
                                .Nil => false,
                                .Int => |i| i != 0,
                                .Float => |f| f != 0.0,
                                .String => |s| s.len > 0,
                                else => true, // Other values are truthy
                            };
                            
                            const right_bool = switch (right) {
                                .Bool => |b| b,
                                .Nil => false,
                                .Int => |i| i != 0,
                                .Float => |f| f != 0.0,
                                .String => |s| s.len > 0,
                                else => true, // Other values are truthy
                            };
                            
                            const result = left_bool or right_bool;
                            
                            // Clean up the stack
                            for (self.stack.items[self.stack.items.len - arg_count_usize - 1 ..]) |*v| {
                                v.deinit(self.allocator);
                            }
                            self.stack.items.len -= (arg_count_usize + 1);
                            
                            left.deinit(self.allocator);
                            right.deinit(self.allocator);
                            
                            try self.stack.append(.{ .Bool = result });
                            return;
                        },
                        .Not => {
                            // Logical not operator
                            if (arg_count_usize != 1) {
                                return error.ArgumentCountMismatch;
                            }
                            
                            const operand_ref = self.stack.items[self.stack.items.len - 1];
                            var operand = try operand_ref.clone(self.allocator);
                            
                            // Convert value to boolean (truthy evaluation)
                            const operand_bool = switch (operand) {
                                .Bool => |b| b,
                                .Nil => false,
                                .Int => |i| i != 0,
                                .Float => |f| f != 0.0,
                                .String => |s| s.len > 0,
                                else => true, // Other values are truthy
                            };
                            
                            const result = !operand_bool;
                            
                            // Clean up the stack
                            for (self.stack.items[self.stack.items.len - arg_count_usize - 1 ..]) |*v| {
                                v.deinit(self.allocator);
                            }
                            self.stack.items.len -= (arg_count_usize + 1);
                            
                            operand.deinit(self.allocator);
                            
                            try self.stack.append(.{ .Bool = result });
                            return;
                        },
                        .BitAnd => {
                            // Bitwise and operator
                            if (arg_count_usize != 2) {
                                return error.ArgumentCountMismatch;
                            }
                            
                            const right_ref = self.stack.items[self.stack.items.len - 1];
                            const left_ref = self.stack.items[self.stack.items.len - 2];
                            
                            var right = try right_ref.clone(self.allocator);
                            var left = try left_ref.clone(self.allocator);
                            
                            const result_value = switch (left) {
                                .Int => |left_val| switch (right) {
                                    .Int => |right_val| types.Value{ .Int = left_val & right_val },
                                    else => return error.TypeMismatch,
                                },
                                else => return error.TypeMismatch,
                            };
                            
                            // Clean up the stack
                            for (self.stack.items[self.stack.items.len - arg_count_usize - 1 ..]) |*v| {
                                v.deinit(self.allocator);
                            }
                            self.stack.items.len -= (arg_count_usize + 1);
                            
                            left.deinit(self.allocator);
                            right.deinit(self.allocator);
                            
                            try self.stack.append(result_value);
                            return;
                        },
                        .BitOr => {
                            // Bitwise or operator
                            if (arg_count_usize != 2) {
                                return error.ArgumentCountMismatch;
                            }
                            
                            const right_ref = self.stack.items[self.stack.items.len - 1];
                            const left_ref = self.stack.items[self.stack.items.len - 2];
                            
                            var right = try right_ref.clone(self.allocator);
                            var left = try left_ref.clone(self.allocator);
                            
                            const result_value = switch (left) {
                                .Int => |left_val| switch (right) {
                                    .Int => |right_val| types.Value{ .Int = left_val | right_val },
                                    else => return error.TypeMismatch,
                                },
                                else => return error.TypeMismatch,
                            };
                            
                            // Clean up the stack
                            for (self.stack.items[self.stack.items.len - arg_count_usize - 1 ..]) |*v| {
                                v.deinit(self.allocator);
                            }
                            self.stack.items.len -= (arg_count_usize + 1);
                            
                            left.deinit(self.allocator);
                            right.deinit(self.allocator);
                            
                            try self.stack.append(result_value);
                            return;
                        },
                        .BitXor => {
                            // Bitwise xor operator
                            if (arg_count_usize != 2) {
                                return error.ArgumentCountMismatch;
                            }
                            
                            const right_ref = self.stack.items[self.stack.items.len - 1];
                            const left_ref = self.stack.items[self.stack.items.len - 2];
                            
                            var right = try right_ref.clone(self.allocator);
                            var left = try left_ref.clone(self.allocator);
                            
                            const result_value = switch (left) {
                                .Int => |left_val| switch (right) {
                                    .Int => |right_val| types.Value{ .Int = left_val ^ right_val },
                                    else => return error.TypeMismatch,
                                },
                                else => return error.TypeMismatch,
                            };
                            
                            // Clean up the stack
                            for (self.stack.items[self.stack.items.len - arg_count_usize - 1 ..]) |*v| {
                                v.deinit(self.allocator);
                            }
                            self.stack.items.len -= (arg_count_usize + 1);
                            
                            left.deinit(self.allocator);
                            right.deinit(self.allocator);
                            
                            try self.stack.append(result_value);
                            return;
                        },
                        .BitNot => {
                            // Bitwise not operator
                            if (arg_count_usize != 1) {
                                return error.ArgumentCountMismatch;
                            }
                            
                            const operand_ref = self.stack.items[self.stack.items.len - 1];
                            var operand = try operand_ref.clone(self.allocator);
                            
                            const result_value = switch (operand) {
                                .Int => |val| types.Value{ .Int = ~val },
                                else => return error.TypeMismatch,
                            };
                            
                            // Clean up the stack
                            for (self.stack.items[self.stack.items.len - arg_count_usize - 1 ..]) |*v| {
                                v.deinit(self.allocator);
                            }
                            self.stack.items.len -= (arg_count_usize + 1);
                            
                            operand.deinit(self.allocator);
                            
                            try self.stack.append(result_value);
                            return;
                        },
                        .Shl => {
                            // Shift left operator
                            if (arg_count_usize != 2) {
                                return error.ArgumentCountMismatch;
                            }
                            
                            const right_ref = self.stack.items[self.stack.items.len - 1];
                            const left_ref = self.stack.items[self.stack.items.len - 2];
                            
                            var right = try right_ref.clone(self.allocator);
                            var left = try left_ref.clone(self.allocator);
                            
                            const result_value = switch (left) {
                                .Int => |left_val| switch (right) {
                                    .Int => |right_val| blk: {
                                        if (right_val < 0 or right_val >= 64) return error.TypeMismatch;
                                        const shift_amount = @as(u6, @intCast(right_val));
                                        break :blk types.Value{ .Int = left_val << shift_amount };
                                    },
                                    else => return error.TypeMismatch,
                                },
                                else => return error.TypeMismatch,
                            };
                            
                            // Clean up the stack
                            for (self.stack.items[self.stack.items.len - arg_count_usize - 1 ..]) |*v| {
                                v.deinit(self.allocator);
                            }
                            self.stack.items.len -= (arg_count_usize + 1);
                            
                            left.deinit(self.allocator);
                            right.deinit(self.allocator);
                            
                            try self.stack.append(result_value);
                            return;
                        },
                        .Shr => {
                            // Shift right operator
                            if (arg_count_usize != 2) {
                                return error.ArgumentCountMismatch;
                            }
                            
                            const right_ref = self.stack.items[self.stack.items.len - 1];
                            const left_ref = self.stack.items[self.stack.items.len - 2];
                            
                            var right = try right_ref.clone(self.allocator);
                            var left = try left_ref.clone(self.allocator);
                            
                            const result_value = switch (left) {
                                .Int => |left_val| switch (right) {
                                    .Int => |right_val| blk: {
                                        if (right_val < 0 or right_val >= 64) return error.TypeMismatch;
                                        const shift_amount = @as(u6, @intCast(right_val));
                                        break :blk types.Value{ .Int = left_val >> shift_amount };
                                    },
                                    else => return error.TypeMismatch,
                                },
                                else => return error.TypeMismatch,
                            };
                            
                            // Clean up the stack
                            for (self.stack.items[self.stack.items.len - arg_count_usize - 1 ..]) |*v| {
                                v.deinit(self.allocator);
                            }
                            self.stack.items.len -= (arg_count_usize + 1);
                            
                            left.deinit(self.allocator);
                            right.deinit(self.allocator);
                            
                            try self.stack.append(result_value);
                            return;
                        },
                        .Concat => {
                            // String concatenation operator
                            if (arg_count_usize != 2) {
                                return error.ArgumentCountMismatch;
                            }
                            
                            const right_ref = self.stack.items[self.stack.items.len - 1];
                            const left_ref = self.stack.items[self.stack.items.len - 2];
                            
                            var right = try right_ref.clone(self.allocator);
                            var left = try left_ref.clone(self.allocator);
                            
                            const result_value = switch (left) {
                                .String => |left_val| switch (right) {
                                    .String => |right_val| blk: {
                                        const result = try std.fmt.allocPrint(self.allocator, "{s}{s}", .{left_val, right_val});
                                        break :blk types.Value{ .String = result };
                                    },
                                    else => return error.TypeMismatch,
                                },
                                else => return error.TypeMismatch,
                            };
                            
                            // Clean up the stack
                            for (self.stack.items[self.stack.items.len - arg_count_usize - 1 ..]) |*v| {
                                v.deinit(self.allocator);
                            }
                            self.stack.items.len -= (arg_count_usize + 1);
                            
                            left.deinit(self.allocator);
                            right.deinit(self.allocator);
                            
                            try self.stack.append(result_value);
                            return;
                        },
                        .Len => {
                            // Length function
                            if (arg_count_usize != 1) {
                                return error.ArgumentCountMismatch;
                            }
                            
                            const operand_ref = self.stack.items[self.stack.items.len - 1];
                            var operand = try operand_ref.clone(self.allocator);
                            
                            const result_value = switch (operand) {
                                .String => |s| types.Value{ .Int = @as(i64, @intCast(s.len)) },
                                .Array => |arr| types.Value{ .Int = @as(i64, @intCast(arr.len)) },
                                .Map => |map| types.Value{ .Int = @as(i64, @intCast(map.count())) },
                                else => return error.TypeMismatch,
                            };
                            
                            // Clean up the stack
                            for (self.stack.items[self.stack.items.len - arg_count_usize - 1 ..]) |*v| {
                                v.deinit(self.allocator);
                            }
                            self.stack.items.len -= (arg_count_usize + 1);
                            
                            operand.deinit(self.allocator);
                            
                            try self.stack.append(result_value);
                            return;
                        },
                        .Type => {
                            // Type introspection function
                            if (arg_count_usize != 1) {
                                return error.ArgumentCountMismatch;
                            }
                            
                            const operand_ref = self.stack.items[self.stack.items.len - 1];
                            var operand = try operand_ref.clone(self.allocator);
                            
                            const type_name = switch (operand) {
                                .Nil => "nil",
                                .Bool => "bool",
                                .Int => "int",
                                .Float => "float",
                                .String => "string",
                                .Symbol => "symbol",
                                .Array => "array",
                                .Map => "map",
                                .Function => "function",
                                .Variable => "variable",
                                .BuiltinOperator => "builtin-operator",
                                .ReturnAddress => "return-address",
                            };
                            
                            const result_value = types.Value{ .String = try self.allocator.dupe(u8, type_name) };
                            
                            // Clean up the stack
                            for (self.stack.items[self.stack.items.len - arg_count_usize - 1 ..]) |*v| {
                                v.deinit(self.allocator);
                            }
                            self.stack.items.len -= (arg_count_usize + 1);
                            
                            operand.deinit(self.allocator);
                            
                            try self.stack.append(result_value);
                            return;
                        },
                        .Print => {
                            // Check if we have one argument
                            if (arg_count_usize != 1) {
                                return error.ArgumentCountMismatch;
                            }

                            // Get the argument
                            const arg_ref = self.stack.items[self.stack.items.len - 1];

                            // Clone the value to avoid use-after-free issues
                            var arg = try arg_ref.clone(self.allocator);

                            // Print the value based on its type
                            switch (arg) {
                                .Int => |val| try self.stdout.print("{}\n", .{val}),
                                .Float => |val| try self.stdout.print("{}\n", .{val}),
                                .String => |val| try self.stdout.print("{s}\n", .{val}),
                                .Bool => |val| try self.stdout.print("{}\n", .{val}),
                                .Nil => try self.stdout.print("nil\n", .{}),
                                .Symbol => |val| try self.stdout.print("{s}\n", .{val}),
                                else => try self.stdout.print("{any}\n", .{arg}),
                            }

                            // Clean up the stack - remove operator AND arguments
                            // Deinit values before shrinking
                            for (self.stack.items[self.stack.items.len - arg_count_usize - 1 ..]) |*v| {
                                v.deinit(self.allocator);
                            }
                            self.stack.items.len -= (arg_count_usize + 1); // Remove operator and arguments

                            // Clean up operand
                            arg.deinit(self.allocator);

                            // Print returns nil
                            try self.stack.append(.{ .Nil = {} });

                            return;
                        },
                    }
                }

                // Handle user-defined functions
                if (func_val != .Function) {
                    debug.log("Error: Expected function on stack, found {any}", .{func_val});
                    debug.log("Stack dump:", .{});
                    for (self.stack.items, 0..) |item, i| {
                        debug.log("  Stack[{}] = {any}", .{ i, item });
                    }

                    // Check if the function is in the global variables
                    if (func_val == .Variable) {
                        const func_name = func_val.Variable.name;
                        debug.log("Looking up function in global variables: {s}", .{func_name});

                        if (self.variables.get(func_name)) |global_val| {
                            debug.log("Found function in global variables: {any}", .{global_val});

                            if (global_val == .BuiltinOperator) {
                                // Handle built-in operator
                                const op = global_val.BuiltinOperator;
                                debug.log("VM: Built-in operator call: {any}", .{op});
                                
                                // Built-in operators expect 2 arguments
                                if (arg_count_usize != 2) {
                                    debug.log("Error: Built-in operator expects 2 arguments, got {}", .{arg_count_usize});
                                    return error.ArgumentCountMismatch;
                                }
                                
                                // Get the two arguments from the stack
                                const arg2 = self.stack.items[self.stack.items.len - 1]; // Top of stack
                                const arg1 = self.stack.items[self.stack.items.len - 2]; // Second from top
                                
                                // Pop the arguments and operator from stack
                                _ = self.stack.pop(); // arg2
                                _ = self.stack.pop(); // arg1  
                                _ = self.stack.pop(); // operator
                                
                                // Perform the operation
                                const result = try self.executeBuiltinOperator(op, arg1, arg2);
                                
                                // Push result onto stack
                                try self.stack.append(result);
                                
                                return; // Don't increment PC, continue with next instruction
                                
                            } else if (global_val == .Function) {
                                // Replace the variable with the actual function
                                self.stack.items[self.stack.items.len - arg_count_usize - 1] = try global_val.clone(self.allocator);
                                debug.log("Replaced variable with function: {any}", .{self.stack.items[self.stack.items.len - arg_count_usize - 1]});
                                // Use the updated value from the stack
                                const updated_func_val = self.stack.items[self.stack.items.len - arg_count_usize - 1];

                                // Continue with the function call using the updated value
                                if (updated_func_val == .Function) {
                                    debug.log("VM: Variable-based function call", .{});
                                    const func = updated_func_val.Function;
                                    debug.log("VM: Function name: {s}", .{func.name});
                                    debug.log("VM: Function has {} parameters", .{func.param_count});
                                    debug.log("VM: Function has {} instructions", .{func.instructions.items.len});

                                    // Check argument count
                                    if (arg_count_usize != func.param_count) {
                                        return error.ArgumentCountMismatch;
                                    }

                                    // Save current state
                                    const old_bp = self.bp;
                                    const return_addr = self.pc + 1;

                                    // Calculate new bp (where arguments start)
                                    const new_bp = self.stack.items.len - arg_count_usize;

                                    // Create new call frame
                                    // Create call frame to store return information
                                    if (self.current_func) |calling_func| {
                                        debug.log("Creating call frame to return to: {s}", .{calling_func.name});
                                        const frame = CallFrame.init(calling_func, new_bp, old_bp, return_addr);
                                        try self.call_frames.append(frame);
                                    } else {
                                        debug.log("Warning: no current function to return to", .{});
                                        // For main function calls, create a minimal frame
                                        const frame = CallFrame.init(func, new_bp, old_bp, return_addr);
                                        try self.call_frames.append(frame);
                                    }

                                    // Set up new stack frame
                                    // The base pointer points to the first argument
                                    self.bp = new_bp;

                                    // Switch to the new function
                                    // Functions are immutable, so we can use them directly without cloning
                                    self.current_func = func;
                                    self.pc = 0; // Start at the beginning of the function

                                    debug.log("Called function {s} with bp={}, old_bp={}, return_addr={}", .{ func.name, self.bp, old_bp, return_addr });

                                    self.function_called = true; // Set the flag to prevent PC increment
                                    return;
                                } else {
                                    debug.log("Updated value is not a function: {any}", .{updated_func_val});
                                    return error.TypeMismatch;
                                }
                            } else {
                                debug.log("Global variable is not a function: {any}", .{global_val});
                                return error.TypeMismatch;
                            }
                        } else {
                            debug.log("Function not found in global variables: {s}", .{func_name});
                            return error.UnknownFunction;
                        }
                    } else {
                        return error.TypeMismatch;
                    }
                } else {
                    // Direct function call (not via variable)
                    debug.log("Found function on stack: {any}", .{func_val});

                    debug.log("VM: Direct function call", .{});
                    const func = func_val.Function;
                    debug.log("VM: Function name: {s}", .{func.name});
                    debug.log("VM: Function has {} parameters", .{func.param_count});
                    debug.log("VM: Function has {} instructions", .{func.instructions.items.len});

                    // Check argument count
                    if (arg_count_usize != func.param_count) {
                        return error.ArgumentCountMismatch;
                    }

                    // Save current state
                    const old_bp = self.bp;
                    const return_addr = self.pc + 1;

                    // Calculate new bp (where arguments start)
                    const new_bp = self.stack.items.len - arg_count_usize;

                    // Create new call frame
                    // Check if current_func is null
                    if (self.current_func == null) {
                        debug.log("Warning: current_func is null, using function being called instead", .{});
                        debug.log("Function being called: {s} with {} parameters", .{ func.name, func.param_count });

                        // Dump function instructions
                        debug.log("Function instructions:", .{});
                        for (func.instructions.items, 0..) |instr, i| {
                            debug.log("  [{}] {any}", .{ i, instr.op });
                            if (instr.operand) |op| {
                                debug.log("    Operand: {any}", .{op});
                            }
                        }

                        const frame = CallFrame.init(func, new_bp, old_bp, return_addr);
                        try self.call_frames.append(frame);
                    } else {
                        debug.log("Using current function for call frame: {s}", .{self.current_func.?.name});
                        const frame = CallFrame.init(self.current_func.?, new_bp, old_bp, return_addr);
                        try self.call_frames.append(frame);
                    }

                    // Set up new stack frame
                    // The base pointer points to the first argument
                    self.bp = new_bp;

                    // Switch to the new function
                    // Functions are immutable, so we can use them directly without cloning
                    self.current_func = func;
                    self.pc = 0; // Start at the beginning of the function

                    debug.log("Called function {s} with bp={}, old_bp={}, return_addr={}", .{ func.name, self.bp, old_bp, return_addr });

                    self.function_called = true; // Set the flag to prevent PC increment
                }

                // Note: We don't need to remove the function and arguments from the stack
                // They will be accessible via the base pointer
            },
            .Return => {
                debug.log("Return operation", .{});
                debug.log("Current stack size: {}", .{self.stack.items.len});

                // Make sure we have at least one item on the stack for the return value
                if (self.stack.items.len == 0) {
                    debug.log("Empty stack on return", .{});
                    // Instead of error, just return 0 as a default value
                    try self.stack.append(.{ .Int = 0 });
                }

                // Get the return value
                const return_value = try self.stack.items[self.stack.items.len - 1].clone(self.allocator);
                debug.log("Return value: {any}", .{return_value});

                // If we have no call frames, we're done with the program
                if (self.call_frames.items.len == 0) {
                    debug.log("No call frames, returning from main function", .{});
                    // Free the cloned return value since we're not using it
                    var mutable_return_value = return_value;
                    mutable_return_value.deinit(self.allocator);
                    return;
                }

                // Store the return value temporarily
                const old_stack_size = self.stack.items.len;
                var call_bp: usize = 0; // The bp from the function call (where arguments start)
                var param_count: usize = 0;

                // Pop the call frame
                if (self.call_frames.pop()) |frame| {
                    // Store the call-time bp before restoring state
                    call_bp = frame.bp; // This is where the arguments start

                    // Restore state
                    // We don't free the current function here because it might be referenced elsewhere
                    // The function will be freed when the Value that contains it is deinitialized
                    self.current_func = frame.func;
                    self.pc = frame.return_addr;
                    self.bp = frame.prev_bp;
                    param_count = frame.func.param_count; // Parameter count of the function being called
                    debug.log("Restored call frame: func={s}, pc={}, bp={}, call_bp={}, param_count={}", .{ frame.func.name, self.pc, self.bp, call_bp, param_count });
                }

                // Clean up the stack, keeping only items below the call frame
                // and the return value
                debug.log("Cleaning up stack: current size={}, bp={}, call_bp={}, param_count={}", .{ self.stack.items.len, self.bp, call_bp, param_count });

                // Simplified stack cleanup: restore to the base pointer of the caller
                // but also remove the function object that was used for the call
                // call_bp points to the first argument, so call_bp-1 is the function object
                const keep_count = if (call_bp > 0) call_bp - 1 else 0;
                debug.log("Keeping {} items on stack (removing function and args)", .{keep_count});

                // Deinit values that will be removed
                for (self.stack.items[keep_count..]) |*value| {
                    value.deinit(self.allocator);
                }

                self.stack.shrinkRetainingCapacity(keep_count);

                // Add the return value to the stack
                try self.stack.append(return_value);
                debug.log("Added return value to stack: {any}", .{return_value});

                debug.log("Returned to caller with pc={}, bp={}, stack_size={}->{}", .{ self.pc, self.bp, old_stack_size, self.stack.items.len });

                // Skip to the next instruction in the caller
                self.pc -= 1; // Will be incremented in the execute loop
            },
            .Jump => {
                debug.log("Jump instruction", .{});
                if (instruction.operand == null) return error.TypeMismatch;
                if (instruction.operand.? != .Int) return error.TypeMismatch;

                const target = @as(usize, @intCast(instruction.operand.?.Int));
                debug.log("Jumping to instruction {}", .{target});

                // Set pc to target - 1 because it will be incremented after this instruction
                self.pc = if (target > 0) target - 1 else 0;
            },
            .JumpIfFalse => {
                debug.log("JumpIfFalse instruction", .{});
                if (self.stack.items.len < 1) {
                    return error.StackUnderflow;
                }
                if (instruction.operand == null) return error.TypeMismatch;
                if (instruction.operand.? != .Int) return error.TypeMismatch;

                // Get the condition value from the stack
                const condition_ref = self.stack.items[self.stack.items.len - 1];
                var condition = try condition_ref.clone(self.allocator);
                self.stack.items.len -= 1; // Remove the condition from stack

                // Check if we should jump
                const should_jump = switch (condition) {
                    .Bool => |b| !b,
                    .Nil => true, // nil is falsy
                    .Int => |n| n == 0, // 0 is falsy
                    else => false, // Everything else is truthy
                };

                condition.deinit(self.allocator);

                if (should_jump) {
                    const target = @as(usize, @intCast(instruction.operand.?.Int));
                    debug.log("Condition is false, jumping to instruction {}", .{target});
                    // Set pc to target - 1 because it will be incremented after this instruction
                    self.pc = if (target > 0) target - 1 else 0;
                } else {
                    debug.log("Condition is true, continuing to next instruction", .{});
                }
            },
            .Array => {
                debug.log("Array operation", .{});
                if (instruction.operand == null) return error.TypeMismatch;
                if (instruction.operand.? != .Int) return error.TypeMismatch;

                const num_elements = @as(usize, @intCast(instruction.operand.?.Int));
                if (self.stack.items.len < num_elements) {
                    return error.StackUnderflow;
                }

                var elements = try self.allocator.alloc(types.Value, num_elements);
                errdefer self.allocator.free(elements);

                // Pop elements from stack in reverse order and store them
                var i: usize = 0;
                while (i < num_elements) : (i += 1) {
                    const val_index = self.stack.items.len - num_elements + i;
                    elements[i] = self.stack.items[val_index];
                }

                // Remove elements from the stack
                self.stack.items.len -= num_elements;

                try self.stack.append(.{ .Array = elements });
                debug.log("Created array with {} elements", .{num_elements});
            },
            .Map => {
                debug.log("Map operation", .{});
                if (instruction.operand == null) return error.TypeMismatch;
                if (instruction.operand.? != .Int) return error.TypeMismatch;

                const num_entries = @as(usize, @intCast(instruction.operand.?.Int));
                if (self.stack.items.len < 2 * num_entries) {
                    return error.StackUnderflow;
                }

                var new_map = std.StringHashMap(types.Value).init(self.allocator);
                errdefer {
                    var it = new_map.iterator();
                    while (it.next()) |entry| {
                        self.allocator.free(entry.key_ptr.*);
                        var value = entry.value_ptr.*;
                        value.deinit(self.allocator);
                    }
                    new_map.deinit();
                }

                // Pop key-value pairs from stack in reverse order
                var i: usize = 0;
                while (i < num_entries) : (i += 1) {
                    const value_index = self.stack.items.len - (2 * num_entries) + (2 * i) + 1;
                    const key_index = self.stack.items.len - (2 * num_entries) + (2 * i);

                    const value = self.stack.items[value_index];
                    const key = self.stack.items[key_index];

                    if (key != .String) {
                        // Keys must be strings for StringHashMap
                        return error.TypeMismatch;
                    }

                    try new_map.put(key.String, value);
                }

                // Remove key-value pairs from the stack
                self.stack.items.len -= (2 * num_entries);

                try self.stack.append(.{ .Map = new_map });
                debug.log("Created map with {} entries", .{num_entries});
            },
        }
    }
    
    fn executeBuiltinOperator(self: *VM, op: types.BuiltinOperatorType, arg1: types.Value, arg2: types.Value) !types.Value {
        _ = self; // Suppress unused parameter warning
        
        return switch (op) {
            .LessThan => {
                // Handle < operator
                if (arg1 == .Int and arg2 == .Int) {
                    return types.Value{ .Bool = arg1.Int < arg2.Int };
                } else {
                    return error.TypeMismatch;
                }
            },
            .GreaterThan => {
                // Handle > operator
                if (arg1 == .Int and arg2 == .Int) {
                    return types.Value{ .Bool = arg1.Int > arg2.Int };
                } else {
                    return error.TypeMismatch;
                }
            },
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
            .Eq => {
                // Handle == operator (not currently used but included for completeness)
                if (arg1 == .Int and arg2 == .Int) {
                    return types.Value{ .Bool = arg1.Int == arg2.Int };
                } else {
                    return error.TypeMismatch;
                }
            },
            .Print => {
                // Print is handled specially in the main call switch, not here
                // This should never be called since Print is handled elsewhere
                return error.TypeMismatch;
            },
            .Mod => {
                // Handle % operator
                if (arg1 == .Int and arg2 == .Int) {
                    if (arg2.Int == 0) return error.DivisionByZero;
                    return types.Value{ .Int = @mod(arg1.Int, arg2.Int) };
                } else {
                    return error.TypeMismatch;
                }
            },
            .Pow => {
                // Handle ** operator
                if (arg1 == .Int and arg2 == .Int) {
                    if (arg2.Int < 0) {
                        const result = std.math.pow(f64, @as(f64, @floatFromInt(arg1.Int)), @as(f64, @floatFromInt(arg2.Int)));
                        return types.Value{ .Float = result };
                    } else {
                        const result = std.math.pow(i64, arg1.Int, @as(u32, @intCast(arg2.Int)));
                        return types.Value{ .Int = result };
                    }
                } else {
                    return error.TypeMismatch;
                }
            },
            .Ne => {
                // Handle != operator
                if (arg1 == .Int and arg2 == .Int) {
                    return types.Value{ .Bool = arg1.Int != arg2.Int };
                } else {
                    return error.TypeMismatch;
                }
            },
            .LessEqual => {
                // Handle <= operator
                if (arg1 == .Int and arg2 == .Int) {
                    return types.Value{ .Bool = arg1.Int <= arg2.Int };
                } else {
                    return error.TypeMismatch;
                }
            },
            .GreaterEqual => {
                // Handle >= operator
                if (arg1 == .Int and arg2 == .Int) {
                    return types.Value{ .Bool = arg1.Int >= arg2.Int };
                } else {
                    return error.TypeMismatch;
                }
            },
            .And => {
                // Handle && operator
                const left_bool = switch (arg1) {
                    .Bool => |b| b,
                    .Int => |i| i != 0,
                    else => true,
                };
                const right_bool = switch (arg2) {
                    .Bool => |b| b,
                    .Int => |i| i != 0,
                    else => true,
                };
                return types.Value{ .Bool = left_bool and right_bool };
            },
            .Or => {
                // Handle || operator
                const left_bool = switch (arg1) {
                    .Bool => |b| b,
                    .Int => |i| i != 0,
                    else => true,
                };
                const right_bool = switch (arg2) {
                    .Bool => |b| b,
                    .Int => |i| i != 0,
                    else => true,
                };
                return types.Value{ .Bool = left_bool or right_bool };
            },
            .Not => {
                // Handle ! operator (unary)
                const operand_bool = switch (arg1) {
                    .Bool => |b| b,
                    .Int => |i| i != 0,
                    else => true,
                };
                return types.Value{ .Bool = !operand_bool };
            },
            .BitAnd => {
                // Handle & operator
                if (arg1 == .Int and arg2 == .Int) {
                    return types.Value{ .Int = arg1.Int & arg2.Int };
                } else {
                    return error.TypeMismatch;
                }
            },
            .BitOr => {
                // Handle | operator
                if (arg1 == .Int and arg2 == .Int) {
                    return types.Value{ .Int = arg1.Int | arg2.Int };
                } else {
                    return error.TypeMismatch;
                }
            },
            .BitXor => {
                // Handle ^ operator
                if (arg1 == .Int and arg2 == .Int) {
                    return types.Value{ .Int = arg1.Int ^ arg2.Int };
                } else {
                    return error.TypeMismatch;
                }
            },
            .BitNot => {
                // Handle ~ operator (unary)
                if (arg1 == .Int) {
                    return types.Value{ .Int = ~arg1.Int };
                } else {
                    return error.TypeMismatch;
                }
            },
            .Shl => {
                // Handle << operator
                if (arg1 == .Int and arg2 == .Int) {
                    if (arg2.Int < 0 or arg2.Int >= 64) return error.TypeMismatch;
                    const shift_amount = @as(u6, @intCast(arg2.Int));
                    return types.Value{ .Int = arg1.Int << shift_amount };
                } else {
                    return error.TypeMismatch;
                }
            },
            .Shr => {
                // Handle >> operator
                if (arg1 == .Int and arg2 == .Int) {
                    if (arg2.Int < 0 or arg2.Int >= 64) return error.TypeMismatch;
                    const shift_amount = @as(u6, @intCast(arg2.Int));
                    return types.Value{ .Int = arg1.Int >> shift_amount };
                } else {
                    return error.TypeMismatch;
                }
            },
            .Concat => {
                // Handle ++ operator
                if (arg1 == .String and arg2 == .String) {
                    // This would need allocator access, but we don't have it here
                    // This function should probably be redesigned
                    return error.TypeMismatch;
                } else {
                    return error.TypeMismatch;
                }
            },
            .Len => {
                // Handle len function (unary)
                const result = switch (arg1) {
                    .String => |s| @as(i64, @intCast(s.len)),
                    .Array => |arr| @as(i64, @intCast(arr.len)),
                    else => return error.TypeMismatch,
                };
                return types.Value{ .Int = result };
            },
            .Type => {
                // Handle type function (unary)
                const type_name = switch (arg1) {
                    .Int => "int",
                    .Bool => "bool",
                    .String => "string",
                    else => "unknown",
                };
                // This would need allocator access to duplicate the string
                // This function should probably be redesigned
                _ = type_name;
                return error.TypeMismatch;
            },
        };
    }
};
