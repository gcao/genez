const std = @import("std");
const types = @import("types.zig");
const debug = @import("debug.zig");
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
        debug.log("Initializing VM", .{});

        // Initialize variables with builtins
        var variables = std.StringArrayHashMap(types.Value).init(allocator);

        // Store the print function as a variable
        variables.put("print", .{ .Variable = .{ .name = "print" } }) catch return VM{
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
        debug.log("Deinitializing VM", .{});

        debug.log("Cleaning up call frames", .{});
        self.call_frames.deinit();

        debug.log("Cleaning up allocated functions", .{});
        for (self.allocated_functions.items) |func| {
            func.deinit();
            self.allocator.destroy(func);
        }
        self.allocated_functions.deinit();

        debug.log("Cleaning up stack with {} items", .{self.stack.items.len});
        for (self.stack.items) |*value| {
            debug.log("Deinitializing stack value:", .{});
            debug.log("{any}", .{value.*});
            value.deinit(self.allocator);
        }
        self.stack.deinit();

        debug.log("Cleaning up variables map with {} entries", .{self.variables.count()});
        var it = self.variables.iterator();
        while (it.next()) |entry| {
            debug.log("Deinitializing variable {s}:", .{entry.key_ptr.*});
            debug.log("{any}", .{entry.value_ptr.*});
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
            debug.log("Executing instruction {}: {any}", .{ self.pc, instruction.op });
            self.function_called = false; // Reset the flag
            try self.executeInstruction(instruction);

            // Only increment PC if a function wasn't called
            if (!self.function_called) {
                self.pc += 1;
            }
        }

        debug.log("Finished execution", .{});
    }

    fn executeInstruction(self: *VM, instruction: bytecode.Instruction) VMError!void {
        switch (instruction.op) {
            .LoadConst => {
                debug.log("LoadConst:", .{});
                debug.logValue(instruction.operand.?);
                // For functions, don't clone - just use the reference directly
                const value = if (instruction.operand.? == .Function)
                    instruction.operand.?
                else
                    try instruction.operand.?.clone(self.allocator);
                try self.stack.append(value);
                debug.log("Stack size after instruction: {}", .{self.stack.items.len});
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
                if (self.call_frames.items.len > 0) {
                    const frame = self.call_frames.items[self.call_frames.items.len - 1];
                    debug.log("Current frame: bp={}, prev_bp={}, return_addr={}", .{ frame.bp, frame.prev_bp, frame.return_addr });

                    // Check if the name matches any parameter
                    const func = frame.func;
                    debug.log("Function has {} parameters", .{func.param_count});

                    // For simplicity, we'll handle the first parameter as 'x' for now
                    // In a real implementation, we'd have a mapping of parameter names
                    if (std.mem.eql(u8, name, "x") and func.param_count > 0) {
                        if (self.bp < self.stack.items.len) {
                            debug.log("Checking for parameter x at bp={}", .{self.bp});
                            const param_value = self.stack.items[self.bp];
                            debug.log("Found parameter 'x' at bp={}: {any}", .{ self.bp, param_value });
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
                debug.log("Stack size after instruction: {}", .{self.stack.items.len});
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
            .Add => {
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
                        else => return error.TypeMismatch,
                    },
                    .Float => |left_val| switch (right) {
                        .Int => |right_val| try self.stack.append(.{ .Float = left_val + @as(f64, @floatFromInt(right_val)) }),
                        .Float => |right_val| try self.stack.append(.{ .Float = left_val + right_val }),
                        else => return error.TypeMismatch,
                    },
                    .String => |left_val| switch (right) {
                        .String => |right_val| {
                            const result = try std.fmt.allocPrint(self.allocator, "{s}{s}", .{ left_val, right_val });
                            try self.stack.append(.{ .String = result });
                        },
                        else => return error.TypeMismatch,
                    },
                    else => return error.TypeMismatch,
                }

                // Clean up operands
                left.deinit(self.allocator);
                right.deinit(self.allocator);
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

                // Now remove the items from the stack
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

                // Now remove the items from the stack
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
                    .Bool => |b| try self.stdout.print("{}\n", .{b}),
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
                debug.log("Stack size after instruction: {}", .{self.stack.items.len});
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

                            if (global_val == .Function) {
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
        }
    }
};
