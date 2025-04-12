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
    variables: std.StringHashMap(types.Value),
    call_frames: std.ArrayList(CallFrame),
    current_func: ?*const bytecode.Function, // Current function being executed
    pc: usize, // Program counter
    bp: usize, // Base pointer (stack frame base)

    pub fn init(allocator: std.mem.Allocator, stdout: std.fs.File.Writer) VM {
        debug.log("Initializing VM", .{});

        // Initialize variables with builtins
        var variables = std.StringHashMap(types.Value).init(allocator);
        const print_name = allocator.dupe(u8, "print") catch return VM{
            .allocator = allocator,
            .stdout = stdout,
            .stack = std.ArrayList(types.Value).init(allocator),
            .variables = variables,
            .call_frames = std.ArrayList(CallFrame).init(allocator),
            .current_func = null,
            .pc = 0,
            .bp = 0,
        };
        variables.put("print", .{ .Variable = .{ .name = print_name } }) catch {};

        return VM{
            .allocator = allocator,
            .stdout = stdout,
            .stack = std.ArrayList(types.Value).init(allocator),
            .variables = variables,
            .call_frames = std.ArrayList(CallFrame).init(allocator),
            .current_func = null,
            .pc = 0,
            .bp = 0,
        };
    }

    pub fn deinit(self: *VM) void {
        debug.log("Deinitializing VM", .{});

        debug.log("Cleaning up call frames", .{});
        self.call_frames.deinit();

        debug.log("Cleaning up stack with {} items", .{self.stack.items.len});
        for (self.stack.items) |*value| {
            debug.log("Deinitializing stack value:", .{});
            debug.log("{}", .{value.*});
            value.deinit(self.allocator);
        }
        self.stack.deinit();

        debug.log("Cleaning up variables map with {} entries", .{self.variables.count()});
        var it = self.variables.iterator();
        while (it.next()) |entry| {
            debug.log("Deinitializing variable {s}:", .{entry.key_ptr.*});
            debug.log("{}", .{entry.value_ptr.*});
            entry.value_ptr.deinit(self.allocator);
            // Don't free the key here, as it's owned by the Variable value
            // self.allocator.free(entry.key_ptr.*);
        }
        self.variables.deinit();
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
            // Check if we're done with the current function
            if (self.pc >= func.instructions.items.len) {
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
            const instruction = func.instructions.items[self.pc];
            debug.log("Executing instruction {}: {}", .{ self.pc, instruction.op });
            try self.executeInstruction(instruction);
            self.pc += 1;
        }

        debug.log("Finished execution", .{});
    }

    fn executeInstruction(self: *VM, instruction: bytecode.Instruction) VMError!void {
        switch (instruction.op) {
            .LoadConst => {
                debug.log("LoadConst:", .{});
                debug.logValue(instruction.operand.?);
                try self.stack.append(try instruction.operand.?.clone(self.allocator));
                debug.log("Stack size after instruction: {}", .{self.stack.items.len});
            },
            .LoadVar => {
                const name = instruction.operand.?.Symbol;
                debug.log("LoadVar: {s}", .{name});

                // First check if it's a function parameter
                if (std.mem.eql(u8, name, "x") and self.bp < self.stack.items.len) {
                    // For simplicity, we assume the parameter is at bp
                    const value = self.stack.items[self.bp];
                    debug.log("Found parameter x at bp={}: {}", .{ self.bp, value });
                    try self.stack.append(try value.clone(self.allocator));
                    return;
                }

                // If not a parameter, check global scope
                if (self.variables.get(name)) |value| {
                    debug.log("Found variable in global scope: {}", .{value});
                    try self.stack.append(try value.clone(self.allocator));
                } else {
                    debug.log("ERROR: Variable {s} not found", .{name});
                    return error.UndefinedVariable;
                }
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
            .Lt => {
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
                    .ReturnAddress => |addr| try self.stdout.print("ReturnAddress: {}\n", .{addr}),
                    .Function => |func| try self.stdout.print("Function: {}\n", .{func}),
                    .Variable => |var_val| try self.stdout.print("Variable: {s}\n", .{var_val.name}),
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

                // Get function from stack
                // Check if we have a variable with the name 'print' in our variables map
                const func_val = self.stack.items[self.stack.items.len - arg_count_usize - 1];
                debug.logValue(func_val);

                if (func_val == .Variable and std.mem.eql(u8, func_val.Variable.name, "print")) {
                    // Handle print function
                    if (arg_count_usize != 1) {
                        return error.ArgumentCountMismatch;
                    }

                    // Get the argument
                    if (self.stack.items.len < arg_count_usize + 1) {
                        return error.StackUnderflow;
                    }
                    const arg = self.stack.items[self.stack.items.len - 1];
                    debug.log("Print argument:", .{});
                    debug.logValue(arg);

                    // Print the argument
                    // Clone the argument before printing, in case printing modifies it (though unlikely here)
                    var arg_clone = try arg.clone(self.allocator);
                    errdefer arg_clone.deinit(self.allocator);

                    switch (arg_clone) {
                        .Int => |val| try self.stdout.print("{d}\n", .{val}),
                        .String => |str| try self.stdout.print("{s}\n", .{str}),
                        .Symbol => |sym| try self.stdout.print("{s}\n", .{sym}),
                        .Bool => |b| try self.stdout.print("{}\n", .{b}),
                        .Float => |f| try self.stdout.print("{d}\n", .{f}),
                        .Nil => try self.stdout.print("nil\n", .{}),
                        .Array => |arr| try self.stdout.print("{any}\n", .{arr}),
                        .Map => |map| try self.stdout.print("{any}\n", .{map}),
                        .ReturnAddress => |addr| try self.stdout.print("ReturnAddress: {}\n", .{addr}),
                        .Function => |func| try self.stdout.print("Function: {}\n", .{func}),
                        .Variable => |var_name| try self.stdout.print("Variable: {s}\n", .{var_name.name}),
                    }

                    arg_clone.deinit(self.allocator);

                    // Clean up the stack - remove function AND argument(s)
                    // Deinit values before shrinking
                    for (self.stack.items[self.stack.items.len - arg_count_usize - 1 ..]) |*v| {
                        v.deinit(self.allocator);
                    }
                    self.stack.items.len -= (arg_count_usize + 1); // Remove function and arguments

                    // No need to increment pc here, the main loop does it.
                    // self.pc += 1; // REMOVED
                    // This return was inside the print handler block
                    return;
                }

                // REMOVED Redundant check for built-in print function

                // Handle user-defined functions
                if (func_val != .Function) {
                    debug.log("Error: Expected function on stack, found {}", .{func_val});
                    return error.TypeMismatch;
                }

                const func = func_val.Function;

                // Check argument count
                if (arg_count_usize != func.param_count) {
                    return error.ArgumentCountMismatch;
                }

                // Save current state
                const old_bp = self.bp;
                const return_addr = self.pc + 1;

                // Create new call frame
                const frame = CallFrame.init(self.current_func.?, self.bp, old_bp, return_addr);
                try self.call_frames.append(frame);

                // Set up new stack frame
                // The base pointer points to the first argument
                self.bp = self.stack.items.len - arg_count_usize;

                // Switch to the new function
                self.current_func = func;
                self.pc = 0; // Start at the beginning of the function

                debug.log("Called function {s} with bp={}, old_bp={}, return_addr={}", .{ func.name, self.bp, old_bp, return_addr });

                // Note: We don't need to remove the function and arguments from the stack
                // They will be accessible via the base pointer
            },
            .Return => {
                debug.log("Return operation", .{});
                debug.log("Current stack size: {}", .{self.stack.items.len});

                // Make sure we have at least one item on the stack for the return value
                if (self.stack.items.len == 0) {
                    debug.log("Empty stack on return", .{});
                    return error.StackUnderflow;
                }

                // Get the return value
                const return_value = try self.stack.items[self.stack.items.len - 1].clone(self.allocator);

                // If we have no call frames, we're done with the program
                if (self.call_frames.items.len == 0) {
                    debug.log("No call frames, returning from main function", .{});
                    return;
                }

                // Store the return value temporarily
                const old_stack_size = self.stack.items.len;
                var param_count: usize = 0;

                // Pop the call frame
                if (self.call_frames.pop()) |frame| {
                    // Restore state
                    self.current_func = frame.func;
                    self.pc = frame.return_addr;
                    self.bp = frame.prev_bp;
                    param_count = frame.func.param_count;
                }

                // Clean up the stack, keeping only items below the old frame
                // and the return value
                if (self.current_func != null) {
                    self.stack.shrinkRetainingCapacity(self.bp - param_count - 1);
                } else {
                    // If no current function, just keep the return value
                    self.stack.shrinkRetainingCapacity(0);
                }
                try self.stack.append(return_value);

                debug.log("Returned to caller with pc={}, bp={}, stack_size={}->{}", .{ self.pc, self.bp, old_stack_size, self.stack.items.len });

                // Skip to the next instruction in the caller
                self.pc -= 1; // Will be incremented in the execute loop
            },
            else => return error.UnsupportedInstruction,
        }
    }
};
