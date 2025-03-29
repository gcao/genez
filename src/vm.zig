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
} || std.mem.Allocator.Error || std.fs.File.WriteError;

pub const VM = struct {
    allocator: std.mem.Allocator,
    stdout: std.fs.File.Writer,
    stack: std.ArrayList(types.Value),
    variables: std.StringHashMap(types.Value),

    pub fn init(allocator: std.mem.Allocator, stdout: std.fs.File.Writer) VM {
        debug.log("Initializing VM", .{});

        // Add builtin variables
        var variables = std.StringHashMap(types.Value).init(allocator);

        const builtins = [_][]const u8{
            "usr",
            "bin",
            "env",
            "gene",
            "run",
            "print",
        };

        for (builtins) |name| {
            debug.log("Adding builtin: {s}", .{name});
            const name_copy = allocator.dupe(u8, name) catch unreachable;
            const value_copy = allocator.dupe(u8, name) catch unreachable;
            variables.put(name_copy, types.Value{ .Symbol = value_copy }) catch unreachable;
        }

        return VM{
            .allocator = allocator,
            .stdout = stdout,
            .stack = std.ArrayList(types.Value).init(allocator),
            .variables = variables,
        };
    }

    pub fn deinit(self: *VM) void {
        debug.log("Deinitializing VM", .{});

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
            self.allocator.free(entry.key_ptr.*);
        }
        self.variables.deinit();
    }

    pub fn execute(self: *VM, func: *const bytecode.Function) VMError!void {
        debug.log("Starting execution with {} instructions", .{func.instructions.items.len});

        for (func.instructions.items, 0..) |instruction, i| {
            debug.log("Executing instruction {}: {}", .{ i, instruction.op });
            try self.executeInstruction(instruction);
        }

        // Print final stack result
        if (self.stack.items.len > 0) {
            const result_const = self.stack.pop();
            var result = result_const; // Create a mutable copy
            defer result.deinit(self.allocator);
            switch (result) {
                .Int => |val| {
                    try self.stdout.print("Result: {d}\n", .{val});
                },
                else => {
                    try self.stdout.print("Result: {}\n", .{result});
                },
            }
        }
    }

    fn executeInstruction(self: *VM, instruction: bytecode.Instruction) VMError!void {
        switch (instruction.op) {
            .LoadConst => {
                debug.log("LoadConst:", .{});
                debug.log("{}", .{instruction.operand.?});
                try self.stack.append(try instruction.operand.?.clone(self.allocator));
                debug.log("Stack size after instruction: {}", .{self.stack.items.len});
            },
            .LoadVar => {
                const name = instruction.operand.?.Symbol;
                debug.log("LoadVar: {s}", .{name});
                if (self.variables.get(name)) |value| {
                    debug.log("Found variable value:", .{});
                    debug.log("{}", .{value});
                    try self.stack.append(try value.clone(self.allocator));
                    debug.log("Stack size after instruction: {}", .{self.stack.items.len});
                } else {
                    return error.UndefinedVariable;
                }
            },
            .Add => {
                if (self.stack.items.len < 2) {
                    return error.StackUnderflow;
                }

                var right = self.stack.pop();
                var left = self.stack.pop();

                switch (left) {
                    .Int => |left_val| switch (right) {
                        .Int => |right_val| try self.stack.append(.{ .Int = left_val + right_val }),
                        else => return error.TypeMismatch,
                    },
                    .String => |left_str| switch (right) {
                        .String => |right_str| {
                            const result = try std.fmt.allocPrint(self.allocator, "{s}{s}", .{ left_str, right_str });
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
            .Print => {
                if (self.stack.items.len < 1) {
                    return error.StackUnderflow;
                }
                var value = self.stack.pop();
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
                }
                value.deinit(self.allocator);
                debug.log("Stack size after instruction: {}", .{self.stack.items.len});
            },
            .Call => {
                debug.log("Call operation", .{});
                // Get function and arguments
                const arg_count = instruction.operand.?.Int;
                const arg_count_usize = @as(usize, @intCast(arg_count));

                debug.log("Current stack size: {}", .{self.stack.items.len});
                debug.log("Arguments needed: {}", .{arg_count_usize});

                if (self.stack.items.len < arg_count_usize + 1) {
                    debug.log("Stack underflow - needed {} items but only have {}", .{ arg_count_usize + 1, self.stack.items.len });
                    return error.StackUnderflow;
                }

                // Check stack depth to prevent overflow
                if (self.stack.items.len > 10000) {
                    debug.log("Stack overflow - current size {}", .{self.stack.items.len});
                    return error.StackOverflow;
                }

                // Get function
                const func_value = self.stack.items[self.stack.items.len - arg_count_usize - 1];
                debug.log("Calling function: {}", .{func_value});

                // Save the return address and argument count
                const return_address = .{ .stack_ptr = self.stack.items.len - arg_count_usize - 1, .arg_count = arg_count_usize };
                debug.log("Pushing return address: {}", .{return_address});
                try self.stack.append(.{ .ReturnAddress = return_address });

                // Execute function
                debug.log("Entering function execution", .{});
                try self.execute(func_value.Function);
                debug.log("Returned from function execution", .{});

                // Function returns its value on the stack
                const return_value = if (self.stack.items.len > 0)
                    self.stack.pop()
                else
                    types.Value.Nil;
                debug.log("Function return value: {}", .{return_value});

                // Clean up stack to return address
                debug.log("Cleaning stack to ptr: {}", .{return_address.stack_ptr});
                debug.log("Stack before cleanup:", .{});
                for (self.stack.items, 0..) |item, i| {
                    debug.log("[{}]: {}", .{ i, item });
                }
                self.stack.shrinkRetainingCapacity(return_address.stack_ptr);

                // Push result onto stack
                debug.log("Pushing return value onto stack", .{});
                try self.stack.append(return_value);
                debug.log("Stack after call operation:", .{});
                for (self.stack.items, 0..) |item, i| {
                    debug.log("[{}]: {}", .{ i, item });
                }
            },
            .Return => {
                debug.log("Return operation", .{});
                debug.log("Current stack:", .{});
                for (self.stack.items, 0..) |item, i| {
                    debug.log("[{}]: {}", .{ i, item });
                }

                // Pop return value if any
                const return_value = if (self.stack.items.len > 0)
                    self.stack.pop()
                else
                    types.Value.Nil;
                debug.log("Return value: {}", .{return_value});

                // Find the return address
                var i = self.stack.items.len;
                while (i > 0) {
                    i -= 1;
                    if (self.stack.items[i] == .ReturnAddress) {
                        const return_address = self.stack.items[i].ReturnAddress;
                        debug.log("Found return address at {}: {}", .{ i, return_address });

                        // Remove all items above return address
                        debug.log("Stack before cleanup:", .{});
                        for (self.stack.items, 0..) |item, j| {
                            debug.log("[{}]: {}", .{ j, item });
                        }
                        self.stack.shrinkRetainingCapacity(return_address.stack_ptr);

                        // Push return value
                        debug.log("Pushing return value", .{});
                        try self.stack.append(return_value);

                        debug.log("Stack after return:", .{});
                        for (self.stack.items, 0..) |item, j| {
                            debug.log("[{}]: {}", .{ j, item });
                        }
                        return;
                    }
                }
                debug.log("No return address found on stack", .{});
                return error.NoReturnAddress;
            },
            else => return error.UnsupportedInstruction,
        }
    }
};
