const std = @import("std");
const debug = @import("debug.zig");
const bytecode = @import("bytecode.zig");
const AstNode = @import("ast.zig").AstNode;

pub const VM = struct {
    allocator: std.mem.Allocator,
    stdout: std.fs.File.Writer,
    stack: std.ArrayList(AstNode.Value),
    variables: std.StringHashMap(AstNode.Value),
    call_stack: std.ArrayList(CallFrame),
    function_map: std.StringHashMap(bytecode.Function),

    pub fn init(allocator: std.mem.Allocator, stdout: std.fs.File.Writer, function_map: std.StringHashMap(bytecode.Function)) VM {
        debug.log("Initializing VM", .{});

        // Add builtin variables
        var variables = std.StringHashMap(AstNode.Value).init(allocator);

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
            variables.put(name_copy, AstNode.Value{ .Symbol = value_copy }) catch unreachable;
        }

        return VM{
            .allocator = allocator,
            .stdout = stdout,
            .stack = std.ArrayList(AstNode.Value).initCapacity(allocator, 1_000_000) catch unreachable,
            .variables = variables,
            .call_stack = std.ArrayList(CallFrame).init(allocator),
            .function_map = function_map,
        };
    }

    pub fn deinit(self: *VM) void {
        debug.log("Deinitializing VM", .{});

        debug.log("Cleaning up stack with {} items", .{self.stack.items.len});
        for (self.stack.items) |*value| {
            debug.log("Deinitializing stack value:", .{});
            debug.log("{}", .{value.*});
            // Create a temporary node to deinit the value
            var tmp_node = AstNode{ .tag = .{ .Expression = .{ .Literal = .{ .value = value.* } } }, .loc = .{ .start = 0, .end = 0 } };
            tmp_node.deinit(self.allocator);
        }
        self.stack.deinit();

        debug.log("Cleaning up variables map with {} entries", .{self.variables.count()});
        var it = self.variables.iterator();
        while (it.next()) |entry| {
            debug.log("Deinitializing variable {s}:", .{entry.key_ptr.*});
            debug.log("{}", .{entry.value_ptr.*});
            // Create a temporary node to deinit the value
            var tmp_node = AstNode{ .tag = .{ .Expression = .{ .Literal = .{ .value = entry.value_ptr.* } } }, .loc = .{ .start = 0, .end = 0 } };
            tmp_node.deinit(self.allocator);
            self.allocator.free(entry.key_ptr.*);
        }
        self.variables.deinit();

        self.call_stack.deinit();
        self.function_map.deinit();
    }

    pub fn execute(self: *VM, func: *const bytecode.Function) !void {
        if (@intFromPtr(func) == 0) {
            std.debug.print("Function is null!\n", .{});
            return;
        }
        debug.log("Starting execution with {} instructions", .{func.instructions.items.len});
        std.debug.print("Number of instructions: {}\n", .{func.instructions.items.len});
        var i: usize = 0;
        while (i < func.instructions.items.len) : (i += 1) {
            std.debug.print("Entering instruction loop\n", .{});
            const instruction = func.instructions.items[i];
            debug.log("Executing instruction {}: {}", .{ i, instruction.op });

            // Print stack before instruction
            std.debug.print("Stack before: {any}\n", .{self.stack.items});

            switch (instruction.op) {
                .LoadConst => {
                    debug.log("LoadConst:", .{});
                    debug.log("{}", .{instruction.operand.?});
                    if (self.stack.items.len >= 10_000_000) {
                        return error.StackOverflow;
                    }
                    try self.stack.append(try instruction.operand.?.clone(self.allocator));
                    debug.log("Stack size after instruction: {}", .{self.stack.items.len});
                },
                .LoadVar => {
                    const name = instruction.operand.?.Symbol;
                    debug.log("LoadVar: {s}", .{name});

                    // Search in current stack frame first
                    var found = false;
                    if (self.call_stack.items.len > 0) {
                        const frame = self.call_stack.items[self.call_stack.items.len - 1];
                        var j = frame.base_pointer;
                        while (j < self.stack.items.len) : (j += 1) {
                            switch (self.stack.items[j]) {
                                .Symbol => |val| {
                                    if (std.mem.eql(u8, val, name)) {
                                        try self.stack.append(try self.stack.items[j].clone(self.allocator));
                                        found = true;
                                        break;
                                    }
                                },
                                else => {},
                            }
                        }
                    }

                    // If not found locally, search in global variables
                    if (!found) {
                        if (self.variables.get(name)) |value| {
                            debug.log("Found variable value:", .{});
                            debug.log("{}", .{value});
                            try self.stack.append(try value.clone(self.allocator));
                            debug.log("Stack size after instruction: {}", .{self.stack.items.len});
                        } else {
                            return error.UndefinedVariable;
                        }
                    }
                },
                .Add => {
                    if (self.stack.items.len < 2) {
                        return error.StackUnderflow;
                    }

                    const right = self.stack.pop();
                    const left = self.stack.pop();

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
                    var left_node = AstNode{ .tag = .{ .Expression = .{ .Literal = .{ .value = left } } }, .loc = .{ .start = 0, .end = 0 } };
                    left_node.deinit(self.allocator);
                    var right_node = AstNode{ .tag = .{ .Expression = .{ .Literal = .{ .value = right } } }, .loc = .{ .start = 0, .end = 0 } };
                    right_node.deinit(self.allocator);
                },
                .Sub => {
                    if (self.stack.items.len < 2) {
                        return error.StackUnderflow;
                    }

                    const right = self.stack.pop();
                    const left = self.stack.pop();

                    switch (left) {
                        .Int => |left_val| switch (right) {
                            .Int => |right_val| try self.stack.append(.{ .Int = left_val - right_val }),
                            else => return error.TypeMismatch,
                        },
                        else => return error.TypeMismatch,
                    }

                    // Clean up operands
                    var left_node = AstNode{ .tag = .{ .Expression = .{ .Literal = .{ .value = left } } }, .loc = .{ .start = 0, .end = 0 } };
                    left_node.deinit(self.allocator);
                    var right_node = AstNode{ .tag = .{ .Expression = .{ .Literal = .{ .value = right } } }, .loc = .{ .start = 0, .end = 0 } };
                    right_node.deinit(self.allocator);
                },
                .Mul => {
                    if (self.stack.items.len < 2) {
                        return error.StackUnderflow;
                    }

                    const right = self.stack.pop();
                    const left = self.stack.pop();

                    switch (left) {
                        .Int => |left_val| switch (right) {
                            .Int => |right_val| try self.stack.append(.{ .Int = left_val * right_val }),
                            else => return error.TypeMismatch,
                        },
                        else => return error.TypeMismatch,
                    }

                    // Clean up operands
                    var left_node = AstNode{ .tag = .{ .Expression = .{ .Literal = .{ .value = left } } }, .loc = .{ .start = 0, .end = 0 } };
                    left_node.deinit(self.allocator);
                    var right_node = AstNode{ .tag = .{ .Expression = .{ .Literal = .{ .value = right } } }, .loc = .{ .start = 0, .end = 0 } };
                    right_node.deinit(self.allocator);
                },
                .Div => {
                    if (self.stack.items.len < 2) {
                        return error.StackUnderflow;
                    }

                    const right = self.stack.pop();
                    const left = self.stack.pop();

                    switch (left) {
                        .Int => |left_val| switch (right) {
                            .Int => |right_val| {
                                if (right_val == 0) return error.DivisionByZero;
                                try self.stack.append(.{ .Int = @divTrunc(left_val, right_val) });
                            },
                            else => return error.TypeMismatch,
                        },
                        else => return error.TypeMismatch,
                    }

                    // Clean up operands
                    var left_node = AstNode{ .tag = .{ .Expression = .{ .Literal = .{ .value = left } } }, .loc = .{ .start = 0, .end = 0 } };
                    left_node.deinit(self.allocator);
                    var right_node = AstNode{ .tag = .{ .Expression = .{ .Literal = .{ .value = right } } }, .loc = .{ .start = 0, .end = 0 } };
                    right_node.deinit(self.allocator);
                },
                .Lt => {
                    if (self.stack.items.len < 2) {
                        return error.StackUnderflow;
                    }

                    const right = self.stack.pop();
                    const left = self.stack.pop();

                    switch (left) {
                        .Int => |left_val| switch (right) {
                            .Int => |right_val| try self.stack.append(.{ .Bool = left_val < right_val }),
                            else => return error.TypeMismatch,
                        },
                        else => return error.TypeMismatch,
                    }

                    // Clean up operands
                    var left_node = AstNode{ .tag = .{ .Expression = .{ .Literal = .{ .value = left } } }, .loc = .{ .start = 0, .end = 0 } };
                    left_node.deinit(self.allocator);
                    var right_node = AstNode{ .tag = .{ .Expression = .{ .Literal = .{ .value = right } } }, .loc = .{ .start = 0, .end = 0 } };
                    right_node.deinit(self.allocator);
                },
                .Gt => {
                    if (self.stack.items.len < 2) {
                        return error.StackUnderflow;
                    }

                    const right = self.stack.pop();
                    const left = self.stack.pop();

                    switch (left) {
                        .Int => |left_val| switch (right) {
                            .Int => |right_val| try self.stack.append(.{ .Bool = left_val > right_val }),
                            else => return error.TypeMismatch,
                        },
                        else => return error.TypeMismatch,
                    }

                    // Clean up operands
                    var left_node = AstNode{ .tag = .{ .Expression = .{ .Literal = .{ .value = left } } }, .loc = .{ .start = 0, .end = 0 } };
                    left_node.deinit(self.allocator);
                    var right_node = AstNode{ .tag = .{ .Expression = .{ .Literal = .{ .value = right } } }, .loc = .{ .start = 0, .end = 0 } };
                    right_node.deinit(self.allocator);
                },
                .Eq => {
                    if (self.stack.items.len < 2) {
                        return error.StackUnderflow;
                    }

                    const right = self.stack.pop();
                    const left = self.stack.pop();

                    switch (left) {
                        .Int => |left_val| switch (right) {
                            .Int => |right_val| try self.stack.append(.{ .Bool = left_val == right_val }),
                            else => return error.TypeMismatch,
                        },
                        .Bool => |left_val| switch (right) {
                            .Bool => |right_val| try self.stack.append(.{ .Bool = left_val == right_val }),
                            else => return error.TypeMismatch,
                        },
                        else => return error.TypeMismatch,
                    }

                    // Clean up operands
                    var left_node = AstNode{ .tag = .{ .Expression = .{ .Literal = .{ .value = left } } }, .loc = .{ .start = 0, .end = 0 } };
                    left_node.deinit(self.allocator);
                    var right_node = AstNode{ .tag = .{ .Expression = .{ .Literal = .{ .value = right } } }, .loc = .{ .start = 0, .end = 0 } };
                    right_node.deinit(self.allocator);
                },
                .JumpIfFalse => {
                    if (self.stack.items.len < 1) {
                        return error.StackUnderflow;
                    }

                    const condition = self.stack.pop();
                    const offset = instruction.operand.?.Int;

                    switch (condition) {
                        .Bool => |val| {
                            if (!val) {
                                // Skip the next offset instructions
                                i = @as(usize, @intCast(offset));
                            } else {
                                i += 1;
                            }
                        },
                        else => return error.TypeMismatch,
                    }

                    var cond_node = AstNode{ .tag = .{ .Expression = .{ .Literal = .{ .value = condition } } }, .loc = .{ .start = 0, .end = 0 } };
                    cond_node.deinit(self.allocator);
                },
                .Jump => {
                    const offset = instruction.operand.?.Int;
                    i = @as(usize, @intCast(offset));
                },
                .DefineFunction => {
                    const name = instruction.operand.?.String;
                    if (self.function_map.get(name)) |_| {} else {
                        return error.UndefinedFunction;
                    }
                },
                .Call => {
                    const arg_count = @as(usize, @intCast(instruction.operand.?.Int));
                    if (self.stack.items.len < arg_count + 1) {
                        return error.StackUnderflow;
                    }
                    if (self.stack.items.len + arg_count + 1 > 10_000_000) {
                        return error.StackOverflow;
                    }

                    // Pop function name
                    const fn_name_val = self.stack.pop();
                    defer {
                        var fn_node = AstNode{ .tag = .{ .Expression = .{ .Literal = .{ .value = fn_name_val } } }, .loc = .{ .start = 0, .end = 0 } };
                        fn_node.deinit(self.allocator);
                    } // Clean up after

                    const fn_name = switch (fn_name_val) {
                        .Symbol => |s| s,
                        else => return error.TypeMismatch,
                    };

                    std.debug.print("Calling function: {s} with {d} arguments\n", .{ fn_name, arg_count });

                    if (std.mem.eql(u8, fn_name, "print")) {
                        // Handle builtin print function
                        var j: usize = 0;
                        while (j < arg_count) : (j += 1) {
                            const arg = self.stack.pop();
                            switch (arg) {
                                .Int => |val| try self.stdout.print("{d}", .{val}),
                                .String => |str| try self.stdout.print("{s}", .{str}),
                                .Symbol => |sym| try self.stdout.print("{s}", .{sym}),
                                .Bool => |b| try self.stdout.print("{}", .{b}),
                                .Float => |f| try self.stdout.print("{d}", .{f}),
                                .Nil => try self.stdout.print("nil", .{}),
                                .Array => |arr| try self.stdout.print("{any}", .{arr}),
                                .Map => |map| try self.stdout.print("{any}", .{map}),
                            }
                            var arg_node = AstNode{ .tag = .{ .Expression = .{ .Literal = .{ .value = arg } } }, .loc = .{ .start = 0, .end = 0 } };
                            arg_node.deinit(self.allocator);
                        }
                        try self.stdout.print("\n", .{});
                    } else {
                        // Look up function in function map
                        if (self.function_map.get(fn_name)) |called_func| {
                            // Create a new call frame
                            const frame = CallFrame{
                                .return_address = i,
                                .base_pointer = self.stack.items.len - arg_count,
                            };
                            try self.call_stack.append(frame);

                            // Set instruction pointer to the beginning of the called function
                            i = 0;
                            try self.execute(&called_func);
                        } else {
                            return error.UndefinedFunction;
                        }
                    }
                },
                .Return => {
                    debug.log("Return operation", .{});
                    if (self.call_stack.items.len == 0) {
                        return; // We're returning from the top-level, so we're done
                    }
                    const frame = self.call_stack.pop();
                    i = frame.return_address;

                    // Clean up stack frame
                    while (self.stack.items.len > frame.base_pointer) {
                        _ = self.stack.pop();
                    }
                },
                else => return error.UnsupportedInstruction,
            }
            // Print stack after instruction
            std.debug.print("Stack after: {any}\n", .{self.stack.items});
        }
    }
};

const CallFrame = struct {
    return_address: usize,
    base_pointer: usize, // Index of the bottom of the stack frame
};
