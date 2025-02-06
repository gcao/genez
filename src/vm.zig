const std = @import("std");
const types = @import("types.zig");
const debug = @import("debug.zig");
const bytecode = @import("bytecode.zig");

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

    pub fn execute(self: *VM, func: *const bytecode.Function) !void {
        debug.log("Starting execution with {} instructions", .{func.instructions.items.len});

        for (func.instructions.items, 0..) |instruction, i| {
            debug.log("Executing instruction {}: {}", .{ i, instruction.op });

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
                    }
                    value.deinit(self.allocator);
                    debug.log("Stack size after instruction: {}", .{self.stack.items.len});
                },
                .Return => {
                    debug.log("Return operation", .{});
                    return;
                },
                else => return error.UnsupportedInstruction,
            }
        }
    }
};
