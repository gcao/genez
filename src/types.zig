const std = @import("std");
const bytecode = @import("bytecode.zig");
const debug = @import("debug.zig");

pub const BuiltinOperatorType = enum {
    Eq,
};

pub const Value = union(enum) {
    Nil: void,
    Bool: bool,
    Int: i64,
    Float: f64,
    String: []const u8,
    Symbol: []const u8,
    Array: []Value,
    Map: std.StringHashMap(Value),
    ReturnAddress: struct {
        stack_ptr: usize,
        arg_count: usize,
    },
    Function: *bytecode.Function,
    Variable: struct {
        name: []const u8,
    },
    BuiltinOperator: BuiltinOperatorType,

    pub fn deinit(self: *Value, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .String => |str| allocator.free(str),
            .Symbol => |sym| allocator.free(sym),
            .Array => |arr| {
                for (arr) |*val| {
                    val.deinit(allocator);
                }
                allocator.free(arr);
            },
            .Map => |*map| {
                var it = map.iterator();
                while (it.next()) |entry| {
                    allocator.free(entry.key_ptr.*);
                    entry.value_ptr.deinit(allocator);
                }
                map.deinit();
            },
            .ReturnAddress => {},
            .Function => |func_ptr| {
                // Deallocate the bytecode function object pointed to
                func_ptr.deinit();
                allocator.destroy(func_ptr);
            },
            .Variable => {}, // No need to free the name as it's a string literal
            .BuiltinOperator => {},
            else => {},
        }
    }

    pub fn clone(self: Value, allocator: std.mem.Allocator) !Value {
        return switch (self) {
            .Int => |val| Value{ .Int = val },
            .Float => |val| Value{ .Float = val },
            .String => |str| Value{ .String = try allocator.dupe(u8, str) },
            .Bool => |val| Value{ .Bool = val },
            .Nil => Value{ .Nil = {} },
            .Symbol => |sym| Value{ .Symbol = try allocator.dupe(u8, sym) },
            .Array => |arr| blk: {
                var new_arr = try allocator.alloc(Value, arr.len);
                errdefer allocator.free(new_arr);
                for (arr, 0..) |val, i| {
                    new_arr[i] = try val.clone(allocator);
                }
                break :blk Value{ .Array = new_arr };
            },
            .Map => |map| blk: {
                var new_map = std.StringHashMap(Value).init(allocator);
                errdefer {
                    var it = new_map.iterator();
                    while (it.next()) |entry| {
                        allocator.free(entry.key_ptr.*);
                        var value = entry.value_ptr.*;
                        value.deinit(allocator);
                    }
                    new_map.deinit();
                }

                var it = map.iterator();
                while (it.next()) |entry| {
                    const key = try allocator.dupe(u8, entry.key_ptr.*);
                    errdefer allocator.free(key);
                    const value = try entry.value_ptr.clone(allocator);
                    errdefer {
                        var tmp = value;
                        tmp.deinit(allocator);
                    }
                    try new_map.put(key, value);
                }
                break :blk Value{ .Map = new_map };
            },
            .ReturnAddress => |addr| Value{ .ReturnAddress = addr },
            .Function => |func| {
                debug.log("Cloning function: {s}", .{func.name});
                // Create a deep copy of the function
                debug.log("Value.clone: Creating deep copy of function: {s}", .{func.name});
                debug.log("Value.clone: Function has {} parameters", .{func.param_count});
                debug.log("Value.clone: Function has {} instructions", .{func.instructions.items.len});

                const func_ptr = try allocator.create(bytecode.Function);
                debug.log("Value.clone: Function pointer created successfully", .{});
                errdefer allocator.destroy(func_ptr);

                // Clone the function name
                const name_copy = try allocator.dupe(u8, func.name);
                debug.log("Value.clone: Function name duplicated successfully: {s}", .{name_copy});
                errdefer allocator.free(name_copy);

                // Create a new instruction list
                var instructions = std.ArrayList(bytecode.Instruction).init(allocator);
                debug.log("Value.clone: Instruction list created successfully", .{});
                errdefer instructions.deinit();

                // Clone each instruction
                debug.log("Value.clone: Cloning {} instructions", .{func.instructions.items.len});
                try instructions.ensureTotalCapacity(func.instructions.items.len);
                for (func.instructions.items, 0..) |instr, i| {
                    debug.log("Value.clone: Cloning instruction {}/{}: {}", .{ i + 1, func.instructions.items.len, instr.op });
                    var new_instr = bytecode.Instruction{
                        .op = instr.op,
                        .operand = null,
                    };

                    // Clone the operand if it exists
                    if (instr.operand) |op| {
                        debug.log("Value.clone: Cloning operand: {}", .{op});
                        new_instr.operand = try op.clone(allocator);
                        debug.log("Value.clone: Operand cloned successfully", .{});
                    }

                    try instructions.append(new_instr);
                    debug.log("Value.clone: Instruction appended successfully", .{});
                }
                debug.log("Value.clone: All instructions cloned successfully", .{});
                debug.log("Value.clone: New instruction list has {} instructions", .{instructions.items.len});

                // Verify the instructions were copied correctly
                if (instructions.items.len != func.instructions.items.len) {
                    debug.log("Value.clone: ERROR - Instruction count mismatch: {} vs {}", .{ instructions.items.len, func.instructions.items.len });
                    return error.OutOfMemory; // Use an existing error type
                }

                // Create the function
                debug.log("Value.clone: Creating function object", .{});
                func_ptr.* = bytecode.Function{
                    .name = name_copy,
                    .instructions = instructions,
                    .allocator = allocator,
                    .param_count = func.param_count,
                };
                debug.log("Value.clone: Function object created successfully", .{});
                debug.log("Value.clone: Function name: {s}", .{func_ptr.name});
                debug.log("Value.clone: Function has {} parameters", .{func_ptr.param_count});
                debug.log("Value.clone: Function has {} instructions", .{func_ptr.instructions.items.len});

                debug.log("Value.clone: Creating function value", .{});
                const result = Value{ .Function = func_ptr };
                debug.log("Value.clone: Function value created successfully", .{});
                return result;
            },
            .Variable => |var_val| Value{ .Variable = .{ .name = var_val.name } },
            .BuiltinOperator => |op| Value{ .BuiltinOperator = op },
        };
    }
};

pub const OpCode = enum {
    LoadConst,
    LoadVar,
    Add,
};

pub const Type = union(enum) {
    Nil,
    Bool,
    Int,
    Float,
    String,
    Symbol,
    Array: *ArrayType,
    Map: *MapType,
    FunctionT: *FunctionType,
    ClassT: *ClassType,
    InstanceT: *InstanceType,
    Namespace: *NamespaceType,
    Any, // For gradual typing support

    pub const ArrayType = struct {
        element_type: *Type,
    };

    pub const MapType = struct {
        key_type: *Type,
        value_type: *Type,
    };

    pub const FunctionType = struct {
        params: []Param,
        return_type: *Type,
        is_pure: bool = false,

        pub const Param = struct {
            name: []const u8,
            type: *Type,
            default_value: ?Value,
        };
    };

    pub const ClassType = struct {
        name: []const u8,
        fields: std.StringHashMap(*Type),
        methods: std.StringHashMap(*FunctionType),
        parent: ?*ClassType,
    };

    pub const InstanceType = struct {
        class: *ClassType,
    };

    pub const NamespaceType = struct {
        name: []const u8,
        types: std.StringHashMap(*Type),
        functions: std.StringHashMap(*FunctionType),
        sub_namespaces: std.StringHashMap(*NamespaceType),
    };

    pub const Function = struct {
        type: *FunctionType,
        implementation: *const fn ([]Value) anyerror!Value,
    };

    pub const Instance = struct {
        type: *InstanceType,
        fields: std.StringHashMap(Value),
    };

    pub fn format(
        self: Type,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .Nil => try writer.writeAll("Nil"),
            .Bool => try writer.writeAll("Bool"),
            .Int => try writer.writeAll("Int"),
            .Float => try writer.writeAll("Float"),
            .String => try writer.writeAll("String"),
            .Symbol => try writer.writeAll("Symbol"),
            .Array => |arr| try writer.print("Array[{}]", .{arr.element_type}),
            .Map => |map| try writer.print("Map[{}, {}]", .{ map.key_type, map.value_type }),
            .Function => |func| {
                try writer.writeAll("fn(");
                for (func.params) |param| {
                    const i = std.mem.indexOfScalar(func.params, param) orelse continue;
                    if (i > 0) try writer.writeAll(", ");
                    try writer.print("{}: {}", .{ param.name, param.type });
                }
                try writer.print(") -> {}", .{func.return_type});
            },
            .Class => |cls| try writer.print("Class({s})", .{cls.name}),
            .Instance => |inst| try writer.print("Instance({s})", .{inst.class.name}),
            .Namespace => |ns| try writer.print("Namespace({s})", .{ns.name}),
            .Any => try writer.writeAll("Any"),
        }
    }
};
