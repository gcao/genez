const std = @import("std");
const bytecode = @import("bytecode.zig");

/// Virtual Machine for executing bytecode
pub const VM = struct {
    stack: std.ArrayList([]const u8),
    arena: std.heap.ArenaAllocator,
    temp_allocator: std.heap.FixedBufferAllocator,
    classes: std.StringHashMap(*ClassDef),
    instances: std.ArrayList(*Instance),

    const ClassDef = struct {
        name: []const u8,
        properties: std.StringHashMap(PropertyDef),
        methods: std.StringHashMap(MethodDef),
    };

    const PropertyDef = struct {
        name: []const u8,
        required: bool,
        type: []const u8,
    };

    const MethodDef = struct {
        name: []const u8,
        function: *const bytecode.Function,
    };

    const Instance = struct {
        class: *const ClassDef,
        properties: std.StringHashMap([]const u8),
    };

    /// Initialize a new VM instance
    /// Returns error if memory allocation fails
    pub fn init() !VM {
        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        const allocator = arena.allocator();

        var stack = std.ArrayList([]const u8).init(allocator);
        try stack.ensureTotalCapacity(1024);

        var classes = std.StringHashMap(*ClassDef).init(allocator);
        var instances = std.ArrayList(*Instance).init(allocator);

        // Add built-in error classes
        const error_class = try allocator.create(ClassDef);
        error_class.* = .{
            .name = try allocator.dupe(u8, "Error"),
            .properties = std.StringHashMap(PropertyDef).init(allocator),
            .methods = std.StringHashMap(MethodDef).init(allocator),
        };
        try error_class.methods.put("toString", .{
            .name = "toString",
            .function = try createToStringMethod(allocator),
        });
        try classes.put(error_class.name, error_class);

        // Create initial error instance
        const error_instance = try allocator.create(Instance);
        error_instance.* = .{
            .class = error_class,
            .properties = std.StringHashMap([]const u8).init(allocator),
        };
        try instances.append(error_instance);

        var temp_buffer: [65536]u8 = undefined;
        const temp_allocator = std.heap.FixedBufferAllocator.init(&temp_buffer);

        var vm = VM{
            .arena = arena,
            .stack = stack,
            .temp_allocator = temp_allocator,
            .classes = classes,
            .instances = instances,
        };

        // Add built-in Object class with common methods
        const object_class = try allocator.create(ClassDef);
        object_class.* = .{
            .name = try allocator.dupe(u8, "Object"),
            .properties = std.StringHashMap(PropertyDef).init(allocator),
            .methods = std.StringHashMap(MethodDef).init(allocator),
        };

        // Add common methods
        try object_class.methods.put("toString", .{
            .name = "toString",
            .function = try createToStringMethod(allocator),
        });
        try object_class.methods.put("hashCode", .{
            .name = "hashCode",
            .function = try createHashCodeMethod(allocator),
        });
        try object_class.methods.put("equals", .{
            .name = "equals",
            .function = try createEqualsMethod(allocator),
        });

        try vm.classes.put(object_class.name, object_class);

        // Create initial instance of Object
        const object_instance = try allocator.create(Instance);
        object_instance.* = .{
            .class = object_class,
            .properties = std.StringHashMap([]const u8).init(allocator),
        };
        try vm.instances.append(object_instance);

        // Add built-in String class
        const string_class = try allocator.create(ClassDef);
        string_class.* = .{
            .name = try allocator.dupe(u8, "String"),
            .properties = std.StringHashMap(PropertyDef).init(allocator),
            .methods = std.StringHashMap(MethodDef).init(allocator),
        };
        try string_class.methods.put("length", .{
            .name = "length",
            .function = try createLengthMethod(allocator),
        });
        try vm.classes.put(string_class.name, string_class);

        // Add built-in Number class
        const number_class = try allocator.create(ClassDef);
        number_class.* = .{
            .name = try allocator.dupe(u8, "Number"),
            .properties = std.StringHashMap(PropertyDef).init(allocator),
            .methods = std.StringHashMap(MethodDef).init(allocator),
        };
        try number_class.methods.put("toInt", .{
            .name = "toInt",
            .function = try createToIntMethod(allocator),
        });
        try number_class.methods.put("toFloat", .{
            .name = "toFloat",
            .function = try createToFloatMethod(allocator),
        });
        try vm.classes.put(number_class.name, number_class);

        // Add built-in Boolean class
        const boolean_class = try allocator.create(ClassDef);
        boolean_class.* = .{
            .name = try allocator.dupe(u8, "Boolean"),
            .properties = std.StringHashMap(PropertyDef).init(allocator),
            .methods = std.StringHashMap(MethodDef).init(allocator),
        };
        try boolean_class.methods.put("toString", .{
            .name = "toString",
            .function = try createToStringMethod(allocator),
        });
        try vm.classes.put(boolean_class.name, boolean_class);

        return vm;
    }

    fn createInitMethod(allocator: std.mem.Allocator) !*const bytecode.Function {
        const instructions = try allocator.alloc(bytecode.BytecodeInstr, 0);
        return &bytecode.Function{
            .instructions = instructions,
            .allocator = allocator,
        };
    }

    fn createToStringMethod(allocator: std.mem.Allocator) !*const bytecode.Function {
        const instructions = try allocator.alloc(bytecode.BytecodeInstr, 0);
        return &bytecode.Function{
            .instructions = instructions,
            .allocator = allocator,
        };
    }

    fn createHashCodeMethod(allocator: std.mem.Allocator) !*const bytecode.Function {
        const instructions = try allocator.alloc(bytecode.BytecodeInstr, 0);
        return &bytecode.Function{
            .instructions = instructions,
            .allocator = allocator,
        };
    }

    fn createEqualsMethod(allocator: std.mem.Allocator) !*const bytecode.Function {
        const instructions = try allocator.alloc(bytecode.BytecodeInstr, 0);
        return &bytecode.Function{
            .instructions = instructions,
            .allocator = allocator,
        };
    }

    fn createLengthMethod(allocator: std.mem.Allocator) !*const bytecode.Function {
        const instructions = try allocator.alloc(bytecode.BytecodeInstr, 0);
        return &bytecode.Function{
            .instructions = instructions,
            .allocator = allocator,
        };
    }

    fn createToIntMethod(allocator: std.mem.Allocator) !*const bytecode.Function {
        const instructions = try allocator.alloc(bytecode.BytecodeInstr, 0);
        return &bytecode.Function{
            .instructions = instructions,
            .allocator = allocator,
        };
    }

    fn createToFloatMethod(allocator: std.mem.Allocator) !*const bytecode.Function {
        const instructions = try allocator.alloc(bytecode.BytecodeInstr, 0);
        return &bytecode.Function{
            .instructions = instructions,
            .allocator = allocator,
        };
    }

    fn createErrorMethod(allocator: std.mem.Allocator) !*const bytecode.Function {
        const instructions = try allocator.alloc(bytecode.BytecodeInstr, 0);
        return &bytecode.Function{
            .instructions = instructions,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *VM) void {
        self.stack.clearAndFree();
        self.stack.deinit();

        // Clean up classes
        var class_iter = self.classes.iterator();
        while (class_iter.next()) |entry| {
            const class_def = entry.value_ptr.*;
            class_def.properties.deinit();
            class_def.methods.deinit();
            self.arena.allocator().free(class_def.name);
            self.arena.allocator().destroy(class_def);
        }
        self.classes.deinit();

        // Clean up instances
        for (self.instances.items) |instance| {
            instance.properties.deinit();
            self.arena.allocator().destroy(instance);
        }
        self.instances.deinit();

        _ = self.arena.deinit();
    }

    pub fn runModule(self: *VM, module: *const bytecode.Module) !void {
        if (module.functions.len == 0) {
            return error.NoFunctionsToRun;
        }
        const stdout = std.io.getStdOut();
        try self.runFunction(&module.functions[0], @TypeOf(stdout.writer()), .{ .writer = stdout.writer() });
    }

    /// Execute a bytecode function
    /// function: The function to execute
    /// Writer: Type of the output writer
    /// options: Configuration containing the writer instance
    /// Returns error if execution fails
    pub fn runFunction(self: *VM, function: *const bytecode.Function, comptime Writer: type, options: struct { writer: Writer }) !void {
        for (function.instructions) |instr| {
            switch (instr.code) {
                .LoadString => |load| {
                    if (load.value.len > 4096) {
                        return error.StringTooLarge;
                    }

                    self.temp_allocator.reset();
                    const allocator = self.temp_allocator.allocator();
                    const str_copy = try allocator.dupe(u8, load.value);
                    try self.stack.append(str_copy);
                },
                .LoadInt => |load| {
                    const int_ptr = try self.temp_allocator.allocator().create(i64);
                    int_ptr.* = load.value;
                    const int_bytes = @as([*]u8, @ptrCast(int_ptr))[0..@sizeOf(i64)];
                    try self.stack.append(int_bytes);
                },
                .Print => {
                    if (self.stack.items.len == 0) {
                        return error.StackUnderflow;
                    }
                    const value = self.stack.pop();
                    try options.writer.print("{s}\n", .{value});
                },
                .NewClass => |new_class| {
                    const allocator = self.arena.allocator();

                    // Check if class already exists
                    if (self.classes.get(new_class.class_name)) |existing_class| {
                        // Create new instance of existing class
                        const instance = try allocator.create(Instance);
                        instance.* = .{
                            .class = existing_class,
                            .properties = std.StringHashMap([]const u8).init(allocator),
                        };
                        try self.instances.append(instance);
                        try self.stack.append(@ptrCast(instance));
                    } else {
                        // Create new class definition
                        const class_def = try allocator.create(ClassDef);
                        class_def.* = .{
                            .name = try allocator.dupe(u8, new_class.class_name),
                            .properties = std.StringHashMap(PropertyDef).init(allocator),
                            .methods = std.StringHashMap(MethodDef).init(allocator),
                        };

                        // Register built-in methods
                        try class_def.methods.put("init", .{
                            .name = "init",
                            .function = try createInitMethod(allocator),
                        });

                        try self.classes.put(class_def.name, class_def);

                        // Create new instance
                        const instance = try allocator.create(Instance);
                        instance.* = .{
                            .class = class_def,
                            .properties = std.StringHashMap([]const u8).init(allocator),
                        };
                        try self.instances.append(instance);
                        try self.stack.append(@ptrCast(instance));
                    }
                },
                .SetProperty => |set_prop| {
                    if (self.stack.items.len < 2) {
                        return error.StackUnderflow;
                    }
                    const value = self.stack.pop();
                    const instance = @as(*Instance, @ptrCast(@alignCast(self.stack.pop())));

                    // Verify property exists in class definition
                    if (instance.class.properties.get(set_prop.property_name) == null) {
                        return error.UndefinedProperty;
                    }

                    try instance.properties.put(set_prop.property_name, value);
                },
                .GetProperty => |get_prop| {
                    if (self.stack.items.len < 1) {
                        return error.StackUnderflow;
                    }
                    const instance = @as(*Instance, @ptrCast(@alignCast(self.stack.pop())));

                    // Verify property exists in class definition
                    if (instance.class.properties.get(get_prop.property_name) == null) {
                        return error.UndefinedProperty;
                    }

                    if (instance.properties.get(get_prop.property_name)) |value| {
                        try self.stack.append(value);
                    } else {
                        return error.PropertyNotInitialized;
                    }
                },
                .CallMethod => |call| {
                    if (self.stack.items.len < call.arg_count + 1) {
                        return error.StackUnderflow;
                    }
                    const instance = @as(*Instance, @ptrCast(@alignCast(self.stack.pop())));
                    if (instance.class.methods.get(call.method_name)) |method| {
                        try self.runFunction(method.function, Writer, options);
                    } else {
                        return error.MethodNotFound;
                    }
                },
            }
        }
    }
};
