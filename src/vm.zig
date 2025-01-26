const std = @import("std");
const bytecode = @import("bytecode.zig");

/// Virtual Machine for executing bytecode
pub const VM = struct {
    stack: std.ArrayList([]u8) = undefined,
    arena: std.heap.ArenaAllocator,
    temp_allocator: std.heap.FixedBufferAllocator,
    classes: std.StringHashMap(*ClassDef),
    instances: std.ArrayList(*Instance),

    const ClassDef = struct {
        name: []const u8,
        name_ptr: ?*[]const u8,
        parent: ?*const ClassDef,
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
    pub fn init() !VM {
        std.debug.print("1. Starting VM init...\n", .{});
        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        std.debug.print("2. Arena allocator created\n", .{});
        const allocator = arena.allocator();
        std.debug.print("3. Got arena allocator\n", .{});

        std.debug.print("4. Initializing stack...\n", .{});
        var stack = std.ArrayList([]u8).init(allocator);
        std.debug.print("5. Stack array list created\n", .{});
        try stack.ensureTotalCapacity(1024);
        std.debug.print("6. Stack capacity ensured\n", .{});
        stack.items.len = 0; // Explicitly initialize length
        std.debug.print("7. Stack length initialized\n", .{});

        std.debug.print("8. Initializing temp allocator...\n", .{});
        var temp_buffer: [65536]u8 = undefined;
        std.debug.print("9. Temp buffer created\n", .{});
        const temp_allocator = std.heap.FixedBufferAllocator.init(&temp_buffer);
        std.debug.print("10. Temp allocator created\n", .{});

        std.debug.print("11. Creating VM instance...\n", .{});
        var vm = VM{
            .arena = arena,
            .stack = stack,
            .temp_allocator = temp_allocator,
            .classes = std.StringHashMap(*ClassDef).init(allocator),
            .instances = std.ArrayList(*Instance).init(allocator),
        };
        std.debug.print("12. VM instance created\n", .{});

        std.debug.print("13. Initializing builtin classes...\n", .{});
        try vm.initBuiltinClasses();
        std.debug.print("14. Builtin classes initialized\n", .{});
        std.debug.print("15. VM initialization complete\n", .{});
        return vm;
    }

    fn initBuiltinClasses(self: *VM) !void {
        std.debug.print("13.1 Creating Object class...\n", .{});
        const object_class = try self.createClass("Object", null);
        std.debug.print("13.2 Object class created\n", .{});

        std.debug.print("13.3 Adding toString method...\n", .{});
        const toString_method = try createToStringMethod(self.arena.allocator());
        std.debug.print("13.4 toString method created\n", .{});
        try object_class.methods.put("toString", .{
            .name = "toString",
            .function = toString_method,
        });
        std.debug.print("13.5 toString method added\n", .{});

        std.debug.print("13.6 Adding Object class to classes map...\n", .{});
        try self.classes.put(object_class.name, object_class);
        std.debug.print("13.7 Object class added\n", .{});

        std.debug.print("13.8 Creating Object instance...\n", .{});
        const object_instance = try createInstance(self.arena.allocator(), object_class);
        std.debug.print("13.9 Object instance created\n", .{});
        try self.instances.append(object_instance);
        std.debug.print("13.10 Object instance added\n", .{});
    }

    fn createClass(self: *VM, name: []const u8, parent: ?*const ClassDef) !*ClassDef {
        std.debug.print("Creating class with name: {s} (len: {})\n", .{ name, name.len });

        // Validate input name
        if (name.len == 0 or name.len > 256) {
            std.debug.print("Invalid class name length: {}\n", .{name.len});
            return error.InvalidClassName;
        }

        // Check for valid UTF-8
        if (!std.unicode.utf8ValidateSlice(name)) {
            std.debug.print("Invalid UTF-8 in class name: {s}\n", .{name});
            return error.InvalidClassNameEncoding;
        }

        // Create class definition
        const class_def = try self.arena.allocator().create(ClassDef);
        errdefer self.arena.allocator().destroy(class_def);

        // Duplicate name with validation
        const class_name = try self.arena.allocator().dupe(u8, name);
        if (class_name.len == 0) {
            std.debug.print("Failed to allocate class name\n", .{});
            return error.ClassNameAllocationFailed;
        }

        // Initialize class first without name_ptr
        class_def.* = .{
            .name = class_name,
            .name_ptr = null,
            .parent = parent,
            .properties = std.StringHashMap(PropertyDef).init(self.arena.allocator()),
            .methods = std.StringHashMap(MethodDef).init(self.arena.allocator()),
        };

        // Allocate stable storage for class name only after class_def is initialized
        const name_ptr = try self.arena.allocator().create([]const u8);
        name_ptr.* = class_name;
        class_def.name_ptr = name_ptr;

        // Validate the name_ptr after assignment
        if (class_def.name_ptr == null or class_def.name_ptr.?.*.len == 0) {
            return error.InvalidClassNamePointer;
        }

        // Verify initialization
        if (class_def.name.len == 0 or class_def.name_ptr == null) {
            std.debug.print("Class definition initialization failed\n", .{});
            return error.ClassDefInitializationFailed;
        }

        std.debug.print("Successfully created class: {s} (ptr: {*})\n", .{ class_def.name, class_def.name_ptr });
        return class_def;
    }

    fn createInstance(allocator: std.mem.Allocator, class_def: *const ClassDef) !*Instance {
        std.debug.print("13.8.1 Validating class definition...\n", .{});
        std.debug.print("ClassDef pointer: {*}\n", .{class_def});

        // Validate class_def fields
        if (class_def.name.len == 0) {
            return error.InvalidClassDefinition;
        }

        std.debug.print("13.8.2 Creating instance...\n", .{});
        const instance = try allocator.create(Instance);
        std.debug.print("13.8.3 Instance created\n", .{});

        std.debug.print("13.8.4 Initializing properties map...\n", .{});
        var properties = std.StringHashMap([]const u8).init(allocator);
        try properties.ensureTotalCapacity(16);
        std.debug.print("13.8.5 Properties map initialized\n", .{});

        std.debug.print("13.8.6 Initializing instance fields...\n", .{});
        instance.* = .{
            .class = class_def,
            .properties = properties,
        };
        std.debug.print("13.8.7 Instance fields initialized\n", .{});

        std.debug.print("13.8.8 Initializing required properties...\n", .{});
        var current_class: ?*const ClassDef = class_def;
        while (current_class) |cls| {
            std.debug.print("13.8.8.1 Validating class pointer: {*}\n", .{cls});
            // Validate class name
            if (cls.name.len == 0) {
                std.debug.print("13.8.8.2 Invalid class name length: 0\n", .{});
                return error.InvalidClassHierarchy;
            }

            // Validate and print class name safely
            if (cls.name_ptr) |name_ptr| {
                std.debug.print("13.8.8.3 Validating name_ptr: {*}\n", .{name_ptr});
                if (name_ptr.*.len > 0 and std.unicode.utf8ValidateSlice(name_ptr.*)) {
                    std.debug.print("13.8.9 Processing class: {s} (len: {}, ptr: {*})\n", .{ name_ptr.*, name_ptr.*.len, name_ptr });
                } else {
                    std.debug.print("13.8.9 Processing class with invalid name (length: {})\n", .{cls.name.len});
                    return error.InvalidClassName;
                }
            } else {
                std.debug.print("13.8.8.4 Class has null name_ptr\n", .{});
            }
            var prop_iter = cls.properties.iterator();
            while (prop_iter.next()) |prop| {
                // Validate property name
                if (prop.value_ptr.name.len == 0) {
                    return error.InvalidPropertyDefinition;
                }

                std.debug.print("13.8.10 Processing property: {s}\n", .{prop.value_ptr.name});
                if (prop.value_ptr.required) {
                    std.debug.print("13.8.11 Initializing required property: {s}\n", .{prop.value_ptr.name});
                    const default_value = try allocator.dupe(u8, "");
                    try instance.properties.put(prop.value_ptr.name, default_value);
                    std.debug.print("13.8.12 Property initialized\n", .{});
                }
            }
            current_class = cls.parent;
        }

        std.debug.print("13.8.13 Instance creation complete\n", .{});
        return instance;
    }

    pub fn deinit(self: *VM) void {
        // Clean up stack items
        while (self.stack.popOrNull()) |value| {
            self.arena.allocator().free(value);
        }
        self.stack.deinit();

        // Clean up classes
        var class_iter = self.classes.iterator();
        while (class_iter.next()) |entry| {
            const class_def = entry.value_ptr.*;

            // Clean up properties
            var prop_iter = class_def.properties.iterator();
            while (prop_iter.next()) |prop| {
                self.arena.allocator().free(prop.value_ptr.name);
                self.arena.allocator().free(prop.value_ptr.type);
            }
            class_def.properties.deinit();

            // Clean up methods
            var method_iter = class_def.methods.iterator();
            while (method_iter.next()) |method| {
                self.arena.allocator().free(method.value_ptr.name);
            }
            class_def.methods.deinit();

            self.arena.allocator().free(class_def.name);
            if (class_def.name_ptr) |name_ptr| {
                self.arena.allocator().destroy(name_ptr);
            }
            self.arena.allocator().destroy(class_def);
        }
        self.classes.deinit();

        // Clean up instances
        for (self.instances.items) |instance| {
            // Clean up instance properties
            var prop_iter = instance.properties.iterator();
            while (prop_iter.next()) |prop| {
                self.arena.allocator().free(prop.value_ptr.*);
            }
            instance.properties.deinit();
            self.arena.allocator().destroy(instance);
        }
        self.instances.deinit();

        // Clean up arena
        _ = self.arena.deinit();
    }

    pub fn runModule(self: *VM, module: *const bytecode.Module) !void {
        if (module.functions.len == 0) {
            return error.NoFunctionsToRun;
        }
        const stdout = std.io.getStdOut();
        try self.runFunction(&module.functions[0], @TypeOf(stdout.writer()), .{ .writer = stdout.writer() });
    }

    fn WriterOptions(comptime Writer: type) type {
        return struct {
            writer: Writer,
        };
    }

    const VmError = error{
        StackUnderflow,
        InvalidInstruction,
        NoInstance,
        PropertyNotFound,
        MethodNotFound,
        InvalidString,
        OutOfMemory,
        DiskQuota,
        FileTooBig,
        InputOutput,
        NoSpaceLeft,
        DeviceBusy,
        InvalidArgument,
        AccessDenied,
        BrokenPipe,
        SystemResources,
        OperationAborted,
        NotOpenForWriting,
        LockViolation,
        WouldBlock,
        ConnectionResetByPeer,
        Unexpected,
        InvalidClassName,
        InvalidClassNameEncoding,
        ClassNameAllocationFailed,
        ClassDefInitializationFailed,
        InvalidClassNamePointer,
    };

    pub fn runFunction(self: *VM, function: *const bytecode.Function, comptime Writer: type, options: WriterOptions(Writer)) VmError!void {
        for (function.instructions) |instr| {
            try self.executeInstruction(instr, Writer, options);
        }
    }

    fn executeInstruction(self: *VM, instr: bytecode.BytecodeInstr, comptime Writer: type, options: WriterOptions(Writer)) VmError!void {
        switch (instr.code) {
            .LoadString => try self.loadString(instr.code),
            .LoadInt => try self.loadInt(instr.code),
            .Print => try self.print(Writer, options),
            .NewClass => try self.newClass(instr.code),
            .SetProperty => try self.setProperty(instr.code),
            .GetProperty => try self.getProperty(instr.code),
            .CallMethod => try self.callMethod(instr.code, Writer, options),
        }
    }

    fn loadString(self: *VM, load: bytecode.InstructionCode) VmError!void {
        switch (load) {
            .LoadString => |data| {
                if (data.value.len == 0) {
                    return error.InvalidString;
                }

                // Handle string ownership
                const value = if (data.owned) blk: {
                    // Take ownership of the string
                    const non_const = try self.arena.allocator().dupe(u8, data.value);
                    self.arena.allocator().free(data.value);
                    break :blk non_const;
                } else blk: {
                    // Duplicate the string
                    break :blk try self.arena.allocator().dupe(u8, data.value);
                };

                // Free any existing value on the stack before pushing new one
                if (self.stack.popOrNull()) |old_value| {
                    self.arena.allocator().free(old_value);
                }

                try self.stack.append(value);
            },
            else => return error.InvalidInstruction,
        }
    }

    fn loadInt(self: *VM, load: bytecode.InstructionCode) VmError!void {
        switch (load) {
            .LoadInt => |data| {
                // Free any existing value on the stack before pushing new one
                if (self.stack.popOrNull()) |old_value| {
                    self.arena.allocator().free(old_value);
                }
                const int_str = try std.fmt.allocPrint(self.arena.allocator(), "{}", .{data.value});
                try self.stack.append(int_str);
            },
            else => return error.InvalidInstruction,
        }
    }

    fn print(self: *VM, comptime Writer: type, options: WriterOptions(Writer)) VmError!void {
        if (self.stack.items.len == 0) {
            return error.StackUnderflow;
        }

        // Get and validate the value
        if (self.stack.items.len == 0) {
            return error.StackUnderflow;
        }
        const value = self.stack.items[self.stack.items.len - 1];
        if (value.len == 0) {
            return error.InvalidString;
        }

        // Print the value
        try options.writer.print("{s}\n", .{value});

        // Pop and free the value after printing
        if (self.stack.popOrNull()) |val| {
            if (val.len > 0) {
                self.arena.allocator().free(val);
            }
        }
    }

    fn newClass(self: *VM, new_class: bytecode.InstructionCode) VmError!void {
        switch (new_class) {
            .NewClass => |data| {
                const class_def = try self.createClass(data.class_name, null);
                try self.classes.put(data.class_name, class_def);
            },
            else => return error.InvalidInstruction,
        }
    }

    fn setProperty(self: *VM, set_prop: bytecode.InstructionCode) VmError!void {
        switch (set_prop) {
            .SetProperty => |data| {
                if (self.instances.popOrNull()) |instance| {
                    if (self.stack.popOrNull()) |value| {
                        // Check if property already exists and free it
                        if (instance.properties.get(data.property_name)) |old_value| {
                            self.arena.allocator().free(old_value);
                        }
                        const value_copy = try self.arena.allocator().dupe(u8, value);
                        try instance.properties.put(data.property_name, value_copy);
                        try self.instances.append(instance);
                    } else {
                        return error.StackUnderflow;
                    }
                } else {
                    return error.NoInstance;
                }
            },
            else => return error.InvalidInstruction,
        }
    }

    fn getProperty(self: *VM, get_prop: bytecode.InstructionCode) VmError!void {
        switch (get_prop) {
            .GetProperty => |data| {
                if (self.instances.popOrNull()) |instance| {
                    if (instance.properties.get(data.property_name)) |value| {
                        // Free any existing value on the stack before pushing new one
                        if (self.stack.popOrNull()) |old_value| {
                            self.arena.allocator().free(old_value);
                        }
                        const value_copy = try self.arena.allocator().dupe(u8, value);
                        try self.stack.append(value_copy);
                        try self.instances.append(instance);
                    } else {
                        return error.PropertyNotFound;
                    }
                } else {
                    return error.NoInstance;
                }
            },
            else => return error.InvalidInstruction,
        }
    }

    fn callMethod(self: *VM, call: bytecode.InstructionCode, comptime Writer: type, options: WriterOptions(Writer)) VmError!void {
        switch (call) {
            .CallMethod => |data| {
                if (self.instances.popOrNull()) |instance| {
                    if (instance.class.methods.get(data.method_name)) |method| {
                        // Save current stack size
                        const stack_size = self.stack.items.len;

                        // Run the method
                        try self.runFunction(method.function, Writer, options);

                        // Clean up any values left on the stack by the method
                        while (self.stack.items.len > stack_size) {
                            if (self.stack.popOrNull()) |value| {
                                self.arena.allocator().free(value);
                            }
                        }

                        try self.instances.append(instance);
                    } else {
                        return error.MethodNotFound;
                    }
                } else {
                    return error.NoInstance;
                }
            },
            else => return error.InvalidInstruction,
        }
    }
};

fn createToStringMethod(allocator: std.mem.Allocator) !*const bytecode.Function {
    // Create a basic toString implementation that returns "Object"
    const instructions = try allocator.alloc(bytecode.BytecodeInstr, 2);

    // Load the string "Object"
    instructions[0] = bytecode.BytecodeInstr{
        .code = .{
            .LoadString = .{
                .value = "Object",
            },
        },
    };

    // Print the string
    instructions[1] = bytecode.BytecodeInstr{
        .code = .Print,
    };

    return &bytecode.Function{
        .instructions = instructions,
        .allocator = allocator,
    };
}
