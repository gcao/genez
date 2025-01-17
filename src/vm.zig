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
        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        const allocator = arena.allocator();

        var stack = std.ArrayList([]const u8).init(allocator);
        try stack.ensureTotalCapacity(1024);

        var temp_buffer: [65536]u8 = undefined;
        const temp_allocator = std.heap.FixedBufferAllocator.init(&temp_buffer);

        var vm = VM{
            .arena = arena,
            .stack = stack,
            .temp_allocator = temp_allocator,
            .classes = std.StringHashMap(*ClassDef).init(allocator),
            .instances = std.ArrayList(*Instance).init(allocator),
        };

        // Initialize built-in classes
        try vm.initBuiltinClasses();

        return vm;
    }

    fn initBuiltinClasses(self: *VM) !void {
        // Initialize Object class
        const object_class = try self.createClass("Object", null);
        try object_class.methods.put("toString", .{
            .name = "toString",
            .function = try createToStringMethod(self.arena.allocator()),
        });
        try self.classes.put(object_class.name, object_class);

        // Create initial Object instance
        const object_instance = try createInstance(self.arena.allocator(), object_class);
        try self.instances.append(object_instance);

        // Initialize other built-in classes (String, Number, Boolean)
        // ... (implementation omitted for brevity)
    }

    fn createClass(self: *VM, name: []const u8, parent: ?*const ClassDef) !*ClassDef {
        const class_def = try self.arena.allocator().create(ClassDef);
        class_def.* = .{
            .name = try self.arena.allocator().dupe(u8, name),
            .parent = parent,
            .properties = std.StringHashMap(PropertyDef).init(self.arena.allocator()),
            .methods = std.StringHashMap(MethodDef).init(self.arena.allocator()),
        };
        return class_def;
    }

    fn createInstance(allocator: std.mem.Allocator, class_def: *const ClassDef) !*Instance {
        const instance = try allocator.create(Instance);
        instance.* = .{
            .class = class_def,
            .properties = std.StringHashMap([]const u8).init(allocator),
        };

        // Initialize required properties
        var current_class: ?*const ClassDef = class_def;
        while (current_class) |cls| {
            var prop_iter = cls.properties.iterator();
            while (prop_iter.next()) |prop| {
                if (prop.value_ptr.required) {
                    return error.MissingRequiredProperty;
                }
            }
            current_class = cls.parent;
        }

        return instance;
    }

    pub fn deinit(self: *VM) void {
        // Clean up resources
        self.stack.deinit();

        var class_iter = self.classes.iterator();
        while (class_iter.next()) |entry| {
            const class_def = entry.value_ptr.*;
            class_def.properties.deinit();
            class_def.methods.deinit();
            self.arena.allocator().free(class_def.name);
            self.arena.allocator().destroy(class_def);
        }
        self.classes.deinit();

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
                const value = try self.arena.allocator().dupe(u8, data.value);
                try self.stack.append(value);
            },
            else => return error.InvalidInstruction,
        }
    }

    fn loadInt(self: *VM, load: bytecode.InstructionCode) VmError!void {
        switch (load) {
            .LoadInt => |data| {
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

        const value = self.stack.pop();
        if (value.len == 0) {
            try options.writer.print("\n", .{});
        } else {
            try options.writer.print("{s}\n", .{value});
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
                        try instance.properties.put(data.property_name, value);
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
                        try self.stack.append(value);
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
                        try self.runFunction(method.function, Writer, options);
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
    const instructions = try allocator.alloc(bytecode.BytecodeInstr, 0);
    return &bytecode.Function{
        .instructions = instructions,
        .allocator = allocator,
    };
}
