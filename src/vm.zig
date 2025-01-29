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
        return VM{
            .arena = std.heap.ArenaAllocator.init(std.heap.page_allocator),
            .temp_allocator = std.heap.FixedBufferAllocator.init(&[_]u8{}),
            .classes = std.StringHashMap(*ClassDef).init(std.heap.page_allocator),
            .instances = std.ArrayList(*Instance).init(std.heap.page_allocator),
        };
    }

    fn initBuiltinClasses(_: *VM) !void {
        // TODO: Implement builtin classes
    }

    fn createClass(_: *VM, _: []const u8, _: ?*const ClassDef) !*ClassDef {
        // TODO: Implement class creation
        return undefined;
    }

    fn createInstance(_: *VM, _: *const ClassDef) !*Instance {
        // TODO: Implement instance creation
        return undefined;
    }

    pub fn deinit(self: *VM) void {
        self.arena.deinit();
        self.classes.deinit();
        self.instances.deinit();
    }

    pub fn runModule(self: *VM, module: *const bytecode.Module) anyerror!void {
        // Run each function in the module
        for (module.functions) |*func| {
            try self.runFunction(func, std.io.getStdOut().writer());
        }
    }

    pub fn runFunction(self: *VM, func: *const bytecode.Function, writer: anytype) anyerror!void {
        self.stack = std.ArrayList([]u8).init(self.arena.allocator());
        defer self.stack.deinit();

        for (func.instructions) |instr| {
            switch (instr.code) {
                .LoadInt => |val| {
                    const num_str = try std.fmt.allocPrint(self.arena.allocator(), "{d}", .{val.value});
                    try self.stack.append(num_str);
                },
                .LoadString => |str_val| {
                    const str_copy = try self.arena.allocator().dupe(u8, str_val.value);
                    try self.stack.append(str_copy);
                },
                .Print => {
                    if (self.stack.items.len == 0) return error.StackUnderflow;
                    const value = self.stack.pop();
                    try writer.writeAll(value);
                    try writer.writeAll("\n");
                    if (@hasDecl(@TypeOf(writer), "flush")) {
                        try writer.flush();
                    }
                },
                else => return error.UnimplementedOpcode,
            }
        }
    }
};

fn createToStringMethod(_: std.mem.Allocator) !*const bytecode.Function {
    // Placeholder implementation
    return undefined;
}
