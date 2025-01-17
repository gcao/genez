const std = @import("std");

pub const Value = union(enum) {
    Nil: void,
    Bool: bool,
    Int: i64,
    Float: f64,
    String: []const u8,
    Symbol: []const u8,
    Array: []Value,
    Map: std.StringHashMap(Value),
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
    Function: *FunctionType,
    Class: *ClassType,
    Instance: *InstanceType,
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
