const std = @import("std");
const bytecode = @import("../backend/bytecode.zig");
const debug = @import("debug.zig");

pub const BuiltinOperatorType = enum {
    Eq,
    Add,
    Sub,
    LessThan,
    GreaterThan,
    Print,
};

pub const Value = union(enum) {
    // Basic values
    Nil: void,
    Bool: bool,
    Int: i64,
    Float: f64,
    Char: u32,  // Unicode code point
    String: []const u8,
    Symbol: []const u8,
    ComplexSymbol: struct {
        namespace: []const u8,
        name: []const u8,
    },
    
    // Collection values
    Array: []Value,
    Map: std.StringHashMap(Value),
    Set: std.StringHashMap(void), // Use HashMap for Set implementation
    
    // Gene container value
    Gene: struct {
        properties: std.StringHashMap(*Value),
        values: []*Value,
    },
    
    // Function and VM values
    Function: *bytecode.Function,
    BuiltinOperator: BuiltinOperatorType,
    ReturnAddress: struct {
        stack_ptr: usize,
        arg_count: usize,
    },
    Variable: struct {
        name: []const u8,
    },
    
    // Object-oriented values
    Class: struct {
        name: []const u8,
        methods: std.StringHashMap(*Value),
        fields: std.StringHashMap(*Type),
        parent: ?*Value,
    },
    Instance: struct {
        class: *Value, // Points to Class value
        fields: std.StringHashMap(*Value),
    },
    Trait: struct {
        name: []const u8,
        methods: std.StringHashMap(*Value),
    },
    
    // Module and namespace values
    Module: struct {
        name: []const u8,
        exports: std.StringHashMap(*Value),
    },
    
    // Reference and concurrency values
    Ref: struct {
        target: *Value,
        is_mutable: bool,
    },
    Atom: struct {
        target: *Value,
        // Would need actual atomic operations in real implementation
    },
    Channel: struct {
        // Simplified channel representation
        buffer: []*Value,
        capacity: usize,
        closed: bool,
    },
    Promise: struct {
        // Simplified promise representation
        state: PromiseState,
        value: ?*Value,
        err: ?*Value,
        
        const PromiseState = enum {
            Pending,
            Fulfilled,
            Rejected,
        };
    },

    pub fn deinit(self: *Value, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .String => |str| allocator.free(str),
            .Symbol => |sym| allocator.free(sym),
            .ComplexSymbol => |csym| {
                allocator.free(csym.namespace);
                allocator.free(csym.name);
            },
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
            .Set => |*set| {
                var it = set.iterator();
                while (it.next()) |entry| {
                    allocator.free(entry.key_ptr.*);
                }
                set.deinit();
            },
            .Gene => |*gene| {
                var it = gene.properties.iterator();
                while (it.next()) |entry| {
                    allocator.free(entry.key_ptr.*);
                    entry.value_ptr.*.deinit(allocator);
                    allocator.destroy(entry.value_ptr.*);
                }
                gene.properties.deinit();
                for (gene.values) |val| {
                    val.deinit(allocator);
                    allocator.destroy(val);
                }
                allocator.free(gene.values);
            },
            .Class => |*cls| {
                allocator.free(cls.name);
                var it = cls.methods.iterator();
                while (it.next()) |entry| {
                    allocator.free(entry.key_ptr.*);
                    entry.value_ptr.*.deinit(allocator);
                    allocator.destroy(entry.value_ptr.*);
                }
                cls.methods.deinit();
                cls.fields.deinit();
            },
            .Instance => |*inst| {
                var it = inst.fields.iterator();
                while (it.next()) |entry| {
                    allocator.free(entry.key_ptr.*);
                    entry.value_ptr.*.deinit(allocator);
                    allocator.destroy(entry.value_ptr.*);
                }
                inst.fields.deinit();
            },
            .Trait => |*trait| {
                allocator.free(trait.name);
                var it = trait.methods.iterator();
                while (it.next()) |entry| {
                    allocator.free(entry.key_ptr.*);
                    entry.value_ptr.*.deinit(allocator);
                    allocator.destroy(entry.value_ptr.*);
                }
                trait.methods.deinit();
            },
            .Module => |*mod| {
                allocator.free(mod.name);
                var it = mod.exports.iterator();
                while (it.next()) |entry| {
                    allocator.free(entry.key_ptr.*);
                    entry.value_ptr.*.deinit(allocator);
                    allocator.destroy(entry.value_ptr.*);
                }
                mod.exports.deinit();
            },
            .Channel => |*chan| {
                for (chan.buffer) |val| {
                    val.deinit(allocator);
                    allocator.destroy(val);
                }
                allocator.free(chan.buffer);
            },
            .Promise => |*promise| {
                if (promise.value) |val| {
                    val.deinit(allocator);
                    allocator.destroy(val);
                }
                if (promise.err) |err| {
                    err.deinit(allocator);
                    allocator.destroy(err);
                }
            },
            .ReturnAddress => {},
            .Function => {
                // Don't free function pointers here since they are shared references
                // Functions should be managed at a higher level (e.g., by the Module)
                // This prevents double-free errors when the same function is referenced
                // in multiple places (module functions, VM variables, instruction operands)
            },
            .Variable => {}, // No need to free the name as it's a string literal
            .BuiltinOperator => {},
            .Nil, .Bool, .Int, .Float, .Char => {},
            .Ref, .Atom => {}, // Don't free target as it's a shared reference
        }
    }

    pub fn clone(self: Value, allocator: std.mem.Allocator) !Value {
        return switch (self) {
            .Nil => Value{ .Nil = {} },
            .Bool => |val| Value{ .Bool = val },
            .Int => |val| Value{ .Int = val },
            .Float => |val| Value{ .Float = val },
            .Char => |val| Value{ .Char = val },
            .String => |str| Value{ .String = try allocator.dupe(u8, str) },
            .Symbol => |sym| Value{ .Symbol = try allocator.dupe(u8, sym) },
            .ComplexSymbol => |csym| Value{ .ComplexSymbol = .{
                .namespace = try allocator.dupe(u8, csym.namespace),
                .name = try allocator.dupe(u8, csym.name),
            }},
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
            .Set => |set| blk: {
                var new_set = std.StringHashMap(void).init(allocator);
                errdefer {
                    var it = new_set.iterator();
                    while (it.next()) |entry| {
                        allocator.free(entry.key_ptr.*);
                    }
                    new_set.deinit();
                }

                var it = set.iterator();
                while (it.next()) |entry| {
                    const key = try allocator.dupe(u8, entry.key_ptr.*);
                    errdefer allocator.free(key);
                    try new_set.put(key, {});
                }
                break :blk Value{ .Set = new_set };
            },
            .Gene => |gene| blk: {
                var new_props = std.StringHashMap(Value).init(allocator);
                errdefer {
                    var it = new_props.iterator();
                    while (it.next()) |entry| {
                        allocator.free(entry.key_ptr.*);
                        var val = entry.value_ptr.*;
                        val.deinit(allocator);
                    }
                    new_props.deinit();
                }

                var it = gene.properties.iterator();
                while (it.next()) |entry| {
                    const key = try allocator.dupe(u8, entry.key_ptr.*);
                    errdefer allocator.free(key);
                    const value = try entry.value_ptr.clone(allocator);
                    errdefer {
                        var tmp = value;
                        tmp.deinit(allocator);
                    }
                    try new_props.put(key, value);
                }

                var new_values = try allocator.alloc(Value, gene.values.len);
                errdefer allocator.free(new_values);
                for (gene.values, 0..) |val, i| {
                    new_values[i] = try val.clone(allocator);
                }

                break :blk Value{ .Gene = .{
                    .properties = new_props,
                    .values = new_values,
                }};
            },
            .Class => |cls| blk: {
                var new_methods = std.StringHashMap(Value).init(allocator);
                errdefer {
                    var it = new_methods.iterator();
                    while (it.next()) |entry| {
                        allocator.free(entry.key_ptr.*);
                        var val = entry.value_ptr.*;
                        val.deinit(allocator);
                    }
                    new_methods.deinit();
                }

                var it = cls.methods.iterator();
                while (it.next()) |entry| {
                    const key = try allocator.dupe(u8, entry.key_ptr.*);
                    errdefer allocator.free(key);
                    const value = try entry.value_ptr.clone(allocator);
                    errdefer {
                        var tmp = value;
                        tmp.deinit(allocator);
                    }
                    try new_methods.put(key, value);
                }

                var new_fields = std.StringHashMap(*Type).init(allocator);
                var fields_it = cls.fields.iterator();
                while (fields_it.next()) |entry| {
                    const key = try allocator.dupe(u8, entry.key_ptr.*);
                    try new_fields.put(key, entry.value_ptr.*);
                }

                break :blk Value{ .Class = .{
                    .name = try allocator.dupe(u8, cls.name),
                    .methods = new_methods,
                    .fields = new_fields,
                    .parent = cls.parent, // Shallow copy of parent reference
                }};
            },
            .Instance => |inst| blk: {
                var new_fields = std.StringHashMap(Value).init(allocator);
                errdefer {
                    var it = new_fields.iterator();
                    while (it.next()) |entry| {
                        allocator.free(entry.key_ptr.*);
                        var val = entry.value_ptr.*;
                        val.deinit(allocator);
                    }
                    new_fields.deinit();
                }

                var it = inst.fields.iterator();
                while (it.next()) |entry| {
                    const key = try allocator.dupe(u8, entry.key_ptr.*);
                    errdefer allocator.free(key);
                    const value = try entry.value_ptr.clone(allocator);
                    errdefer {
                        var tmp = value;
                        tmp.deinit(allocator);
                    }
                    try new_fields.put(key, value);
                }

                break :blk Value{ .Instance = .{
                    .class = inst.class, // Shallow copy of class reference
                    .fields = new_fields,
                }};
            },
            .Trait => |trait| blk: {
                var new_methods = std.StringHashMap(Value).init(allocator);
                errdefer {
                    var it = new_methods.iterator();
                    while (it.next()) |entry| {
                        allocator.free(entry.key_ptr.*);
                        var val = entry.value_ptr.*;
                        val.deinit(allocator);
                    }
                    new_methods.deinit();
                }

                var it = trait.methods.iterator();
                while (it.next()) |entry| {
                    const key = try allocator.dupe(u8, entry.key_ptr.*);
                    errdefer allocator.free(key);
                    const value = try entry.value_ptr.clone(allocator);
                    errdefer {
                        var tmp = value;
                        tmp.deinit(allocator);
                    }
                    try new_methods.put(key, value);
                }

                break :blk Value{ .Trait = .{
                    .name = try allocator.dupe(u8, trait.name),
                    .methods = new_methods,
                }};
            },
            .Module => |mod| blk: {
                var new_exports = std.StringHashMap(Value).init(allocator);
                errdefer {
                    var it = new_exports.iterator();
                    while (it.next()) |entry| {
                        allocator.free(entry.key_ptr.*);
                        var val = entry.value_ptr.*;
                        val.deinit(allocator);
                    }
                    new_exports.deinit();
                }

                var it = mod.exports.iterator();
                while (it.next()) |entry| {
                    const key = try allocator.dupe(u8, entry.key_ptr.*);
                    errdefer allocator.free(key);
                    const value = try entry.value_ptr.clone(allocator);
                    errdefer {
                        var tmp = value;
                        tmp.deinit(allocator);
                    }
                    try new_exports.put(key, value);
                }

                break :blk Value{ .Module = .{
                    .name = try allocator.dupe(u8, mod.name),
                    .exports = new_exports,
                }};
            },
            .Ref => |ref| Value{ .Ref = .{
                .target = ref.target, // Shallow copy - references share targets
                .is_mutable = ref.is_mutable,
            }},
            .Atom => |atom| Value{ .Atom = .{
                .target = atom.target, // Shallow copy - atoms share targets
            }},
            .Channel => |chan| blk: {
                var new_buffer = try allocator.alloc(Value, chan.buffer.len);
                errdefer allocator.free(new_buffer);
                for (chan.buffer, 0..) |val, i| {
                    new_buffer[i] = try val.clone(allocator);
                }

                break :blk Value{ .Channel = .{
                    .buffer = new_buffer,
                    .capacity = chan.capacity,
                    .closed = chan.closed,
                }};
            },
            .Promise => |promise| blk: {
                const new_value = if (promise.value) |val| try val.clone(allocator) else null;
                const new_err = if (promise.err) |err| try err.clone(allocator) else null;

                break :blk Value{ .Promise = .{
                    .state = promise.state,
                    .value = new_value,
                    .err = new_err,
                }};
            },
            .ReturnAddress => |addr| Value{ .ReturnAddress = addr },
            .Function => |func| {
                debug.log("Cloning function: {s}", .{func.name});
                // Simple shallow copy - functions should be immutable
                debug.log("Value.clone: Using shallow copy for function: {s}", .{func.name});
                return Value{ .Function = func };
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
    // Root type - all values are Any
    Any,
    
    // Void and Nil types
    Void,    // No value (function returns)
    Nil,     // The nil value
    
    // Basic types
    Bool,    // true/false
    
    // Numeric types (abstract hierarchy)
    Number,  // Abstract numeric type
    Int,     // 48-bit integers (NaN-boxed)
    Float,   // 64-bit IEEE 754 floats
    
    // Text and symbol types
    Char,           // Unicode character
    String,         // UTF-8 string (was Str in design)
    Symbol,         // Interned symbol
    ComplexSymbol,  // Symbol with namespace/path
    
    // Collection types
    Map: *MapType,       // Key-value mapping
    Array: *ArrayType,   // Indexed sequence  
    Set: *SetType,       // Unique value collection
    
    // Gene container type
    Gene: *GeneType,     // Generic container (X ^prop val ...)
    
    // Function and callable types
    Fn: *FunctionType,   // Function type (renamed from FunctionT)
    
    // Object-oriented types
    Class: *ClassType,     // Class metaobject (renamed from ClassT)
    Trait: *TraitType,     // Trait metaobject
    Instance: *InstanceType, // Instance of a class (renamed from InstanceT)
    
    // Module and namespace types
    Module: *ModuleType,     // Module/namespace (renamed from Namespace)
    Namespace: *NamespaceType, // Keep for compatibility
    
    // Concurrency and reference types
    Ref: *RefType,       // Mutable reference
    Atom: *AtomType,     // Atomic reference
    Chan: *ChanType,     // Channel for concurrency
    Promise: *PromiseType, // Future value
    
    // Union and intersection types for gradual typing
    Union: *UnionType,        // Union of types (T | U)
    Intersection: *IntersectionType, // Intersection of types (T & U)
    
    // Generic type variables
    Generic: *GenericType,    // Generic type parameter
    
    // User-defined and extension types  
    UserDefined: *UserDefinedType, // User-defined types

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

    // New type definitions for expanded type system
    
    pub const SetType = struct {
        element_type: *Type,
    };
    
    pub const GeneType = struct {
        properties: std.StringHashMap(*Type),
        values: std.StringHashMap(Value),
    };
    
    pub const TraitType = struct {
        name: []const u8,
        methods: std.StringHashMap(*FunctionType),
        associated_types: std.StringHashMap(*Type),
        super_traits: []const *TraitType,
    };
    
    pub const ModuleType = struct {
        name: []const u8,
        public_types: std.StringHashMap(*Type),
        public_functions: std.StringHashMap(*FunctionType),
        public_values: std.StringHashMap(Value),
        private_types: std.StringHashMap(*Type),
        private_functions: std.StringHashMap(*FunctionType),
        private_values: std.StringHashMap(Value),
        imports: []const []const u8,
        exports: []const []const u8,
    };
    
    pub const RefType = struct {
        target_type: *Type,
        is_mutable: bool,
    };
    
    pub const AtomType = struct {
        target_type: *Type,
    };
    
    pub const ChanType = struct {
        element_type: *Type,
        buffer_size: ?usize, // None for unbuffered
    };
    
    pub const PromiseType = struct {
        result_type: *Type,
        error_type: ?*Type,
    };
    
    pub const UnionType = struct {
        types: []const *Type,
    };
    
    pub const IntersectionType = struct {
        types: []const *Type,
    };
    
    pub const GenericType = struct {
        name: []const u8,
        constraints: []const *Type, // Traits/types this generic must satisfy
        variance: Variance,
        
        pub const Variance = enum {
            Invariant,
            Covariant,
            Contravariant,
        };
    };
    
    pub const UserDefinedType = struct {
        name: []const u8,
        definition: TypeDefinition,
        
        pub const TypeDefinition = union(enum) {
            Enum: EnumDef,
            Struct: StructDef,
            Union: UnionDef,
            Alias: *Type,
            
            pub const EnumDef = struct {
                variants: []const Variant,
                
                pub const Variant = struct {
                    name: []const u8,
                    associated_type: ?*Type,
                };
            };
            
            pub const StructDef = struct {
                fields: []const Field,
                
                pub const Field = struct {
                    name: []const u8,
                    type: *Type,
                    is_public: bool,
                    default_value: ?Value,
                };
            };
            
            pub const UnionDef = struct {
                variants: []const *Type,
            };
        };
    };

    pub const Function = struct {
        type: *FunctionType,
        implementation: *const fn ([]Value) anyerror!Value,
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
            .Any => try writer.writeAll("Any"),
            .Void => try writer.writeAll("Void"),
            .Nil => try writer.writeAll("Nil"),
            .Bool => try writer.writeAll("Bool"),
            .Number => try writer.writeAll("Number"),
            .Int => try writer.writeAll("Int"),
            .Float => try writer.writeAll("Float"),
            .Char => try writer.writeAll("Char"),
            .String => try writer.writeAll("String"),
            .Symbol => try writer.writeAll("Symbol"),
            .ComplexSymbol => try writer.writeAll("ComplexSymbol"),
            .Array => |arr| try writer.print("Array[{}]", .{arr.element_type}),
            .Map => |map| try writer.print("Map[{}, {}]", .{ map.key_type, map.value_type }),
            .Set => |set| try writer.print("Set[{}]", .{set.element_type}),
            .Gene => |gene| {
                try writer.writeAll("Gene{");
                var it = gene.properties.iterator();
                var first = true;
                while (it.next()) |entry| {
                    if (!first) try writer.writeAll(", ");
                    try writer.print("{s}: {}", .{ entry.key_ptr.*, entry.value_ptr.* });
                    first = false;
                }
                try writer.writeAll("}");
            },
            .Fn => |func| {
                try writer.writeAll("fn(");
                for (func.params, 0..) |param, i| {
                    if (i > 0) try writer.writeAll(", ");
                    try writer.print("{s}: {}", .{ param.name, param.type });
                }
                try writer.print(") -> {}", .{func.return_type});
            },
            .Class => |cls| try writer.print("Class({s})", .{cls.name}),
            .Trait => |trait| try writer.print("Trait({s})", .{trait.name}),
            .Instance => |inst| try writer.print("Instance({s})", .{inst.class.name}),
            .Module => |mod| try writer.print("Module({s})", .{mod.name}),
            .Namespace => |ns| try writer.print("Namespace({s})", .{ns.name}),
            .Ref => |ref| try writer.print("Ref[{}]", .{ref.target_type}),
            .Atom => |atom| try writer.print("Atom[{}]", .{atom.target_type}),
            .Chan => |chan| {
                if (chan.buffer_size) |size| {
                    try writer.print("Chan[{}, {}]", .{ chan.element_type, size });
                } else {
                    try writer.print("Chan[{}]", .{chan.element_type});
                }
            },
            .Promise => |promise| {
                if (promise.error_type) |err_type| {
                    try writer.print("Promise[{}, {}]", .{ promise.result_type, err_type });
                } else {
                    try writer.print("Promise[{}]", .{promise.result_type});
                }
            },
            .Union => |union_type| {
                try writer.writeAll("(");
                for (union_type.types, 0..) |t, i| {
                    if (i > 0) try writer.writeAll(" | ");
                    try writer.print("{}", .{t});
                }
                try writer.writeAll(")");
            },
            .Intersection => |intersection| {
                try writer.writeAll("(");
                for (intersection.types, 0..) |t, i| {
                    if (i > 0) try writer.writeAll(" & ");
                    try writer.print("{}", .{t});
                }
                try writer.writeAll(")");
            },
            .Generic => |generic| {
                try writer.print("Generic({s})", .{generic.name});
                if (generic.constraints.len > 0) {
                    try writer.writeAll(": ");
                    for (generic.constraints, 0..) |constraint, i| {
                        if (i > 0) try writer.writeAll(" + ");
                        try writer.print("{}", .{constraint});
                    }
                }
            },
            .UserDefined => |user| try writer.print("UserDefined({s})", .{user.name}),
        }
    }
};
