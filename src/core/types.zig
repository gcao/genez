const std = @import("std");
const bytecode = @import("../backend/bytecode.zig");
const debug = @import("debug.zig");

// Forward declarations
pub const ClassDefinition = struct {
    name: []const u8,
    parent: ?*ClassDefinition,
    fields: std.StringHashMap(FieldDefinition),
    methods: std.StringHashMap(*bytecode.Function),
    allocator: std.mem.Allocator,
    
    pub const FieldDefinition = struct {
        name: []const u8,
        type_name: ?[]const u8,
        default_value: ?Value,
        is_public: bool,
    };
    
    pub fn init(allocator: std.mem.Allocator, name: []const u8) ClassDefinition {
        return .{
            .name = name,
            .parent = null,
            .fields = std.StringHashMap(FieldDefinition).init(allocator),
            .methods = std.StringHashMap(*bytecode.Function).init(allocator),
            .allocator = allocator,
        };
    }
    
    pub fn deinit(self: *ClassDefinition) void {
        self.allocator.free(self.name);
        
        var field_iter = self.fields.iterator();
        while (field_iter.next()) |entry| {
            self.allocator.free(entry.value_ptr.name);
            if (entry.value_ptr.type_name) |type_name| {
                self.allocator.free(type_name);
            }
            if (entry.value_ptr.default_value) |*default| {
                var val = default;
                val.deinit(self.allocator);
            }
        }
        self.fields.deinit();
        
        // Clean up method functions
        var method_iter = self.methods.iterator();
        while (method_iter.next()) |entry| {
            // Free the method name key
            self.allocator.free(entry.key_ptr.*);
            // Deinit and destroy the function
            entry.value_ptr.*.deinit();
            self.allocator.destroy(entry.value_ptr.*);
        }
        self.methods.deinit();
    }
};

pub const ObjectInstance = struct {
    class: *ClassDefinition,
    fields: std.StringHashMap(Value),
    allocator: std.mem.Allocator,
    id: u32, // Unique object ID for reference semantics
    
    pub fn init(allocator: std.mem.Allocator, class: *ClassDefinition, id: u32) ObjectInstance {
        return .{
            .class = class,
            .fields = std.StringHashMap(Value).init(allocator),
            .allocator = allocator,
            .id = id,
        };
    }
    
    pub fn deinit(self: *ObjectInstance) void {
        var iter = self.fields.iterator();
        while (iter.next()) |entry| {
            // Free the field name (key)
            self.allocator.free(entry.key_ptr.*);
            // Free the value
            var value = entry.value_ptr.*;
            value.deinit(self.allocator);
        }
        self.fields.deinit();
    }
};

pub const BuiltinOperatorType = enum {
    // Arithmetic operators
    Add,
    Sub,
    Mul,
    Div,
    Mod,     // Modulo operator %
    Pow,     // Power operator **
    
    // Comparison operators
    Eq,      // Equal ==
    Ne,      // Not equal !=
    LessThan,        // Less than <
    GreaterThan,     // Greater than >
    LessEqual,       // Less or equal <=
    GreaterEqual,    // Greater or equal >=
    
    // Logical operators
    And,     // Logical and &&
    Or,      // Logical or ||
    Not,     // Logical not !
    
    // Bitwise operators
    BitAnd,  // Bitwise and &
    BitOr,   // Bitwise or |
    BitXor,  // Bitwise xor ^
    BitNot,  // Bitwise not ~
    Shl,     // Shift left <<
    Shr,     // Shift right >>
    
    // String operators
    Concat,  // String concatenation ++
    
    // Built-in functions
    Print,
    Println, // Print with newline
    Len,     // Length function
    Type,    // Type introspection
    
    // GC operations
    GCCollect,  // Force garbage collection
    GCDisable,  // Disable GC
    GCEnable,   // Enable GC  
    GCStats,    // Get GC statistics
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
    Class: *ClassDefinition,
    Object: u32, // Object ID instead of pointer for reference semantics
    Module: *@import("module_registry.zig").CompiledModule, // Module namespace
    
    // Standard library types
    StdlibFunction: @import("stdlib.zig").StdlibFunction,
    FileHandle: *@import("stdlib.zig").FileHandle,
    
    // Error handling
    Error: struct {
        type: []const u8,    // Error type/class name
        message: []const u8, // Error message
        stack_trace: ?[]const u8 = null, // Optional stack trace
    },
    
    // FFI types
    CPtr: ?*anyopaque, // C pointer (nullable)
    CFunction: *const fn() callconv(.C) void, // C function pointer
    CStruct: *anyopaque, // Opaque pointer to C struct
    CArray: struct {
        ptr: [*]u8, // Pointer to first element
        len: usize, // Number of elements
        element_size: usize, // Size of each element
    },

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
            .Function => {
                // Don't free function pointers here since they are shared references
                // Functions should be managed at a higher level (e.g., by the Module)
                // This prevents double-free errors when the same function is referenced
                // in multiple places (module functions, VM variables, instruction operands)
            },
            .Variable => {}, // No need to free the name as it's a string literal
            .BuiltinOperator => {},
            .Class => {
                // Classes are shared and should be managed by the module/runtime
                // Don't free them here to avoid double-free
            },
            .Object => {
                // Objects are managed by the VM's object pool, not freed here
            },
            .Module => {
                // Modules are managed by the module registry, not freed here
            },
            .StdlibFunction => {}, // Enum value, nothing to free
            .FileHandle => |handle| {
                // File handles are managed by the VM, not freed here
                _ = handle;
            },
            .Error => |*err| {
                allocator.free(err.type);
                allocator.free(err.message);
                if (err.stack_trace) |trace| {
                    allocator.free(trace);
                }
            },
            .CPtr => {}, // C pointers are not owned by Gene, don't free
            .CFunction => {}, // Function pointers are not owned by Gene
            .CStruct => {}, // C structs are not owned by Gene
            .CArray => {}, // C arrays are not owned by Gene
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
                // Simple shallow copy - functions should be immutable
                debug.log("Value.clone: Using shallow copy for function: {s}", .{func.name});
                return Value{ .Function = func };
            },
            .Variable => |var_val| Value{ .Variable = .{ .name = var_val.name } },
            .BuiltinOperator => |op| Value{ .BuiltinOperator = op },
            .Class => |class| Value{ .Class = class }, // Classes are immutable, shallow copy
            .Object => |id| Value{ .Object = id }, // Just copy the object ID
            .Module => |module| Value{ .Module = module }, // Modules are shared references
            .StdlibFunction => |func| Value{ .StdlibFunction = func }, // Enum value, simple copy
            .FileHandle => |handle| Value{ .FileHandle = handle }, // File handles are shared references
            .Error => |err| Value{ .Error = .{
                .type = try allocator.dupe(u8, err.type),
                .message = try allocator.dupe(u8, err.message),
                .stack_trace = if (err.stack_trace) |trace| try allocator.dupe(u8, trace) else null,
            }},
            .CPtr => |ptr| Value{ .CPtr = ptr }, // Just copy the pointer
            .CFunction => |func| Value{ .CFunction = func }, // Just copy the function pointer
            .CStruct => |ptr| Value{ .CStruct = ptr }, // Just copy the struct pointer
            .CArray => |arr| Value{ .CArray = arr }, // Copy the array descriptor
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
    
    // FFI types
    CPtr: *CPtrType, // C pointer type
    CFunction: *CFunctionType, // C function pointer type
    CStruct: *CStructType, // C struct type
    CUnion: *CUnionType, // C union type
    CArray: *CArrayType, // C array type with fixed size

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
    
    // FFI type definitions
    pub const CPtrType = struct {
        pointee_type: ?*Type, // null for void* / opaque pointers
        is_const: bool,
    };
    
    pub const CFunctionType = struct {
        params: []const CParam,
        return_type: ?*Type, // null for void
        calling_convention: CallingConvention,
        is_variadic: bool,
        
        pub const CParam = struct {
            name: ?[]const u8, // Optional parameter name
            type: *Type,
        };
        
        pub const CallingConvention = enum {
            C, // Default C calling convention
            Stdcall, // Windows stdcall
            Fastcall, // Fast calling convention
            Thiscall, // C++ this calling convention
        };
    };
    
    pub const CStructType = struct {
        name: []const u8,
        fields: []const CField,
        is_packed: bool, // Whether struct is packed
        alignment: ?usize, // Explicit alignment requirement
        
        pub const CField = struct {
            name: []const u8,
            type: *Type,
            offset: ?usize, // Explicit offset for layout control
            bit_size: ?u8, // For bit fields
        };
    };
    
    pub const CUnionType = struct {
        name: []const u8,
        fields: []const CField,
        alignment: ?usize,
        
        pub const CField = struct {
            name: []const u8,
            type: *Type,
        };
    };
    
    pub const CArrayType = struct {
        element_type: *Type,
        size: usize, // Fixed size for C arrays
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
            .CPtr => |ptr| {
                if (ptr.pointee_type) |pointee| {
                    try writer.print("CPtr[{}]", .{pointee});
                } else {
                    try writer.writeAll("CPtr[void]");
                }
            },
            .CFunction => |cfunc| {
                try writer.writeAll("CFn(");
                for (cfunc.params, 0..) |param, i| {
                    if (i > 0) try writer.writeAll(", ");
                    if (param.name) |name| {
                        try writer.print("{s}: {}", .{ name, param.type });
                    } else {
                        try writer.print("{}", .{param.type});
                    }
                }
                if (cfunc.is_variadic) {
                    if (cfunc.params.len > 0) try writer.writeAll(", ");
                    try writer.writeAll("...");
                }
                try writer.writeAll(") -> ");
                if (cfunc.return_type) |ret| {
                    try writer.print("{}", .{ret});
                } else {
                    try writer.writeAll("void");
                }
            },
            .CStruct => |cstruct| try writer.print("CStruct({s})", .{cstruct.name}),
            .CUnion => |cunion| try writer.print("CUnion({s})", .{cunion.name}),
            .CArray => |carr| try writer.print("CArray[{}, {}]", .{ carr.element_type, carr.size }),
        }
    }
};
