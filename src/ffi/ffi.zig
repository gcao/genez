const std = @import("std");
const types = @import("../core/types.zig");
const Value = types.Value;

/// FFI error types
pub const FFIError = error{
    LibraryNotFound,
    SymbolNotFound,
    InvalidArguments,
    TypeMismatch,
    CallFailed,
    UnsupportedType,
    NullPointer,
    InvalidCallingConvention,
};

/// Calling conventions supported by FFI
pub const CallingConvention = enum {
    C,        // Default C calling convention
    Stdcall,  // Windows stdcall
    Fastcall, // Fast calling convention
    Thiscall, // C++ this calling convention
};

/// Type mapping between Gene and C types
pub const TypeMap = struct {
    /// Convert Gene type string to C type info
    pub fn geneToCType(gene_type: []const u8) !CTypeInfo {
        if (std.mem.eql(u8, gene_type, "Int")) {
            return CTypeInfo{ .kind = .Int64, .size = 8, .alignment = 8 };
        } else if (std.mem.eql(u8, gene_type, "Float")) {
            return CTypeInfo{ .kind = .Double, .size = 8, .alignment = 8 };
        } else if (std.mem.eql(u8, gene_type, "Bool")) {
            return CTypeInfo{ .kind = .Bool, .size = 1, .alignment = 1 };
        } else if (std.mem.eql(u8, gene_type, "CStr")) {
            return CTypeInfo{ .kind = .Pointer, .size = @sizeOf(*anyopaque), .alignment = @alignOf(*anyopaque) };
        } else if (std.mem.eql(u8, gene_type, "CPtr")) {
            return CTypeInfo{ .kind = .Pointer, .size = @sizeOf(*anyopaque), .alignment = @alignOf(*anyopaque) };
        } else if (std.mem.eql(u8, gene_type, "Void")) {
            return CTypeInfo{ .kind = .Void, .size = 0, .alignment = 0 };
        } else {
            return FFIError.UnsupportedType;
        }
    }
    
    /// Convert Gene value to C-compatible representation
    pub fn geneToC(allocator: std.mem.Allocator, value: Value) !CValue {
        _ = allocator; // For future use with string conversions
        
        return switch (value) {
            .Int => |i| CValue{ .int64 = i },
            .Float => |f| CValue{ .double = f },
            .Bool => |b| CValue{ .bool = b },
            .String => |s| CValue{ .ptr = @constCast(@ptrCast(s.ptr)) },
            .CPtr => |p| CValue{ .ptr = p orelse null },
            .CFunction => |f| CValue{ .ptr = @ptrCast(@constCast(f)) },
            .CStruct => |s| CValue{ .ptr = s },
            .Nil => CValue{ .ptr = null },
            else => FFIError.UnsupportedType,
        };
    }
    
    /// Convert C value back to Gene value
    pub fn cToGene(allocator: std.mem.Allocator, c_value: CValue, type_info: CTypeInfo) !Value {
        _ = allocator; // For future use
        
        return switch (type_info.kind) {
            .Int64 => Value{ .Int = c_value.int64 },
            .Double => Value{ .Float = c_value.double },
            .Bool => Value{ .Bool = c_value.bool },
            .Pointer => {
                if (c_value.ptr) |ptr| {
                    return Value{ .CPtr = ptr };
                } else {
                    return Value{ .CPtr = null };
                }
            },
            .Void => Value{ .Nil = {} },
            else => FFIError.UnsupportedType,
        };
    }
};

/// C type information
pub const CTypeInfo = struct {
    kind: CTypeKind,
    size: usize,
    alignment: usize,
};

/// C type kinds
pub const CTypeKind = enum {
    Void,
    Bool,
    Int8,
    UInt8,
    Int16,
    UInt16,
    Int32,
    UInt32,
    Int64,
    UInt64,
    Float,
    Double,
    Pointer,
    Struct,
    Union,
    Array,
    Function,
};

/// C value representation
pub const CValue = union {
    bool: bool,
    int8: i8,
    uint8: u8,
    int16: i16,
    uint16: u16,
    int32: i32,
    uint32: u32,
    int64: i64,
    uint64: u64,
    float: f32,
    double: f64,
    ptr: ?*anyopaque,
};

/// External library handle
pub const Library = struct {
    handle: std.DynLib,
    name: []const u8,
    allocator: std.mem.Allocator,
    
    pub fn load(allocator: std.mem.Allocator, name: []const u8) !Library {
        const handle = try std.DynLib.open(name);
        return Library{
            .handle = handle,
            .name = try allocator.dupe(u8, name),
            .allocator = allocator,
        };
    }
    
    pub fn deinit(self: *Library) void {
        self.handle.close();
        self.allocator.free(self.name);
    }
    
    pub fn getSymbol(self: *Library, name: []const u8, comptime T: type) ?T {
        return self.handle.lookup(T, name);
    }
};

/// FFI function binding
pub const FFIFunction = struct {
    name: []const u8,
    library: *Library,
    symbol: *anyopaque,
    param_types: []CTypeInfo,
    return_type: CTypeInfo,
    calling_convention: CallingConvention,
    is_variadic: bool,
    
    pub fn call(self: *FFIFunction, allocator: std.mem.Allocator, args: []const Value) !Value {
        // Validate argument count
        if (!self.is_variadic and args.len != self.param_types.len) {
            return FFIError.InvalidArguments;
        }
        
        // Convert arguments to C values
        var c_args = try allocator.alloc(CValue, args.len);
        defer allocator.free(c_args);
        
        for (args, 0..) |arg, i| {
            c_args[i] = try TypeMap.geneToC(allocator, arg);
        }
        
        // TODO: Implement actual FFI call mechanism
        // This would require platform-specific assembly or libffi integration
        _ = self.symbol;
        
        // For now, just return nil - in a real implementation,
        // we would use the c_args to call the function
        for (c_args) |arg| {
            _ = arg;
        }
        
        return Value{ .Nil = {} };
    }
};

/// C struct definition
pub const CStructDef = struct {
    name: []const u8,
    fields: []CFieldDef,
    size: usize,
    alignment: ?usize,
    is_packed: bool,
    
    pub const CFieldDef = struct {
        name: []const u8,
        type_info: CTypeInfo,
        offset: usize,
        bit_size: ?u8, // For bit fields
    };
    
    pub fn getField(self: *CStructDef, name: []const u8) ?*CFieldDef {
        for (self.fields) |*field| {
            if (std.mem.eql(u8, field.name, name)) {
                return field;
            }
        }
        return null;
    }
    
    pub fn create(self: *CStructDef, allocator: std.mem.Allocator) !*anyopaque {
        // Use the requested alignment or default to 8
        const align_val = if (self.alignment) |a| @max(a, 8) else 8;
        const mem = if (align_val <= 8)
            try allocator.alignedAlloc(u8, 8, self.size)
        else if (align_val <= 16)
            try allocator.alignedAlloc(u8, 16, self.size)
        else if (align_val <= 32)
            try allocator.alignedAlloc(u8, 32, self.size)
        else
            return error.UnsupportedAlignment;
        @memset(mem, 0);
        return @ptrCast(mem);
    }
    
    pub fn destroy(self: *CStructDef, allocator: std.mem.Allocator, ptr: *anyopaque) void {
        const mem: [*]u8 = @ptrCast(@alignCast(ptr));
        allocator.free(mem[0..self.size]);
    }
};

/// FFI registry for managing libraries and bindings
pub const FFIRegistry = struct {
    allocator: std.mem.Allocator,
    libraries: std.StringHashMap(*Library),
    functions: std.StringHashMap(*FFIFunction),
    structs: std.StringHashMap(*CStructDef),
    
    pub fn init(allocator: std.mem.Allocator) FFIRegistry {
        return .{
            .allocator = allocator,
            .libraries = std.StringHashMap(*Library).init(allocator),
            .functions = std.StringHashMap(*FFIFunction).init(allocator),
            .structs = std.StringHashMap(*CStructDef).init(allocator),
        };
    }
    
    pub fn deinit(self: *FFIRegistry) void {
        // Clean up functions
        var func_iter = self.functions.iterator();
        while (func_iter.next()) |entry| {
            self.allocator.free(entry.value_ptr.*.name);
            self.allocator.free(entry.value_ptr.*.param_types);
            self.allocator.destroy(entry.value_ptr.*);
        }
        self.functions.deinit();
        
        // Clean up structs
        var struct_iter = self.structs.iterator();
        while (struct_iter.next()) |entry| {
            const struct_def = entry.value_ptr.*;
            self.allocator.free(struct_def.name);
            for (struct_def.fields) |*field| {
                self.allocator.free(field.name);
            }
            self.allocator.free(struct_def.fields);
            self.allocator.destroy(struct_def);
        }
        self.structs.deinit();
        
        // Clean up libraries
        var lib_iter = self.libraries.iterator();
        while (lib_iter.next()) |entry| {
            entry.value_ptr.*.deinit();
            self.allocator.destroy(entry.value_ptr.*);
        }
        self.libraries.deinit();
    }
    
    pub fn loadLibrary(self: *FFIRegistry, name: []const u8) !*Library {
        if (self.libraries.get(name)) |lib| {
            return lib;
        }
        
        const lib = try self.allocator.create(Library);
        errdefer self.allocator.destroy(lib);
        
        lib.* = try Library.load(self.allocator, name);
        try self.libraries.put(try self.allocator.dupe(u8, name), lib);
        
        return lib;
    }
    
    pub fn registerFunction(self: *FFIRegistry, func: *FFIFunction) !void {
        const name = try self.allocator.dupe(u8, func.name);
        try self.functions.put(name, func);
    }
    
    pub fn getFunction(self: *FFIRegistry, name: []const u8) ?*FFIFunction {
        return self.functions.get(name);
    }
    
    pub fn registerStruct(self: *FFIRegistry, struct_def: *CStructDef) !void {
        const name = try self.allocator.dupe(u8, struct_def.name);
        try self.structs.put(name, struct_def);
    }
    
    pub fn getStruct(self: *FFIRegistry, name: []const u8) ?*CStructDef {
        return self.structs.get(name);
    }
};