const std = @import("std");
const Gene = @import("gene.zig").Gene;

/// Value representation in Gene v2
/// Uses NaN-boxing for efficient storage of immediate values
pub const Value = union(enum) {
    // Immediate values (fit in 64 bits via NaN-boxing)
    Nil,
    Bool: bool,
    Int: i48,  // 48-bit integers for NaN-boxing
    Float: f64,
    SmallString: SmallStr,  // Strings up to 6 bytes
    
    // Heap-allocated values (pointers)
    Gene: *Gene,
    String: *String,
    Symbol: *Symbol,
    Array: *Array,
    Map: *Map,
    Class: *Class,
    Object: *Object,
    Function: *Function,
    
    // VM internal values
    Register: u8,
    ReturnAddress: usize,

    /// Check if two values are equal
    pub fn equals(self: *const Value, other: *const Value) bool {
        if (@as(std.meta.Tag(Value), self.*) != @as(std.meta.Tag(Value), other.*)) return false;
        
        switch (self.*) {
            .Nil => return true,
            .Bool => |b| return b == other.Bool,
            .Int => |i| return i == other.Int,
            .Float => |f| return f == other.Float,
            .SmallString => |s| return s.equals(other.SmallString),
            .Gene => |g| return g == other.Gene,  // Pointer equality for now
            .String => |s| return std.mem.eql(u8, s.data, other.String.data),
            .Symbol => |s| return s == other.Symbol,  // Symbols are interned
            .Array => |a| return a == other.Array,  // Pointer equality for now
            .Map => |m| return m == other.Map,  // Pointer equality for now
            .Class => |c| return c == other.Class,
            .Object => |o| return o == other.Object,
            .Function => |f| return f == other.Function,
            .Register => |r| return r == other.Register,
            .ReturnAddress => |a| return a == other.ReturnAddress,
        }
    }

    /// Convert value to string representation
    pub fn toString(self: *const Value, allocator: std.mem.Allocator) std.mem.Allocator.Error![]u8 {
        switch (self.*) {
            .Nil => return allocator.dupe(u8, "nil"),
            .Bool => |b| return allocator.dupe(u8, if (b) "true" else "false"),
            .Int => |i| return std.fmt.allocPrint(allocator, "{}", .{i}),
            .Float => |f| return std.fmt.allocPrint(allocator, "{d}", .{f}),
            .SmallString => |s| return s.toString(allocator),
            .Gene => |g| return g.toString(allocator),
            .String => |s| return std.fmt.allocPrint(allocator, "\"{s}\"", .{s.data}),
            .Symbol => |s| return std.fmt.allocPrint(allocator, ":{s}", .{s.name}),
            .Array => |a| return a.toString(allocator),
            .Map => |m| return m.toString(allocator),
            .Class => |c| return std.fmt.allocPrint(allocator, "<Class:{s}>", .{c.name}),
            .Object => |o| return std.fmt.allocPrint(allocator, "<{s} instance>", .{o.class.name}),
            .Function => |f| return std.fmt.allocPrint(allocator, "<Function:{s}>", .{f.name}),
            .Register => |r| return std.fmt.allocPrint(allocator, "R{}", .{r}),
            .ReturnAddress => |a| return std.fmt.allocPrint(allocator, "RetAddr:{x}", .{a}),
        }
    }

    /// Get the class of this value
    pub fn getClass(self: *const Value) *Class {
        return switch (self.*) {
            .Nil => &NilClass,
            .Bool => &BoolClass,
            .Int => &IntClass,
            .Float => &FloatClass,
            .SmallString, .String => &StringClass,
            .Symbol => &SymbolClass,
            .Gene => &GeneClass,
            .Array => &ArrayClass,
            .Map => &MapClass,
            .Class => &ClassClass,
            .Object => |o| o.class,
            .Function => &FunctionClass,
            .Register, .ReturnAddress => &InternalClass,
        };
    }

    /// Check if value is truthy (for conditionals)
    pub fn isTruthy(self: *const Value) bool {
        return switch (self.*) {
            .Nil => false,
            .Bool => |b| b,
            .Int => |i| i != 0,
            .Float => |f| f != 0.0,
            .SmallString => |s| s.len > 0,
            .String => |s| s.len > 0,
            else => true,
        };
    }
};

/// Small string optimization - strings up to 6 bytes stored inline
pub const SmallStr = struct {
    data: [6]u8,
    len: u8,

    pub fn init(str: []const u8) SmallStr {
        var result = SmallStr{ .data = .{0} ** 6, .len = 0 };
        if (str.len <= 6) {
            std.mem.copyForwards(u8, &result.data, str);
            result.len = @intCast(str.len);
        }
        return result;
    }

    pub fn equals(self: SmallStr, other: SmallStr) bool {
        if (self.len != other.len) return false;
        return std.mem.eql(u8, self.data[0..self.len], other.data[0..other.len]);
    }

    pub fn toString(self: SmallStr, allocator: std.mem.Allocator) ![]u8 {
        return allocator.dupe(u8, self.data[0..self.len]);
    }
};

/// Object header for all heap-allocated objects
pub const ObjectHeader = packed struct {
    /// GC mark bits, generation, etc.
    gc_bits: u8,
    /// Quick type identification
    type_tag: TypeTag,
    /// Object size for GC
    size: u32,
};

pub const TypeTag = enum(u8) {
    Gene,
    String,
    Array,
    Map,
    Class,
    Object,
    Function,
};

/// String type for heap-allocated strings
pub const String = struct {
    header: ObjectHeader,
    data: []const u8,
    len: usize,

    pub fn init(allocator: std.mem.Allocator, data: []const u8) !*String {
        const str = try allocator.create(String);
        str.* = .{
            .header = .{
                .gc_bits = 0,
                .type_tag = .String,
                .size = @intCast(@sizeOf(String) + data.len),
            },
            .data = try allocator.dupe(u8, data),
            .len = data.len,
        };
        return str;
    }

    pub fn toString(self: *const String, allocator: std.mem.Allocator) ![]u8 {
        return std.fmt.allocPrint(allocator, "\"{s}\"", .{self.data});
    }
};

/// Symbol type - interned strings
pub const Symbol = struct {
    header: ObjectHeader,
    name: []const u8,
    hash: u64,

    pub fn init(allocator: std.mem.Allocator, name: []const u8) !*Symbol {
        const sym = try allocator.create(Symbol);
        sym.* = .{
            .header = .{
                .gc_bits = 0,
                .type_tag = .String,  // Reuse String tag
                .size = @intCast(@sizeOf(Symbol) + name.len),
            },
            .name = try allocator.dupe(u8, name),
            .hash = std.hash_map.hashString(name),
        };
        return sym;
    }

    pub fn deinit(self: *Symbol, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
        allocator.destroy(self);
    }
};

/// Array type
pub const Array = struct {
    header: ObjectHeader,
    items: []*Value,
    capacity: usize,

    pub fn init(allocator: std.mem.Allocator, capacity: usize) !*Array {
        const arr = try allocator.create(Array);
        arr.* = .{
            .header = .{
                .gc_bits = 0,
                .type_tag = .Array,
                .size = @sizeOf(Array),
            },
            .items = try allocator.alloc(*Value, capacity),
            .capacity = capacity,
        };
        return arr;
    }

    pub fn toString(self: *const Array, allocator: std.mem.Allocator) ![]u8 {
        var buffer = std.ArrayList(u8).init(allocator);
        errdefer buffer.deinit();
        
        try buffer.append('[');
        for (self.items, 0..) |item, i| {
            if (i > 0) try buffer.appendSlice(", ");
            const item_str = try item.toString(allocator);
            defer allocator.free(item_str);
            try buffer.appendSlice(item_str);
        }
        try buffer.append(']');
        
        return buffer.toOwnedSlice();
    }
};

/// Map type
pub const Map = struct {
    header: ObjectHeader,
    data: std.StringHashMap(*Value),

    pub fn init(allocator: std.mem.Allocator) !*Map {
        const map = try allocator.create(Map);
        map.* = .{
            .header = .{
                .gc_bits = 0,
                .type_tag = .Map,
                .size = @sizeOf(Map),
            },
            .data = std.StringHashMap(*Value).init(allocator),
        };
        return map;
    }

    pub fn toString(self: *const Map, allocator: std.mem.Allocator) ![]u8 {
        var buffer = std.ArrayList(u8).init(allocator);
        errdefer buffer.deinit();
        
        try buffer.append('{');
        var iter = self.data.iterator();
        var first = true;
        while (iter.next()) |entry| {
            if (!first) try buffer.appendSlice(", ");
            first = false;
            try buffer.append('^');
            try buffer.appendSlice(entry.key_ptr.*);
            try buffer.append(' ');
            const val_str = try entry.value_ptr.*.toString(allocator);
            defer allocator.free(val_str);
            try buffer.appendSlice(val_str);
        }
        try buffer.append('}');
        
        return buffer.toOwnedSlice();
    }
};

/// Class type
pub const Class = struct {
    header: ObjectHeader,
    name: []const u8,
    parent: ?*Class,
    methods: std.StringHashMap(*Function),
    fields: std.StringHashMap(FieldDef),

    pub const FieldDef = struct {
        name: []const u8,
        type: ?*Class,
        offset: u32,
    };
};

/// Object instance
pub const Object = struct {
    header: ObjectHeader,
    class: *Class,
    fields: []*Value,
};

/// Function type
pub const Function = struct {
    header: ObjectHeader,
    name: []const u8,
    arity: u8,
    code: []const u8,  // Bytecode
    constants: []*Value,
};

// Built-in class definitions (to be initialized at runtime)
var NilClass: Class = undefined;
var BoolClass: Class = undefined;
var IntClass: Class = undefined;
var FloatClass: Class = undefined;
var StringClass: Class = undefined;
var SymbolClass: Class = undefined;
var GeneClass: Class = undefined;
var ArrayClass: Class = undefined;
var MapClass: Class = undefined;
var ClassClass: Class = undefined;
var FunctionClass: Class = undefined;
var InternalClass: Class = undefined;