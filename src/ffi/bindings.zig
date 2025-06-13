const std = @import("std");
const builtin = @import("builtin");
const types = @import("../core/types.zig");
const ffi = @import("ffi.zig");
const Value = types.Value;

/// Platform-specific FFI bindings
pub const PlatformBindings = struct {
    /// Create bindings for standard C library functions
    pub fn createStdlibBindings(allocator: std.mem.Allocator, registry: *ffi.FFIRegistry) !void {
        // Load standard C library
        const libc_name = switch (builtin.os.tag) {
            .linux => "libc.so.6",
            .macos => "libc.dylib",
            .windows => "msvcrt.dll",
            else => return error.UnsupportedPlatform,
        };
        
        const libc = try registry.loadLibrary(libc_name);
        
        // Register printf
        try registerPrintf(allocator, registry, libc);
        
        // Register malloc/free
        try registerMemoryFunctions(allocator, registry, libc);
        
        // Register math functions
        try registerMathFunctions(allocator, registry, libc);
    }
    
    fn registerPrintf(allocator: std.mem.Allocator, registry: *ffi.FFIRegistry, lib: *ffi.Library) !void {
        const printf_func = try allocator.create(ffi.FFIFunction);
        printf_func.* = .{
            .name = "printf",
            .library = lib,
            .symbol = lib.getSymbol("printf", *anyopaque) orelse return error.SymbolNotFound,
            .param_types = try allocator.alloc(ffi.CTypeInfo, 1),
            .return_type = .{ .kind = .Int32, .size = 4, .alignment = 4 },
            .calling_convention = .C,
            .is_variadic = true,
        };
        
        // First parameter is the format string
        printf_func.param_types[0] = .{ .kind = .Pointer, .size = @sizeOf(*anyopaque), .alignment = @alignOf(*anyopaque) };
        
        try registry.registerFunction(printf_func);
    }
    
    fn registerMemoryFunctions(allocator: std.mem.Allocator, registry: *ffi.FFIRegistry, lib: *ffi.Library) !void {
        // malloc
        const malloc_func = try allocator.create(ffi.FFIFunction);
        malloc_func.* = .{
            .name = "malloc",
            .library = lib,
            .symbol = lib.getSymbol("malloc", *anyopaque) orelse return error.SymbolNotFound,
            .param_types = try allocator.alloc(ffi.CTypeInfo, 1),
            .return_type = .{ .kind = .Pointer, .size = @sizeOf(*anyopaque), .alignment = @alignOf(*anyopaque) },
            .calling_convention = .C,
            .is_variadic = false,
        };
        malloc_func.param_types[0] = .{ .kind = .UInt64, .size = 8, .alignment = 8 }; // size_t
        try registry.registerFunction(malloc_func);
        
        // free
        const free_func = try allocator.create(ffi.FFIFunction);
        free_func.* = .{
            .name = "free",
            .library = lib,
            .symbol = lib.getSymbol("free", *anyopaque) orelse return error.SymbolNotFound,
            .param_types = try allocator.alloc(ffi.CTypeInfo, 1),
            .return_type = .{ .kind = .Void, .size = 0, .alignment = 0 },
            .calling_convention = .C,
            .is_variadic = false,
        };
        free_func.param_types[0] = .{ .kind = .Pointer, .size = @sizeOf(*anyopaque), .alignment = @alignOf(*anyopaque) };
        try registry.registerFunction(free_func);
    }
    
    fn registerMathFunctions(allocator: std.mem.Allocator, registry: *ffi.FFIRegistry, lib: *ffi.Library) !void {
        // sin
        const sin_func = try allocator.create(ffi.FFIFunction);
        sin_func.* = .{
            .name = "sin",
            .library = lib,
            .symbol = lib.getSymbol("sin", *anyopaque) orelse return error.SymbolNotFound,
            .param_types = try allocator.alloc(ffi.CTypeInfo, 1),
            .return_type = .{ .kind = .Double, .size = 8, .alignment = 8 },
            .calling_convention = .C,
            .is_variadic = false,
        };
        sin_func.param_types[0] = .{ .kind = .Double, .size = 8, .alignment = 8 };
        try registry.registerFunction(sin_func);
        
        // cos
        const cos_func = try allocator.create(ffi.FFIFunction);
        cos_func.* = .{
            .name = "cos",
            .library = lib,
            .symbol = lib.getSymbol("cos", *anyopaque) orelse return error.SymbolNotFound,
            .param_types = try allocator.alloc(ffi.CTypeInfo, 1),
            .return_type = .{ .kind = .Double, .size = 8, .alignment = 8 },
            .calling_convention = .C,
            .is_variadic = false,
        };
        cos_func.param_types[0] = .{ .kind = .Double, .size = 8, .alignment = 8 };
        try registry.registerFunction(cos_func);
        
        // sqrt
        const sqrt_func = try allocator.create(ffi.FFIFunction);
        sqrt_func.* = .{
            .name = "sqrt",
            .library = lib,
            .symbol = lib.getSymbol("sqrt", *anyopaque) orelse return error.SymbolNotFound,
            .param_types = try allocator.alloc(ffi.CTypeInfo, 1),
            .return_type = .{ .kind = .Double, .size = 8, .alignment = 8 },
            .calling_convention = .C,
            .is_variadic = false,
        };
        sqrt_func.param_types[0] = .{ .kind = .Double, .size = 8, .alignment = 8 };
        try registry.registerFunction(sqrt_func);
    }
};

/// String conversion utilities for FFI
pub const StringUtils = struct {
    /// Convert Gene string to null-terminated C string
    pub fn toCString(allocator: std.mem.Allocator, gene_str: []const u8) ![:0]const u8 {
        return try allocator.dupeZ(u8, gene_str);
    }
    
    /// Convert C string to Gene string
    pub fn fromCString(c_str: [*:0]const u8) []const u8 {
        return std.mem.span(c_str);
    }
    
    /// Free a C string allocated by toCString
    pub fn freeCString(allocator: std.mem.Allocator, c_str: [:0]const u8) void {
        allocator.free(c_str);
    }
};

/// Memory management utilities for FFI
pub const MemoryUtils = struct {
    /// Pin Gene memory for C access
    pub fn pinMemory(value: *Value) !PinnedMemory {
        // In a real implementation, this would prevent the GC from moving the memory
        // For now, we just return a handle
        return PinnedMemory{
            .value = value,
            .ptr = switch (value.*) {
                .String => |s| @constCast(@ptrCast(s.ptr)),
                .Array => |arr| @ptrCast(arr.ptr),
                .CArray => |carr| @ptrCast(carr.ptr),
                else => return error.InvalidValueType,
            },
        };
    }
    
    pub const PinnedMemory = struct {
        value: *Value,
        ptr: *anyopaque,
        
        pub fn unpin(self: *PinnedMemory) void {
            // In a real implementation, this would allow the GC to move the memory again
            _ = self;
        }
    };
    
    /// Create a direct buffer for zero-copy operations
    pub fn createDirectBuffer(allocator: std.mem.Allocator, size: usize, comptime alignment: u29) !DirectBuffer {
        const mem = try allocator.alignedAlloc(u8, alignment, size);
        return DirectBuffer{
            .allocator = allocator,
            .ptr = mem.ptr,
            .len = size,
        };
    }
    
    pub const DirectBuffer = struct {
        allocator: std.mem.Allocator,
        ptr: [*]u8,
        len: usize,
        
        pub fn deinit(self: *DirectBuffer) void {
            self.allocator.free(self.ptr[0..self.len]);
        }
        
        pub fn toValue(self: *DirectBuffer) Value {
            return Value{
                .CArray = .{
                    .ptr = self.ptr,
                    .len = self.len,
                    .element_size = 1,
                },
            };
        }
    };
};

/// Callback wrapper for Gene functions called from C
pub const CallbackWrapper = struct {
    gene_func: *Value, // Should be a Function value
    allocator: std.mem.Allocator,
    
    /// Create a C-compatible function pointer from a Gene function
    pub fn createCCallback(self: *CallbackWrapper, comptime RetType: type, comptime Args: type) !*const fn (Args) callconv(.C) RetType {
        // This is a simplified example - in reality, we'd need to generate
        // platform-specific trampoline code
        _ = self;
        return error.NotImplemented;
    }
};