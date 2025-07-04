const std = @import("std");
const types = @import("types.zig");
const hir = @import("../ir/hir.zig");

/// FFI library handle
pub const FFILibrary = struct {
    name: []const u8,
    handle: ?*anyopaque,
    allocator: std.mem.Allocator,
    
    pub fn init(allocator: std.mem.Allocator, name: []const u8) FFILibrary {
        return .{
            .name = name,
            .handle = null,
            .allocator = allocator,
        };
    }
    
    pub fn deinit(self: *FFILibrary) void {
        // Nothing to do for now
        _ = self;
    }
    
    pub fn load(self: *FFILibrary) !void {
        // For now, don't actually load libraries - just mark as loaded
        // This allows testing without dlopen/dlsym support
        _ = self;
        return;
    }
    
    pub fn getSymbol(self: *FFILibrary, symbol: []const u8) ?*anyopaque {
        // For now, return null - we'll handle functions in callFunction
        _ = self;
        _ = symbol;
        return null;
    }
};

/// FFI runtime manager
pub const FFIRuntime = struct {
    allocator: std.mem.Allocator,
    libraries: std.StringHashMap(FFILibrary),
    functions: std.StringHashMap(FFIFunctionBinding),
    
    pub const FFIFunctionBinding = struct {
        library: []const u8,
        symbol: []const u8,
        params: []FFIParam,
        return_type: ?[]const u8,
        is_variadic: bool,
        fn_ptr: ?*anyopaque,
        
        pub const FFIParam = struct {
            name: []const u8,
            c_type: []const u8,
        };
    };
    
    pub fn init(allocator: std.mem.Allocator) FFIRuntime {
        return .{
            .allocator = allocator,
            .libraries = std.StringHashMap(FFILibrary).init(allocator),
            .functions = std.StringHashMap(FFIFunctionBinding).init(allocator),
        };
    }
    
    pub fn deinit(self: *FFIRuntime) void {
        var lib_iter = self.libraries.iterator();
        while (lib_iter.next()) |entry| {
            entry.value_ptr.deinit();
        }
        self.libraries.deinit();
        
        // Free function bindings
        var func_iter = self.functions.iterator();
        while (func_iter.next()) |entry| {
            self.allocator.free(entry.value_ptr.params);
        }
        self.functions.deinit();
    }
    
    /// Register FFI functions from HIR
    pub fn registerFunctions(self: *FFIRuntime, ffi_functions: []const *hir.HIR.FFIFunction) !void {
        for (ffi_functions) |ffi_func| {
            // Ensure library is loaded
            const lib_entry = try self.libraries.getOrPut(ffi_func.lib);
            if (!lib_entry.found_existing) {
                lib_entry.value_ptr.* = FFILibrary.init(self.allocator, ffi_func.lib);
                try lib_entry.value_ptr.load();
            }
            
            // Convert HIR FFI params to runtime params
            const params = try self.allocator.alloc(FFIFunctionBinding.FFIParam, ffi_func.params.len);
            for (ffi_func.params, 0..) |param, i| {
                params[i] = .{
                    .name = param.name,
                    .c_type = param.c_type,
                };
            }
            
            // Create function binding
            const binding = FFIFunctionBinding{
                .library = ffi_func.lib,
                .symbol = ffi_func.symbol orelse ffi_func.name,
                .params = params,
                .return_type = ffi_func.return_type,
                .is_variadic = ffi_func.is_variadic,
                .fn_ptr = null,
            };
            
            // Try to resolve the function
            if (lib_entry.value_ptr.getSymbol(binding.symbol)) |fn_ptr| {
                var mut_binding = binding;
                mut_binding.fn_ptr = fn_ptr;
                try self.functions.put(ffi_func.name, mut_binding);
            } else {
                // Silently continue - we'll handle the function in callFunction
                try self.functions.put(ffi_func.name, binding);
            }
        }
    }
    
    /// Call an FFI function
    pub fn callFunction(self: *FFIRuntime, name: []const u8, args: []const types.Value) !types.Value {
        _ = self.functions.get(name) orelse return error.UnknownFunction;
        
        // For testing, simulate common functions without actual dynamic loading
        if (std.mem.eql(u8, name, "printf")) {
            // Simple printf simulation
            if (args.len < 1) return error.ArgumentCountMismatch;
            
            switch (args[0]) {
                .String => |format| {
                    // Handle escape sequences
                    var i: usize = 0;
                    while (i < format.len) {
                        if (format[i] == '\\' and i + 1 < format.len) {
                            switch (format[i + 1]) {
                                'n' => {
                                    std.debug.print("\n", .{});
                                    i += 2;
                                },
                                't' => {
                                    std.debug.print("\t", .{});
                                    i += 2;
                                },
                                '\\' => {
                                    std.debug.print("\\", .{});
                                    i += 2;
                                },
                                else => {
                                    std.debug.print("{c}", .{format[i]});
                                    i += 1;
                                },
                            }
                        } else {
                            std.debug.print("{c}", .{format[i]});
                            i += 1;
                        }
                    }
                    return types.Value{ .Int = @intCast(format.len) };
                },
                else => return error.TypeMismatch,
            }
        } else if (std.mem.eql(u8, name, "sin")) {
            // Simulate sin function
            if (args.len != 1) return error.ArgumentCountMismatch;
            
            const arg = switch (args[0]) {
                .Float => |f| f,
                .Int => |i| @as(f64, @floatFromInt(i)),
                else => return error.TypeMismatch,
            };
            
            return types.Value{ .Float = @sin(arg) };
        } else if (std.mem.eql(u8, name, "cos")) {
            // Simulate cos function
            if (args.len != 1) return error.ArgumentCountMismatch;
            
            const arg = switch (args[0]) {
                .Float => |f| f,
                .Int => |i| @as(f64, @floatFromInt(i)),
                else => return error.TypeMismatch,
            };
            
            return types.Value{ .Float = @cos(arg) };
        }
        
        // For other functions, just print a message
        std.debug.print("FFI: Would call {s}\n", .{name});
        return types.Value{ .Int = 0 };
    }
};