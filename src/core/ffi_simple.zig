const std = @import("std");
const types = @import("types.zig");
const hir = @import("../ir/hir.zig");

/// Simplified FFI runtime without dynamic loading for initial testing
pub const FFIRuntime = struct {
    allocator: std.mem.Allocator,
    functions: std.StringHashMap(FFIFunction),
    
    pub const FFIFunction = struct {
        name: []const u8,
        params: []FFIParam,
        return_type: ?[]const u8,
        is_variadic: bool,
        
        pub const FFIParam = struct {
            name: []const u8,
            c_type: []const u8,
        };
    };
    
    pub fn init(allocator: std.mem.Allocator) FFIRuntime {
        return .{
            .allocator = allocator,
            .functions = std.StringHashMap(FFIFunction).init(allocator),
        };
    }
    
    pub fn deinit(self: *FFIRuntime) void {
        var iter = self.functions.iterator();
        while (iter.next()) |entry| {
            // We only need to free the params array, not the strings
            // The strings are owned by the FFI functions from the pipeline
            self.allocator.free(entry.value_ptr.params);
        }
        self.functions.deinit();
    }
    
    /// Register FFI functions from HIR
    pub fn registerFunctions(self: *FFIRuntime, ffi_functions: []const *hir.HIR.FFIFunction) !void {
        for (ffi_functions) |ffi_func| {
            // The FFI functions are already copied by the pipeline, so we can use them directly
            // We just need to convert the HIR params to our runtime format
            const params = try self.allocator.alloc(FFIFunction.FFIParam, ffi_func.params.len);
            for (ffi_func.params, 0..) |param, i| {
                params[i] = .{
                    .name = param.name,  // Already owned by the copied FFI function
                    .c_type = param.c_type,  // Already owned by the copied FFI function
                };
            }
            
            // Create function binding using the already-copied data
            const func = FFIFunction{
                .name = ffi_func.name,  // Already owned by the copied FFI function
                .params = params,
                .return_type = ffi_func.return_type,  // Already owned by the copied FFI function
                .is_variadic = ffi_func.is_variadic,
            };
            
            try self.functions.put(ffi_func.name, func);
        }
    }
    
    /// Call an FFI function (simplified version for testing)
    pub fn callFunction(self: *FFIRuntime, name: []const u8, args: []const types.Value) !types.Value {
        _ = self.functions.get(name) orelse return error.UnknownFunction;
        
        // For testing, just simulate some common functions
        if (std.mem.eql(u8, name, "printf")) {
            // Simple printf simulation
            if (args.len < 1) return error.ArgumentCountMismatch;
            
            switch (args[0]) {
                .String => |format| {
                    // Just print the format string for now
                    std.debug.print("{s}", .{format});
                    return types.Value{ .Int = @intCast(format.len) };
                },
                else => return error.TypeMismatch,
            }
        } else if (std.mem.eql(u8, name, "sin")) {
            // Simple sin simulation
            if (args.len != 1) return error.ArgumentCountMismatch;
            
            const arg = switch (args[0]) {
                .Float => |f| f,
                .Int => |i| @as(f64, @floatFromInt(i)),
                else => return error.TypeMismatch,
            };
            
            // Use Zig's built-in sin function
            const result = @sin(arg);
            return types.Value{ .Float = result };
        }
        
        // For other functions, return a placeholder
        std.debug.print("FFI: Simulated call to {s}\n", .{name});
        return types.Value{ .Int = 0 };
    }
};