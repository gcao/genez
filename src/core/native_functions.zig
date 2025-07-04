const std = @import("std");
const types = @import("types.zig");
const VM = @import("../backend/vm.zig").VM;
const ComptimeStringMap = std.StaticStringMap;

/// Native function signature - functions implemented in Zig that can be called from Gene
pub const NativeFn = *const fn (vm: *anyopaque, args: []const types.Value) types.NativeError!types.Value;

/// Print function - prints all arguments separated by spaces
fn nativePrint(vm_ptr: *anyopaque, args: []const types.Value) types.NativeError!types.Value {
    const vm: *VM = @ptrCast(@alignCast(vm_ptr));
    for (args, 0..) |arg, i| {
        if (i > 0) vm.stdout.print(" ", .{}) catch {};
        
        switch (arg) {
            .Int => |val| vm.stdout.print("{d}", .{val}) catch {},
            .String => |str| vm.stdout.print("{s}", .{str}) catch {},
            .Symbol => |sym| vm.stdout.print("{s}", .{sym}) catch {},
            .Bool => |b| vm.stdout.print("{}", .{b}) catch {},
            .Float => |f| vm.stdout.print("{d}", .{f}) catch {},
            .Nil => vm.stdout.print("nil", .{}) catch {},
            .Array => |arr| {
                vm.stdout.print("[", .{}) catch {};
                for (arr, 0..) |item, idx| {
                    if (idx > 0) vm.stdout.print(" ", .{}) catch {};
                    // Recursively print array items
                    _ = nativePrint(vm, &[_]types.Value{item}) catch {};
                }
                vm.stdout.print("]", .{}) catch {};
            },
            else => vm.stdout.print("{any}", .{arg}) catch {},
        }
    }
    vm.stdout.print("\n", .{}) catch {};
    
    return .{ .Nil = {} };
}

/// Math functions
fn nativeSin(vm_ptr: *anyopaque, args: []const types.Value) types.NativeError!types.Value {
    _ = vm_ptr; // unused
    if (args.len != 1) {
        return error.ArgumentCountMismatch;
    }
    
    const val = args[0];
    switch (val) {
        .Float => |f| return .{ .Float = @sin(f) },
        .Int => |i| return .{ .Float = @sin(@as(f64, @floatFromInt(i))) },
        else => return error.TypeMismatch,
    }
}

fn nativeCos(vm_ptr: *anyopaque, args: []const types.Value) types.NativeError!types.Value {
    _ = vm_ptr; // unused
    if (args.len != 1) {
        return error.ArgumentCountMismatch;
    }
    
    const val = args[0];
    switch (val) {
        .Float => |f| return .{ .Float = @cos(f) },
        .Int => |i| return .{ .Float = @cos(@as(f64, @floatFromInt(i))) },
        else => return error.TypeMismatch,
    }
}

fn nativeSqrt(vm_ptr: *anyopaque, args: []const types.Value) types.NativeError!types.Value {
    _ = vm_ptr; // unused
    if (args.len != 1) {
        return error.ArgumentCountMismatch;
    }
    
    const val = args[0];
    switch (val) {
        .Float => |f| {
            if (f < 0) return error.MathDomainError;
            return .{ .Float = @sqrt(f) };
        },
        .Int => |i| {
            if (i < 0) return error.MathDomainError;
            return .{ .Float = @sqrt(@as(f64, @floatFromInt(i))) };
        },
        else => return error.TypeMismatch,
    }
}

/// Type conversion functions
fn nativeInt(vm_ptr: *anyopaque, args: []const types.Value) types.NativeError!types.Value {
    _ = vm_ptr; // unused
    if (args.len != 1) {
        return error.ArgumentCountMismatch;
    }
    
    const val = args[0];
    switch (val) {
        .Int => return val,
        .Float => |f| return .{ .Int = @as(i64, @intFromFloat(f)) },
        .String => |s| {
            const i = std.fmt.parseInt(i64, s, 10) catch {
                return error.ParseError;
            };
            return .{ .Int = i };
        },
        .Bool => |b| return .{ .Int = if (b) 1 else 0 },
        else => return error.TypeMismatch,
    }
}

fn nativeFloat(vm_ptr: *anyopaque, args: []const types.Value) types.NativeError!types.Value {
    _ = vm_ptr; // unused
    if (args.len != 1) {
        return error.ArgumentCountMismatch;
    }
    
    const val = args[0];
    switch (val) {
        .Float => return val,
        .Int => |i| return .{ .Float = @as(f64, @floatFromInt(i)) },
        .String => |s| {
            const f = std.fmt.parseFloat(f64, s) catch {
                return error.ParseError;
            };
            return .{ .Float = f };
        },
        else => return error.TypeMismatch,
    }
}

fn nativeStr(vm_ptr: *anyopaque, args: []const types.Value) types.NativeError!types.Value {
    const vm: *VM = @ptrCast(@alignCast(vm_ptr));
    if (args.len != 1) {
        return error.ArgumentCountMismatch;
    }
    
    const val = args[0];
    switch (val) {
        .String => return val,
        .Int => |i| {
            const s = try std.fmt.allocPrint(vm.allocator, "{d}", .{i});
            return .{ .String = s };
        },
        .Float => |f| {
            const s = try std.fmt.allocPrint(vm.allocator, "{d}", .{f});
            return .{ .String = s };
        },
        .Bool => |b| {
            const s = try vm.allocator.dupe(u8, if (b) "true" else "false");
            return .{ .String = s };
        },
        .Nil => {
            const s = try vm.allocator.dupe(u8, "nil");
            return .{ .String = s };
        },
        else => return error.TypeMismatch,
    }
}

/// Array operations
fn nativeLen(vm_ptr: *anyopaque, args: []const types.Value) types.NativeError!types.Value {
    _ = vm_ptr; // unused
    if (args.len != 1) {
        return error.ArgumentCountMismatch;
    }
    
    const val = args[0];
    switch (val) {
        .Array => |arr| return .{ .Int = @as(i64, @intCast(arr.len)) },
        .String => |s| return .{ .Int = @as(i64, @intCast(s.len)) },
        .Map => |map| return .{ .Int = @as(i64, @intCast(map.count())) },
        else => return error.TypeMismatch,
    }
}

fn nativeConcat(vm_ptr: *anyopaque, args: []const types.Value) types.NativeError!types.Value {
    const vm: *VM = @ptrCast(@alignCast(vm_ptr));
    if (args.len != 2) {
        return error.ArgumentCountMismatch;
    }
    
    const val1 = args[0];
    const val2 = args[1];
    
    // Array concatenation
    if (val1 == .Array and val2 == .Array) {
        const arr1 = val1.Array;
        const arr2 = val2.Array;
        
        var result = try vm.allocator.alloc(types.Value, arr1.len + arr2.len);
        
        // Copy first array
        for (arr1, 0..) |item, i| {
            result[i] = try item.clone(vm.allocator);
        }
        
        // Copy second array
        for (arr2, 0..) |item, i| {
            result[arr1.len + i] = try item.clone(vm.allocator);
        }
        
        return .{ .Array = result };
    }
    
    // String concatenation
    if (val1 == .String and val2 == .String) {
        const s1 = val1.String;
        const s2 = val2.String;
        
        const result = try vm.allocator.alloc(u8, s1.len + s2.len);
        @memcpy(result[0..s1.len], s1);
        @memcpy(result[s1.len..], s2);
        
        return .{ .String = result };
    }
    
    return error.TypeMismatch;
}

/// Map function - applies a function to each element of an array
fn nativeMap(vm_ptr: *anyopaque, args: []const types.Value) types.NativeError!types.Value {
    const vm: *VM = @ptrCast(@alignCast(vm_ptr));
    if (args.len != 2) {
        return error.ArgumentCountMismatch;
    }
    
    const array = args[0];
    const func = args[1];
    
    if (array != .Array) {
        return error.TypeMismatch;
    }
    
    // Create result array
    var result = try vm.allocator.alloc(types.Value, array.Array.len);
    errdefer vm.allocator.free(result);
    
    // Apply function to each element
    for (array.Array, 0..) |item, i| {
        const func_args = [_]types.Value{item};
        result[i] = vm.callGeneValue(func, &func_args) catch {
            // Clean up partially created array
            for (result[0..i]) |*val| {
                val.deinit(vm.allocator);
            }
            vm.allocator.free(result);
            return error.TypeMismatch;
        };
    }
    
    return .{ .Array = result };
}

/// Filter function - filters array elements based on predicate
fn nativeFilter(vm_ptr: *anyopaque, args: []const types.Value) types.NativeError!types.Value {
    const vm: *VM = @ptrCast(@alignCast(vm_ptr));
    if (args.len != 2) {
        return error.ArgumentCountMismatch;
    }
    
    const array = args[0];
    const predicate = args[1];
    
    if (array != .Array) {
        return error.TypeMismatch;
    }
    
    // First pass: count matching elements
    var count: usize = 0;
    for (array.Array) |item| {
        const func_args = [_]types.Value{item};
        var result = vm.callGeneValue(predicate, &func_args) catch {
            return error.TypeMismatch;
        };
        defer result.deinit(vm.allocator);
        
        if (result == .Bool and result.Bool) {
            count += 1;
        }
    }
    
    // Create result array
    var result = try vm.allocator.alloc(types.Value, count);
    errdefer vm.allocator.free(result);
    
    // Second pass: collect matching elements
    var idx: usize = 0;
    for (array.Array) |item| {
        const func_args = [_]types.Value{item};
        var pred_result = vm.callGeneValue(predicate, &func_args) catch {
            // Clean up partially created array
            for (result[0..idx]) |*val| {
                val.deinit(vm.allocator);
            }
            vm.allocator.free(result);
            return error.TypeMismatch;
        };
        defer pred_result.deinit(vm.allocator);
        
        if (pred_result == .Bool and pred_result.Bool) {
            result[idx] = try item.clone(vm.allocator);
            idx += 1;
        }
    }
    
    return .{ .Array = result };
}

/// Reduce function - reduces array to single value using accumulator
fn nativeReduce(vm_ptr: *anyopaque, args: []const types.Value) types.NativeError!types.Value {
    const vm: *VM = @ptrCast(@alignCast(vm_ptr));
    if (args.len != 3) {
        return error.ArgumentCountMismatch;
    }
    
    const array = args[0];
    const func = args[1];
    const initial = args[2];
    
    if (array != .Array) {
        return error.TypeMismatch;
    }
    
    var accumulator = try initial.clone(vm.allocator);
    errdefer accumulator.deinit(vm.allocator);
    
    for (array.Array) |item| {
        const func_args = [_]types.Value{ accumulator, item };
        const new_acc = vm.callGeneValue(func, &func_args) catch {
            accumulator.deinit(vm.allocator);
            return error.TypeMismatch;
        };
        accumulator.deinit(vm.allocator);
        accumulator = new_acc;
    }
    
    return accumulator;
}

/// Registry of all native functions
pub const native_functions = ComptimeStringMap(NativeFn).initComptime(.{
    // I/O
    .{ "print", nativePrint },
    
    // Math
    .{ "sin", nativeSin },
    .{ "cos", nativeCos },
    .{ "sqrt", nativeSqrt },
    
    // Type conversion
    .{ "int", nativeInt },
    .{ "float", nativeFloat },
    .{ "str", nativeStr },
    
    // Collections
    .{ "len", nativeLen },
    .{ "++", nativeConcat },
    
    // Higher-order functions
    .{ "map", nativeMap },
    .{ "filter", nativeFilter },
    .{ "reduce", nativeReduce },
});

/// Register all native functions with the VM
pub fn registerNativeFunctions(vm: *VM) !void {
    const debug = @import("debug.zig");
    debug.log("Registering native functions...", .{});
    
    // Register each function manually since StaticStringMap doesn't have iterator
    // Note: Don't override "print" as it's already a BuiltinOperator
    // try vm.variables.put("print", .{ .NativeFunction = nativePrint });
    try vm.variables.put("sin", .{ .NativeFunction = nativeSin });
    try vm.variables.put("cos", .{ .NativeFunction = nativeCos });
    try vm.variables.put("sqrt", .{ .NativeFunction = nativeSqrt });
    try vm.variables.put("int", .{ .NativeFunction = nativeInt });
    try vm.variables.put("float", .{ .NativeFunction = nativeFloat });
    try vm.variables.put("str", .{ .NativeFunction = nativeStr });
    try vm.variables.put("len", .{ .NativeFunction = nativeLen });
    try vm.variables.put("++", .{ .NativeFunction = nativeConcat });
    try vm.variables.put("map", .{ .NativeFunction = nativeMap });
    try vm.variables.put("filter", .{ .NativeFunction = nativeFilter });
    try vm.variables.put("reduce", .{ .NativeFunction = nativeReduce });
    
    debug.log("Registered {} native functions", .{11});
    debug.log("Total variables in VM: {}", .{vm.variables.count()});
}