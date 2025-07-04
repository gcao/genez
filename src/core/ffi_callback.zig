const std = @import("std");
const types = @import("types.zig");
const bytecode = @import("../backend/bytecode.zig");
const VM = @import("../backend/vm.zig").VM;

/// Callback wrapper that can be passed to C functions
pub const CallbackWrapper = struct {
    vm: *VM,
    function: types.Value,
    signature: ?[]const u8,
    
    pub fn init(vm: *VM, function: types.Value, signature: ?[]const u8) CallbackWrapper {
        return .{
            .vm = vm,
            .function = function,
            .signature = signature,
        };
    }
};

/// Create a C-compatible callback from a Gene function
pub fn createCallback(vm: *VM, function: types.Value, signature: ?[]const u8) !*CallbackWrapper {
    const wrapper = try vm.allocator.create(CallbackWrapper);
    wrapper.* = CallbackWrapper.init(vm, function, signature);
    return wrapper;
}

// ===== Trampoline Functions =====
// These are C-callable functions that convert arguments and call Gene functions

/// Comparator trampoline for qsort
export fn gene_qsort_comparator(wrapper_ptr: ?*anyopaque, a: *const anyopaque, b: *const anyopaque) callconv(.C) c_int {
    const wrapper = @as(*CallbackWrapper, @ptrCast(@alignCast(wrapper_ptr orelse return 0)));
    
    // Convert pointers to integers for comparison
    const val_a = @as(*const c_int, @ptrCast(@alignCast(a))).*;
    const val_b = @as(*const c_int, @ptrCast(@alignCast(b))).*;
    
    // Create Gene value handles
    const arg1 = gene_make_int(wrapper.vm, @intCast(val_a)) orelse return 0;
    defer gene_free_value(wrapper.vm, arg1);
    
    const arg2 = gene_make_int(wrapper.vm, @intCast(val_b)) orelse return 0;
    defer gene_free_value(wrapper.vm, arg2);
    
    var args: [2]GeneValueHandle = .{ arg1, arg2 };
    
    // Get function handle
    const func_handle = wrapper.vm.allocator.create(types.Value) catch return 0;
    func_handle.* = wrapper.function;
    defer wrapper.vm.allocator.destroy(func_handle);
    
    // Call the Gene function
    const result_handle = gene_call(wrapper.vm, @ptrCast(func_handle), &args, 2) orelse return 0;
    defer gene_free_value(wrapper.vm, result_handle);
    
    // Convert result to C int
    const result = @as(*types.Value, @ptrCast(@alignCast(result_handle))).*;
    return switch (result) {
        .Int => |n| @intCast(n),
        else => 0,
    };
}

/// Simple void callback
export fn gene_void_callback(wrapper_ptr: ?*anyopaque) callconv(.C) void {
    const wrapper = @as(*CallbackWrapper, @ptrCast(@alignCast(wrapper_ptr orelse return)));
    
    // Get function handle
    const func_handle = wrapper.vm.allocator.create(types.Value) catch return;
    func_handle.* = wrapper.function;
    defer wrapper.vm.allocator.destroy(func_handle);
    
    // Call with no arguments
    const empty_args: [*]GeneValueHandle = undefined;
    const result = gene_call(wrapper.vm, @ptrCast(func_handle), empty_args, 0);
    if (result) |handle| {
        gene_free_value(wrapper.vm, handle);
    }
}

/// Callback with int parameter and void return
export fn gene_void_int_callback(wrapper_ptr: ?*anyopaque, value: c_int) callconv(.C) void {
    const wrapper = @as(*CallbackWrapper, @ptrCast(@alignCast(wrapper_ptr orelse return)));
    
    // Create Gene value handle
    const arg = gene_make_int(wrapper.vm, @intCast(value)) orelse return;
    defer gene_free_value(wrapper.vm, arg);
    
    var args: [1]GeneValueHandle = .{arg};
    
    // Get function handle
    const func_handle = wrapper.vm.allocator.create(types.Value) catch return;
    func_handle.* = wrapper.function;
    defer wrapper.vm.allocator.destroy(func_handle);
    
    // Call the function
    const result = gene_call(wrapper.vm, @ptrCast(func_handle), &args, 1);
    if (result) |handle| {
        gene_free_value(wrapper.vm, handle);
    }
}

/// Callback with int parameter and int return
export fn gene_int_int_callback(wrapper_ptr: ?*anyopaque, value: c_int) callconv(.C) c_int {
    const wrapper = @as(*CallbackWrapper, @ptrCast(@alignCast(wrapper_ptr orelse return -1)));
    
    // Create Gene value handle
    const arg = gene_make_int(wrapper.vm, @intCast(value)) orelse return -1;
    defer gene_free_value(wrapper.vm, arg);
    
    var args: [1]GeneValueHandle = .{arg};
    
    // Get function handle
    const func_handle = wrapper.vm.allocator.create(types.Value) catch return -1;
    func_handle.* = wrapper.function;
    defer wrapper.vm.allocator.destroy(func_handle);
    
    // Call the function
    const result_handle = gene_call(wrapper.vm, @ptrCast(func_handle), &args, 1) orelse return -1;
    defer gene_free_value(wrapper.vm, result_handle);
    
    // Convert result to C int
    const result = @as(*types.Value, @ptrCast(@alignCast(result_handle))).*;
    return switch (result) {
        .Int => |n| @intCast(n),
        else => -1,
    };
}

/// Get the appropriate trampoline function for a signature
pub fn getTrampolineForSignature(signature: []const u8) ?*const fn() callconv(.C) void {
    if (std.mem.eql(u8, signature, "void()")) {
        return @ptrCast(&gene_void_callback);
    } else if (std.mem.eql(u8, signature, "void(int)")) {
        return @ptrCast(&gene_void_int_callback);
    } else if (std.mem.eql(u8, signature, "int(int)")) {
        return @ptrCast(&gene_int_int_callback);
    } else if (std.mem.eql(u8, signature, "int(const void*, const void*)")) {
        return @ptrCast(&gene_qsort_comparator);
    }
    return null;
}

// Import the VM module for GeneValueHandle type
const GeneValueHandle = @import("../backend/vm.zig").GeneValueHandle;

// Import the C API functions from VM
extern fn gene_make_int(vm_ptr: *VM, value: i64) callconv(.C) ?GeneValueHandle;
extern fn gene_make_float(vm_ptr: *VM, value: f64) callconv(.C) ?GeneValueHandle;
extern fn gene_make_string(vm_ptr: *VM, str: [*:0]const u8) callconv(.C) ?GeneValueHandle;
extern fn gene_make_bool(vm_ptr: *VM, value: bool) callconv(.C) ?GeneValueHandle;
extern fn gene_make_nil(vm_ptr: *VM) callconv(.C) ?GeneValueHandle;
extern fn gene_get_function(vm_ptr: *VM, name: [*:0]const u8) callconv(.C) ?GeneValueHandle;
extern fn gene_get_method(vm_ptr: *VM, object: GeneValueHandle, method_name: [*:0]const u8) callconv(.C) ?GeneValueHandle;
extern fn gene_is_callable(value: GeneValueHandle) callconv(.C) bool;
extern fn gene_call(vm_ptr: *VM, callable: GeneValueHandle, args: [*]GeneValueHandle, arg_count: usize) callconv(.C) ?GeneValueHandle;
extern fn gene_call_safe(vm_ptr: *VM, callable: GeneValueHandle, args: [*]GeneValueHandle, arg_count: usize, result: *GeneValueHandle) callconv(.C) c_int;
extern fn gene_free_value(vm_ptr: *VM, handle: GeneValueHandle) callconv(.C) void;