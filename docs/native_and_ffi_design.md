# Native Functions and FFI Design for Gene (Zig Implementation)

## Overview

The Gene language should support two distinct mechanisms for calling non-Gene code:

1. **Native Functions** - Functions written in Zig that are compiled into the Gene interpreter
2. **Foreign Function Interface (FFI)** - Calling external C libraries at runtime

## 1. Native Functions (Zig Functions)

### Purpose
- High-performance operations that benefit from Zig's speed
- Core functionality that needs direct access to VM internals
- Built-in functions like `print`, `sin`, `cos`, etc.
- Higher-order functions that can call back into Gene code

### Implementation Approach

```zig
// In src/core/native_functions.zig
pub const NativeFn = *const fn (vm: *VM, args: []Value) Value;

pub const native_functions = std.ComptimeStringMap(NativeFn, .{
    .{ "print", nativePrint },
    .{ "sin", nativeSin },
    .{ "cos", nativeCos },
    .{ "sqrt", nativeSqrt },
    // ... more functions
});

fn nativePrint(vm: *VM, args: []Value) Value {
    for (args) |arg| {
        vm.stdout.print("{}", .{arg}) catch {};
    }
    vm.stdout.print("\n", .{}) catch {};
    return .Nil;
}

fn nativeSin(vm: *VM, args: []Value) Value {
    if (args.len != 1) return .{ .Error = "sin expects 1 argument" };
    switch (args[0]) {
        .Float => |f| return .{ .Float = @sin(f) },
        .Int => |i| return .{ .Float = @sin(@as(f64, @floatFromInt(i))) },
        else => return .{ .Error = "sin expects a number" },
    }
}

// Higher-order function example - map
fn nativeMap(vm_ptr: *anyopaque, args: []const types.Value) types.NativeError!types.Value {
    const vm: *VM = @ptrCast(@alignCast(vm_ptr));
    if (args.len != 2) return error.ArgumentCountMismatch;
    
    const array = args[0];
    const func = args[1];
    
    if (array != .Array) return error.TypeMismatch;
    
    var result = try vm.allocator.alloc(types.Value, array.Array.len);
    
    // Call Gene function for each element
    for (array.Array, 0..) |item, i| {
        const func_args = [_]types.Value{item};
        result[i] = try vm.callGeneValue(func, &func_args);
    }
    
    return .{ .Array = result };
}
```

### Registration in VM

```zig
// In VM initialization
pub fn init(allocator: Allocator) VM {
    var vm = VM{
        .allocator = allocator,
        // ...
    };
    
    // Register all native functions
    var iter = native_functions.iterator();
    while (iter.next()) |entry| {
        vm.variables.put(entry.key_ptr.*, .{ .NativeFunction = entry.value_ptr.* }) catch {};
    }
    
    return vm;
}
```

### Usage in Gene

```gene
# Native functions are available globally
(print "Hello, World!")
(sin 1.5708)

# Higher-order native functions can call Gene functions
(fn double [x] (* x 2))
(map [1 2 3] double)  # Returns [2 4 6]
```

### Calling Gene Functions from Native Code

Native functions can call back into Gene code using the VM's `callGeneValue` and `callGeneFunction` methods:

```zig
// In the VM struct
pub fn callGeneFunction(self: *VM, func: *bytecode.Function, args: []const types.Value) !types.Value {
    // Save current VM state
    const saved_func = self.current_func;
    const saved_pc = self.pc;
    const saved_register_base = self.current_register_base;
    const saved_call_frames_len = self.call_frames.items.len;
    
    // Set up new call frame
    const new_register_base = self.next_free_register;
    const result_reg = new_register_base + @as(u16, @intCast(func.param_count + func.register_count));
    
    const frame = CallFrame.init(saved_func, new_register_base, saved_register_base, saved_pc, result_reg);
    try self.call_frames.append(frame);
    
    // Allocate registers and copy arguments
    const needed_regs = @as(u16, @intCast(func.param_count + func.register_count + 1));
    const allocated_base = try self.allocateRegisters(needed_regs);
    
    for (args, 0..) |arg, i| {
        const dest_reg = allocated_base + @as(u16, @intCast(i));
        while (dest_reg >= self.registers.items.len) {
            try self.registers.append(.{ .Nil = {} });
        }
        self.registers.items[dest_reg].deinit(self.allocator);
        self.registers.items[dest_reg] = try arg.clone(self.allocator);
    }
    
    // Switch to the function and execute
    self.current_func = func;
    self.pc = 0;
    self.current_register_base = allocated_base;
    
    // Run until function returns
    var result: types.Value = .{ .Nil = {} };
    while (self.pc < func.instructions.items.len and self.call_frames.items.len > saved_call_frames_len) {
        const instruction = &func.instructions.items[self.pc];
        const old_pc = self.pc;
        try self.executeInstruction(instruction.*);
        if (!self.function_called and self.pc == old_pc) {
            self.pc += 1;
        }
        self.function_called = false;
    }
    
    // Get result and restore VM state
    if (result_reg < self.registers.items.len) {
        result = try self.registers.items[result_reg].clone(self.allocator);
    }
    
    self.next_free_register = new_register_base;
    self.current_func = saved_func;
    self.pc = saved_pc;
    self.current_register_base = saved_register_base;
    
    return result;
}

pub fn callGeneValue(self: *VM, value: types.Value, args: []const types.Value) !types.Value {
    switch (value) {
        .Function => |func| return self.callGeneFunction(func, args),
        // TODO: Add support for closures when implemented
        else => return error.TypeMismatch,
    }
}
```

### Built-in Higher-Order Functions

The following higher-order native functions are provided:

1. **map** - Applies a function to each element of an array
   ```gene
   (map [1 2 3] (fn [x] (* x x)))  # Returns [1 4 9]
   ```

2. **filter** - Filters array elements based on a predicate
   ```gene
   (filter [1 2 3 4 5] (fn [x] (> x 2)))  # Returns [3 4 5]
   ```

3. **reduce** - Reduces an array to a single value
   ```gene
   (reduce [1 2 3 4] (fn [acc x] (+ acc x)) 0)  # Returns 10
   ```

## 2. Foreign Function Interface (FFI)

### Purpose
- Call functions from external C libraries
- Support for variadic functions
- Dynamic library loading

### Redesigned Architecture

The key insight is to separate FFI metadata from the compilation pipeline:

```zig
// In src/core/ffi_registry.zig
pub const FFIRegistry = struct {
    allocator: Allocator,
    functions: std.StringHashMap(FFIFunction),
    loaded_libs: std.StringHashMap(*anyopaque),
    
    pub fn init(allocator: Allocator) FFIRegistry {
        return .{
            .allocator = allocator,
            .functions = std.StringHashMap(FFIFunction).init(allocator),
            .loaded_libs = std.StringHashMap(*anyopaque).init(allocator),
        };
    }
    
    pub fn deinit(self: *FFIRegistry) void {
        // Clean up FFI functions
        var iter = self.functions.iterator();
        while (iter.next()) |entry| {
            entry.value_ptr.deinit(self.allocator);
        }
        self.functions.deinit();
        
        // Unload libraries
        var lib_iter = self.loaded_libs.iterator();
        while (lib_iter.next()) |entry| {
            // dlclose equivalent
        }
        self.loaded_libs.deinit();
    }
};

pub const FFIFunction = struct {
    name: []const u8,
    lib: []const u8,
    symbol: ?[]const u8,
    params: []FFIParam,
    return_type: ?[]const u8,
    is_variadic: bool,
    
    // Function pointer resolved at runtime
    ptr: ?*anyopaque = null,
    
    pub fn deinit(self: *FFIFunction, allocator: Allocator) void {
        allocator.free(self.name);
        allocator.free(self.lib);
        if (self.symbol) |s| allocator.free(s);
        for (self.params) |*param| {
            param.deinit(allocator);
        }
        allocator.free(self.params);
        if (self.return_type) |rt| allocator.free(rt);
    }
};
```

### Pipeline Integration

```zig
// In src/pipeline.zig
pub const CompiledResult = struct {
    main_func: bytecode.Function,
    created_functions: std.ArrayList(*bytecode.Function),
    parse_arena: *std.heap.ArenaAllocator,
    allocator: std.mem.Allocator,
    module_registry: ?*ModuleRegistry,
    // Don't store FFI functions here - they go directly to the registry
};

// In compileSourceWithFilename
pub fn compileSourceWithFilename(allocator: Allocator, source: []const u8, filename: ?[]const u8, options: CompilerOptions) !CompiledResult {
    // ... existing code ...
    
    // Instead of copying FFI functions, register them immediately
    if (options.ffi_registry) |registry| {
        for (conversion_result.ffi_functions.items) |ffi_func| {
            // Create a proper copy in the registry's allocator
            const copy = try registry.registerFunction(ffi_func);
        }
    }
    
    // ... rest of compilation ...
}
```

### Runtime FFI Resolution

```zig
// In VM execution
.Call => {
    const func_val = try self.getRegister(func_reg);
    
    switch (func_val) {
        .FFIFunction => |name| {
            // Resolve FFI function from registry
            if (self.ffi_registry) |registry| {
                if (registry.functions.get(name)) |ffi_func| {
                    // Resolve function pointer if not already done
                    if (ffi_func.ptr == null) {
                        ffi_func.ptr = try self.ffi_runtime.resolveSymbol(ffi_func);
                    }
                    
                    // Call the FFI function
                    const result = try self.ffi_runtime.call(ffi_func, args);
                    try self.setRegister(dst_reg, result);
                }
            }
        },
        .NativeFunction => |native_fn| {
            // Call native Zig function
            const result = native_fn(self, args);
            try self.setRegister(dst_reg, result);
        },
        // ... other cases
    }
}
```

## 3. Comparison with Current Implementation

### Current Issues:
1. FFI functions allocated in arena during compilation
2. Memory corruption when accessing them at runtime
3. Mixing of FFI and native function concepts

### New Design Benefits:
1. **Separation of Concerns**: Native functions vs FFI are clearly separated
2. **Memory Safety**: FFI registry owns its data with proper lifecycle
3. **Performance**: Native functions have direct access, no dynamic lookup
4. **Flexibility**: Can add native functions at compile time, FFI at runtime

## 4. Migration Plan

1. **Phase 1**: Implement native function system
   - Create `native_functions.zig`
   - Register built-in functions (print, math operations)
   - Update VM to handle NativeFunction value type

2. **Phase 2**: Redesign FFI system
   - Create FFI registry
   - Move FFI registration out of compilation pipeline
   - Update c-extern to register with FFI registry

3. **Phase 3**: Fix tests
   - Update FFI tests to use new system
   - Ensure native functions work correctly
   - Add tests for both native and FFI functions

## 5. Example Usage

```gene
# Native function (compiled into Gene)
(print "Hello from native Zig!")

# FFI function (external C library)
(c-extern printf ([format "char*"]) "int" "libc" :variadic true)
(printf "Hello from C FFI!\n")

# User can't tell the difference in usage, but implementation is very different
```

This design solves our memory issues while providing a clean separation between high-performance native functions and flexible FFI support.

## 6. Native Function Signatures

Native functions use a specific signature for WASM compatibility:

```zig
pub const NativeFn = *const fn (vm: *anyopaque, args: []const types.Value) types.NativeError!types.Value;

pub const NativeError = error{
    OutOfMemory,
    ArgumentCountMismatch,
    TypeMismatch,
    ParseError,
    MathDomainError,
};
```

Key points:
- The VM is passed as `*anyopaque` instead of `*VM` for WASM compatibility
- Arguments are passed as a slice of Values
- Errors are returned using Zig's error union type
- The function must return a Value (use `.Nil` for void functions)

## 7. Callback Pattern Examples

### Event Handling
```gene
(fn on_click [event]
  (print "Button clicked:" event))

# Native function could call this when button is clicked
(register_handler "button1" on_click)
```

### Custom Sorting
```gene
(fn compare_by_age [a b]
  (- a/age b/age))

# Native sort function could use this comparator
(sort people compare_by_age)
```

### Asynchronous Callbacks
```gene
(fn on_complete [result]
  (print "Operation completed:" result))

# Native async function could call this when done
(async_operation on_complete)
```

The ability for native functions to call Gene functions enables powerful patterns while maintaining the performance benefits of native code where needed.