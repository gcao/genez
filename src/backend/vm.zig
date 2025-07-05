const std = @import("std");
const types = @import("../core/types.zig");
const debug = @import("../core/debug.zig");
const bytecode = @import("bytecode.zig");
const builtin_classes = @import("../core/builtin_classes.zig");
const gc = @import("../core/gc.zig");
const stdlib = @import("../core/stdlib.zig");
const ffi = @import("../core/ffi.zig");
const native_functions = @import("../core/native_functions.zig");

const ExceptionHandler = struct {
    catch_addr: usize,
    finally_addr: ?usize,
    frame_ptr: usize,
};

pub const VMError = error{
    StackOverflow,
    StackUnderflow,
    TypeMismatch,
    UndefinedVariable,
    UnsupportedInstruction,
    ExpectedLParen,
    ExpectedRParen,
    NoReturnAddress,
    ArgumentCountMismatch,
    UnknownFunction,
    UnsupportedFunction,
    NotImplemented,
    DivisionByZero,
    InvalidInstruction,
    FieldNotFound,
    MethodNotFound,
    ModuleNotFound,
    UserException,  // For user-thrown errors
    NoExceptionHandler,
    FunctionNotResolved, // For unresolved FFI functions
    LibraryNotFound, // For missing FFI libraries
    ParseError, // For parsing errors in native functions
    MathDomainError, // For math domain errors in native functions
} || std.mem.Allocator.Error || std.fs.File.WriteError || std.fs.File.GetSeekPosError || std.fs.File.ReadError;

pub const CallFrame = struct {
    caller_func: ?*const bytecode.Function, // Calling function to restore to
    pc: usize, // Program counter within the function
    register_base: u16, // Base register index for this frame
    prev_register_base: u16, // Previous frame's register base
    return_addr: usize, // Return address (instruction index to return to)
    return_reg: u16, // Register where the return value should be stored

    pub fn init(caller_func: ?*const bytecode.Function, register_base: u16, prev_register_base: u16, return_addr: usize, return_reg: u16) CallFrame {
        return CallFrame{
            .caller_func = caller_func,
            .pc = 0,
            .register_base = register_base,
            .prev_register_base = prev_register_base,
            .return_addr = return_addr,
            .return_reg = return_reg,
        };
    }
};

pub const VM = struct {
    allocator: std.mem.Allocator,
    stdout: std.fs.File.Writer,
    registers: std.ArrayList(types.Value), // Register file
    variables: std.StringArrayHashMap(types.Value), // Global variables
    call_frames: std.ArrayList(CallFrame),
    current_func: ?*const bytecode.Function, // Current function being executed
    pc: usize, // Program counter
    current_register_base: u16, // Current frame's register base
    next_free_register: u16, // Next available register for allocation
    allocated_functions: std.ArrayList(*bytecode.Function), // Track allocated function objects for cleanup
    function_called: bool, // Flag to indicate if a function was just called
    constructor_called: bool, // Flag to indicate if a constructor was just called
    object_pool: std.ArrayList(*types.ObjectInstance), // Object pool for reference semantics
    next_object_id: u32, // Next available object ID
    allocated_classes: std.ArrayList(*types.ClassDefinition), // Track allocated class objects for cleanup
    core_classes: ?builtin_classes.CoreClasses, // Core built-in classes
    garbage_collector: ?*gc.GC, // Garbage collector
    module_registry: ?*@import("../core/module_registry.zig").ModuleRegistry, // Module registry for imports
    namespace_stack: std.ArrayList(*@import("../core/module_registry.zig").Namespace), // Stack of active namespaces
    module_stack: std.ArrayList(*@import("../core/module_registry.zig").CompiledModule), // Stack of modules for namespaces
    allocated_modules: std.ArrayList(*@import("../core/module_registry.zig").CompiledModule), // Track allocated modules for cleanup
    current_exception: ?types.Value, // Current thrown exception
    exception_handlers: std.ArrayList(ExceptionHandler), // Stack of exception handlers
    ffi_runtime: ?*ffi.FFIRuntime, // FFI runtime for C function calls
    ffi_function_names: std.ArrayList([]const u8), // Track FFI function names for cleanup

    pub fn init(allocator: std.mem.Allocator, stdout: std.fs.File.Writer) VM {

        // Initialize variables with builtins
        var variables = std.StringArrayHashMap(types.Value).init(allocator);

        // Store the print function as a built-in operator
        variables.put("print", .{ .BuiltinOperator = .Print }) catch unreachable;
        variables.put("println", .{ .BuiltinOperator = .Println }) catch unreachable;

        // Store built-in operators
        // Arithmetic operators
        variables.put("+", .{ .BuiltinOperator = .Add }) catch unreachable;
        variables.put("-", .{ .BuiltinOperator = .Sub }) catch unreachable;
        variables.put("*", .{ .BuiltinOperator = .Mul }) catch unreachable;
        variables.put("/", .{ .BuiltinOperator = .Div }) catch unreachable;
        variables.put("%", .{ .BuiltinOperator = .Mod }) catch unreachable;
        variables.put("**", .{ .BuiltinOperator = .Pow }) catch unreachable;

        // Comparison operators
        variables.put("==", .{ .BuiltinOperator = .Eq }) catch unreachable;
        variables.put("!=", .{ .BuiltinOperator = .Ne }) catch unreachable;
        variables.put("<", .{ .BuiltinOperator = .LessThan }) catch unreachable;
        variables.put(">", .{ .BuiltinOperator = .GreaterThan }) catch unreachable;
        variables.put("<=", .{ .BuiltinOperator = .LessEqual }) catch unreachable;
        variables.put(">=", .{ .BuiltinOperator = .GreaterEqual }) catch unreachable;

        // Logical operators
        variables.put("&&", .{ .BuiltinOperator = .And }) catch unreachable;
        variables.put("||", .{ .BuiltinOperator = .Or }) catch unreachable;
        variables.put("!", .{ .BuiltinOperator = .Not }) catch unreachable;

        // Bitwise operators
        variables.put("&", .{ .BuiltinOperator = .BitAnd }) catch unreachable;
        variables.put("|", .{ .BuiltinOperator = .BitOr }) catch unreachable;
        variables.put("^", .{ .BuiltinOperator = .BitXor }) catch unreachable;
        variables.put("~", .{ .BuiltinOperator = .BitNot }) catch unreachable;
        variables.put("<<", .{ .BuiltinOperator = .Shl }) catch unreachable;
        variables.put(">>", .{ .BuiltinOperator = .Shr }) catch unreachable;

        // String operators
        variables.put("++", .{ .BuiltinOperator = .Concat }) catch unreachable;

        // Built-in functions
        variables.put("len", .{ .BuiltinOperator = .Len }) catch unreachable;
        variables.put("type", .{ .BuiltinOperator = .Type }) catch unreachable;

        // GC functions (use underscore instead of slash to avoid parser issues)
        variables.put("gc_collect", .{ .BuiltinOperator = .GCCollect }) catch unreachable;
        variables.put("gc_disable", .{ .BuiltinOperator = .GCDisable }) catch unreachable;
        variables.put("gc_enable", .{ .BuiltinOperator = .GCEnable }) catch unreachable;
        variables.put("gc_stats", .{ .BuiltinOperator = .GCStats }) catch unreachable;
        
        // Reference functions for mutable state
        variables.put("ref", .{ .BuiltinOperator = .MakeRef }) catch unreachable;
        variables.put("deref", .{ .BuiltinOperator = .RefGet }) catch unreachable;
        variables.put("set!", .{ .BuiltinOperator = .RefSet }) catch unreachable;
        
        // Register standard library functions
        // File I/O
        variables.put("file_open", .{ .StdlibFunction = .FileOpen }) catch unreachable;
        variables.put("file_close", .{ .StdlibFunction = .FileClose }) catch unreachable;
        variables.put("file_read_all", .{ .StdlibFunction = .FileReadAll }) catch unreachable;
        variables.put("file_write_all", .{ .StdlibFunction = .FileWriteAll }) catch unreachable;
        variables.put("file_exists", .{ .StdlibFunction = .FileExists }) catch unreachable;
        
        // System operations
        variables.put("exit", .{ .StdlibFunction = .Exit }) catch unreachable;
        
        // Math operations
        variables.put("math_sqrt", .{ .StdlibFunction = .MathSqrt }) catch unreachable;
        
        // String operations
        variables.put("string_concat", .{ .StdlibFunction = .StringConcat }) catch unreachable;
        
        // Error handling
        variables.put("throw", .{ .StdlibFunction = .Throw }) catch unreachable;
        variables.put("error_new", .{ .StdlibFunction = .ErrorNew }) catch unreachable;
        variables.put("error_type", .{ .StdlibFunction = .ErrorType }) catch unreachable;
        variables.put("error_message", .{ .StdlibFunction = .ErrorMessage }) catch unreachable;
        
        debug.log("Registered {} variables in VM", .{variables.count()});

        var vm = VM{
            .allocator = allocator,
            .stdout = stdout,
            .registers = std.ArrayList(types.Value).init(allocator),
            .variables = variables,
            .call_frames = std.ArrayList(CallFrame).init(allocator),
            .current_func = null,
            .pc = 0,
            .current_register_base = 0,
            .next_free_register = 0,
            .allocated_functions = std.ArrayList(*bytecode.Function).init(allocator),
            .function_called = false,
            .constructor_called = false,
            .object_pool = std.ArrayList(*types.ObjectInstance).init(allocator),
            .next_object_id = 0,
            .allocated_classes = std.ArrayList(*types.ClassDefinition).init(allocator),
            .core_classes = null,
            .garbage_collector = null,
            .module_registry = null,
            .namespace_stack = std.ArrayList(*@import("../core/module_registry.zig").Namespace).init(allocator),
            .module_stack = std.ArrayList(*@import("../core/module_registry.zig").CompiledModule).init(allocator),
            .allocated_modules = std.ArrayList(*@import("../core/module_registry.zig").CompiledModule).init(allocator),
            .current_exception = null,
            .exception_handlers = std.ArrayList(ExceptionHandler).init(allocator),
            .ffi_runtime = null,
            .ffi_function_names = std.ArrayList([]const u8).init(allocator),
        };

        // Initialize garbage collector
        vm.garbage_collector = gc.GC.init(allocator) catch null;

        // Initialize core classes
        vm.initCoreClasses() catch unreachable;

        // Initialize FFI runtime
        const ffi_runtime_ptr = allocator.create(ffi.FFIRuntime) catch null;
        if (ffi_runtime_ptr) |ptr| {
            ptr.* = ffi.FFIRuntime.init(allocator);
            vm.ffi_runtime = ptr;
        }
        
        // Register native functions
        native_functions.registerNativeFunctions(&vm) catch unreachable;

        return vm;
    }

    /// Clone a value - for now, keep using manual allocation
    /// TODO: Properly integrate GC by tracking allocated values
    pub fn cloneValue(self: *VM, value: types.Value) !types.Value {
        return try value.clone(self.allocator);
    }
    
    /// Register FFI functions from HIR
    pub fn registerFFIFunctions(self: *VM, ffi_functions: []const *@import("../ir/hir.zig").HIR.FFIFunction) !void {
        debug.log("Registering {} FFI functions", .{ffi_functions.len});
        if (self.ffi_runtime) |ffi_runtime_ptr| {
            try ffi_runtime_ptr.registerFunctions(ffi_functions);
            
            // Also register FFI functions as variables in the VM for lookup
            for (ffi_functions) |ffi_func| {
                debug.log("Registering FFI function: {s}", .{ffi_func.name});
                // Create copies of the name for key and value separately
                const key_copy = try self.allocator.dupe(u8, ffi_func.name);
                const value_copy = try self.allocator.dupe(u8, ffi_func.name);
                // Track the key for cleanup
                try self.ffi_function_names.append(key_copy);
                // Use FFIFunction value type to indicate this is an unresolved FFI function
                try self.variables.put(key_copy, .{ .FFIFunction = value_copy });
            }
        } else {
            debug.log("No FFI runtime available", .{});
        }
    }

    pub fn deinit(self: *VM) void {
        // Clean up FFI runtime
        if (self.ffi_runtime) |ffi_ptr| {
            ffi_ptr.deinit();
            self.allocator.destroy(ffi_ptr);
        }
        
        // Clean up garbage collector
        if (self.garbage_collector) |gc_ptr| {
            gc_ptr.deinit();
        }
        
        // Clean up current exception if any
        if (self.current_exception) |*exc| {
            exc.deinit(self.allocator);
        }
        
        // Clean up exception handlers
        self.exception_handlers.deinit();

        self.call_frames.deinit();

        for (self.allocated_functions.items) |func| {
            func.deinit();
            self.allocator.destroy(func);
        }
        self.allocated_functions.deinit();

        for (self.registers.items) |*value| {
            value.deinit(self.allocator);
        }
        self.registers.deinit();

        var it = self.variables.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.deinit(self.allocator);
            // Don't free the key here, as it's owned by the Variable value
            // self.allocator.free(entry.key_ptr.*);
        }
        self.variables.deinit();
        
        // Clean up FFI function names (these are the keys in variables map)
        for (self.ffi_function_names.items) |name| {
            self.allocator.free(name);
        }
        self.ffi_function_names.deinit();

        // Clean up object pool
        for (self.object_pool.items) |obj| {
            obj.deinit();
            self.allocator.destroy(obj);
        }
        self.object_pool.deinit();

        // Clean up allocated classes
        for (self.allocated_classes.items) |class| {
            class.deinit();
            self.allocator.destroy(class);
        }
        self.allocated_classes.deinit();

        // Clean up namespace stack (we don't own the namespaces, they're owned by modules)
        self.namespace_stack.deinit();

        // Clean up module stack (we don't own the modules either)
        self.module_stack.deinit();

        // Clean up allocated modules
        for (self.allocated_modules.items) |module| {
            module.deinit();
            self.allocator.destroy(module);
        }
        self.allocated_modules.deinit();

        // Clean up current function if it exists
        // Note: We don't deinit the function itself as it's owned by the caller
        // or has already been cleaned up as part of the stack or variables
        self.current_func = null;
    }

    pub fn setModuleRegistry(self: *VM, registry: *@import("../core/module_registry.zig").ModuleRegistry) void {
        self.module_registry = registry;
    }

    pub fn getObjectById(self: *VM, id: u32) ?*types.ObjectInstance {
        for (self.object_pool.items) |obj| {
            if (obj.id == id) return obj;
        }
        return null;
    }

    fn initCoreClasses(self: *VM) !void {
        // Initialize all core classes
        self.core_classes = try builtin_classes.CoreClasses.init(self.allocator);

        // Register all core classes in the global variables
        try self.core_classes.?.registerInVM(&self.variables);

        // Track all created classes for cleanup
        try self.allocated_classes.append(self.core_classes.?.class_class);
        try self.allocated_classes.append(self.core_classes.?.any_class);
        try self.allocated_classes.append(self.core_classes.?.number_class);
        try self.allocated_classes.append(self.core_classes.?.int_class);
        try self.allocated_classes.append(self.core_classes.?.float_class);
        try self.allocated_classes.append(self.core_classes.?.string_class);
        try self.allocated_classes.append(self.core_classes.?.bool_class);
        try self.allocated_classes.append(self.core_classes.?.nil_class);
        try self.allocated_classes.append(self.core_classes.?.symbol_class);
        try self.allocated_classes.append(self.core_classes.?.fn_class);
        try self.allocated_classes.append(self.core_classes.?.builtin_fn_class);
        try self.allocated_classes.append(self.core_classes.?.macro_class);
        try self.allocated_classes.append(self.core_classes.?.array_class);
        try self.allocated_classes.append(self.core_classes.?.map_class);
    }

    pub fn setVariable(self: *VM, name: []const u8, value: types.Value) !void {
        try self.variables.put(name, value);
    }
    
    /// Call a Gene function from external code (e.g., FFI callbacks)
    pub fn callGeneValueOld(self: *VM, func: types.Value, args: []const types.Value) !types.Value {
        switch (func) {
            .Function => |gene_func| {
                // Save current VM state
                const saved_func = self.current_func;
                const saved_pc = self.pc;
                const saved_base = self.current_register_base;
                
                // Allocate registers for the function call
                const new_base = self.next_free_register;
                const needed_regs = @as(u16, @intCast(gene_func.param_count + gene_func.register_count));
                _ = try self.allocateRegisters(needed_regs);
                
                // Copy arguments to registers
                for (args, 0..) |arg, i| {
                    if (i >= gene_func.param_count) break;
                    const reg = new_base + @as(u16, @intCast(i));
                    try self.setRegisterAbsolute(reg, arg);
                }
                
                // Create a call frame
                const frame = CallFrame.init(self.current_func, new_base, self.current_register_base, self.pc, new_base);
                try self.call_frames.append(frame);
                
                // Switch to the function
                self.current_func = gene_func;
                self.pc = 0;
                self.current_register_base = new_base;
                
                // Execute the function
                var result: types.Value = .{ .Nil = {} };
                while (self.pc < gene_func.instructions.items.len) {
                    const instruction = gene_func.instructions.items[self.pc];
                    if (instruction.op == .Return) {
                        // Get return value if any
                        if (instruction.src1) |ret_reg| {
                            result = try self.getRegister(ret_reg);
                        }
                        break;
                    }
                    try self.executeInstruction(instruction);
                    self.pc += 1;
                }
                
                // Restore VM state
                _ = self.call_frames.pop();
                self.current_func = saved_func;
                self.pc = saved_pc;
                self.current_register_base = saved_base;
                
                return result;
            },
            .BuiltinOperator, .StdlibFunction => {
                // For builtin functions, call them directly
                // This would need to be implemented based on the specific function
                return error.NotImplemented;
            },
            else => return error.TypeMismatch,
        }
    }
    
    fn setRegisterAbsolute(self: *VM, absolute_reg: u16, value: types.Value) !void {
        while (absolute_reg >= self.registers.items.len) {
            try self.registers.append(.{ .Nil = {} });
        }
        self.registers.items[absolute_reg].deinit(self.allocator);
        self.registers.items[absolute_reg] = value;
    }

    /// Get an iterator over all variables in the VM
    pub fn getVariables(self: *const VM) std.StringArrayHashMap(types.Value).Iterator {
        return self.variables.iterator();
    }

    /// Allocate a GC-managed value if GC is enabled
    fn allocGCValue(self: *VM, value: types.Value) !types.Value {
        if (self.garbage_collector) |gc_ptr| {
            // For now, only manage heap-allocated values
            switch (value) {
                .String, .Array, .Map, .Object => {
                    const managed_ptr = try gc_ptr.allocValue(value);
                    return managed_ptr.*;
                },
                else => return value,
            }
        }
        return value;
    }

    pub fn execute(self: *VM, func: *const bytecode.Function) VMError!void {
        debug.log("Executing function: {s}", .{func.name});
        self.current_func = func;
        self.pc = 0;

        // Allocate registers for the main function before execution
        const base = try self.allocateRegisters(func.register_count);
        self.current_register_base = base;

        // Register VM registers as GC roots
        if (self.garbage_collector) |gc_ptr| {
            // Add all registers as potential roots
            for (self.registers.items) |*reg| {
                try gc_ptr.addRoot(reg);
            }
        }

        // Print all instructions before execution
        debug.log("Instructions:", .{});
        for (func.instructions.items, 0..) |instr, i| {
            debug.log("  [{d}] {s}", .{ i, @tagName(instr.op) });
            if (instr.immediate) |immediate| {
                switch (immediate) {
                    .Int => |val| debug.log("    Immediate: Int {d}", .{val}),
                    .String => |val| debug.log("    Immediate: String \"{s}\"", .{val}),
                    else => debug.log("    Immediate: Other", .{}),
                }
            }
            if (instr.var_name) |var_name| {
                debug.log("    Variable: {s}", .{var_name});
            }
        }

        while (true) {
            // Use current_func if available, otherwise use the original func
            const executing_func = self.current_func orelse func;

            // Check if we're done with the current function
            debug.log("VM loop: pc={}, instruction_count={}", .{ self.pc, executing_func.instructions.items.len });
            if (self.pc >= executing_func.instructions.items.len) {
                debug.log("VM: PC {} >= instruction count {}, checking call frames", .{ self.pc, executing_func.instructions.items.len });
                // If we have no call frames, we're done with the program
                if (self.call_frames.items.len == 0) {
                    break;
                }

                // Otherwise, return to the caller
                const frame = self.call_frames.items[self.call_frames.items.len - 1];
                _ = self.call_frames.pop();
                self.current_func = frame.caller_func;
                self.pc = frame.return_addr;
                self.current_register_base = frame.prev_register_base;
                continue;
            }

            // Execute the current instruction
            const instruction = executing_func.instructions.items[self.pc];
            if (instruction.op == .Throw or instruction.op == .TryStart or instruction.op == .TryEnd or instruction.op == .ClearException or instruction.op == .Jump) {
                // std.debug.print("DEBUG VM: pc={}, executing {s}, handlers={}\n", .{self.pc, @tagName(instruction.op), self.exception_handlers.items.len});
            }
            self.function_called = false; // Reset the flag
            // constructor_called is reset in Return instruction handler
            try self.executeInstruction(instruction);

            // Only increment PC if a function wasn't called
            if (!self.function_called) {
                self.pc += 1;
            }
        }
    }

    // Register management functions
    fn getRegister(self: *VM, reg: bytecode.Reg) VMError!types.Value {
        const absolute_reg = self.current_register_base + reg;
        if (absolute_reg >= self.registers.items.len) {
            return error.StackUnderflow; // Reusing this error for register bounds
        }
        return try self.registers.items[absolute_reg].clone(self.allocator);
    }

    fn setRegister(self: *VM, reg: bytecode.Reg, value: types.Value) VMError!void {
        const absolute_reg = self.current_register_base + reg;

        // Extend register file if needed
        while (absolute_reg >= self.registers.items.len) {
            try self.registers.append(.{ .Nil = {} });
        }

        // Clean up old value
        self.registers.items[absolute_reg].deinit(self.allocator);
        self.registers.items[absolute_reg] = value;
    }

    fn allocateRegisters(self: *VM, count: u16) VMError!u16 {
        const base = self.next_free_register;

        debug.log("Allocating {} registers at base {} (current len {})", .{ count, base, self.registers.items.len });

        // Extend register file if needed
        while (self.next_free_register + count > self.registers.items.len) {
            try self.registers.append(.{ .Nil = {} });
        }

        self.next_free_register += count;
        debug.log("New register count: {}", .{self.registers.items.len});
        return base;
    }

    fn executeInstruction(self: *VM, instruction: bytecode.Instruction) VMError!void {
        switch (instruction.op) {
            .Add => {
                // Register-based addition: Add Rs1, Rs2 -> Rd
                const src1_reg = instruction.src1 orelse return error.UnsupportedInstruction;
                const src2_reg = instruction.src2 orelse return error.UnsupportedInstruction;
                const dst_reg = instruction.dst orelse return error.UnsupportedInstruction;

                debug.log("Add instruction: R{} = R{} + R{}", .{ dst_reg, src1_reg, src2_reg });

                var left = try self.getRegister(src1_reg);
                defer left.deinit(self.allocator);
                var right = try self.getRegister(src2_reg);
                defer right.deinit(self.allocator);

                // Handle addition based on types
                const result = switch (left) {
                    .Int => |left_val| switch (right) {
                        .Int => |right_val| types.Value{ .Int = left_val + right_val },
                        .Float => |right_val| types.Value{ .Float = @as(f64, @floatFromInt(left_val)) + right_val },
                        else => {
                            debug.log("TypeMismatch in Add: left={}, right={}", .{ left, right });
                            return error.TypeMismatch;
                        },
                    },
                    .Float => |left_val| switch (right) {
                        .Int => |right_val| types.Value{ .Float = left_val + @as(f64, @floatFromInt(right_val)) },
                        .Float => |right_val| types.Value{ .Float = left_val + right_val },
                        else => {
                            debug.log("TypeMismatch in Add: left={}, right={}", .{ left, right });
                            return error.TypeMismatch;
                        },
                    },
                    .String => |left_val| switch (right) {
                        .String => |right_val| blk: {
                            const concat_result = try std.fmt.allocPrint(self.allocator, "{s}{s}", .{ left_val, right_val });
                            break :blk types.Value{ .String = concat_result };
                        },
                        else => {
                            debug.log("TypeMismatch in Add: left={}, right={}", .{ left, right });
                            return error.TypeMismatch;
                        },
                    },
                    else => {
                        debug.log("TypeMismatch in Add: left={}, right={}", .{ left, right });
                        return error.TypeMismatch;
                    },
                };

                try self.setRegister(dst_reg, result);
            },
            .LoadConst => {
                // Register-based LoadConst: LoadConst Rd, #constant
                const dst_reg = instruction.dst orelse return error.UnsupportedInstruction;
                const value = instruction.immediate orelse return error.UnsupportedInstruction;

                debug.log("LoadConst: R{} = {any}", .{ dst_reg, value });

                // For functions, don't clone - just use the reference directly
                const reg_value = if (value == .Function)
                    value
                else
                    try self.cloneValue(value);

                try self.setRegister(dst_reg, reg_value);
            },
            .LoadVar => {
                // Register-based LoadVar: LoadVar Rd, var_name
                const dst_reg = instruction.dst orelse return error.UnsupportedInstruction;
                const name = instruction.var_name orelse return error.UnsupportedInstruction;

                debug.log("LoadVar: R{} = {s}", .{ dst_reg, name });

                // First check if it's a function parameter
                if (self.current_func) |current_func| {
                    debug.log("Checking parameters in current function: {s}", .{current_func.name});
                    debug.log("Current function has {} parameters", .{current_func.param_count});

                    // For simplicity, we'll handle the first parameter as 'n' for now
                    // In a real implementation, we'd have a mapping of parameter names
                    if ((std.mem.eql(u8, name, "n") or std.mem.eql(u8, name, "x")) and current_func.param_count > 0) {
                        // Parameters are stored in the first registers of the frame
                        const param_reg: u16 = 0; // First parameter in register 0 of current frame
                        const param_value = try self.getRegister(param_reg);
                        debug.log("Found parameter '{s}' in R{}: {any}", .{ name, param_reg, param_value });
                        try self.setRegister(dst_reg, param_value);
                        return;
                    }
                }

                // Check current namespace first if we're inside one
                if (self.namespace_stack.items.len > 0) {
                    const current_ns = self.namespace_stack.items[self.namespace_stack.items.len - 1];
                    if (current_ns.lookup(name)) |value| {
                        debug.log("Found variable in current namespace: {any}", .{value});
                        try self.setRegister(dst_reg, try value.clone(self.allocator));
                        return;
                    }
                }

                // Check for built-in operators and global variables
                if (self.variables.get(name)) |value| {
                    debug.log("Found variable in global scope: {any}", .{value});
                    try self.setRegister(dst_reg, try value.clone(self.allocator));
                } else {
                    // Check if this is a module member access (contains /)
                    if (std.mem.indexOf(u8, name, "/")) |slash_pos| {
                        const module_name = name[0..slash_pos];
                        const member_name = name[slash_pos + 1 ..];
                        debug.log("Checking for module member: {s}/{s}", .{ module_name, member_name });

                        // First check if it's a namespace variable
                        if (self.variables.get(module_name)) |module_val| {
                            if (module_val == .Module) {
                                if (module_val.Module.getMember(member_name)) |member| {
                                    debug.log("Found namespace member: {any}", .{member});
                                    try self.setRegister(dst_reg, try member.clone(self.allocator));
                                    return;
                                }
                            }
                        }

                        // Then check module registry
                        if (self.module_registry) |registry| {
                            if (registry.resolveMember(module_name, member_name)) |value| {
                                debug.log("Found module member: {any}", .{value});
                                try self.setRegister(dst_reg, try value.clone(self.allocator));
                                return;
                            }
                        }

                        debug.log("ERROR: Module member {s}/{s} not found", .{ module_name, member_name });
                        return error.UndefinedVariable;
                    } else {
                        debug.log("ERROR: Variable {s} not found", .{name});
                        debug.log("Available variables:", .{});
                        var it = self.variables.iterator();
                        while (it.next()) |entry| {
                            debug.log("  - {s}", .{entry.key_ptr.*});
                        }
                        return error.UndefinedVariable;
                    }
                }
            },
            .LoadModule => {
                // Load module from file: LoadModule Rd, module_path
                const dst_reg = instruction.dst orelse return error.InvalidInstruction;
                const module_path = instruction.var_name orelse return error.InvalidInstruction;

                debug.log("LoadModule: Loading module from {s}", .{module_path});

                // Extract module name from path
                const module_name = if (std.mem.lastIndexOfScalar(u8, module_path, '/')) |idx|
                    module_path[idx + 1 ..]
                else
                    module_path;

                // Remove .gene extension if present
                const name = if (std.mem.endsWith(u8, module_name, ".gene"))
                    module_name[0 .. module_name.len - 5]
                else
                    module_name;

                // Check if module is already loaded
                if (self.module_registry) |registry| {
                    debug.log("Looking for module in registry. Keys available:", .{});
                    var it = registry.modules.iterator();
                    while (it.next()) |entry| {
                        debug.log("  - {s}", .{entry.key_ptr.*});
                    }

                    // Try with the extracted name first
                    if (registry.getModule(name)) |existing_module| {
                        debug.log("Module {s} found in registry", .{name});
                        try self.setRegister(dst_reg, .{ .Module = existing_module });
                        return;
                    }
                    // Also try with the full path
                    if (registry.getModule(module_path)) |existing_module| {
                        debug.log("Module {s} found in registry", .{module_path});
                        try self.setRegister(dst_reg, .{ .Module = existing_module });
                        return;
                    }
                }

                // Module not found in registry - set to Nil instead of error
                debug.log("WARNING: Module {s} not found in registry, setting to Nil", .{module_path});
                try self.setRegister(dst_reg, .{ .Nil = {} });
            },
            .LoadParam => {
                // Register-based LoadParam: LoadParam Rd, #param_index
                const dst_reg = instruction.dst orelse return error.UnsupportedInstruction;
                const param_index = if (instruction.immediate) |imm| switch (imm) {
                    .Int => |idx| @as(u16, @intCast(idx)),
                    else => return error.TypeMismatch,
                } else return error.UnsupportedInstruction;

                debug.log("LoadParam: R{} = param[{}]", .{ dst_reg, param_index });

                // Parameters are stored in the first registers of the current frame
                // getRegister will add current_register_base, so we just pass the index
                debug.log("Fetching parameter {} from current frame (base {})", .{ param_index, self.current_register_base });
                const param_value = try self.getRegister(param_index);
                debug.log("Loaded parameter {}: {any}", .{ param_index, param_value });

                try self.setRegister(dst_reg, param_value);
            },
            .StoreVar => {
                // Register-based StoreVar: StoreVar var_name, Rs
                const src_reg = instruction.src1 orelse return error.UnsupportedInstruction;
                const name = instruction.var_name orelse return error.UnsupportedInstruction;

                debug.log("StoreVar: {s} = R{}", .{ name, src_reg });

                // Get the value from the source register
                var value = try self.getRegister(src_reg);
                defer value.deinit(self.allocator);

                // Check if we're inside a namespace context
                if (self.namespace_stack.items.len > 0) {
                    // Store in the current namespace
                    const current_ns = self.namespace_stack.items[self.namespace_stack.items.len - 1];
                    try current_ns.define(name, value);
                    debug.log("Stored variable {s} in namespace {s}", .{ name, current_ns.name });
                } else {
                    // Store in global scope
                    // Check if the variable already exists
                    if (self.variables.getIndex(name)) |idx| {
                        // Free the old value
                        var old_value_copy = self.variables.values()[idx];
                        old_value_copy.deinit(self.allocator);

                        // Update the value in place (clone since we'll defer deinit the original)
                        self.variables.values()[idx] = try value.clone(self.allocator);
                    } else {
                        // No existing key, add a new entry (clone since we'll defer deinit the original)
                        try self.variables.put(name, try value.clone(self.allocator));
                    }

                    debug.log("Stored variable {s} = {any}", .{ name, value });
                }
            },
            .StoreGlobal => {
                // Register-based StoreGlobal: StoreGlobal global_name, Rs
                const src_reg = instruction.src1 orelse return error.UnsupportedInstruction;
                const name = instruction.var_name orelse return error.UnsupportedInstruction;

                debug.log("StoreGlobal: {s} = R{}", .{ name, src_reg });

                // Get the value from the source register
                var value = try self.getRegister(src_reg);
                defer value.deinit(self.allocator);

                // Store the value in the variables map (same as StoreVar)
                if (self.variables.getIndex(name)) |idx| {
                    // Free the old value
                    var old_value_copy = self.variables.values()[idx];
                    old_value_copy.deinit(self.allocator);

                    // Update the value in place (clone since we'll defer deinit the original)
                    self.variables.values()[idx] = try value.clone(self.allocator);
                } else {
                    // No existing key, add a new entry (clone since we'll defer deinit the original)
                    try self.variables.put(name, try value.clone(self.allocator));
                }

                debug.log("Stored global variable {s} = {any}", .{ name, value });
            },
            .Move => {
                // Register-based Move: Move Rd, Rs
                const dst_reg = instruction.dst orelse return error.UnsupportedInstruction;
                const src_reg = instruction.src1 orelse return error.UnsupportedInstruction;

                debug.log("Move: R{} = R{}", .{ dst_reg, src_reg });

                const value = try self.getRegister(src_reg);
                try self.setRegister(dst_reg, value);
            },
            .Sub => {
                // Register-based subtraction: Sub Rs1, Rs2 -> Rd
                const src1_reg = instruction.src1 orelse return error.UnsupportedInstruction;
                const src2_reg = instruction.src2 orelse return error.UnsupportedInstruction;
                const dst_reg = instruction.dst orelse return error.UnsupportedInstruction;

                debug.log("Sub instruction: R{} = R{} - R{}", .{ dst_reg, src1_reg, src2_reg });

                var left = try self.getRegister(src1_reg);
                defer left.deinit(self.allocator);
                var right = try self.getRegister(src2_reg);
                defer right.deinit(self.allocator);

                // Handle subtraction based on types
                const result = switch (left) {
                    .Int => |left_val| switch (right) {
                        .Int => |right_val| types.Value{ .Int = left_val - right_val },
                        .Float => |right_val| types.Value{ .Float = @as(f64, @floatFromInt(left_val)) - right_val },
                        else => {
                            debug.log("TypeMismatch in Sub: left={}, right={}", .{ left, right });
                            return error.TypeMismatch;
                        },
                    },
                    .Float => |left_val| switch (right) {
                        .Int => |right_val| types.Value{ .Float = left_val - @as(f64, @floatFromInt(right_val)) },
                        .Float => |right_val| types.Value{ .Float = left_val - right_val },
                        else => {
                            debug.log("TypeMismatch in Sub: left={}, right={}", .{ left, right });
                            return error.TypeMismatch;
                        },
                    },
                    else => {
                        debug.log("TypeMismatch in Sub: left={}, right={}", .{ left, right });
                        return error.TypeMismatch;
                    },
                };

                try self.setRegister(dst_reg, result);
            },
            .Mul => {
                // Register-based multiplication: Mul Rs1, Rs2 -> Rd
                const src1_reg = instruction.src1 orelse return error.UnsupportedInstruction;
                const src2_reg = instruction.src2 orelse return error.UnsupportedInstruction;
                const dst_reg = instruction.dst orelse return error.UnsupportedInstruction;

                debug.log("Mul instruction: R{} = R{} * R{}", .{ dst_reg, src1_reg, src2_reg });

                var left = try self.getRegister(src1_reg);
                defer left.deinit(self.allocator);
                var right = try self.getRegister(src2_reg);
                defer right.deinit(self.allocator);

                // Handle multiplication based on types
                const result = switch (left) {
                    .Int => |left_val| switch (right) {
                        .Int => |right_val| types.Value{ .Int = left_val * right_val },
                        .Float => |right_val| types.Value{ .Float = @as(f64, @floatFromInt(left_val)) * right_val },
                        else => {
                            debug.log("TypeMismatch in Mul: left={}, right={}", .{ left, right });
                            return error.TypeMismatch;
                        },
                    },
                    .Float => |left_val| switch (right) {
                        .Int => |right_val| types.Value{ .Float = left_val * @as(f64, @floatFromInt(right_val)) },
                        .Float => |right_val| types.Value{ .Float = left_val * right_val },
                        else => {
                            debug.log("TypeMismatch in Mul: left={}, right={}", .{ left, right });
                            return error.TypeMismatch;
                        },
                    },
                    else => {
                        debug.log("TypeMismatch in Mul: left={}, right={}", .{ left, right });
                        return error.TypeMismatch;
                    },
                };

                try self.setRegister(dst_reg, result);
            },
            .Div => {
                // Register-based division: Div Rs1, Rs2 -> Rd
                const src1_reg = instruction.src1 orelse return error.UnsupportedInstruction;
                const src2_reg = instruction.src2 orelse return error.UnsupportedInstruction;
                const dst_reg = instruction.dst orelse return error.UnsupportedInstruction;

                debug.log("Div instruction: R{} = R{} / R{}", .{ dst_reg, src1_reg, src2_reg });

                var left = try self.getRegister(src1_reg);
                defer left.deinit(self.allocator);
                var right = try self.getRegister(src2_reg);
                defer right.deinit(self.allocator);

                // Handle division based on types
                const result = switch (left) {
                    .Int => |left_val| switch (right) {
                        .Int => |right_val| blk: {
                            if (right_val == 0) {
                                debug.log("Division by zero in Div instruction", .{});
                                return error.DivisionByZero;
                            }
                            break :blk types.Value{ .Int = @divTrunc(left_val, right_val) };
                        },
                        .Float => |right_val| blk: {
                            if (right_val == 0.0) {
                                debug.log("Division by zero in Div instruction", .{});
                                return error.DivisionByZero;
                            }
                            break :blk types.Value{ .Float = @as(f64, @floatFromInt(left_val)) / right_val };
                        },
                        else => {
                            debug.log("TypeMismatch in Div: left={}, right={}", .{ left, right });
                            return error.TypeMismatch;
                        },
                    },
                    .Float => |left_val| switch (right) {
                        .Int => |right_val| blk: {
                            if (right_val == 0) {
                                debug.log("Division by zero in Div instruction", .{});
                                return error.DivisionByZero;
                            }
                            break :blk types.Value{ .Float = left_val / @as(f64, @floatFromInt(right_val)) };
                        },
                        .Float => |right_val| blk: {
                            if (right_val == 0.0) {
                                debug.log("Division by zero in Div instruction", .{});
                                return error.DivisionByZero;
                            }
                            break :blk types.Value{ .Float = left_val / right_val };
                        },
                        else => {
                            debug.log("TypeMismatch in Div: left={}, right={}", .{ left, right });
                            return error.TypeMismatch;
                        },
                    },
                    else => {
                        debug.log("TypeMismatch in Div: left={}, right={}", .{ left, right });
                        return error.TypeMismatch;
                    },
                };

                try self.setRegister(dst_reg, result);
            },
            .Lt => {
                // Register-based less than: Lt Rs1, Rs2 -> Rd
                const src1_reg = instruction.src1 orelse return error.UnsupportedInstruction;
                const src2_reg = instruction.src2 orelse return error.UnsupportedInstruction;
                const dst_reg = instruction.dst orelse return error.UnsupportedInstruction;

                debug.log("Lt instruction: R{} = R{} < R{}", .{ dst_reg, src1_reg, src2_reg });

                var left = try self.getRegister(src1_reg);
                defer left.deinit(self.allocator);
                var right = try self.getRegister(src2_reg);
                defer right.deinit(self.allocator);

                // Handle comparison based on types
                const result = switch (left) {
                    .Int => |left_val| switch (right) {
                        .Int => |right_val| left_val < right_val,
                        .Float => |right_val| @as(f64, @floatFromInt(left_val)) < right_val,
                        else => {
                            debug.log("TypeMismatch in Lt: left={}, right={}", .{ left, right });
                            return error.TypeMismatch;
                        },
                    },
                    .Float => |left_val| switch (right) {
                        .Int => |right_val| left_val < @as(f64, @floatFromInt(right_val)),
                        .Float => |right_val| left_val < right_val,
                        else => {
                            debug.log("TypeMismatch in Lt: left={}, right={}", .{ left, right });
                            return error.TypeMismatch;
                        },
                    },
                    else => {
                        debug.log("TypeMismatch in Lt: left={}, right={}", .{ left, right });
                        return error.TypeMismatch;
                    },
                };

                try self.setRegister(dst_reg, .{ .Bool = result });
            },
            .Gt => {
                // Register-based greater than: Gt Rs1, Rs2 -> Rd
                const src1_reg = instruction.src1 orelse return error.UnsupportedInstruction;
                const src2_reg = instruction.src2 orelse return error.UnsupportedInstruction;
                const dst_reg = instruction.dst orelse return error.UnsupportedInstruction;

                debug.log("Gt instruction: R{} = R{} > R{}", .{ dst_reg, src1_reg, src2_reg });

                var left = try self.getRegister(src1_reg);
                defer left.deinit(self.allocator);
                var right = try self.getRegister(src2_reg);
                defer right.deinit(self.allocator);

                // Handle comparison based on types
                const result = switch (left) {
                    .Int => |left_val| switch (right) {
                        .Int => |right_val| left_val > right_val,
                        .Float => |right_val| @as(f64, @floatFromInt(left_val)) > right_val,
                        else => {
                            debug.log("TypeMismatch in Gt: left={}, right={}", .{ left, right });
                            return error.TypeMismatch;
                        },
                    },
                    .Float => |left_val| switch (right) {
                        .Int => |right_val| left_val > @as(f64, @floatFromInt(right_val)),
                        .Float => |right_val| left_val > right_val,
                        else => {
                            debug.log("TypeMismatch in Gt: left={}, right={}", .{ left, right });
                            return error.TypeMismatch;
                        },
                    },
                    else => {
                        debug.log("TypeMismatch in Gt: left={}, right={}", .{ left, right });
                        return error.TypeMismatch;
                    },
                };

                try self.setRegister(dst_reg, .{ .Bool = result });
            },
            .Eq => {
                // Register-based equality: Eq Rs1, Rs2 -> Rd
                const src1_reg = instruction.src1 orelse return error.UnsupportedInstruction;
                const src2_reg = instruction.src2 orelse return error.UnsupportedInstruction;
                const dst_reg = instruction.dst orelse return error.UnsupportedInstruction;

                debug.log("Eq instruction: R{} = R{} == R{}", .{ dst_reg, src1_reg, src2_reg });

                var left = try self.getRegister(src1_reg);
                defer left.deinit(self.allocator);
                var right = try self.getRegister(src2_reg);
                defer right.deinit(self.allocator);

                // Handle equality comparison based on types
                const result = switch (left) {
                    .Int => |left_val| switch (right) {
                        .Int => |right_val| left_val == right_val,
                        .Float => |right_val| @as(f64, @floatFromInt(left_val)) == right_val,
                        else => false, // Different types are never equal
                    },
                    .Float => |left_val| switch (right) {
                        .Int => |right_val| left_val == @as(f64, @floatFromInt(right_val)),
                        .Float => |right_val| left_val == right_val,
                        else => false, // Different types are never equal
                    },
                    .String => |left_val| switch (right) {
                        .String => |right_val| std.mem.eql(u8, left_val, right_val),
                        else => false, // Different types are never equal
                    },
                    .Bool => |left_val| switch (right) {
                        .Bool => |right_val| left_val == right_val,
                        else => false, // Different types are never equal
                    },
                    .Nil => switch (right) {
                        .Nil => true, // nil == nil
                        else => false, // nil is not equal to any other type
                    },
                    .Symbol => |left_val| switch (right) {
                        .Symbol => |right_val| std.mem.eql(u8, left_val, right_val),
                        else => false, // Different types are never equal
                    },
                    else => false, // Other types not supported for equality comparison yet
                };

                try self.setRegister(dst_reg, .{ .Bool = result });
            },
            .Ne => {
                // Register-based not equal: Ne Rs1, Rs2 -> Rd
                const src1_reg = instruction.src1 orelse return error.UnsupportedInstruction;
                const src2_reg = instruction.src2 orelse return error.UnsupportedInstruction;
                const dst_reg = instruction.dst orelse return error.UnsupportedInstruction;

                debug.log("Ne instruction: R{} = R{} != R{}", .{ dst_reg, src1_reg, src2_reg });

                var left = try self.getRegister(src1_reg);
                defer left.deinit(self.allocator);
                var right = try self.getRegister(src2_reg);
                defer right.deinit(self.allocator);

                // Ne is just the opposite of Eq
                const eq_result = switch (left) {
                    .Int => |left_val| switch (right) {
                        .Int => |right_val| left_val == right_val,
                        .Float => |right_val| @as(f64, @floatFromInt(left_val)) == right_val,
                        else => false,
                    },
                    .Float => |left_val| switch (right) {
                        .Int => |right_val| left_val == @as(f64, @floatFromInt(right_val)),
                        .Float => |right_val| left_val == right_val,
                        else => false,
                    },
                    .String => |left_val| switch (right) {
                        .String => |right_val| std.mem.eql(u8, left_val, right_val),
                        else => false,
                    },
                    .Bool => |left_val| switch (right) {
                        .Bool => |right_val| left_val == right_val,
                        else => false,
                    },
                    .Nil => switch (right) {
                        .Nil => true,
                        else => false,
                    },
                    .Symbol => |left_val| switch (right) {
                        .Symbol => |right_val| std.mem.eql(u8, left_val, right_val),
                        else => false,
                    },
                    else => false,
                };

                try self.setRegister(dst_reg, .{ .Bool = !eq_result });
            },
            .Le => {
                // Register-based less than or equal: Le Rs1, Rs2 -> Rd
                const src1_reg = instruction.src1 orelse return error.UnsupportedInstruction;
                const src2_reg = instruction.src2 orelse return error.UnsupportedInstruction;
                const dst_reg = instruction.dst orelse return error.UnsupportedInstruction;

                debug.log("Le instruction: R{} = R{} <= R{}", .{ dst_reg, src1_reg, src2_reg });

                var left = try self.getRegister(src1_reg);
                defer left.deinit(self.allocator);
                var right = try self.getRegister(src2_reg);
                defer right.deinit(self.allocator);

                const result = switch (left) {
                    .Int => |left_val| switch (right) {
                        .Int => |right_val| left_val <= right_val,
                        .Float => |right_val| @as(f64, @floatFromInt(left_val)) <= right_val,
                        else => {
                            debug.log("TypeMismatch in Le: left={}, right={}", .{ left, right });
                            return error.TypeMismatch;
                        },
                    },
                    .Float => |left_val| switch (right) {
                        .Int => |right_val| left_val <= @as(f64, @floatFromInt(right_val)),
                        .Float => |right_val| left_val <= right_val,
                        else => {
                            debug.log("TypeMismatch in Le: left={}, right={}", .{ left, right });
                            return error.TypeMismatch;
                        },
                    },
                    else => {
                        debug.log("TypeMismatch in Le: left={}, right={}", .{ left, right });
                        return error.TypeMismatch;
                    },
                };

                try self.setRegister(dst_reg, .{ .Bool = result });
            },
            .Ge => {
                // Register-based greater than or equal: Ge Rs1, Rs2 -> Rd
                const src1_reg = instruction.src1 orelse return error.UnsupportedInstruction;
                const src2_reg = instruction.src2 orelse return error.UnsupportedInstruction;
                const dst_reg = instruction.dst orelse return error.UnsupportedInstruction;

                debug.log("Ge instruction: R{} = R{} >= R{}", .{ dst_reg, src1_reg, src2_reg });

                var left = try self.getRegister(src1_reg);
                defer left.deinit(self.allocator);
                var right = try self.getRegister(src2_reg);
                defer right.deinit(self.allocator);

                const result = switch (left) {
                    .Int => |left_val| switch (right) {
                        .Int => |right_val| left_val >= right_val,
                        .Float => |right_val| @as(f64, @floatFromInt(left_val)) >= right_val,
                        else => {
                            debug.log("TypeMismatch in Ge: left={}, right={}", .{ left, right });
                            return error.TypeMismatch;
                        },
                    },
                    .Float => |left_val| switch (right) {
                        .Int => |right_val| left_val >= @as(f64, @floatFromInt(right_val)),
                        .Float => |right_val| left_val >= right_val,
                        else => {
                            debug.log("TypeMismatch in Ge: left={}, right={}", .{ left, right });
                            return error.TypeMismatch;
                        },
                    },
                    else => {
                        debug.log("TypeMismatch in Ge: left={}, right={}", .{ left, right });
                        return error.TypeMismatch;
                    },
                };

                try self.setRegister(dst_reg, .{ .Bool = result });
            },
            .Not => {
                // Register-based logical not: Not Rs -> Rd
                const src_reg = instruction.src1 orelse return error.UnsupportedInstruction;
                const dst_reg = instruction.dst orelse return error.UnsupportedInstruction;

                debug.log("Not instruction: R{} = !R{}", .{ dst_reg, src_reg });

                var value = try self.getRegister(src_reg);
                defer value.deinit(self.allocator);

                // Determine truthiness of the value
                const is_truthy = switch (value) {
                    .Nil => false,
                    .Bool => |b| b,
                    .Int => |i| i != 0,
                    .Float => |f| f != 0.0,
                    .String => |s| s.len > 0,
                    .Array => |a| a.len > 0,
                    .Map => |m| m.count() > 0,
                    else => true, // Other values are considered truthy
                };

                try self.setRegister(dst_reg, .{ .Bool = !is_truthy });
            },
            .Print => {
                // Register-based print: Print Rs
                const src_reg = instruction.src1 orelse return error.UnsupportedInstruction;

                debug.log("Print instruction: print R{}", .{src_reg});

                var value = try self.getRegister(src_reg);
                defer value.deinit(self.allocator);

                switch (value) {
                    .Int => |val| try self.stdout.print("{d}\n", .{val}),
                    .String => |str| try self.stdout.print("{s}\n", .{str}),
                    .Symbol => |sym| try self.stdout.print("{s}\n", .{sym}),
                    .Bool => |b| try self.stdout.print("{}\n", .{b}),
                    .Float => |f| try self.stdout.print("{d}\n", .{f}),
                    .Nil => try self.stdout.print("nil\n", .{}),
                    .Array => |arr| try self.stdout.print("{any}\n", .{arr}),
                    .Map => |map| try self.stdout.print("{any}\n", .{map}),
                    .ReturnAddress => |addr| try self.stdout.print("ReturnAddress: {any}\n", .{addr}),
                    .Function => |func| try self.stdout.print("Function: {any}\n", .{func}),
                    .Variable => |var_val| try self.stdout.print("Variable: {s}\n", .{var_val.name}),
                    .BuiltinOperator => |op| try self.stdout.print("BuiltinOperator: {any}\n", .{op}),
                    .Class => |class| try self.stdout.print("Class: {s}\n", .{class.name}),
                    .Object => |obj_id| {
                        if (self.getObjectById(obj_id)) |obj| {
                            try self.stdout.print("Object of {s}\n", .{obj.class.name});
                        } else {
                            try self.stdout.print("Object(invalid ID: {})\n", .{obj_id});
                        }
                    },
                    .CPtr => |ptr| {
                        if (ptr) |p| {
                            try self.stdout.print("CPtr: {*}\n", .{p});
                        } else {
                            try self.stdout.print("CPtr: null\n", .{});
                        }
                    },
                    .CFunction => |func| try self.stdout.print("CFunction: {*}\n", .{func}),
                    .CStruct => |ptr| try self.stdout.print("CStruct: {*}\n", .{ptr}),
                    .CArray => |arr| try self.stdout.print("CArray: ptr={*}, len={}, element_size={}\n", .{ arr.ptr, arr.len, arr.element_size }),
                    .Module => |module| try self.stdout.print("Module: {s}\n", .{module.id}),
                    .StdlibFunction => |func| try self.stdout.print("StdlibFunction: {s}\n", .{@tagName(func)}),
                    .FileHandle => |handle| try self.stdout.print("FileHandle: {s}\n", .{handle.path}),
                    .Error => |err| try self.stdout.print("Error({s}): {s}\n", .{ err.type, err.message }),
                    .FFIFunction => |name| try self.stdout.print("FFIFunction: {s}\n", .{name}),
                    .NativeFunction => try self.stdout.print("NativeFunction\n", .{}),
                    .CCallback => |cb| try self.stdout.print("CCallback({s})\n", .{cb.signature orelse "dynamic"}),
                    .Ref => |ref| {
                        try self.stdout.print("Ref -> ", .{});
                        // Print the referenced value recursively
                        const ref_copy = ref.*;
                        switch (ref_copy) {
                            .Int => |val| try self.stdout.print("{d}\n", .{val}),
                            .String => |str| try self.stdout.print("{s}\n", .{str}),
                            .Bool => |b| try self.stdout.print("{}\n", .{b}),
                            .Float => |f| try self.stdout.print("{d}\n", .{f}),
                            .Nil => try self.stdout.print("nil\n", .{}),
                            else => try self.stdout.print("<complex value>\n", .{}),
                        }
                    },
                }
            },
            .Call => {
                // Register-based function call: Call function_reg, [R0, R1, ...] -> Rd
                debug.log("Call operation", .{});
                const func_reg = instruction.src1 orelse return error.UnsupportedInstruction;
                const dst_reg = instruction.dst orelse return error.UnsupportedInstruction;
                const arg_count = if (instruction.immediate) |imm| switch (imm) {
                    .Int => @as(usize, @intCast(imm.Int)),
                    else => return error.TypeMismatch,
                } else return error.UnsupportedInstruction;

                debug.log("Register-based call: R{} = call R{} with {} args -> R{}", .{ dst_reg, func_reg, arg_count, dst_reg });

                // Get function from register
                var func_val = try self.getRegister(func_reg);
                defer func_val.deinit(self.allocator);
                debug.logValue(func_val);

                // Handle native functions first
                if (func_val == .NativeFunction) {
                    const native_fn = func_val.NativeFunction;
                    
                    // Collect arguments
                    var args = try self.allocator.alloc(types.Value, arg_count);
                    defer self.allocator.free(args);
                    
                    for (0..arg_count) |i| {
                        const arg_reg = func_reg + 1 + @as(u16, @intCast(i));
                        args[i] = try self.getRegister(arg_reg);
                    }
                    
                    // Call native function
                    const result = try native_fn(@ptrCast(self), args);
                    
                    // Clean up arguments
                    for (args) |*arg| {
                        arg.deinit(self.allocator);
                    }
                    
                    // Store result
                    try self.setRegister(dst_reg, result);
                    return;
                }
                
                // Handle built-in functions
                if (func_val == .BuiltinOperator) {
                    const builtin_op = func_val.BuiltinOperator;

                    // Handle print function
                    if (builtin_op == .Print) {
                        // Print can take any number of arguments
                        for (0..arg_count) |i| {
                            // Arguments always follow the function register
                            const arg_reg = func_reg + 1 + @as(u16, @intCast(i));
                            var arg = try self.getRegister(arg_reg);
                            defer arg.deinit(self.allocator);

                            // Print without newline except for the last argument
                            switch (arg) {
                                .Int => |val| try self.stdout.print("{d}", .{val}),
                                .String => |str| try self.stdout.print("{s}", .{str}),
                                .Symbol => |sym| try self.stdout.print("{s}", .{sym}),
                                .Bool => |b| try self.stdout.print("{}", .{b}),
                                .Float => |f| try self.stdout.print("{d}", .{f}),
                                .Nil => try self.stdout.print("nil", .{}),
                                .Array => |arr| {
                                    try self.stdout.print("[", .{});
                                    for (arr, 0..) |item, idx| {
                                        if (idx > 0) try self.stdout.print(" ", .{});
                                        switch (item) {
                                            .Int => |val| try self.stdout.print("{d}", .{val}),
                                            .String => |str| try self.stdout.print("\"{s}\"", .{str}),
                                            .Symbol => |sym| try self.stdout.print("{s}", .{sym}),
                                            .Bool => |b| try self.stdout.print("{}", .{b}),
                                            .Float => |f| try self.stdout.print("{d}", .{f}),
                                            .Nil => try self.stdout.print("nil", .{}),
                                            else => try self.stdout.print("{any}", .{item}),
                                        }
                                    }
                                    try self.stdout.print("]", .{});
                                },
                                .Map => |map| try self.stdout.print("{any}", .{map}),
                                .ReturnAddress => |addr| try self.stdout.print("ReturnAddress: {any}", .{addr}),
                                .Function => |func| try self.stdout.print("Function: {s}", .{func.name}),
                                .Variable => |var_name| try self.stdout.print("{s}", .{var_name.name}),
                                .BuiltinOperator => |op| try self.stdout.print("BuiltinOperator: {any}", .{op}),
                                .Class => |class| try self.stdout.print("Class: {s}", .{class.name}),
                                .Object => |obj_id| {
                                    if (self.getObjectById(obj_id)) |obj| {
                                        try self.stdout.print("Object of {s}", .{obj.class.name});
                                    } else {
                                        try self.stdout.print("Object(invalid ID: {})", .{obj_id});
                                    }
                                },
                                .Module => |module| try self.stdout.print("Module: {s}", .{module.id}),
                                .StdlibFunction => |func| try self.stdout.print("StdlibFunction: {s}", .{@tagName(func)}),
                                .FileHandle => |handle| try self.stdout.print("FileHandle: {s}", .{handle.path}),
                                .CPtr => |ptr| {
                                    if (ptr) |p| {
                                        try self.stdout.print("CPtr: {*}", .{p});
                                    } else {
                                        try self.stdout.print("CPtr: null", .{});
                                    }
                                },
                                .CFunction => |func| try self.stdout.print("CFunction: {*}", .{func}),
                                .CStruct => |ptr| try self.stdout.print("CStruct: {*}", .{ptr}),
                                .CArray => |arr| try self.stdout.print("CArray[{} x {}]", .{ arr.len, arr.element_size }),
                                .Error => |err| try self.stdout.print("Error({s}): {s}", .{ err.type, err.message }),
                                .FFIFunction => |name| try self.stdout.print("FFIFunction: {s}", .{name}),
                                .NativeFunction => try self.stdout.print("NativeFunction", .{}),
                                .CCallback => |cb| try self.stdout.print("CCallback({s})", .{cb.signature orelse "dynamic"}),
                                .Ref => |ref| {
                                    try self.stdout.print("Ref -> ", .{});
                                    // Print referenced value (simplified)
                                    switch (ref.*) {
                                        .Int => |val| try self.stdout.print("{d}", .{val}),
                                        .String => |str| try self.stdout.print("{s}", .{str}),
                                        .Bool => |b| try self.stdout.print("{}", .{b}),
                                        .Float => |f| try self.stdout.print("{d}", .{f}),
                                        .Nil => try self.stdout.print("nil", .{}),
                                        else => try self.stdout.print("<ref>", .{}),
                                    }
                                },
                            }

                            // Add space between arguments except for the last one
                            if (i < arg_count - 1) {
                                try self.stdout.print(" ", .{});
                            }
                        }

                        // Add newline after all arguments
                        try self.stdout.print("\n", .{});

                        // Print function returns nil
                        try self.setRegister(dst_reg, .{ .Nil = {} });
                        return;
                    }

                    // Handle println function (same as print, already adds newline)
                    if (builtin_op == .Println) {
                        // Println can take any number of arguments
                        for (0..arg_count) |i| {
                            // Arguments always follow the function register
                            const arg_reg = func_reg + 1 + @as(u16, @intCast(i));
                            var arg = try self.getRegister(arg_reg);
                            defer arg.deinit(self.allocator);

                            // Print without newline except for the last argument
                            switch (arg) {
                                .Int => |val| try self.stdout.print("{d}", .{val}),
                                .String => |str| try self.stdout.print("{s}", .{str}),
                                .Symbol => |sym| try self.stdout.print("{s}", .{sym}),
                                .Bool => |b| try self.stdout.print("{}", .{b}),
                                .Float => |f| try self.stdout.print("{d}", .{f}),
                                .Nil => try self.stdout.print("nil", .{}),
                                .Array => |arr| {
                                    try self.stdout.print("[", .{});
                                    for (arr, 0..) |item, idx| {
                                        if (idx > 0) try self.stdout.print(" ", .{});
                                        switch (item) {
                                            .Int => |val| try self.stdout.print("{d}", .{val}),
                                            .String => |str| try self.stdout.print("\"{s}\"", .{str}),
                                            .Symbol => |sym| try self.stdout.print("{s}", .{sym}),
                                            .Bool => |b| try self.stdout.print("{}", .{b}),
                                            .Float => |f| try self.stdout.print("{d}", .{f}),
                                            .Nil => try self.stdout.print("nil", .{}),
                                            else => try self.stdout.print("{any}", .{item}),
                                        }
                                    }
                                    try self.stdout.print("]", .{});
                                },
                                .Map => |map| try self.stdout.print("{any}", .{map}),
                                .ReturnAddress => |addr| try self.stdout.print("ReturnAddress: {any}", .{addr}),
                                .Function => |func| try self.stdout.print("Function: {s}", .{func.name}),
                                .Variable => |var_name| try self.stdout.print("{s}", .{var_name.name}),
                                .BuiltinOperator => |op| try self.stdout.print("BuiltinOperator: {any}", .{op}),
                                .Class => |class| try self.stdout.print("Class: {s}", .{class.name}),
                                .Object => |obj_id| {
                                    if (self.getObjectById(obj_id)) |obj| {
                                        try self.stdout.print("Object of {s}", .{obj.class.name});
                                    } else {
                                        try self.stdout.print("Object(invalid ID: {})", .{obj_id});
                                    }
                                },
                                .Module => |module| try self.stdout.print("Module: {s}", .{module.id}),
                                .StdlibFunction => |func| try self.stdout.print("StdlibFunction: {s}", .{@tagName(func)}),
                                .FileHandle => |handle| try self.stdout.print("FileHandle: {s}", .{handle.path}),
                                .CPtr => |ptr| {
                                    if (ptr) |p| {
                                        try self.stdout.print("CPtr: {*}", .{p});
                                    } else {
                                        try self.stdout.print("CPtr: null", .{});
                                    }
                                },
                                .CFunction => |func| try self.stdout.print("CFunction: {*}", .{func}),
                                .CStruct => |ptr| try self.stdout.print("CStruct: {*}", .{ptr}),
                                .CArray => |arr| try self.stdout.print("CArray[{} x {}]", .{ arr.len, arr.element_size }),
                                .Error => |err| try self.stdout.print("Error({s}): {s}", .{ err.type, err.message }),
                                .FFIFunction => |name| try self.stdout.print("FFIFunction: {s}", .{name}),
                                .NativeFunction => try self.stdout.print("NativeFunction", .{}),
                                .CCallback => |cb| try self.stdout.print("CCallback({s})", .{cb.signature orelse "dynamic"}),
                                .Ref => |ref| {
                                    try self.stdout.print("Ref -> ", .{});
                                    // Print referenced value (simplified)
                                    switch (ref.*) {
                                        .Int => |val| try self.stdout.print("{d}", .{val}),
                                        .String => |str| try self.stdout.print("{s}", .{str}),
                                        .Bool => |b| try self.stdout.print("{}", .{b}),
                                        .Float => |f| try self.stdout.print("{d}", .{f}),
                                        .Nil => try self.stdout.print("nil", .{}),
                                        else => try self.stdout.print("<ref>", .{}),
                                    }
                                },
                            }

                            // Add space between arguments except for the last one
                            if (i < arg_count - 1) {
                                try self.stdout.print(" ", .{});
                            }
                        }

                        // Add newline after all arguments
                        try self.stdout.print("\n", .{});

                        // Println function returns nil
                        try self.setRegister(dst_reg, .{ .Nil = {} });
                        return;
                    }

                    // Handle add operator
                    if (builtin_op == .Add) {
                        if (arg_count != 2) {
                            debug.log("Add operator requires exactly 2 arguments, got {}", .{arg_count});
                            return error.ArgumentCountMismatch;
                        }

                        // Get the two arguments
                        var left = try self.getRegister(func_reg + 1);
                        defer left.deinit(self.allocator);
                        var right = try self.getRegister(func_reg + 2);
                        defer right.deinit(self.allocator);

                        // Perform addition based on types
                        const result = switch (left) {
                            .Int => |left_val| switch (right) {
                                .Int => |right_val| types.Value{ .Int = left_val + right_val },
                                .Float => |right_val| types.Value{ .Float = @as(f64, @floatFromInt(left_val)) + right_val },
                                else => {
                                    debug.log("TypeMismatch in Add: left={}, right={}", .{ left, right });
                                    return error.TypeMismatch;
                                },
                            },
                            .Float => |left_val| switch (right) {
                                .Int => |right_val| types.Value{ .Float = left_val + @as(f64, @floatFromInt(right_val)) },
                                .Float => |right_val| types.Value{ .Float = left_val + right_val },
                                else => {
                                    debug.log("TypeMismatch in Add: left={}, right={}", .{ left, right });
                                    return error.TypeMismatch;
                                },
                            },
                            .String => |left_val| switch (right) {
                                .String => |right_val| blk: {
                                    const concat_result = try std.fmt.allocPrint(self.allocator, "{s}{s}", .{ left_val, right_val });
                                    break :blk types.Value{ .String = concat_result };
                                },
                                else => {
                                    debug.log("TypeMismatch in Add: left={}, right={}", .{ left, right });
                                    return error.TypeMismatch;
                                },
                            },
                            else => {
                                debug.log("TypeMismatch in Add: left={}, right={}", .{ left, right });
                                return error.TypeMismatch;
                            },
                        };

                        try self.setRegister(dst_reg, result);
                        return;
                    }

                    // Handle subtract operator
                    if (builtin_op == .Sub) {
                        if (arg_count != 2) {
                            debug.log("Sub operator requires exactly 2 arguments, got {}", .{arg_count});
                            return error.ArgumentCountMismatch;
                        }

                        // Get the two arguments
                        var left = try self.getRegister(func_reg + 1);
                        defer left.deinit(self.allocator);
                        var right = try self.getRegister(func_reg + 2);
                        defer right.deinit(self.allocator);

                        // Perform subtraction based on types
                        const result = switch (left) {
                            .Int => |left_val| switch (right) {
                                .Int => |right_val| types.Value{ .Int = left_val - right_val },
                                .Float => |right_val| types.Value{ .Float = @as(f64, @floatFromInt(left_val)) - right_val },
                                else => {
                                    debug.log("TypeMismatch in Sub: left={}, right={}", .{ left, right });
                                    return error.TypeMismatch;
                                },
                            },
                            .Float => |left_val| switch (right) {
                                .Int => |right_val| types.Value{ .Float = left_val - @as(f64, @floatFromInt(right_val)) },
                                .Float => |right_val| types.Value{ .Float = left_val - right_val },
                                else => {
                                    debug.log("TypeMismatch in Sub: left={}, right={}", .{ left, right });
                                    return error.TypeMismatch;
                                },
                            },
                            else => {
                                debug.log("TypeMismatch in Sub: left={}, right={}", .{ left, right });
                                return error.TypeMismatch;
                            },
                        };

                        try self.setRegister(dst_reg, result);
                        return;
                    }

                    // Handle less than operator
                    if (builtin_op == .LessThan) {
                        if (arg_count != 2) {
                            debug.log("LessThan operator requires exactly 2 arguments, got {}", .{arg_count});
                            return error.ArgumentCountMismatch;
                        }

                        // Get the two arguments
                        var left = try self.getRegister(func_reg + 1);
                        defer left.deinit(self.allocator);
                        var right = try self.getRegister(func_reg + 2);
                        defer right.deinit(self.allocator);

                        // Perform comparison based on types
                        const result = switch (left) {
                            .Int => |left_val| switch (right) {
                                .Int => |right_val| left_val < right_val,
                                .Float => |right_val| @as(f64, @floatFromInt(left_val)) < right_val,
                                else => {
                                    debug.log("TypeMismatch in LessThan: left={}, right={}", .{ left, right });
                                    return error.TypeMismatch;
                                },
                            },
                            .Float => |left_val| switch (right) {
                                .Int => |right_val| left_val < @as(f64, @floatFromInt(right_val)),
                                .Float => |right_val| left_val < right_val,
                                else => {
                                    debug.log("TypeMismatch in LessThan: left={}, right={}", .{ left, right });
                                    return error.TypeMismatch;
                                },
                            },
                            else => {
                                debug.log("TypeMismatch in LessThan: left={}, right={}", .{ left, right });
                                return error.TypeMismatch;
                            },
                        };

                        try self.setRegister(dst_reg, .{ .Bool = result });
                        return;
                    }

                    // Handle multiplication operator
                    if (builtin_op == .Mul) {
                        if (arg_count != 2) {
                            debug.log("Multiply operator requires exactly 2 arguments, got {}", .{arg_count});
                            return error.ArgumentCountMismatch;
                        }

                        // Get the two arguments
                        var left = try self.getRegister(func_reg + 1);
                        defer left.deinit(self.allocator);
                        var right = try self.getRegister(func_reg + 2);
                        defer right.deinit(self.allocator);

                        // Perform multiplication based on types
                        const result = switch (left) {
                            .Int => |left_val| switch (right) {
                                .Int => |right_val| types.Value{ .Int = left_val * right_val },
                                .Float => |right_val| types.Value{ .Float = @as(f64, @floatFromInt(left_val)) * right_val },
                                else => {
                                    debug.log("TypeMismatch in Multiply: left={}, right={}", .{ left, right });
                                    return error.TypeMismatch;
                                },
                            },
                            .Float => |left_val| switch (right) {
                                .Int => |right_val| types.Value{ .Float = left_val * @as(f64, @floatFromInt(right_val)) },
                                .Float => |right_val| types.Value{ .Float = left_val * right_val },
                                else => {
                                    debug.log("TypeMismatch in Multiply: left={}, right={}", .{ left, right });
                                    return error.TypeMismatch;
                                },
                            },
                            else => {
                                debug.log("TypeMismatch in Multiply: left={}, right={}", .{ left, right });
                                return error.TypeMismatch;
                            },
                        };

                        try self.setRegister(dst_reg, result);
                        return;
                    }

                    // Handle division operator
                    if (builtin_op == .Div) {
                        if (arg_count != 2) {
                            debug.log("Divide operator requires exactly 2 arguments, got {}", .{arg_count});
                            return error.ArgumentCountMismatch;
                        }

                        // Get the two arguments
                        var left = try self.getRegister(func_reg + 1);
                        defer left.deinit(self.allocator);
                        var right = try self.getRegister(func_reg + 2);
                        defer right.deinit(self.allocator);

                        // Check for division by zero
                        const is_zero = switch (right) {
                            .Int => |val| val == 0,
                            .Float => |val| val == 0.0,
                            else => false,
                        };
                        if (is_zero) {
                            debug.log("Division by zero", .{});
                            return error.DivisionByZero;
                        }

                        // Perform division based on types
                        const result = switch (left) {
                            .Int => |left_val| switch (right) {
                                .Int => |right_val| types.Value{ .Int = @divTrunc(left_val, right_val) },
                                .Float => |right_val| types.Value{ .Float = @as(f64, @floatFromInt(left_val)) / right_val },
                                else => {
                                    debug.log("TypeMismatch in Divide: left={}, right={}", .{ left, right });
                                    return error.TypeMismatch;
                                },
                            },
                            .Float => |left_val| switch (right) {
                                .Int => |right_val| types.Value{ .Float = left_val / @as(f64, @floatFromInt(right_val)) },
                                .Float => |right_val| types.Value{ .Float = left_val / right_val },
                                else => {
                                    debug.log("TypeMismatch in Divide: left={}, right={}", .{ left, right });
                                    return error.TypeMismatch;
                                },
                            },
                            else => {
                                debug.log("TypeMismatch in Divide: left={}, right={}", .{ left, right });
                                return error.TypeMismatch;
                            },
                        };

                        try self.setRegister(dst_reg, result);
                        return;
                    }

                    // Handle greater than or equal operator
                    if (builtin_op == .GreaterEqual) {
                        if (arg_count != 2) {
                            debug.log("GreaterEqual operator requires exactly 2 arguments, got {}", .{arg_count});
                            return error.ArgumentCountMismatch;
                        }

                        // Get the two arguments
                        var left = try self.getRegister(func_reg + 1);
                        defer left.deinit(self.allocator);
                        var right = try self.getRegister(func_reg + 2);
                        defer right.deinit(self.allocator);

                        // Perform comparison based on types
                        const result = switch (left) {
                            .Int => |left_val| switch (right) {
                                .Int => |right_val| left_val >= right_val,
                                .Float => |right_val| @as(f64, @floatFromInt(left_val)) >= right_val,
                                else => {
                                    debug.log("TypeMismatch in GreaterEqual: left={}, right={}", .{ left, right });
                                    return error.TypeMismatch;
                                },
                            },
                            .Float => |left_val| switch (right) {
                                .Int => |right_val| left_val >= @as(f64, @floatFromInt(right_val)),
                                .Float => |right_val| left_val >= right_val,
                                else => {
                                    debug.log("TypeMismatch in GreaterEqual: left={}, right={}", .{ left, right });
                                    return error.TypeMismatch;
                                },
                            },
                            else => {
                                debug.log("TypeMismatch in GreaterEqual: left={}, right={}", .{ left, right });
                                return error.TypeMismatch;
                            },
                        };

                        try self.setRegister(dst_reg, .{ .Bool = result });
                        return;
                    }

                    // Handle greater than operator
                    if (builtin_op == .GreaterThan) {
                        if (arg_count != 2) {
                            debug.log("GreaterThan operator requires exactly 2 arguments, got {}", .{arg_count});
                            return error.ArgumentCountMismatch;
                        }

                        // Get the two arguments
                        var left = try self.getRegister(func_reg + 1);
                        defer left.deinit(self.allocator);
                        var right = try self.getRegister(func_reg + 2);
                        defer right.deinit(self.allocator);

                        // Perform comparison based on types
                        const result = switch (left) {
                            .Int => |left_val| switch (right) {
                                .Int => |right_val| left_val > right_val,
                                .Float => |right_val| @as(f64, @floatFromInt(left_val)) > right_val,
                                else => {
                                    debug.log("TypeMismatch in GreaterThan: left={}, right={}", .{ left, right });
                                    return error.TypeMismatch;
                                },
                            },
                            .Float => |left_val| switch (right) {
                                .Int => |right_val| left_val > @as(f64, @floatFromInt(right_val)),
                                .Float => |right_val| left_val > right_val,
                                else => {
                                    debug.log("TypeMismatch in GreaterThan: left={}, right={}", .{ left, right });
                                    return error.TypeMismatch;
                                },
                            },
                            else => {
                                debug.log("TypeMismatch in GreaterThan: left={}, right={}", .{ left, right });
                                return error.TypeMismatch;
                            },
                        };

                        try self.setRegister(dst_reg, .{ .Bool = result });
                        return;
                    }

                    // Handle equality operator
                    if (builtin_op == .Eq) {
                        if (arg_count != 2) {
                            debug.log("Eq operator requires exactly 2 arguments, got {}", .{arg_count});
                            return error.ArgumentCountMismatch;
                        }

                        // Get the two arguments
                        var left = try self.getRegister(func_reg + 1);
                        defer left.deinit(self.allocator);
                        var right = try self.getRegister(func_reg + 2);
                        defer right.deinit(self.allocator);

                        // Perform equality check based on types
                        const result = switch (left) {
                            .Int => |left_val| switch (right) {
                                .Int => |right_val| left_val == right_val,
                                .Float => |right_val| @as(f64, @floatFromInt(left_val)) == right_val,
                                else => false,
                            },
                            .Float => |left_val| switch (right) {
                                .Int => |right_val| left_val == @as(f64, @floatFromInt(right_val)),
                                .Float => |right_val| left_val == right_val,
                                else => false,
                            },
                            .Bool => |left_val| switch (right) {
                                .Bool => |right_val| left_val == right_val,
                                else => false,
                            },
                            .String => |left_val| switch (right) {
                                .String => |right_val| std.mem.eql(u8, left_val, right_val),
                                else => false,
                            },
                            .Nil => switch (right) {
                                .Nil => true,
                                else => false,
                            },
                            .Symbol => |left_val| switch (right) {
                                .Symbol => |right_val| std.mem.eql(u8, left_val, right_val),
                                else => false,
                            },
                            else => false, // Other types not comparable yet
                        };

                        try self.setRegister(dst_reg, .{ .Bool = result });
                        return;
                    }

                    // Handle inequality operator
                    if (builtin_op == .Ne) {
                        if (arg_count != 2) {
                            debug.log("Ne operator requires exactly 2 arguments, got {}", .{arg_count});
                            return error.ArgumentCountMismatch;
                        }

                        // Get the two arguments
                        var left = try self.getRegister(func_reg + 1);
                        defer left.deinit(self.allocator);
                        var right = try self.getRegister(func_reg + 2);
                        defer right.deinit(self.allocator);

                        // Perform comparison based on types (same as Eq but negated)
                        const result = switch (left) {
                            .Int => |left_val| switch (right) {
                                .Int => |right_val| left_val != right_val,
                                .Float => |right_val| @as(f64, @floatFromInt(left_val)) != right_val,
                                else => true, // Different types are not equal
                            },
                            .Float => |left_val| switch (right) {
                                .Int => |right_val| left_val != @as(f64, @floatFromInt(right_val)),
                                .Float => |right_val| left_val != right_val,
                                else => true, // Different types are not equal
                            },
                            .String => |left_val| switch (right) {
                                .String => |right_val| !std.mem.eql(u8, left_val, right_val),
                                else => true, // Different types are not equal
                            },
                            .Bool => |left_val| switch (right) {
                                .Bool => |right_val| left_val != right_val,
                                else => true, // Different types are not equal
                            },
                            .Nil => switch (right) {
                                .Nil => false, // nil != nil is false
                                else => true, // nil != anything else is true
                            },
                            .Symbol => |left_val| switch (right) {
                                .Symbol => |right_val| !std.mem.eql(u8, left_val, right_val),
                                else => true, // Different types are not equal
                            },
                            else => true, // For other types, assume not equal
                        };

                        try self.setRegister(dst_reg, .{ .Bool = result });
                        return;
                    }

                    // Handle logical NOT operator
                    if (builtin_op == .Not) {
                        if (arg_count != 1) {
                            debug.log("Not operator requires exactly 1 argument, got {}", .{arg_count});
                            return error.ArgumentCountMismatch;
                        }

                        // Get the argument
                        var arg = try self.getRegister(func_reg + 1);
                        defer arg.deinit(self.allocator);

                        // Convert to boolean and negate
                        const result = switch (arg) {
                            .Bool => |b| !b,
                            .Nil => true, // !nil is true
                            .Int => |i| i == 0, // !0 is true, !non-zero is false
                            .Float => |f| f == 0.0, // !0.0 is true, !non-zero is false
                            .String => |s| s.len == 0, // !"" is true, !non-empty is false
                            .Array => |arr| arr.len == 0, // ![] is true, !non-empty is false
                            .Map => |map| map.count() == 0, // !{} is true, !non-empty is false
                            else => false, // For other types, !value is false
                        };

                        try self.setRegister(dst_reg, .{ .Bool = result });
                        return;
                    }

                    // Handle logical AND operator
                    if (builtin_op == .And) {
                        if (arg_count != 2) {
                            debug.log("And operator requires exactly 2 arguments, got {}", .{arg_count});
                            return error.ArgumentCountMismatch;
                        }

                        // Get the first argument and check if it's truthy
                        var left = try self.getRegister(func_reg + 1);
                        defer left.deinit(self.allocator);
                        
                        const left_truthy = switch (left) {
                            .Nil => false,
                            .Bool => |b| b,
                            else => true,
                        };
                        
                        if (!left_truthy) {
                            // Short-circuit: return first falsy value
                            try self.setRegister(dst_reg, try left.clone(self.allocator));
                            return;
                        }
                        
                        // First is truthy, return the second value
                        var right = try self.getRegister(func_reg + 2);
                        defer right.deinit(self.allocator);
                        try self.setRegister(dst_reg, try right.clone(self.allocator));
                        return;
                    }

                    // Handle logical OR operator
                    if (builtin_op == .Or) {
                        if (arg_count != 2) {
                            debug.log("Or operator requires exactly 2 arguments, got {}", .{arg_count});
                            return error.ArgumentCountMismatch;
                        }

                        // Get the first argument and check if it's truthy
                        var left = try self.getRegister(func_reg + 1);
                        defer left.deinit(self.allocator);
                        
                        const left_truthy = switch (left) {
                            .Nil => false,
                            .Bool => |b| b,
                            else => true,
                        };
                        
                        if (left_truthy) {
                            // Short-circuit: return first truthy value
                            try self.setRegister(dst_reg, try left.clone(self.allocator));
                            return;
                        }
                        
                        // First is falsy, return the second value
                        var right = try self.getRegister(func_reg + 2);
                        defer right.deinit(self.allocator);
                        try self.setRegister(dst_reg, try right.clone(self.allocator));
                        return;
                    }

                    // Handle logical NOT operator
                    if (builtin_op == .Not) {
                        if (arg_count != 1) {
                            debug.log("Not operator requires exactly 1 argument, got {}", .{arg_count});
                            return error.ArgumentCountMismatch;
                        }

                        // Get the argument and check if it's truthy
                        var value = try self.getRegister(func_reg + 1);
                        defer value.deinit(self.allocator);
                        
                        const is_truthy = switch (value) {
                            .Nil => false,
                            .Bool => |b| b,
                            .Int => |i| i != 0,
                            .Float => |f| f != 0.0,
                            .String => |s| s.len > 0,
                            .Array => |a| a.len > 0,
                            .Map => |m| m.count() > 0,
                            else => true,
                        };
                        
                        try self.setRegister(dst_reg, .{ .Bool = !is_truthy });
                        return;
                    }

                    // Handle GC operations
                    if (builtin_op == .GCCollect) {
                        if (self.garbage_collector) |gc_ptr| {
                            try gc_ptr.collect();
                            try self.setRegister(dst_reg, .Nil);
                        } else {
                            try self.setRegister(dst_reg, .{ .Bool = false });
                        }
                        return;
                    }

                    if (builtin_op == .GCDisable) {
                        if (self.garbage_collector) |gc_ptr| {
                            gc_ptr.disable();
                            try self.setRegister(dst_reg, .{ .Bool = true });
                        } else {
                            try self.setRegister(dst_reg, .{ .Bool = false });
                        }
                        return;
                    }

                    if (builtin_op == .GCEnable) {
                        if (self.garbage_collector) |gc_ptr| {
                            gc_ptr.enable();
                            try self.setRegister(dst_reg, .{ .Bool = true });
                        } else {
                            try self.setRegister(dst_reg, .{ .Bool = false });
                        }
                        return;
                    }

                    if (builtin_op == .GCStats) {
                        if (self.garbage_collector) |gc_ptr| {
                            const stats = gc_ptr.getStats();
                            // For now, return a simple map with stats
                            var stats_map = std.StringHashMap(types.Value).init(self.allocator);
                            try stats_map.put("collections", .{ .Int = @intCast(stats.collections) });
                            try stats_map.put("total_allocated", .{ .Int = @intCast(stats.total_allocated) });
                            try stats_map.put("total_freed", .{ .Int = @intCast(stats.total_freed) });
                            try self.setRegister(dst_reg, .{ .Map = stats_map });
                        } else {
                            try self.setRegister(dst_reg, .Nil);
                        }
                        return;
                    }

                    // Handle reference operations
                    if (builtin_op == .MakeRef) {
                        if (arg_count != 1) {
                            debug.log("ref requires exactly 1 argument, got {}", .{arg_count});
                            return error.ArgumentCountMismatch;
                        }

                        const value = try self.getRegister(func_reg + 1);
                        const ref_value = try self.allocator.create(types.Value);
                        ref_value.* = try value.clone(self.allocator);
                        
                        try self.setRegister(dst_reg, .{ .Ref = ref_value });
                        return;
                    }

                    if (builtin_op == .RefGet) {
                        if (arg_count != 1) {
                            debug.log("deref requires exactly 1 argument, got {}", .{arg_count});
                            return error.ArgumentCountMismatch;
                        }

                        var ref_val = try self.getRegister(func_reg + 1);
                        defer ref_val.deinit(self.allocator);
                        
                        if (ref_val != .Ref) {
                            debug.log("deref requires a reference, got {}", .{ref_val});
                            return error.TypeMismatch;
                        }
                        
                        const value = try ref_val.Ref.clone(self.allocator);
                        try self.setRegister(dst_reg, value);
                        return;
                    }

                    if (builtin_op == .RefSet) {
                        if (arg_count != 2) {
                            debug.log("set! requires exactly 2 arguments, got {}", .{arg_count});
                            return error.ArgumentCountMismatch;
                        }

                        const ref_val = try self.getRegister(func_reg + 1);
                        
                        if (ref_val != .Ref) {
                            debug.log("set! requires a reference as first argument, got {}", .{ref_val});
                            return error.TypeMismatch;
                        }
                        
                        const new_value = try self.getRegister(func_reg + 2);
                        
                        // Get the actual reference pointer
                        const ref_ptr = ref_val.Ref;
                        
                        // Deinit old value and replace with new
                        ref_ptr.deinit(self.allocator);
                        ref_ptr.* = try new_value.clone(self.allocator);
                        
                        // set! returns the new value
                        try self.setRegister(dst_reg, try new_value.clone(self.allocator));
                        return;
                    }

                    // Handle less than or equal operator
                    if (builtin_op == .LessEqual) {
                        if (arg_count != 2) {
                            debug.log("LessEqual operator requires exactly 2 arguments, got {}", .{arg_count});
                            return error.ArgumentCountMismatch;
                        }

                        var left = try self.getRegister(func_reg + 1);
                        defer left.deinit(self.allocator);
                        var right = try self.getRegister(func_reg + 2);
                        defer right.deinit(self.allocator);

                        const result = switch (left) {
                            .Int => |left_val| switch (right) {
                                .Int => |right_val| left_val <= right_val,
                                .Float => |right_val| @as(f64, @floatFromInt(left_val)) <= right_val,
                                else => {
                                    debug.log("TypeMismatch in <=: left={}, right={}", .{ left, right });
                                    return error.TypeMismatch;
                                },
                            },
                            .Float => |left_val| switch (right) {
                                .Int => |right_val| left_val <= @as(f64, @floatFromInt(right_val)),
                                .Float => |right_val| left_val <= right_val,
                                else => {
                                    debug.log("TypeMismatch in <=: left={}, right={}", .{ left, right });
                                    return error.TypeMismatch;
                                },
                            },
                            else => {
                                debug.log("TypeMismatch in <=: left={}, right={}", .{ left, right });
                                return error.TypeMismatch;
                            },
                        };

                        try self.setRegister(dst_reg, .{ .Bool = result });
                        return;
                    }

                    // Handle greater than or equal operator
                    if (builtin_op == .GreaterEqual) {
                        if (arg_count != 2) {
                            debug.log("GreaterEqual operator requires exactly 2 arguments, got {}", .{arg_count});
                            return error.ArgumentCountMismatch;
                        }

                        var left = try self.getRegister(func_reg + 1);
                        defer left.deinit(self.allocator);
                        var right = try self.getRegister(func_reg + 2);
                        defer right.deinit(self.allocator);

                        const result = switch (left) {
                            .Int => |left_val| switch (right) {
                                .Int => |right_val| left_val >= right_val,
                                .Float => |right_val| @as(f64, @floatFromInt(left_val)) >= right_val,
                                else => {
                                    debug.log("TypeMismatch in >=: left={}, right={}", .{ left, right });
                                    return error.TypeMismatch;
                                },
                            },
                            .Float => |left_val| switch (right) {
                                .Int => |right_val| left_val >= @as(f64, @floatFromInt(right_val)),
                                .Float => |right_val| left_val >= right_val,
                                else => {
                                    debug.log("TypeMismatch in >=: left={}, right={}", .{ left, right });
                                    return error.TypeMismatch;
                                },
                            },
                            else => {
                                debug.log("TypeMismatch in >=: left={}, right={}", .{ left, right });
                                return error.TypeMismatch;
                            },
                        };

                        try self.setRegister(dst_reg, .{ .Bool = result });
                        return;
                    }

                    // Handle not equal operator
                    if (builtin_op == .Ne) {
                        if (arg_count != 2) {
                            debug.log("NotEqual operator requires exactly 2 arguments, got {}", .{arg_count});
                            return error.ArgumentCountMismatch;
                        }

                        var left = try self.getRegister(func_reg + 1);
                        defer left.deinit(self.allocator);
                        var right = try self.getRegister(func_reg + 2);
                        defer right.deinit(self.allocator);

                        // Use the same equality logic as Equal but negate the result
                        const eq_result = switch (left) {
                            .Int => |left_val| switch (right) {
                                .Int => |right_val| left_val == right_val,
                                .Float => |right_val| @as(f64, @floatFromInt(left_val)) == right_val,
                                else => false,
                            },
                            .Float => |left_val| switch (right) {
                                .Int => |right_val| left_val == @as(f64, @floatFromInt(right_val)),
                                .Float => |right_val| left_val == right_val,
                                else => false,
                            },
                            .String => |left_val| switch (right) {
                                .String => |right_val| std.mem.eql(u8, left_val, right_val),
                                else => false,
                            },
                            .Bool => |left_val| switch (right) {
                                .Bool => |right_val| left_val == right_val,
                                else => false,
                            },
                            .Nil => switch (right) {
                                .Nil => true,
                                else => false,
                            },
                            else => false,
                        };

                        try self.setRegister(dst_reg, .{ .Bool = !eq_result });
                        return;
                    }

                    // Handle other built-in operators here...
                    debug.log("Unsupported built-in operator: {any}", .{builtin_op});
                    return error.UnsupportedFunction;
                }
                
                // Handle standard library functions
                if (func_val == .StdlibFunction) {
                    try self.executeStdlibFunction(func_val.StdlibFunction, dst_reg, func_reg, arg_count);
                    return;
                }

                if (func_val == .Variable) {
                    const func_name = func_val.Variable.name;

                    // Handle print function by name (for backward compatibility)
                    if (std.mem.eql(u8, func_name, "print")) {
                        // Print can take any number of arguments
                        for (0..arg_count) |i| {
                            const arg_reg = func_reg + 1 + @as(u16, @intCast(i)); // Arguments follow function register
                            var arg = try self.getRegister(arg_reg);
                            defer arg.deinit(self.allocator);

                            // Print without newline except for the last argument
                            switch (arg) {
                                .Int => |val| try self.stdout.print("{d}", .{val}),
                                .String => |str| try self.stdout.print("{s}", .{str}),
                                .Symbol => |sym| try self.stdout.print("{s}", .{sym}),
                                .Bool => |b| try self.stdout.print("{}", .{b}),
                                .Float => |f| try self.stdout.print("{d}", .{f}),
                                .Nil => try self.stdout.print("nil", .{}),
                                .Array => |arr| {
                                    try self.stdout.print("[", .{});
                                    for (arr, 0..) |item, idx| {
                                        if (idx > 0) try self.stdout.print(" ", .{});
                                        switch (item) {
                                            .Int => |val| try self.stdout.print("{d}", .{val}),
                                            .String => |str| try self.stdout.print("\"{s}\"", .{str}),
                                            .Symbol => |sym| try self.stdout.print("{s}", .{sym}),
                                            .Bool => |b| try self.stdout.print("{}", .{b}),
                                            .Float => |f| try self.stdout.print("{d}", .{f}),
                                            .Nil => try self.stdout.print("nil", .{}),
                                            else => try self.stdout.print("{any}", .{item}),
                                        }
                                    }
                                    try self.stdout.print("]", .{});
                                },
                                .Map => |map| try self.stdout.print("{any}", .{map}),
                                .ReturnAddress => |addr| try self.stdout.print("ReturnAddress: {any}", .{addr}),
                                .Function => |func| try self.stdout.print("Function: {s}", .{func.name}),
                                .Variable => |var_name| try self.stdout.print("{s}", .{var_name.name}),
                                .BuiltinOperator => |op| try self.stdout.print("BuiltinOperator: {any}", .{op}),
                                .Class => |class| try self.stdout.print("Class: {s}", .{class.name}),
                                .Object => |obj_id| {
                                    if (self.getObjectById(obj_id)) |obj| {
                                        try self.stdout.print("Object of {s}", .{obj.class.name});
                                    } else {
                                        try self.stdout.print("Object(invalid ID: {})", .{obj_id});
                                    }
                                },
                                .Module => |module| try self.stdout.print("Module: {s}", .{module.id}),
                                .StdlibFunction => |func| try self.stdout.print("StdlibFunction: {s}", .{@tagName(func)}),
                                .FileHandle => |handle| try self.stdout.print("FileHandle: {s}", .{handle.path}),
                                .CPtr => |ptr| {
                                    if (ptr) |p| {
                                        try self.stdout.print("CPtr: {*}", .{p});
                                    } else {
                                        try self.stdout.print("CPtr: null", .{});
                                    }
                                },
                                .CFunction => |func| try self.stdout.print("CFunction: {*}", .{func}),
                                .CStruct => |ptr| try self.stdout.print("CStruct: {*}", .{ptr}),
                                .CArray => |arr| try self.stdout.print("CArray[{} x {}]", .{ arr.len, arr.element_size }),
                                .Error => |err| try self.stdout.print("Error({s}): {s}", .{ err.type, err.message }),
                                .FFIFunction => |name| try self.stdout.print("FFIFunction: {s}", .{name}),
                                .NativeFunction => try self.stdout.print("NativeFunction", .{}),
                                .CCallback => |cb| try self.stdout.print("CCallback({s})", .{cb.signature orelse "dynamic"}),
                                .Ref => |ref| {
                                    try self.stdout.print("Ref -> ", .{});
                                    // Print referenced value (simplified)
                                    switch (ref.*) {
                                        .Int => |val| try self.stdout.print("{d}", .{val}),
                                        .String => |str| try self.stdout.print("{s}", .{str}),
                                        .Bool => |b| try self.stdout.print("{}", .{b}),
                                        .Float => |f| try self.stdout.print("{d}", .{f}),
                                        .Nil => try self.stdout.print("nil", .{}),
                                        else => try self.stdout.print("<ref>", .{}),
                                    }
                                },
                            }

                            // Add space between arguments except for the last one
                            if (i < arg_count - 1) {
                                try self.stdout.print(" ", .{});
                            }
                        }

                        // Add newline after all arguments
                        try self.stdout.print("\n", .{});

                        // Print function returns nil
                        try self.setRegister(dst_reg, .{ .Nil = {} });
                        return;
                    }

                    // Check if this is an FFI function
                    if (self.ffi_runtime) |ffi_runtime_ptr| {
                        if (ffi_runtime_ptr.functions.contains(func_name)) {
                            // Collect arguments
                            var args = std.ArrayList(types.Value).init(self.allocator);
                            defer args.deinit();
                            
                            for (0..arg_count) |i| {
                                const arg_reg = func_reg + 1 + @as(u16, @intCast(i));
                                const arg = try self.getRegister(arg_reg);
                                try args.append(arg);
                            }
                            
                            // Call FFI function
                            const result = try ffi_runtime_ptr.callFunction(func_name, args.items);
                            try self.setRegister(dst_reg, result);
                            return;
                        }
                    }
                    
                    // Handle other built-in functions here...
                    debug.log("Unsupported built-in function: {s}", .{func_name});
                    return error.UnsupportedFunction;
                }

                // Handle FFI functions
                if (func_val == .FFIFunction) {
                    const ffi_name = func_val.FFIFunction;
                    debug.log("Calling FFI function: {s}", .{ffi_name});
                    
                    if (self.ffi_runtime) |ffi_runtime_ptr| {
                        // Collect arguments
                        var args = try self.allocator.alloc(types.Value, arg_count);
                        defer self.allocator.free(args);
                        
                        for (0..arg_count) |i| {
                            const arg_reg = func_reg + 1 + @as(u16, @intCast(i));
                            args[i] = try self.getRegister(arg_reg);
                        }
                        
                        // Call the FFI function
                        const result = try ffi_runtime_ptr.callFunction(ffi_name, args);
                        
                        // Clean up arguments
                        for (args) |*arg| {
                            arg.deinit(self.allocator);
                        }
                        
                        // Store result
                        try self.setRegister(dst_reg, result);
                        return;
                    } else {
                        debug.log("No FFI runtime available", .{});
                        return error.FunctionNotResolved;
                    }
                }
                
                // Handle user-defined functions
                if (func_val == .Function) {
                    const user_func = func_val.Function;
                    debug.log("Calling user-defined function: {s}", .{user_func.name});

                    // Set up new call frame for the function
                    const new_register_base = self.next_free_register;
                    const frame = CallFrame.init(self.current_func, new_register_base, self.current_register_base, self.pc + 1, dst_reg);
                    try self.call_frames.append(frame);

                    // Allocate registers for the function (parameters + locals)
                    const needed_regs = @as(u16, @intCast(user_func.param_count + user_func.register_count));
                    const allocated_base = try self.allocateRegisters(needed_regs);

                    // Copy arguments to parameter registers
                    const expected_params = user_func.param_count;
                    const has_rest_param = user_func.rest_param != null;
                    
                    // Copy regular parameters
                    // If there's a rest parameter, the last parameter slot is for the rest array
                    const regular_params = if (has_rest_param) expected_params - 1 else @min(expected_params, arg_count);
                    for (0..regular_params) |i| {
                        // Arguments should be in registers following the function register
                        const arg_reg = func_reg + 1 + @as(u16, @intCast(i));
                        const arg = try self.getRegister(arg_reg);
                        debug.log("Loading argument {} from R{}: {any}", .{ i, arg_reg, arg });

                        const dest_reg = allocated_base + @as(u16, @intCast(i));
                        // Directly store argument into the absolute register of the new frame
                        while (dest_reg >= self.registers.items.len) {
                            try self.registers.append(.{ .Nil = {} });
                        }
                        self.registers.items[dest_reg].deinit(self.allocator);
                        self.registers.items[dest_reg] = arg;

                        debug.log("Stored argument {} in R{} (base + {})", .{ i, dest_reg, i });
                    }
                    
                    // Handle rest parameter - collect extra arguments into an array
                    if (has_rest_param) {
                        var rest_args = std.ArrayList(types.Value).init(self.allocator);
                        errdefer rest_args.deinit();
                        
                        // Collect extra arguments beyond regular parameters
                        if (arg_count > regular_params) {
                            for (regular_params..arg_count) |i| {
                                const arg_reg = func_reg + 1 + @as(u16, @intCast(i));
                                const arg = try self.getRegister(arg_reg);
                                try rest_args.append(arg);
                            }
                        }
                        
                        // Store rest array in the last parameter position
                        const rest_array = types.Value{ .Array = try rest_args.toOwnedSlice() };
                        const dest_reg = allocated_base + @as(u16, @intCast(regular_params));
                        while (dest_reg >= self.registers.items.len) {
                            try self.registers.append(.{ .Nil = {} });
                        }
                        self.registers.items[dest_reg].deinit(self.allocator);
                        self.registers.items[dest_reg] = rest_array;
                        
                        debug.log("Stored rest array with {} elements in R{}", .{ if (arg_count > regular_params) arg_count - regular_params else 0, dest_reg });
                    } else {
                        // No rest parameter - fill missing parameters with nil
                        for (arg_count..expected_params) |i| {
                            const dest_reg = allocated_base + @as(u16, @intCast(i));
                            while (dest_reg >= self.registers.items.len) {
                                try self.registers.append(.{ .Nil = {} });
                            }
                            self.registers.items[dest_reg].deinit(self.allocator);
                            self.registers.items[dest_reg] = .{ .Nil = {} };
                            debug.log("Filled missing parameter {} with nil in R{}", .{ i, dest_reg });
                        }
                    }

                    // Switch to the new function
                    self.current_func = user_func;
                    self.pc = 0; // Start at beginning of function
                    self.current_register_base = allocated_base;
                    self.function_called = true; // Don't increment PC

                    return;
                }

                debug.log("User-defined function calls not yet implemented for this function type", .{});
                return error.NotImplemented;
            },
            .Return => {
                // Register-based return: Return [Rs] (optional return value)
                debug.log("Return operation", .{});
                debug.log("Current register count: {}", .{self.registers.items.len});

                // Get return value if specified
                var return_value: ?types.Value = null;
                if (instruction.src1) |return_reg| {
                    return_value = try self.getRegister(return_reg);
                    debug.log("Return value from R{}: {any}", .{ return_reg, return_value.? });

                    // If the specified register contains Nil, try to find a non-Nil register
                    // This handles cases where control flow skipped some instructions
                    if (return_value.? == .Nil) {
                        debug.log("Return register R{} contains Nil, searching for non-Nil value", .{return_reg});
                        // Search backwards from the specified register to find a non-Nil value
                        var search_reg = return_reg;
                        while (search_reg > 0) {
                            search_reg -= 1;
                            var candidate_value = try self.getRegister(search_reg);
                            if (candidate_value != .Nil) {
                                debug.log("Found non-Nil value in R{}: {any}", .{ search_reg, candidate_value });
                                candidate_value.deinit(self.allocator);
                                return_value = try self.getRegister(search_reg); // Get a fresh copy
                                break;
                            }
                            candidate_value.deinit(self.allocator);
                        }
                    }
                } else {
                    debug.log("Return with no value", .{});
                    return_value = .{ .Nil = {} };
                }

                // If we have call frames, restore the previous one
                debug.log("Return: call_frames.len = {}", .{self.call_frames.items.len});
                if (self.call_frames.items.len > 0) {
                    const frame = self.call_frames.items[self.call_frames.items.len - 1];
                    _ = self.call_frames.pop();

                    // Restore the previous function context first
                    self.current_func = frame.caller_func;
                    self.pc = frame.return_addr;
                    self.current_register_base = frame.prev_register_base;

                    // Store return value in the destination register of the calling context
                    if (return_value) |ret_val| {
                        // Check if we're returning from a constructor
                        if (self.constructor_called) {
                            debug.log("Returning from constructor, ignoring return value", .{});
                            self.constructor_called = false; // Reset the flag
                        } else {
                            debug.log("Storing return value in R{}: {any}", .{ frame.return_reg, ret_val });
                            if (frame.caller_func) |caller| {
                                debug.log("Caller was: {s}", .{caller.name});
                            }
                            try self.setRegister(frame.return_reg, ret_val);
                        }
                    }

                    debug.log("Restored call frame: pc={}, base={}", .{ self.pc, self.current_register_base });

                    // Set flag to prevent PC increment after return
                    self.function_called = true;
                } else {
                    // No call frames - this is the main function returning
                    debug.log("Main function returning", .{});
                    if (return_value) |*ret_val| {
                        ret_val.deinit(self.allocator);
                    }
                    return; // Exit the VM
                }
            },
            .Jump => {
                // Register-based jump: Jump #target_address
                const target = if (instruction.immediate) |imm| switch (imm) {
                    .Int => @as(usize, @intCast(imm.Int)),
                    else => return error.TypeMismatch,
                } else return error.UnsupportedInstruction;

                debug.log("Jump instruction: jump to {}", .{target});

                // Set PC to target address (minus 1 because it will be incremented)
                self.pc = target - 1;
            },
            .JumpIfFalse => {
                // Register-based conditional jump: JumpIfFalse Rs, #target_address
                const condition_reg = instruction.src1 orelse return error.UnsupportedInstruction;
                const target = if (instruction.immediate) |imm| switch (imm) {
                    .Int => @as(usize, @intCast(imm.Int)),
                    else => return error.TypeMismatch,
                } else return error.UnsupportedInstruction;

                debug.log("JumpIfFalse instruction: if !R{} then jump to {}", .{ condition_reg, target });

                var condition_value = try self.getRegister(condition_reg);
                defer condition_value.deinit(self.allocator);

                // Check if condition is false
                const should_jump = switch (condition_value) {
                    .Bool => |b| !b,
                    .Nil => true, // nil is considered false
                    .Int => |i| i == 0, // 0 is considered false
                    else => false, // other values are considered true
                };

                if (should_jump) {
                    // Set PC to target address (minus 1 because it will be incremented)
                    self.pc = target - 1;
                }
            },
            .DefineClass => {
                // DefineClass is handled at compile time, classes are loaded as constants
                debug.log("DefineClass instruction should not appear at runtime", .{});
                return error.InvalidInstruction;
            },
            .New => {
                // Create object instance: New Rd, class_reg, [arg1, arg2, ...]
                const dst_reg = instruction.dst orelse return error.InvalidInstruction;
                const class_reg = instruction.src1 orelse return error.InvalidInstruction;

                // Get the class
                const class_val = try self.getRegister(class_reg);
                // Don't defer deinit for class values as they are shared

                if (class_val != .Class) {
                    debug.log("New instruction requires a Class, got {}", .{class_val});
                    return error.TypeMismatch;
                }

                const class = class_val.Class;

                // Create the object instance
                var obj = try self.allocator.create(types.ObjectInstance);
                const obj_id = self.next_object_id;
                self.next_object_id += 1;
                obj.* = types.ObjectInstance.init(self.allocator, class, obj_id);

                // Add to object pool
                try self.object_pool.append(obj);

                // Initialize fields with default values
                var field_iter = class.fields.iterator();
                while (field_iter.next()) |entry| {
                    const field_def = entry.value_ptr.*;
                    // Duplicate the field name for the object's own copy
                    const field_name_copy = try self.allocator.dupe(u8, entry.key_ptr.*);
                    if (field_def.default_value) |default| {
                        try obj.fields.put(field_name_copy, try default.clone(self.allocator));
                    } else {
                        try obj.fields.put(field_name_copy, .{ .Nil = {} });
                    }
                }

                try self.setRegister(dst_reg, .{ .Object = obj_id });
                debug.log("Created instance of class {s} in R{}", .{ class.name, dst_reg });

                // Check if there's a constructor to call
                // Constructor is named ClassName_init
                const ctor_name = try std.fmt.allocPrint(self.allocator, "{s}_init", .{class.name});
                defer self.allocator.free(ctor_name);

                // std.debug.print("Looking for constructor: {s}\n", .{ctor_name});
                // std.debug.print("Class has {} methods\n", .{class.methods.count()});
                // var method_iter = class.methods.iterator();
                // while (method_iter.next()) |entry| {
                //     std.debug.print("  Method: {s}\n", .{entry.key_ptr.*});
                // }

                if (class.methods.get(ctor_name)) |ctor_method| {
                    debug.log("Found constructor {s} in class {s}, calling it", .{ ctor_name, class.name });

                    // Get number of constructor arguments from immediate value
                    const arg_count: usize = if (instruction.immediate) |imm| blk: {
                        if (imm == .Int) break :blk @intCast(imm.Int);
                        break :blk 0;
                    } else 0;

                    // Set up new call frame for the constructor
                    const new_register_base = self.next_free_register;
                    const frame = CallFrame.init(self.current_func, new_register_base, self.current_register_base, self.pc + 1, dst_reg);
                    try self.call_frames.append(frame);

                    // Allocate registers for the constructor (self + parameters + locals)
                    const needed_regs = @as(u16, @intCast(ctor_method.param_count + ctor_method.register_count));
                    const allocated_base = try self.allocateRegisters(needed_regs);

                    // Set 'self' as first parameter (the newly created object)
                    const self_value = types.Value{ .Object = obj_id };
                    debug.log("Setting self (object ID {}) in register {}", .{ obj_id, allocated_base });
                    // Directly store 'self' into the absolute register of the new frame
                    while (allocated_base >= self.registers.items.len) {
                        try self.registers.append(.{ .Nil = {} });
                    }
                    self.registers.items[allocated_base].deinit(self.allocator);
                    self.registers.items[allocated_base] = self_value;

                    // Copy constructor arguments to parameter registers (after 'self')
                    for (0..arg_count) |i| {
                        const arg_reg = class_reg + 1 + @as(u16, @intCast(i));
                        const arg = try self.getRegister(arg_reg);
                        const dest_reg = allocated_base + 1 + @as(u16, @intCast(i));
                        // Directly store argument into the absolute register of the new frame
                        while (dest_reg >= self.registers.items.len) {
                            try self.registers.append(.{ .Nil = {} });
                        }
                        self.registers.items[dest_reg].deinit(self.allocator);
                        self.registers.items[dest_reg] = arg;
                    }

                    // Switch to the constructor
                    self.current_func = ctor_method;
                    self.pc = 0;
                    self.current_register_base = allocated_base;
                    self.function_called = true;

                    debug.log("Called constructor with {} args, self is object ID {}", .{ arg_count, obj_id });

                    // Mark that we called a constructor - we need to ignore its return value
                    self.constructor_called = true;
                } else {
                    debug.log("No constructor found for class {s}", .{class.name});
                }
            },
            .Get => {
                // Universal get for objects, maps, arrays, modules
                const dst_reg = instruction.dst orelse return error.InvalidInstruction;
                const obj_reg = instruction.src1 orelse return error.InvalidInstruction;
                const key_reg = instruction.src2 orelse return error.InvalidInstruction;

                var obj_val = try self.getRegister(obj_reg);
                defer obj_val.deinit(self.allocator);

                var key_val = try self.getRegister(key_reg);
                defer key_val.deinit(self.allocator);

                switch (obj_val) {
                    .Object => |obj_id| {
                        const obj = self.getObjectById(obj_id) orelse return error.InvalidInstruction;
                        if (key_val != .String) return error.TypeMismatch;
                        const field_name = key_val.String;
                        if (obj.fields.get(field_name)) |field_value| {
                            try self.setRegister(dst_reg, try field_value.clone(self.allocator));
                        } else {
                            return error.FieldNotFound;
                        }
                    },
                    .Map => |map| {
                        if (key_val != .String) return error.TypeMismatch;
                        const key = key_val.String;
                        if (map.get(key)) |value| {
                            try self.setRegister(dst_reg, try value.clone(self.allocator));
                        } else {
                            try self.setRegister(dst_reg, .Nil);
                        }
                    },
                    .Array => |array| {
                        if (key_val != .Int) return error.TypeMismatch;
                        const index = @as(usize, @intCast(key_val.Int));
                        if (index >= array.len) return error.StackOverflow;
                        try self.setRegister(dst_reg, try array[index].clone(self.allocator));
                    },
                    .Module => |module| {
                        if (key_val != .String) return error.TypeMismatch;
                        const member_name = key_val.String;
                        if (module.getMember(member_name)) |member| {
                            try self.setRegister(dst_reg, try member.clone(self.allocator));
                        } else {
                            return error.FieldNotFound;
                        }
                    },
                    else => return error.TypeMismatch,
                }
            },
            .Set => {
                const obj_reg = instruction.src1 orelse return error.InvalidInstruction;
                const key_reg = instruction.src2 orelse return error.InvalidInstruction;
                const value_reg = instruction.dst orelse return error.InvalidInstruction; // Using dst as value reg

                var obj_val = try self.getRegister(obj_reg);
                defer obj_val.deinit(self.allocator);

                var key_val = try self.getRegister(key_reg);
                defer key_val.deinit(self.allocator);

                switch (obj_val) {
                    .Object => |obj_id| {
                        const obj = self.getObjectById(obj_id) orelse return error.InvalidInstruction;
                        if (key_val != .String) return error.TypeMismatch;
                        const field_name = key_val.String;
                        const value = try self.getRegister(value_reg);
                        if (obj.fields.get(field_name)) |old_value| {
                            var old_value_copy = old_value;
                            old_value_copy.deinit(self.allocator);
                            try obj.fields.put(field_name, value);
                        } else {
                            try obj.fields.put(try self.allocator.dupe(u8, field_name), value);
                        }
                    },
                    .Map => |map| {
                        // Maps have value semantics, so mutations don't persist
                        // TODO: Implement proper map mutation support
                        _ = map;
                        debug.log("Warning: Map mutations don't persist due to value semantics", .{});
                    },
                    .Array => |array| {
                        if (key_val != .Int) return error.TypeMismatch;
                        const index = @as(usize, @intCast(key_val.Int));
                        if (index >= array.len) return error.StackOverflow;
                        const value = try self.getRegister(value_reg);
                        array[index].deinit(self.allocator);
                        array[index] = value;
                    },
                    else => return error.TypeMismatch,
                }
            },
            .CallMethod => {
                // Call method: CallMethod Rd, obj_reg, method_name, [arg1, arg2, ...]
                const dst_reg = instruction.dst orelse return error.InvalidInstruction;
                const obj_reg = instruction.src1 orelse return error.InvalidInstruction;
                const method_name = instruction.var_name orelse return error.InvalidInstruction;
                const arg_count = if (instruction.immediate) |imm|
                    if (imm == .Int) @as(usize, @intCast(imm.Int)) else 0
                else
                    0;

                var obj_val = try self.getRegister(obj_reg);
                defer obj_val.deinit(self.allocator);

                // Handle method calls on modules
                if (obj_val == .Module) {
                    const module = obj_val.Module;
                    debug.log("DEBUG: Method call on module: {s}.{s}", .{ module.id, method_name });

                    // Special handling for get_member
                    if (std.mem.eql(u8, method_name, "get_member")) {
                        // get_member(key) - get a member from the module
                        if (arg_count != 1) {
                            debug.log("get_member expects 1 argument, got {}", .{arg_count});
                            return error.ArgumentCountMismatch;
                        }

                        // Get the key argument
                        const key_reg = obj_reg + 1;
                        var key_val = try self.getRegister(key_reg);
                        defer key_val.deinit(self.allocator);

                        if (key_val != .String) {
                            debug.log("get_member key must be String, got {}", .{key_val});
                            return error.TypeMismatch;
                        }

                        const member_name = key_val.String;
                        if (module.getMember(member_name)) |member| {
                            try self.setRegister(dst_reg, try member.clone(self.allocator));
                            debug.log("Found module member {s}", .{member_name});
                        } else {
                            debug.log("Module member {s} not found", .{member_name});
                            try self.setRegister(dst_reg, .{ .Nil = {} });
                        }
                        return;
                    }

                    // Look for the member in the module's namespace
                    if (module.namespace.members.get(method_name)) |member| {
                        // Found the member, it should be a function
                        if (member == .Function) {
                            const func = member.Function;

                            // Set up new call frame for the function
                            const new_register_base = self.next_free_register;
                            const frame = CallFrame.init(self.current_func, new_register_base, self.current_register_base, self.pc + 1, dst_reg);
                            try self.call_frames.append(frame);

                            // Allocate registers for the function (parameters + locals)
                            const needed_regs = @as(u16, @intCast(func.param_count + func.register_count));
                            const allocated_base = try self.allocateRegisters(needed_regs);

                            // Copy arguments to parameter registers
                            for (0..arg_count) |i| {
                                // Arguments should be in registers following the object register
                                const arg_reg = obj_reg + 1 + @as(u16, @intCast(i));
                                const arg = try self.getRegister(arg_reg);
                                debug.log("Loading argument {} from R{}: {any}", .{ i, arg_reg, arg });

                                const dest_reg = allocated_base + @as(u16, @intCast(i));
                                const absolute_dest_reg = dest_reg - self.current_register_base;
                                self.registers.items[absolute_dest_reg] = arg;
                            }

                            // Switch to the new function
                            self.current_func = func;
                            self.pc = 0;
                            self.current_register_base = new_register_base;

                            debug.log("Switching to module function with {} params", .{func.param_count});
                            self.function_called = true;
                            return;
                        } else {
                            debug.log("Module member {s} is not a function", .{method_name});
                            return error.TypeMismatch;
                        }
                    } else {
                        debug.log("Method {s} not found in module {s}", .{ method_name, module.id });
                        return error.MethodNotFound;
                    }
                }

                // Handle get_member on maps
                if (obj_val == .Map and std.mem.eql(u8, method_name, "get_member")) {
                    // get_member(key) - get a value from the map
                    if (arg_count != 1) {
                        debug.log("get_member expects 1 argument, got {}", .{arg_count});
                        return error.ArgumentCountMismatch;
                    }

                    // Get the key argument
                    const key_reg = obj_reg + 1;
                    var key_val = try self.getRegister(key_reg);
                    defer key_val.deinit(self.allocator);

                    if (key_val != .String) {
                        debug.log("get_member key must be String, got {}", .{key_val});
                        return error.TypeMismatch;
                    }

                    const key = key_val.String;
                    const map = obj_val.Map;

                    debug.log("Map has {} entries, looking for key: {s}", .{ map.count(), key });
                    var it = map.iterator();
                    while (it.next()) |entry| {
                        debug.log("  Map entry: {s} => {s}", .{ entry.key_ptr.*, @tagName(entry.value_ptr.*) });
                    }

                    if (map.get(key)) |value| {
                        const cloned_value = try value.clone(self.allocator);
                        debug.log("Found map entry {s}, value type: {s}", .{ key, @tagName(cloned_value) });
                        try self.setRegister(dst_reg, cloned_value);
                    } else {
                        debug.log("Map entry {s} not found", .{key});
                        try self.setRegister(dst_reg, .{ .Nil = {} });
                    }
                    return;
                }

                // Handle set_member on maps
                if (obj_val == .Map and std.mem.eql(u8, method_name, "set_member")) {
                    // set_member(key, value) - set a value in the map
                    if (arg_count != 2) {
                        debug.log("set_member expects 2 arguments, got {}", .{arg_count});
                        return error.ArgumentCountMismatch;
                    }

                    // Get the key and value arguments
                    const key_reg = obj_reg + 1;
                    const value_reg = obj_reg + 2;
                    var key_val = try self.getRegister(key_reg);
                    defer key_val.deinit(self.allocator);
                    var value_val = try self.getRegister(value_reg);
                    defer value_val.deinit(self.allocator);

                    if (key_val != .String) {
                        debug.log("set_member key must be String, got {}", .{key_val});
                        return error.TypeMismatch;
                    }

                    const key = key_val.String;
                    var map = obj_val.Map;

                    // Note: Maps have value semantics, so this mutation won't persist
                    // This is a known limitation mentioned in CLAUDE.md
                    if (map.get(key)) |old_value| {
                        var old_value_copy = old_value;
                        old_value_copy.deinit(self.allocator);
                    }
                    try map.put(try self.allocator.dupe(u8, key), try value_val.clone(self.allocator));

                    // Return the map itself for chaining
                    try self.setRegister(dst_reg, obj_val);
                    debug.log("Set map entry {s} (note: may not persist due to value semantics)", .{key});
                    return;
                }

                // Handle get_member on objects
                if (obj_val == .Object and std.mem.eql(u8, method_name, "get_member")) {
                    // get_member(key) - get a field from the object
                    if (arg_count != 1) {
                        debug.log("get_member expects 1 argument, got {}", .{arg_count});
                        return error.ArgumentCountMismatch;
                    }

                    // Get the key argument
                    const key_reg = obj_reg + 1;
                    var key_val = try self.getRegister(key_reg);
                    defer key_val.deinit(self.allocator);

                    if (key_val != .String) {
                        debug.log("get_member key must be String, got {}", .{key_val});
                        return error.TypeMismatch;
                    }

                    const field_name = key_val.String;
                    const obj = self.getObjectById(obj_val.Object) orelse {
                        debug.log("Object with ID {} not found in pool", .{obj_val.Object});
                        return error.InvalidInstruction;
                    };

                    if (obj.fields.get(field_name)) |field_value| {
                        try self.setRegister(dst_reg, try field_value.clone(self.allocator));
                        debug.log("Found object field {s}", .{field_name});
                    } else {
                        debug.log("Object field {s} not found", .{field_name});
                        try self.setRegister(dst_reg, .{ .Nil = {} });
                    }
                    return;
                }

                // Handle set_member on objects
                if (obj_val == .Object and std.mem.eql(u8, method_name, "set_member")) {
                    // set_member(key, value) - set a field on the object
                    if (arg_count != 2) {
                        debug.log("set_member expects 2 arguments, got {}", .{arg_count});
                        return error.ArgumentCountMismatch;
                    }

                    // Get the key and value arguments
                    const key_reg = obj_reg + 1;
                    const value_reg = obj_reg + 2;
                    var key_val = try self.getRegister(key_reg);
                    defer key_val.deinit(self.allocator);
                    var value_val = try self.getRegister(value_reg);
                    defer value_val.deinit(self.allocator);

                    if (key_val != .String) {
                        debug.log("set_member key must be String, got {}", .{key_val});
                        return error.TypeMismatch;
                    }

                    const field_name = key_val.String;
                    const obj = self.getObjectById(obj_val.Object) orelse {
                        debug.log("Object with ID {} not found in pool", .{obj_val.Object});
                        return error.InvalidInstruction;
                    };

                    // Update or insert the field
                    if (obj.fields.get(field_name)) |old_value| {
                        // Field exists, update it
                        var old_value_copy = old_value;
                        old_value_copy.deinit(self.allocator);
                        try obj.fields.put(field_name, try value_val.clone(self.allocator));
                    } else {
                        // New field, need to duplicate the key
                        try obj.fields.put(try self.allocator.dupe(u8, field_name), try value_val.clone(self.allocator));
                    }

                    // Return the object itself for chaining
                    try self.setRegister(dst_reg, obj_val);
                    debug.log("Set object field {s}", .{field_name});
                    return;
                }

                // Get the class for the value (works for both objects and primitives)
                const class = if (obj_val == .Object) blk: {
                    const obj = self.getObjectById(obj_val.Object) orelse {
                        debug.log("Object with ID {} not found in pool", .{obj_val.Object});
                        return error.InvalidInstruction;
                    };
                    break :blk obj.class;
                } else if (self.core_classes) |core|
                    core.getClassForValue(obj_val)
                else {
                    debug.log("Core classes not initialized", .{});
                    return error.InvalidInstruction;
                };

                // Look up the method in the class
                debug.log("Looking for method {s} in class {s}", .{ method_name, class.name });
                debug.log("Class has {} methods", .{class.methods.count()});
                var method_iter = class.methods.iterator();
                while (method_iter.next()) |entry| {
                    debug.log("  Method: {s}", .{entry.key_ptr.*});
                }

                // First check if method is in the class (including inherited methods)
                var current_class: ?*types.ClassDefinition = class;
                var found_method: ?*bytecode.Function = null;

                while (current_class) |c| {
                    if (c.methods.get(method_name)) |method| {
                        found_method = method;
                        break;
                    }
                    current_class = c.parent;
                }

                if (found_method) |method| {
                    debug.log("Calling method {s} on value of class {s}", .{ method_name, class.name });

                    // Set up new call frame for the method
                    const new_register_base = self.next_free_register;
                    const frame = CallFrame.init(self.current_func, new_register_base, self.current_register_base, self.pc + 1, dst_reg);
                    try self.call_frames.append(frame);

                    // Allocate registers for the method (self + parameters + locals)
                    const needed_regs = @as(u16, @intCast(method.param_count + method.register_count));
                    const allocated_base = try self.allocateRegisters(needed_regs);

                    // Set 'self' as first parameter
                    const self_value = try obj_val.clone(self.allocator);
                    // Directly store 'self' into the absolute register of the new frame
                    while (allocated_base >= self.registers.items.len) {
                        try self.registers.append(.{ .Nil = {} });
                    }
                    self.registers.items[allocated_base].deinit(self.allocator);
                    self.registers.items[allocated_base] = self_value;

                    // Copy arguments to parameter registers (after 'self')
                    for (0..arg_count) |i| {
                        const arg_reg = obj_reg + 1 + @as(u16, @intCast(i));
                        const arg = try self.getRegister(arg_reg);
                        const dest_reg = allocated_base + 1 + @as(u16, @intCast(i));
                        // Directly store argument into the absolute register of the new frame
                        while (dest_reg >= self.registers.items.len) {
                            try self.registers.append(.{ .Nil = {} });
                        }
                        self.registers.items[dest_reg].deinit(self.allocator);
                        self.registers.items[dest_reg] = arg;
                    }

                    // Switch to the method
                    self.current_func = method;
                    self.pc = 0;
                    self.current_register_base = allocated_base;
                    self.next_free_register = allocated_base + needed_regs;
                    self.function_called = true;
                } else {
                    // Try to find a function with the pattern ClassName_methodName
                    const func_name = try std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ class.name, method_name });
                    defer self.allocator.free(func_name);

                    debug.log("Method {s} not found in class, looking for function {s}", .{ method_name, func_name });

                    if (self.variables.get(func_name)) |func_val| {
                        if (func_val == .Function) {
                            const func = func_val.Function;
                            debug.log("Found function {s} as method implementation", .{func_name});

                            // Set up call similar to regular function call but with object as first argument
                            const new_register_base = self.next_free_register;
                            const frame = CallFrame.init(self.current_func, new_register_base, self.current_register_base, self.pc + 1, dst_reg);
                            try self.call_frames.append(frame);

                            // Allocate registers for the function (self + parameters + locals)
                            const needed_regs = @as(u16, @intCast(func.param_count + func.register_count));
                            const allocated_base = try self.allocateRegisters(needed_regs);

                            // Set object as first parameter (self)
                            const self_value = try obj_val.clone(self.allocator);
                            while (allocated_base >= self.registers.items.len) {
                                try self.registers.append(.{ .Nil = {} });
                            }
                            self.registers.items[allocated_base].deinit(self.allocator);
                            self.registers.items[allocated_base] = self_value;

                            // Copy arguments to parameter registers (after 'self')
                            for (0..arg_count) |i| {
                                const arg_reg = obj_reg + 1 + @as(u16, @intCast(i));
                                const arg = try self.getRegister(arg_reg);
                                const dest_reg = allocated_base + 1 + @as(u16, @intCast(i));
                                while (dest_reg >= self.registers.items.len) {
                                    try self.registers.append(.{ .Nil = {} });
                                }
                                self.registers.items[dest_reg].deinit(self.allocator);
                                self.registers.items[dest_reg] = arg;
                            }

                            // Switch to the function
                            self.current_func = func;
                            self.pc = 0;
                            self.current_register_base = allocated_base;
                            self.next_free_register = allocated_base + needed_regs;
                            self.function_called = true;
                            return;
                        }
                    }

                    debug.log("Method {s} not found on class {s} or as function {s}", .{ method_name, class.name, func_name });
                    return error.MethodNotFound;
                }
            },
            .ClassName => {
                // Get class name: ClassName Rd, class_reg
                const dst_reg = instruction.dst orelse return error.InvalidInstruction;
                const class_reg = instruction.src1 orelse return error.InvalidInstruction;

                var class_val = try self.getRegister(class_reg);
                defer class_val.deinit(self.allocator);

                if (class_val != .Class) {
                    debug.log("ClassName requires a Class, got {}", .{class_val});
                    return error.TypeMismatch;
                }

                const class = class_val.Class;
                const name = try self.allocator.dupe(u8, class.name);
                try self.setRegister(dst_reg, .{ .String = name });
            },
            .ClassParent => {
                // Two modes:
                // 1. Get parent class: ClassParent Rd, class_reg (no src2)
                // 2. Set parent class: ClassParent class_reg, parent_reg (src2 present)
                const dst_reg = instruction.dst orelse return error.InvalidInstruction;
                const class_reg = instruction.src1;
                
                if (class_reg) |parent_reg| {
                    // Set parent mode: dst_reg is the class, src1 is the parent
                    var class_val = try self.getRegister(dst_reg);
                    defer class_val.deinit(self.allocator);
                    
                    if (class_val != .Class) {
                        debug.log("ClassParent set mode requires a Class in dst, got {}", .{class_val});
                        return error.TypeMismatch;
                    }
                    
                    var parent_val = try self.getRegister(parent_reg);
                    defer parent_val.deinit(self.allocator);
                    
                    if (parent_val != .Class) {
                        debug.log("ClassParent set mode requires a Class as parent, got {}", .{parent_val});
                        return error.TypeMismatch;
                    }
                    
                    // Set the parent on the class
                    const class = class_val.Class;
                    class.parent = parent_val.Class;
                    debug.log("Set parent of class {s} to {s}", .{class.name, parent_val.Class.name});
                } else {
                    // Get parent mode: original behavior
                    var class_val = try self.getRegister(dst_reg);
                    defer class_val.deinit(self.allocator);

                    if (class_val != .Class) {
                        debug.log("ClassParent requires a Class, got {}", .{class_val});
                        return error.TypeMismatch;
                    }

                    const class = class_val.Class;
                    if (class.parent) |parent| {
                        try self.setRegister(dst_reg, .{ .Class = parent });
                    } else {
                        try self.setRegister(dst_reg, .{ .Nil = {} });
                    }
                }
            },
            .Length => {
                // Get length: Length Rd, Rs
                const dst_reg = instruction.dst orelse return error.InvalidInstruction;
                const src_reg = instruction.src1 orelse return error.InvalidInstruction;

                var value = try self.getRegister(src_reg);
                defer value.deinit(self.allocator);

                const length: i64 = switch (value) {
                    .String => |s| @intCast(s.len),
                    .Array => |arr| @intCast(arr.len),
                    .Map => |map| @intCast(map.count()),
                    else => {
                        debug.log("Length not supported for type: {}", .{value});
                        return error.TypeMismatch;
                    },
                };

                try self.setRegister(dst_reg, .{ .Int = length });
            },
            .ArrayGet => {
                // Get array element: ArrayGet Rd, array_reg, index_reg
                const dst_reg = instruction.dst orelse return error.InvalidInstruction;
                const array_reg = instruction.src1 orelse return error.InvalidInstruction;
                const index_reg = instruction.src2 orelse return error.InvalidInstruction;

                var array_val = try self.getRegister(array_reg);
                defer array_val.deinit(self.allocator);
                var index_val = try self.getRegister(index_reg);
                defer index_val.deinit(self.allocator);

                if (array_val != .Array) {
                    debug.log("ArrayGet requires an Array, got {}", .{array_val});
                    return error.TypeMismatch;
                }
                if (index_val != .Int) {
                    debug.log("ArrayGet index must be Int, got {}", .{index_val});
                    return error.TypeMismatch;
                }

                const array = array_val.Array;
                const index = index_val.Int;

                if (index < 0 or index >= array.len) {
                    debug.log("Array index out of bounds: {} (length: {})", .{ index, array.len });
                    return error.InvalidInstruction;
                }

                const element = array[@intCast(index)];
                try self.setRegister(dst_reg, try element.clone(self.allocator));
            },
            .ArraySet => {
                // Set array element: ArraySet array_reg, index_reg, value_reg
                const array_reg = instruction.src1 orelse return error.InvalidInstruction;
                const index_reg = instruction.src2 orelse return error.InvalidInstruction;
                const value_reg = instruction.dst orelse return error.InvalidInstruction; // Using dst for value

                var array_val = try self.getRegister(array_reg);
                defer array_val.deinit(self.allocator);
                var index_val = try self.getRegister(index_reg);
                defer index_val.deinit(self.allocator);
                var value = try self.getRegister(value_reg);
                defer value.deinit(self.allocator);

                if (array_val != .Array) {
                    debug.log("ArraySet requires an Array, got {}", .{array_val});
                    return error.TypeMismatch;
                }
                if (index_val != .Int) {
                    debug.log("ArraySet index must be Int, got {}", .{index_val});
                    return error.TypeMismatch;
                }

                const array = array_val.Array;
                const index = index_val.Int;

                if (index < 0 or index >= array.len) {
                    debug.log("Array index out of bounds: {} (length: {})", .{ index, array.len });
                    return error.InvalidInstruction;
                }

                // Update the array element
                array[@intCast(index)].deinit(self.allocator);
                array[@intCast(index)] = try value.clone(self.allocator);
            },
            .ArrayPush => {
                // Push element to array: ArrayPush Rd, array_reg, value_reg
                const dst_reg = instruction.dst orelse return error.InvalidInstruction;
                const array_reg = instruction.src1 orelse return error.InvalidInstruction;
                const value_reg = instruction.src2 orelse return error.InvalidInstruction;

                var array_val = try self.getRegister(array_reg);
                defer array_val.deinit(self.allocator);
                var value = try self.getRegister(value_reg);
                defer value.deinit(self.allocator);

                if (array_val != .Array) {
                    debug.log("ArrayPush requires an Array, got {}", .{array_val});
                    return error.TypeMismatch;
                }

                const old_array = array_val.Array;
                
                // Create new array with one more element
                var new_array = try self.allocator.alloc(types.Value, old_array.len + 1);
                errdefer self.allocator.free(new_array);
                
                // Copy old elements
                for (old_array, 0..) |elem, i| {
                    new_array[i] = try elem.clone(self.allocator);
                }
                
                // Add new element
                new_array[old_array.len] = try value.clone(self.allocator);
                
                // Set result
                try self.setRegister(dst_reg, .{ .Array = new_array });
            },
            .ArrayPop => {
                // Pop element from array: ArrayPop Rd, array_reg
                const dst_reg = instruction.dst orelse return error.InvalidInstruction;
                const array_reg = instruction.src1 orelse return error.InvalidInstruction;

                var array_val = try self.getRegister(array_reg);
                defer array_val.deinit(self.allocator);

                if (array_val != .Array) {
                    debug.log("ArrayPop requires an Array, got {}", .{array_val});
                    return error.TypeMismatch;
                }

                const array = array_val.Array;
                if (array.len == 0) {
                    // Return nil for empty array
                    try self.setRegister(dst_reg, .{ .Nil = {} });
                } else {
                    // Return the last element
                    const last_elem = array[array.len - 1];
                    try self.setRegister(dst_reg, try last_elem.clone(self.allocator));
                    
                    // Note: We don't modify the original array - Gene uses immutable data structures
                    // If we wanted to return a new array without the last element, we'd need a different approach
                }
            },
            .Substring => {
                // Get substring: Substring Rd, str_reg, start_reg, end_reg
                const dst_reg = instruction.dst orelse return error.InvalidInstruction;
                const str_reg = instruction.src1 orelse return error.InvalidInstruction;
                const start_reg = instruction.src2 orelse return error.InvalidInstruction;
                // For now, using immediate for end position
                const end_val = if (instruction.immediate) |imm| switch (imm) {
                    .Int => @as(usize, @intCast(imm.Int)),
                    else => return error.TypeMismatch,
                } else return error.InvalidInstruction;

                var str_val = try self.getRegister(str_reg);
                defer str_val.deinit(self.allocator);
                var start_val = try self.getRegister(start_reg);
                defer start_val.deinit(self.allocator);

                if (str_val != .String) {
                    debug.log("Substring requires a String, got {}", .{str_val});
                    return error.TypeMismatch;
                }
                if (start_val != .Int) {
                    debug.log("Substring start must be Int, got {}", .{start_val});
                    return error.TypeMismatch;
                }

                const str = str_val.String;
                const start = @as(usize, @intCast(start_val.Int));
                const end = @min(end_val, str.len);

                if (start > str.len) {
                    debug.log("Substring start out of bounds: {} (length: {})", .{ start, str.len });
                    return error.InvalidInstruction;
                }

                const substring = try self.allocator.dupe(u8, str[start..end]);
                try self.setRegister(dst_reg, .{ .String = substring });
            },
            .MapGet => {
                // Get map value: MapGet Rd, map_reg, key_reg
                const dst_reg = instruction.dst orelse return error.InvalidInstruction;
                const map_reg = instruction.src1 orelse return error.InvalidInstruction;
                const key_reg = instruction.src2 orelse return error.InvalidInstruction;

                var map_val = try self.getRegister(map_reg);
                defer map_val.deinit(self.allocator);
                var key_val = try self.getRegister(key_reg);
                defer key_val.deinit(self.allocator);

                if (map_val != .Map) {
                    debug.log("MapGet requires a Map, got {}", .{map_val});
                    return error.TypeMismatch;
                }
                if (key_val != .String) {
                    debug.log("MapGet key must be String, got {}", .{key_val});
                    return error.TypeMismatch;
                }

                const map = map_val.Map;
                const key = key_val.String;

                if (map.get(key)) |value| {
                    try self.setRegister(dst_reg, try value.clone(self.allocator));
                } else {
                    try self.setRegister(dst_reg, .{ .Nil = {} });
                }
            },
            .MapSet => {
                // Set map value: MapSet map_reg, key_reg, value_reg
                const map_reg = instruction.src1 orelse return error.InvalidInstruction;
                const key_reg = instruction.src2 orelse return error.InvalidInstruction;
                const value_reg = if (instruction.immediate) |imm| switch (imm) {
                    .Int => @as(u16, @intCast(imm.Int)),
                    else => return error.TypeMismatch,
                } else return error.InvalidInstruction;

                var map_val = try self.getRegister(map_reg);
                defer map_val.deinit(self.allocator);
                var key_val = try self.getRegister(key_reg);
                defer key_val.deinit(self.allocator);
                var value = try self.getRegister(value_reg);
                defer value.deinit(self.allocator);

                if (map_val != .Map) {
                    debug.log("MapSet requires a Map, got {}", .{map_val});
                    return error.TypeMismatch;
                }
                if (key_val != .String) {
                    debug.log("MapSet key must be String, got {}", .{key_val});
                    return error.TypeMismatch;
                }

                var map = map_val.Map;
                const key = key_val.String;

                // Update or insert the key-value pair
                if (map.get(key)) |old_value| {
                    var old_value_copy = old_value;
                    old_value_copy.deinit(self.allocator);
                }
                try map.put(try self.allocator.dupe(u8, key), try value.clone(self.allocator));
            },
            .IsArray => {
                // Check if value is an array: IsArray Rd, Rs
                const dst_reg = instruction.dst orelse return error.InvalidInstruction;
                const src_reg = instruction.src1 orelse return error.InvalidInstruction;

                var src_val = try self.getRegister(src_reg);
                defer src_val.deinit(self.allocator);

                const is_array = src_val == .Array;
                try self.setRegister(dst_reg, .{ .Bool = is_array });
            },
            .IsMap => {
                // Check if value is a map: IsMap Rd, Rs
                const dst_reg = instruction.dst orelse return error.InvalidInstruction;
                const src_reg = instruction.src1 orelse return error.InvalidInstruction;

                var src_val = try self.getRegister(src_reg);
                defer src_val.deinit(self.allocator);

                const is_map = src_val == .Map;
                try self.setRegister(dst_reg, .{ .Bool = is_map });
            },
            .ArrayContains => {
                // Check if array contains value: ArrayContains Rd, array_reg, value_reg
                const dst_reg = instruction.dst orelse return error.InvalidInstruction;
                const array_reg = instruction.src1 orelse return error.InvalidInstruction;
                const value_reg = instruction.src2 orelse return error.InvalidInstruction;

                var array_val = try self.getRegister(array_reg);
                defer array_val.deinit(self.allocator);
                var search_val = try self.getRegister(value_reg);
                defer search_val.deinit(self.allocator);

                if (array_val != .Array) {
                    debug.log("ArrayContains requires an Array, got {}", .{array_val});
                    return error.TypeMismatch;
                }

                const array = array_val.Array;
                var contains = false;

                // Search for the value in the array
                for (array) |element| {
                    if (try self.valuesEqual(element, search_val)) {
                        contains = true;
                        break;
                    }
                }

                try self.setRegister(dst_reg, .{ .Bool = contains });
            },
            .MapHas => {
                // Check if map has key: MapHas Rd, map_reg, key_reg
                const dst_reg = instruction.dst orelse return error.InvalidInstruction;
                const map_reg = instruction.src1 orelse return error.InvalidInstruction;
                const key_reg = instruction.src2 orelse return error.InvalidInstruction;

                var map_val = try self.getRegister(map_reg);
                defer map_val.deinit(self.allocator);
                var key_val = try self.getRegister(key_reg);
                defer key_val.deinit(self.allocator);

                if (map_val != .Map) {
                    debug.log("MapHas requires a Map, got {}", .{map_val});
                    return error.TypeMismatch;
                }
                if (key_val != .String) {
                    debug.log("MapHas key must be String, got {}", .{key_val});
                    return error.TypeMismatch;
                }

                const map = map_val.Map;
                const key = key_val.String;
                const has_key = map.contains(key);

                try self.setRegister(dst_reg, .{ .Bool = has_key });
            },
            .MapKeys => {
                // Get map keys as array: MapKeys Rd, map_reg
                const dst_reg = instruction.dst orelse return error.InvalidInstruction;
                const map_reg = instruction.src1 orelse return error.InvalidInstruction;

                var map_val = try self.getRegister(map_reg);
                defer map_val.deinit(self.allocator);

                if (map_val != .Map) {
                    debug.log("MapKeys requires a Map, got {}", .{map_val});
                    return error.TypeMismatch;
                }

                const map = map_val.Map;
                
                // Create array of keys
                var keys = try self.allocator.alloc(types.Value, map.count());
                errdefer self.allocator.free(keys);
                
                var iterator = map.iterator();
                var i: usize = 0;
                while (iterator.next()) |entry| {
                    keys[i] = .{ .String = try self.allocator.dupe(u8, entry.key_ptr.*) };
                    i += 1;
                }

                try self.setRegister(dst_reg, .{ .Array = keys });
            },
            .ToString => {
                // Convert value to string: ToString Rd, Rs
                const dst_reg = instruction.dst orelse return error.InvalidInstruction;
                const src_reg = instruction.src1 orelse return error.InvalidInstruction;

                var value = try self.getRegister(src_reg);
                defer value.deinit(self.allocator);

                const str = switch (value) {
                    .String => |s| try self.allocator.dupe(u8, s),
                    .Int => |i| try std.fmt.allocPrint(self.allocator, "{d}", .{i}),
                    .Float => |f| try std.fmt.allocPrint(self.allocator, "{d}", .{f}),
                    .Bool => |b| try self.allocator.dupe(u8, if (b) "true" else "false"),
                    .Nil => try self.allocator.dupe(u8, "nil"),
                    .Symbol => |s| try std.fmt.allocPrint(self.allocator, ":{s}", .{s}),
                    .Array => try std.fmt.allocPrint(self.allocator, "[Array of {} elements]", .{value.Array.len}),
                    .Map => try std.fmt.allocPrint(self.allocator, "[Map with {} entries]", .{value.Map.count()}),
                    .Object => |obj_id| blk: {
                        if (self.getObjectById(obj_id)) |obj| {
                            break :blk try std.fmt.allocPrint(self.allocator, "[{s} instance]", .{obj.class.name});
                        } else {
                            break :blk try self.allocator.dupe(u8, "[Invalid object]");
                        }
                    },
                    .Class => |class| try std.fmt.allocPrint(self.allocator, "[Class {s}]", .{class.name}),
                    .Function => try self.allocator.dupe(u8, "[Function]"),
                    .BuiltinOperator => |op| try std.fmt.allocPrint(self.allocator, "[BuiltinOperator {s}]", .{@tagName(op)}),
                    else => try self.allocator.dupe(u8, "[Unknown]"),
                };

                try self.setRegister(dst_reg, .{ .String = str });
            },
            .StringSplit => {
                // Split string: StringSplit Rd, str_reg, separator_reg
                const dst_reg = instruction.dst orelse return error.InvalidInstruction;
                const str_reg = instruction.src1 orelse return error.InvalidInstruction;
                const sep_reg = instruction.src2 orelse return error.InvalidInstruction;

                var str_val = try self.getRegister(str_reg);
                defer str_val.deinit(self.allocator);
                var sep_val = try self.getRegister(sep_reg);
                defer sep_val.deinit(self.allocator);

                if (str_val != .String) {
                    debug.log("StringSplit requires a String, got {}", .{str_val});
                    return error.TypeMismatch;
                }
                if (sep_val != .String) {
                    debug.log("StringSplit separator must be String, got {}", .{sep_val});
                    return error.TypeMismatch;
                }

                const str = str_val.String;
                const sep = sep_val.String;

                // Split the string
                var parts = std.ArrayList(types.Value).init(self.allocator);
                defer parts.deinit();

                if (sep.len == 0) {
                    // Split into individual characters
                    for (str) |ch| {
                        const char_str = try self.allocator.alloc(u8, 1);
                        char_str[0] = ch;
                        try parts.append(.{ .String = char_str });
                    }
                } else {
                    // Split by separator
                    var it = std.mem.splitSequence(u8, str, sep);
                    while (it.next()) |part| {
                        const part_copy = try self.allocator.dupe(u8, part);
                        try parts.append(.{ .String = part_copy });
                    }
                }

                const result_array = try parts.toOwnedSlice();
                try self.setRegister(dst_reg, .{ .Array = result_array });
            },
            .StringTrim => {
                // Trim whitespace: StringTrim Rd, str_reg
                const dst_reg = instruction.dst orelse return error.InvalidInstruction;
                const str_reg = instruction.src1 orelse return error.InvalidInstruction;

                var str_val = try self.getRegister(str_reg);
                defer str_val.deinit(self.allocator);

                if (str_val != .String) {
                    debug.log("StringTrim requires a String, got {}", .{str_val});
                    return error.TypeMismatch;
                }

                const str = str_val.String;
                const trimmed = std.mem.trim(u8, str, " \t\n\r");
                const result = try self.allocator.dupe(u8, trimmed);
                
                try self.setRegister(dst_reg, .{ .String = result });
            },
            .StringIndexOf => {
                // Find substring index: StringIndexOf Rd, str_reg, substr_reg
                const dst_reg = instruction.dst orelse return error.InvalidInstruction;
                const str_reg = instruction.src1 orelse return error.InvalidInstruction;
                const substr_reg = instruction.src2 orelse return error.InvalidInstruction;

                var str_val = try self.getRegister(str_reg);
                defer str_val.deinit(self.allocator);
                var substr_val = try self.getRegister(substr_reg);
                defer substr_val.deinit(self.allocator);

                if (str_val != .String) {
                    debug.log("StringIndexOf requires a String, got {}", .{str_val});
                    return error.TypeMismatch;
                }
                if (substr_val != .String) {
                    debug.log("StringIndexOf substring must be String, got {}", .{substr_val});
                    return error.TypeMismatch;
                }

                const str = str_val.String;
                const substr = substr_val.String;

                const index = std.mem.indexOf(u8, str, substr);
                const result: i64 = if (index) |idx| @intCast(idx) else -1;
                
                try self.setRegister(dst_reg, .{ .Int = result });
            },
            .StringContains => {
                // Check if contains substring: StringContains Rd, str_reg, substr_reg
                const dst_reg = instruction.dst orelse return error.InvalidInstruction;
                const str_reg = instruction.src1 orelse return error.InvalidInstruction;
                const substr_reg = instruction.src2 orelse return error.InvalidInstruction;

                var str_val = try self.getRegister(str_reg);
                defer str_val.deinit(self.allocator);
                var substr_val = try self.getRegister(substr_reg);
                defer substr_val.deinit(self.allocator);

                if (str_val != .String) {
                    debug.log("StringContains requires a String, got {}", .{str_val});
                    return error.TypeMismatch;
                }
                if (substr_val != .String) {
                    debug.log("StringContains substring must be String, got {}", .{substr_val});
                    return error.TypeMismatch;
                }

                const str = str_val.String;
                const substr = substr_val.String;
                const contains = std.mem.indexOf(u8, str, substr) != null;
                
                try self.setRegister(dst_reg, .{ .Bool = contains });
            },
            .StringStartsWith => {
                // Check if starts with prefix: StringStartsWith Rd, str_reg, prefix_reg
                const dst_reg = instruction.dst orelse return error.InvalidInstruction;
                const str_reg = instruction.src1 orelse return error.InvalidInstruction;
                const prefix_reg = instruction.src2 orelse return error.InvalidInstruction;

                var str_val = try self.getRegister(str_reg);
                defer str_val.deinit(self.allocator);
                var prefix_val = try self.getRegister(prefix_reg);
                defer prefix_val.deinit(self.allocator);

                if (str_val != .String) {
                    debug.log("StringStartsWith requires a String, got {}", .{str_val});
                    return error.TypeMismatch;
                }
                if (prefix_val != .String) {
                    debug.log("StringStartsWith prefix must be String, got {}", .{prefix_val});
                    return error.TypeMismatch;
                }

                const str = str_val.String;
                const prefix = prefix_val.String;
                const starts_with = std.mem.startsWith(u8, str, prefix);
                
                try self.setRegister(dst_reg, .{ .Bool = starts_with });
            },
            .StringEndsWith => {
                // Check if ends with suffix: StringEndsWith Rd, str_reg, suffix_reg
                const dst_reg = instruction.dst orelse return error.InvalidInstruction;
                const str_reg = instruction.src1 orelse return error.InvalidInstruction;
                const suffix_reg = instruction.src2 orelse return error.InvalidInstruction;

                var str_val = try self.getRegister(str_reg);
                defer str_val.deinit(self.allocator);
                var suffix_val = try self.getRegister(suffix_reg);
                defer suffix_val.deinit(self.allocator);

                if (str_val != .String) {
                    debug.log("StringEndsWith requires a String, got {}", .{str_val});
                    return error.TypeMismatch;
                }
                if (suffix_val != .String) {
                    debug.log("StringEndsWith suffix must be String, got {}", .{suffix_val});
                    return error.TypeMismatch;
                }

                const str = str_val.String;
                const suffix = suffix_val.String;
                const ends_with = std.mem.endsWith(u8, str, suffix);
                
                try self.setRegister(dst_reg, .{ .Bool = ends_with });
            },
            .StringReplace => {
                // Replace first occurrence: StringReplace Rd, str_reg, old_reg, new_reg
                const dst_reg = instruction.dst orelse return error.InvalidInstruction;
                const str_reg = instruction.src1 orelse return error.InvalidInstruction;
                const old_reg = instruction.src2 orelse return error.InvalidInstruction;
                // Get new string register from immediate (contains register index)
                const new_reg = if (instruction.immediate) |imm| switch (imm) {
                    .Int => @as(u16, @intCast(imm.Int)),
                    else => return error.TypeMismatch,
                } else return error.InvalidInstruction;

                var str_val = try self.getRegister(str_reg);
                defer str_val.deinit(self.allocator);
                var old_val = try self.getRegister(old_reg);
                defer old_val.deinit(self.allocator);
                var new_val = try self.getRegister(new_reg);
                defer new_val.deinit(self.allocator);

                if (str_val != .String) {
                    debug.log("StringReplace requires a String, got {}", .{str_val});
                    return error.TypeMismatch;
                }
                if (old_val != .String) {
                    debug.log("StringReplace old must be String, got {}", .{old_val});
                    return error.TypeMismatch;
                }
                if (new_val != .String) {
                    debug.log("StringReplace new must be String, got {}", .{new_val});
                    return error.TypeMismatch;
                }

                const str = str_val.String;
                const old = old_val.String;
                const new = new_val.String;

                // Find first occurrence and replace
                if (std.mem.indexOf(u8, str, old)) |idx| {
                    const result = try self.allocator.alloc(u8, str.len - old.len + new.len);
                    @memcpy(result[0..idx], str[0..idx]);
                    @memcpy(result[idx..idx + new.len], new);
                    @memcpy(result[idx + new.len..], str[idx + old.len..]);
                    try self.setRegister(dst_reg, .{ .String = result });
                } else {
                    // No match, return original string
                    const result = try self.allocator.dupe(u8, str);
                    try self.setRegister(dst_reg, .{ .String = result });
                }
            },
            .StringToUpper => {
                // Convert to uppercase: StringToUpper Rd, str_reg
                const dst_reg = instruction.dst orelse return error.InvalidInstruction;
                const str_reg = instruction.src1 orelse return error.InvalidInstruction;

                var str_val = try self.getRegister(str_reg);
                defer str_val.deinit(self.allocator);

                if (str_val != .String) {
                    debug.log("StringToUpper requires a String, got {}", .{str_val});
                    return error.TypeMismatch;
                }

                const str = str_val.String;
                const result = try self.allocator.alloc(u8, str.len);
                
                for (str, 0..) |ch, i| {
                    result[i] = std.ascii.toUpper(ch);
                }
                
                try self.setRegister(dst_reg, .{ .String = result });
            },
            .StringToLower => {
                // Convert to lowercase: StringToLower Rd, str_reg
                const dst_reg = instruction.dst orelse return error.InvalidInstruction;
                const str_reg = instruction.src1 orelse return error.InvalidInstruction;

                var str_val = try self.getRegister(str_reg);
                defer str_val.deinit(self.allocator);

                if (str_val != .String) {
                    debug.log("StringToLower requires a String, got {}", .{str_val});
                    return error.TypeMismatch;
                }

                const str = str_val.String;
                const result = try self.allocator.alloc(u8, str.len);
                
                for (str, 0..) |ch, i| {
                    result[i] = std.ascii.toLower(ch);
                }
                
                try self.setRegister(dst_reg, .{ .String = result });
            },
            .CreateArray => {
                // Create array from N elements: CreateArray Rd, #count
                const dst_reg = instruction.dst orelse return error.InvalidInstruction;
                const count = if (instruction.immediate) |imm| switch (imm) {
                    .Int => @as(usize, @intCast(imm.Int)),
                    else => return error.TypeMismatch,
                } else return error.InvalidInstruction;
                const first_reg = instruction.src1;

                debug.log("CreateArray: creating array with {} elements", .{count});

                // Create array with the specified number of elements
                const array = try self.allocator.alloc(types.Value, count);
                errdefer self.allocator.free(array);

                // Populate array from registers
                // If first_reg is provided, elements are in consecutive registers starting from first_reg
                // Otherwise, they were pushed in previous instructions
                if (first_reg) |start_reg| {
                    for (0..count) |i| {
                        const src_reg = start_reg + @as(u16, @intCast(i));
                        array[i] = try self.getRegister(src_reg);
                    }
                } else {
                    // Elements should have been prepared in previous instructions
                    for (0..count) |i| {
                        array[i] = .{ .Nil = {} }; // Default to nil
                    }
                }

                try self.setRegister(dst_reg, .{ .Array = array });
            },
            .CreateMap => {
                // Create map from N key-value pairs: CreateMap Rd, #count
                const dst_reg = instruction.dst orelse return error.InvalidInstruction;
                const count = if (instruction.immediate) |imm| switch (imm) {
                    .Int => @as(usize, @intCast(imm.Int)),
                    else => return error.TypeMismatch,
                } else return error.InvalidInstruction;
                const first_reg = instruction.src1;

                debug.log("CreateMap: creating map with {} key-value pairs", .{count});

                // Create map
                var map = std.StringHashMap(types.Value).init(self.allocator);
                errdefer map.deinit();

                // Populate map from registers
                // If first_reg is provided, key-value pairs are in consecutive registers
                // Keys are at even indices, values at odd indices
                if (first_reg) |start_reg| {
                    for (0..count) |i| {
                        const key_reg = start_reg + @as(u16, @intCast(i * 2));
                        const val_reg = start_reg + @as(u16, @intCast(i * 2 + 1));

                        var key_val = try self.getRegister(key_reg);
                        defer key_val.deinit(self.allocator);
                        const value_val = try self.getRegister(val_reg);

                        // Convert key to string
                        const key_str = switch (key_val) {
                            .String => |s| try self.allocator.dupe(u8, s),
                            .Symbol => |s| try self.allocator.dupe(u8, s),
                            .Int => |int_val| try std.fmt.allocPrint(self.allocator, "{}", .{int_val}),
                            else => try self.allocator.dupe(u8, "unknown_key"),
                        };

                        try map.put(key_str, value_val);
                    }
                }

                try self.setRegister(dst_reg, .{ .Map = map });
            },
            .Dup => {
                // Duplicate register value: Dup Rs -> same register used as source
                // Note: In our MIR->bytecode, we use Move to implement Dup
                return error.UnsupportedInstruction;
            },
            .Pop => {
                // Pop is a no-op in register-based VM - handled by register allocation
                // Note: Our MIR->bytecode handles Pop by not generating any instruction
                return error.UnsupportedInstruction;
            },
            .CreateNamespace => {
                // Create namespace: CreateNamespace Rd, Rs (name)
                const dst_reg = instruction.dst orelse return error.InvalidInstruction;
                const name_reg = instruction.src1 orelse return error.InvalidInstruction;

                var name_val = try self.getRegister(name_reg);
                defer name_val.deinit(self.allocator);

                if (name_val != .String) {
                    debug.log("CreateNamespace requires a String name, got {}", .{name_val});
                    return error.TypeMismatch;
                }

                const name = name_val.String;

                // Create a module with namespace
                const module = try @import("../core/module_registry.zig").CompiledModule.init(self.allocator, name, "<namespace>");

                // Track the allocated module
                try self.allocated_modules.append(module);

                try self.setRegister(dst_reg, .{ .Module = module });
            },
            .PushNamespace => {
                // Push namespace onto context stack: PushNamespace Rs
                const ns_reg = instruction.src1 orelse return error.InvalidInstruction;

                const ns_val = try self.getRegister(ns_reg);

                if (ns_val != .Module) {
                    debug.log("PushNamespace requires a Module, got {}", .{ns_val});
                    return error.TypeMismatch;
                }

                // Push both the namespace and the module
                try self.namespace_stack.append(ns_val.Module.namespace);
                try self.module_stack.append(ns_val.Module);
            },
            .PopNamespace => {
                // Pop namespace from context stack: PopNamespace Rd
                const dst_reg = instruction.dst orelse return error.InvalidInstruction;

                if (self.namespace_stack.items.len == 0) {
                    debug.log("PopNamespace with empty namespace stack", .{});
                    return error.StackUnderflow;
                }

                // Pop both stacks
                _ = self.namespace_stack.pop();
                const module_opt = self.module_stack.pop();
                const module = module_opt orelse return error.StackUnderflow;

                // Store the module in the register
                try self.setRegister(dst_reg, .{ .Module = module });
            },
            .TryStart => {
                // Start of try block with catch target
                // Push the current exception handler onto a stack
                const catch_target = instruction.jump_target orelse return error.InvalidInstruction;
                try self.exception_handlers.append(.{
                    .catch_addr = catch_target,
                    .finally_addr = null,
                    .frame_ptr = self.call_frames.items.len,
                });
            },
            .TryEnd => {
                // End of try/catch/finally block
                // Only pop the exception handler if it belongs to the current frame
                // (it might have been popped already by a throw)
                if (self.exception_handlers.items.len > 0) {
                    const handler = self.exception_handlers.items[self.exception_handlers.items.len - 1];
                    if (handler.frame_ptr == self.call_frames.items.len) {
                        _ = self.exception_handlers.pop();
                        debug.log("TryEnd: Popped exception handler, remaining handlers: {}", .{self.exception_handlers.items.len});
                    }
                }
            },
            .Throw => {
                // Throw exception from register
                const error_reg = instruction.src1 orelse return error.InvalidInstruction;
                const error_val = try self.getRegister(error_reg);
                
                // Store the exception (getRegister already clones, so we can use it directly)
                if (self.current_exception) |*exc| {
                    exc.deinit(self.allocator);
                }
                self.current_exception = error_val;
                
                debug.log("Throw: Stored exception: {}", .{error_val});
                
                // Find the nearest exception handler
                if (self.exception_handlers.items.len > 0) {
                    // Find the appropriate handler for the current call stack depth
                    var handler_index: ?usize = null;
                    var i = self.exception_handlers.items.len;
                    while (i > 0) {
                        i -= 1;
                        // Always use the first handler we find - it will handle unwinding if needed
                        handler_index = i;
                        break;
                    }
                    
                    if (handler_index) |idx| {
                        const handler = self.exception_handlers.items[idx];
                        const target_pc = handler.catch_addr;
                        
                        // Pop this handler and any handlers below it on the stack
                        while (self.exception_handlers.items.len > idx) {
                            _ = self.exception_handlers.pop();
                        }
                        
                        // If we're jumping to a handler in a parent frame, we need to unwind the call stack
                        // std.debug.print("DEBUG: Unwinding from {} frames to {} frames\n", .{self.call_frames.items.len, handler.frame_ptr});
                        while (self.call_frames.items.len > handler.frame_ptr) {
                            const popped_frame = self.call_frames.items[self.call_frames.items.len - 1];
                            _ = self.call_frames.pop();
                            // When we pop a frame, restore to the caller's context
                            self.current_func = popped_frame.caller_func;
                            self.current_register_base = popped_frame.prev_register_base;
                            // std.debug.print("DEBUG: Popped frame, restored to function: {s}\n", .{if (popped_frame.caller_func) |f| f.name else "null"});
                        }
                        
                        debug.log("Throw: Jumping to catch block at pc={}, remaining handlers={}", .{target_pc, self.exception_handlers.items.len});
                        // if (self.current_func) |func| {
                        //     std.debug.print("DEBUG: Current function after unwind: {s}\n", .{func.name});
                        // } else {
                        //     std.debug.print("DEBUG: Current function is null after unwind\n", .{});
                        // }
                        self.pc = target_pc - 1;
                    } else {
                        // No handler found
                        return error.UserException;
                    }
                } else {
                    // No handler, propagate error
                    return error.UserException;
                }
            },
            .LoadException => {
                // Load current exception onto register
                const dst_reg = instruction.dst orelse return error.InvalidInstruction;
                if (self.current_exception) |exc| {
                    // Clone the exception to avoid double-free when ClearException is called
                    const cloned_exc = try exc.clone(self.allocator);
                    try self.setRegister(dst_reg, cloned_exc);
                } else {
                    try self.setRegister(dst_reg, .Nil);
                }
            },
            .ClearException => {
                // Clear current exception
                if (self.current_exception) |*exc| {
                    exc.deinit(self.allocator);
                    self.current_exception = null;
                }
            },
            .CreateCallback => {
                // Create a C callback wrapper
                const dst_reg = instruction.dst orelse return error.InvalidInstruction;
                const func_reg = instruction.src1 orelse return error.InvalidInstruction;
                const sig_reg = instruction.src2 orelse return error.InvalidInstruction;
                
                const func_val = try self.getRegister(func_reg);
                var sig_val = try self.getRegister(sig_reg);
                defer sig_val.deinit(self.allocator);
                
                // Extract signature string if provided
                const signature = switch (sig_val) {
                    .String => |s| s,
                    .Nil => null,
                    else => return error.TypeMismatch,
                };
                
                // Create the callback wrapper
                const ffi_callback = @import("../core/ffi_callback.zig");
                const wrapper = try ffi_callback.createCallback(self, func_val, signature);
                
                // Create CCallback value
                const callback_value = types.Value{ .CCallback = .{
                    .function = try self.allocator.create(types.Value),
                    .signature = if (signature) |sig| try self.allocator.dupe(u8, sig) else null,
                    .callback_id = @intFromPtr(wrapper), // Store wrapper pointer as ID
                }};
                callback_value.CCallback.function.* = try func_val.clone(self.allocator);
                
                try self.setRegister(dst_reg, callback_value);
            },
            .CreateModule => {
                // Create module: CreateModule Rd (name from src1)
                const dst_reg = instruction.dst orelse return error.InvalidInstruction;
                const name_reg = instruction.src1 orelse return error.InvalidInstruction;
                
                var name_val = try self.getRegister(name_reg);
                defer name_val.deinit(self.allocator);
                
                if (name_val != .String) {
                    debug.log("CreateModule requires a String name, got {}", .{name_val});
                    return error.TypeMismatch;
                }
                
                // For now, create an empty module (Map-based)
                const module_map = std.StringHashMap(types.Value).init(self.allocator);
                try self.setRegister(dst_reg, .{ .Map = module_map });
            },
            .PushModule => {
                // Push module onto context stack: PushModule Rs
                const module_reg = instruction.src1 orelse return error.InvalidInstruction;
                var module_val = try self.getRegister(module_reg);
                
                // For now, just consume the value (no module stack implemented yet)
                module_val.deinit(self.allocator);
            },
            .PopModule => {
                // Pop module from context stack: PopModule Rd
                const dst_reg = instruction.dst orelse return error.InvalidInstruction;
                
                // For now, return an empty module
                const module_map = std.StringHashMap(types.Value).init(self.allocator);
                try self.setRegister(dst_reg, .{ .Map = module_map });
            },
            .MarkExport => {
                // Mark name for export: MarkExport (name from src1)
                const name_reg = instruction.src1 orelse return error.InvalidInstruction;
                var name_val = try self.getRegister(name_reg);
                
                // For now, just consume the value
                name_val.deinit(self.allocator);
            },
            .Export => {
                // Export value with name: Export Rs1 (value), Rs2 (name)
                const value_reg = instruction.src1 orelse return error.InvalidInstruction;
                const name_reg = instruction.src2 orelse return error.InvalidInstruction;
                
                var value_val = try self.getRegister(value_reg);
                var name_val = try self.getRegister(name_reg);
                
                // For now, just consume the values
                value_val.deinit(self.allocator);
                name_val.deinit(self.allocator);
            },
        }
    }

    fn valuesEqual(self: *VM, val1: types.Value, val2: types.Value) !bool {
        // Check if types are different
        if (@intFromEnum(val1) != @intFromEnum(val2)) {
            return false;
        }

        // Compare values based on type
        return switch (val1) {
            .Int => |n1| n1 == val2.Int,
            .Float => |f1| f1 == val2.Float,
            .String => |s1| std.mem.eql(u8, s1, val2.String),
            .Bool => |b1| b1 == val2.Bool,
            .Nil => true, // All nils are equal
            .Symbol => |s1| std.mem.eql(u8, s1, val2.Symbol),
            .Array => |arr1| blk: {
                const arr2 = val2.Array;
                if (arr1.len != arr2.len) break :blk false;
                for (arr1, arr2) |elem1, elem2| {
                    if (!try self.valuesEqual(elem1, elem2)) break :blk false;
                }
                break :blk true;
            },
            .Map => false, // Maps are compared by reference for now
            .Object => |id1| id1 == val2.Object, // Objects compared by ID
            else => false, // Other types not comparable
        };
    }

    fn executeBuiltinOperator(self: *VM, op: types.BuiltinOperatorType, arg1: types.Value, arg2: types.Value) !types.Value {
        _ = self; // Suppress unused parameter warning

        return switch (op) {
            .Add => {
                // Handle + operator
                if (arg1 == .Int and arg2 == .Int) {
                    return types.Value{ .Int = arg1.Int + arg2.Int };
                } else {
                    return error.TypeMismatch;
                }
            },
            .Sub => {
                // Handle - operator
                if (arg1 == .Int and arg2 == .Int) {
                    return types.Value{ .Int = arg1.Int - arg2.Int };
                } else {
                    return error.TypeMismatch;
                }
            },
            .Mul => {
                // Handle * operator
                if (arg1 == .Int and arg2 == .Int) {
                    return types.Value{ .Int = arg1.Int * arg2.Int };
                } else {
                    return error.TypeMismatch;
                }
            },
            .Div => {
                // Handle / operator
                if (arg1 == .Int and arg2 == .Int) {
                    if (arg2.Int == 0) {
                        return error.DivisionByZero;
                    }
                    return types.Value{ .Int = @divTrunc(arg1.Int, arg2.Int) };
                } else {
                    return error.TypeMismatch;
                }
            },
            .And => {
                // Handle && operator - short-circuit logical AND
                // In Gene, nil and false are falsy, everything else is truthy
                const arg1_truthy = switch (arg1) {
                    .Nil => false,
                    .Bool => |b| b,
                    else => true,
                };
                
                if (!arg1_truthy) {
                    // Short-circuit: return first falsy value
                    return arg1;
                }
                
                // Return second value (which determines the final result)
                return arg2;
            },
            .Or => {
                // Handle || operator - short-circuit logical OR
                // In Gene, nil and false are falsy, everything else is truthy
                const arg1_truthy = switch (arg1) {
                    .Nil => false,
                    .Bool => |b| b,
                    else => true,
                };
                
                if (arg1_truthy) {
                    // Short-circuit: return first truthy value
                    return arg1;
                }
                
                // Return second value (which determines the final result)
                return arg2;
            },
            else => {
                return error.TypeMismatch;
            },
        };
    }
    
    fn executeStdlibFunction(self: *VM, func: stdlib.StdlibFunction, dst_reg: u16, func_reg: u16, arg_count: usize) !void {
        switch (func) {
            .FileOpen => {
                // file_open(path, mode) -> FileHandle
                if (arg_count != 2) {
                    debug.log("file_open requires 2 arguments, got {}", .{arg_count});
                    return error.ArgumentCountMismatch;
                }
                
                var path_val = try self.getRegister(func_reg + 1);
                defer path_val.deinit(self.allocator);
                var mode_val = try self.getRegister(func_reg + 2);
                defer mode_val.deinit(self.allocator);
                
                if (path_val != .String or mode_val != .String) {
                    debug.log("file_open requires string arguments", .{});
                    return error.TypeMismatch;
                }
                
                const path = path_val.String;
                const mode = mode_val.String;
                
                // Parse mode string
                const file = if (std.mem.eql(u8, mode, "r")) blk: {
                    break :blk std.fs.cwd().openFile(path, .{}) catch |err| {
                        debug.log("Failed to open file for reading: {}", .{err});
                        try self.setRegister(dst_reg, .Nil);
                        return;
                    };
                } else if (std.mem.eql(u8, mode, "w")) blk: {
                    break :blk std.fs.cwd().createFile(path, .{}) catch |err| {
                        debug.log("Failed to open file for writing: {}", .{err});
                        try self.setRegister(dst_reg, .Nil);
                        return;
                    };
                } else if (std.mem.eql(u8, mode, "a")) blk: {
                    break :blk std.fs.cwd().createFile(path, .{ .truncate = false }) catch |err| {
                        debug.log("Failed to open file for appending: {}", .{err});
                        try self.setRegister(dst_reg, .Nil);
                        return;
                    };
                } else {
                    debug.log("Invalid file mode: {s}", .{mode});
                    try self.setRegister(dst_reg, .Nil);
                    return;
                };
                
                // Create file handle
                const handle = try self.allocator.create(stdlib.FileHandle);
                handle.* = try stdlib.FileHandle.init(file, self.allocator, path);
                
                try self.setRegister(dst_reg, .{ .FileHandle = handle });
            },
            
            .FileClose => {
                // file_close(handle) -> Bool
                if (arg_count != 1) {
                    debug.log("file_close requires 1 argument, got {}", .{arg_count});
                    return error.ArgumentCountMismatch;
                }
                
                var handle_val = try self.getRegister(func_reg + 1);
                defer handle_val.deinit(self.allocator);
                
                if (handle_val != .FileHandle) {
                    debug.log("file_close requires FileHandle argument", .{});
                    return error.TypeMismatch;
                }
                
                const handle = handle_val.FileHandle;
                handle.deinit();
                self.allocator.destroy(handle);
                
                try self.setRegister(dst_reg, .{ .Bool = true });
            },
            
            .FileReadAll => {
                // file_read_all(handle) -> String
                if (arg_count != 1) {
                    debug.log("file_read_all requires 1 argument, got {}", .{arg_count});
                    return error.ArgumentCountMismatch;
                }
                
                var handle_val = try self.getRegister(func_reg + 1);
                defer handle_val.deinit(self.allocator);
                
                if (handle_val != .FileHandle) {
                    debug.log("file_read_all requires FileHandle argument", .{});
                    return error.TypeMismatch;
                }
                
                const handle = handle_val.FileHandle;
                const content = handle.file.readToEndAlloc(self.allocator, std.math.maxInt(usize)) catch |err| {
                    debug.log("Failed to read file: {}", .{err});
                    try self.setRegister(dst_reg, .Nil);
                    return;
                };
                
                try self.setRegister(dst_reg, .{ .String = content });
            },
            
            .FileWriteAll => {
                // file_write_all(handle, content) -> Bool
                if (arg_count != 2) {
                    debug.log("file_write_all requires 2 arguments, got {}", .{arg_count});
                    return error.ArgumentCountMismatch;
                }
                
                var handle_val = try self.getRegister(func_reg + 1);
                defer handle_val.deinit(self.allocator);
                var content_val = try self.getRegister(func_reg + 2);
                defer content_val.deinit(self.allocator);
                
                if (handle_val != .FileHandle or content_val != .String) {
                    debug.log("file_write_all requires FileHandle and String arguments", .{});
                    return error.TypeMismatch;
                }
                
                const handle = handle_val.FileHandle;
                const content = content_val.String;
                
                handle.file.writeAll(content) catch |err| {
                    debug.log("Failed to write to file: {}", .{err});
                    try self.setRegister(dst_reg, .{ .Bool = false });
                    return;
                };
                
                try self.setRegister(dst_reg, .{ .Bool = true });
            },
            
            .FileExists => {
                // file_exists(path) -> Bool
                if (arg_count != 1) {
                    debug.log("file_exists requires 1 argument, got {}", .{arg_count});
                    return error.ArgumentCountMismatch;
                }
                
                var path_val = try self.getRegister(func_reg + 1);
                defer path_val.deinit(self.allocator);
                
                if (path_val != .String) {
                    debug.log("file_exists requires String argument", .{});
                    return error.TypeMismatch;
                }
                
                const path = path_val.String;
                const exists = if (std.fs.cwd().access(path, .{})) |_| true else |_| false;
                
                try self.setRegister(dst_reg, .{ .Bool = exists });
            },
            
            .Exit => {
                // exit(code) -> Never
                if (arg_count != 1) {
                    debug.log("exit requires 1 argument, got {}", .{arg_count});
                    return error.ArgumentCountMismatch;
                }
                
                var code_val = try self.getRegister(func_reg + 1);
                defer code_val.deinit(self.allocator);
                
                if (code_val != .Int) {
                    debug.log("exit requires Int argument", .{});
                    return error.TypeMismatch;
                }
                
                const code = code_val.Int;
                std.process.exit(@intCast(code));
            },
            
            .MathSqrt => {
                // math_sqrt(x) -> Float
                if (arg_count != 1) {
                    debug.log("math_sqrt requires 1 argument, got {}", .{arg_count});
                    return error.ArgumentCountMismatch;
                }
                
                var x_val = try self.getRegister(func_reg + 1);
                defer x_val.deinit(self.allocator);
                
                const x = switch (x_val) {
                    .Int => |i| @as(f64, @floatFromInt(i)),
                    .Float => |f| f,
                    else => {
                        debug.log("math_sqrt requires numeric argument", .{});
                        return error.TypeMismatch;
                    },
                };
                
                try self.setRegister(dst_reg, .{ .Float = @sqrt(x) });
            },
            
            .StringConcat => {
                // string_concat(parts...) -> String
                debug.log("StringConcat: arg_count={}, func_reg={}, dst_reg={}", .{arg_count, func_reg, dst_reg});
                
                if (arg_count == 0) {
                    try self.setRegister(dst_reg, .{ .String = try self.allocator.dupe(u8, "") });
                    return;
                }
                
                // Calculate total length needed
                var total_len: usize = 0;
                for (0..arg_count) |i| {
                    const reg_index = func_reg + 1 + @as(u16, @intCast(i));
                    debug.log("StringConcat: reading register {} for part {}", .{reg_index, i});
                    const part = try self.getRegister(reg_index);
                    
                    const str = switch (part) {
                        .String => |s| s,
                        .Int => |n| blk: {
                            var buf: [32]u8 = undefined;
                            const str_result = try std.fmt.bufPrint(&buf, "{}", .{n});
                            break :blk str_result;
                        },
                        .Float => |f| blk: {
                            var buf: [32]u8 = undefined;
                            const str_result = try std.fmt.bufPrint(&buf, "{d}", .{f});
                            break :blk str_result;
                        },
                        .Bool => |b| if (b) "true" else "false",
                        .Nil => "nil",
                        else => {
                            debug.log("string_concat: unsupported type {}", .{part});
                            return error.TypeMismatch;
                        },
                    };
                    total_len += str.len;
                }
                
                // Allocate result string
                var result = try self.allocator.alloc(u8, total_len);
                var offset: usize = 0;
                
                // Copy all parts
                for (0..arg_count) |i| {
                    const part = try self.getRegister(func_reg + 1 + @as(u16, @intCast(i)));
                    
                    const str = switch (part) {
                        .String => |s| s,
                        .Int => |n| blk: {
                            var buf: [32]u8 = undefined;
                            const str_result = try std.fmt.bufPrint(&buf, "{}", .{n});
                            break :blk str_result;
                        },
                        .Float => |f| blk: {
                            var buf: [32]u8 = undefined;
                            const str_result = try std.fmt.bufPrint(&buf, "{d}", .{f});
                            break :blk str_result;
                        },
                        .Bool => |b| if (b) "true" else "false",
                        .Nil => "nil",
                        else => unreachable,
                    };
                    
                    @memcpy(result[offset..offset + str.len], str);
                    offset += str.len;
                }
                
                try self.setRegister(dst_reg, .{ .String = result });
            },
            
            .Throw => {
                // throw(error) -> Never
                if (arg_count != 1) {
                    debug.log("throw requires 1 argument, got {}", .{arg_count});
                    return error.ArgumentCountMismatch;
                }
                
                const error_val = try self.getRegister(func_reg + 1);
                // Don't defer deinit - we're storing this value
                
                // Store the exception
                if (self.current_exception) |*exc| {
                    exc.deinit(self.allocator);
                }
                self.current_exception = error_val;
                
                // Return the UserException error to unwind the stack
                return error.UserException;
            },
            
            .ErrorNew => {
                // error_new(type, message) -> Error
                if (arg_count != 2) {
                    debug.log("error_new requires 2 arguments, got {}", .{arg_count});
                    return error.ArgumentCountMismatch;
                }
                
                var type_val = try self.getRegister(func_reg + 1);
                defer type_val.deinit(self.allocator);
                var msg_val = try self.getRegister(func_reg + 2);
                defer msg_val.deinit(self.allocator);
                
                if (type_val != .String or msg_val != .String) {
                    debug.log("error_new requires string arguments", .{});
                    return error.TypeMismatch;
                }
                
                const err_type = try self.allocator.dupe(u8, type_val.String);
                const err_msg = try self.allocator.dupe(u8, msg_val.String);
                
                try self.setRegister(dst_reg, .{ .Error = .{
                    .type = err_type,
                    .message = err_msg,
                    .stack_trace = null,
                }});
            },
            
            .ErrorType => {
                // error_type(error) -> String
                if (arg_count != 1) {
                    debug.log("error_type requires 1 argument, got {}", .{arg_count});
                    return error.ArgumentCountMismatch;
                }
                
                var error_val = try self.getRegister(func_reg + 1);
                defer error_val.deinit(self.allocator);
                
                if (error_val != .Error) {
                    debug.log("error_type requires Error argument", .{});
                    return error.TypeMismatch;
                }
                
                const err_type = try self.allocator.dupe(u8, error_val.Error.type);
                try self.setRegister(dst_reg, .{ .String = err_type });
            },
            
            .ErrorMessage => {
                // error_message(error) -> String
                if (arg_count != 1) {
                    debug.log("error_message requires 1 argument, got {}", .{arg_count});
                    return error.ArgumentCountMismatch;
                }
                
                var error_val = try self.getRegister(func_reg + 1);
                defer error_val.deinit(self.allocator);
                
                if (error_val != .Error) {
                    debug.log("error_message requires Error argument", .{});
                    return error.TypeMismatch;
                }
                
                const err_msg = try self.allocator.dupe(u8, error_val.Error.message);
                try self.setRegister(dst_reg, .{ .String = err_msg });
            },
            
            else => {
                debug.log("Stdlib function not yet implemented: {}", .{func});
                return error.NotImplemented;
            },
        }
    }
    
    /// Call a Gene function from native code
    /// This allows native functions to call back into Gene code
    pub fn callGeneFunction(self: *VM, func: *bytecode.Function, args: []const types.Value) !types.Value {
        debug.log("Native calling Gene function: {s} with {} args", .{ func.name, args.len });
        
        // Save current VM state
        const saved_func = self.current_func;
        const saved_pc = self.pc;
        const saved_register_base = self.current_register_base;
        const saved_call_frames_len = self.call_frames.items.len;
        
        // Set up new call frame for the function
        const new_register_base = self.next_free_register;
        const result_reg = new_register_base + @as(u16, @intCast(func.param_count + func.register_count));
        
        // Create a dummy frame that will help us track where to store the result
        const frame = CallFrame.init(saved_func, new_register_base, saved_register_base, saved_pc, result_reg);
        try self.call_frames.append(frame);
        
        // Allocate registers for the function (parameters + locals)
        const needed_regs = @as(u16, @intCast(func.param_count + func.register_count + 1)); // +1 for result
        const allocated_base = try self.allocateRegisters(needed_regs);
        
        // Copy arguments to parameter registers
        for (args, 0..) |arg, i| {
            const dest_reg = allocated_base + @as(u16, @intCast(i));
            // Ensure register exists
            while (dest_reg >= self.registers.items.len) {
                try self.registers.append(.{ .Nil = {} });
            }
            self.registers.items[dest_reg].deinit(self.allocator);
            self.registers.items[dest_reg] = try arg.clone(self.allocator);
            debug.log("Set arg {} in R{}", .{ i, dest_reg });
        }
        
        // Initialize remaining parameter registers to nil
        for (args.len..func.param_count) |i| {
            const dest_reg = allocated_base + @as(u16, @intCast(i));
            while (dest_reg >= self.registers.items.len) {
                try self.registers.append(.{ .Nil = {} });
            }
            self.registers.items[dest_reg].deinit(self.allocator);
            self.registers.items[dest_reg] = .{ .Nil = {} };
        }
        
        // Switch to the function
        self.current_func = func;
        self.pc = 0;
        self.current_register_base = allocated_base;
        
        // Run the function until it returns
        var result: types.Value = .{ .Nil = {} };
        errdefer result.deinit(self.allocator);
        
        while (self.pc < func.instructions.items.len and self.call_frames.items.len > saved_call_frames_len) {
            const instruction = &func.instructions.items[self.pc];
            debug.log("Gene function PC={}: {s}", .{ self.pc, @tagName(instruction.op) });
            
            const old_pc = self.pc;
            try self.executeInstruction(instruction.*);
            
            if (!self.function_called and self.pc == old_pc) {
                self.pc += 1;
            }
            self.function_called = false;
        }
        
        // Get the result from the result register if available
        if (result_reg < self.registers.items.len) {
            result = try self.registers.items[result_reg].clone(self.allocator);
        }
        
        // Clean up allocated registers
        self.next_free_register = new_register_base;
        
        // Restore VM state
        self.current_func = saved_func;
        self.pc = saved_pc;
        self.current_register_base = saved_register_base;
        
        // Pop any remaining frames from nested calls
        while (self.call_frames.items.len > saved_call_frames_len) {
            _ = self.call_frames.pop();
        }
        
        debug.log("Native call to Gene function completed, result: {any}", .{result});
        return result;
    }
    
    /// Call a Gene value (function or closure) from native code
    pub fn callGeneValue(self: *VM, value: types.Value, args: []const types.Value) !types.Value {
        switch (value) {
            .Function => |func| return self.callGeneFunction(func, args),
            // TODO: Add support for closures when implemented
            else => {
                debug.log("callGeneValue: expected Function, got {}", .{value});
                return error.TypeMismatch;
            },
        }
    }
};

// ===== C API for FFI Callbacks =====
// These functions allow C code to interact with the Gene VM dynamically
// Note: For WASM compatibility, we use opaque pointers instead of returning Values directly

/// Opaque handle to a Gene value
pub const GeneValueHandle = *anyopaque;

/// Create a Gene integer value from C
export fn gene_make_int(vm_ptr: *VM, value: i64) callconv(.C) ?GeneValueHandle {
    const val = vm_ptr.allocator.create(types.Value) catch return null;
    val.* = .{ .Int = value };
    return @ptrCast(val);
}

/// Create a Gene float value from C
export fn gene_make_float(vm_ptr: *VM, value: f64) callconv(.C) ?GeneValueHandle {
    const val = vm_ptr.allocator.create(types.Value) catch return null;
    val.* = .{ .Float = value };
    return @ptrCast(val);
}

/// Create a Gene string value from C
export fn gene_make_string(vm_ptr: *VM, str: [*:0]const u8) callconv(.C) ?GeneValueHandle {
    const len = std.mem.len(str);
    const copy = vm_ptr.allocator.dupe(u8, str[0..len]) catch return null;
    const val = vm_ptr.allocator.create(types.Value) catch {
        vm_ptr.allocator.free(copy);
        return null;
    };
    val.* = .{ .String = copy };
    return @ptrCast(val);
}

/// Create a Gene boolean value from C
export fn gene_make_bool(vm_ptr: *VM, value: bool) callconv(.C) ?GeneValueHandle {
    const val = vm_ptr.allocator.create(types.Value) catch return null;
    val.* = .{ .Bool = value };
    return @ptrCast(val);
}

/// Create a Gene nil value from C
export fn gene_make_nil(vm_ptr: *VM) callconv(.C) ?GeneValueHandle {
    const val = vm_ptr.allocator.create(types.Value) catch return null;
    val.* = .{ .Nil = {} };
    return @ptrCast(val);
}

/// Look up a function by name in the VM's global scope
export fn gene_get_function(vm_ptr: *VM, name: [*:0]const u8) callconv(.C) ?GeneValueHandle {
    const name_slice = std.mem.span(name);
    const value = vm_ptr.variables.get(name_slice) orelse return null;
    const val = vm_ptr.allocator.create(types.Value) catch return null;
    val.* = value;
    return @ptrCast(val);
}

/// Get a method from a Gene value
export fn gene_get_method(vm_ptr: *VM, object_handle: GeneValueHandle, method_name: [*:0]const u8) callconv(.C) ?GeneValueHandle {
    const object = @as(*types.Value, @ptrCast(@alignCast(object_handle))).*;
    const name = std.mem.span(method_name);
    
    // For core types, get methods from core classes
    if (vm_ptr.core_classes) |core| {
        const class = switch (object) {
            .Int => core.int_class,
            .Float => core.float_class,
            .String => core.string_class,
            .Bool => core.bool_class,
            .Array => core.array_class,
            .Map => core.map_class,
            .Object => |id| blk: {
                const obj = vm_ptr.getObjectById(id) orelse return null;
                break :blk obj.class;
            },
            else => return null,
        };
        
        // Look up method in class
        if (class.methods.get(name)) |method| {
            const val = vm_ptr.allocator.create(types.Value) catch return null;
            val.* = .{ .Function = method };
            return @ptrCast(val);
        }
    }
    
    return null;
}

/// Check if a value is callable
export fn gene_is_callable(handle: GeneValueHandle) callconv(.C) bool {
    const value = @as(*types.Value, @ptrCast(@alignCast(handle))).*;
    return switch (value) {
        .Function, .BuiltinOperator, .StdlibFunction => true,
        else => false,
    };
}

/// Call a Gene function/callable from C
export fn gene_call(vm_ptr: *VM, callable_handle: GeneValueHandle, args: [*]GeneValueHandle, arg_count: usize) callconv(.C) ?GeneValueHandle {
    const callable = @as(*types.Value, @ptrCast(@alignCast(callable_handle))).*;
    
    // Convert handles to values
    var args_values = vm_ptr.allocator.alloc(types.Value, arg_count) catch return null;
    defer vm_ptr.allocator.free(args_values);
    
    for (0..arg_count) |i| {
        args_values[i] = @as(*types.Value, @ptrCast(@alignCast(args[i]))).*;
    }
    
    const result_value = vm_ptr.callGeneValue(callable, args_values) catch |err| {
        // On error, return an Error value
        const err_name = @errorName(err);
        const type_copy = vm_ptr.allocator.dupe(u8, "RuntimeError") catch return null;
        const msg_copy = vm_ptr.allocator.dupe(u8, err_name) catch {
            vm_ptr.allocator.free(type_copy);
            return null;
        };
        const val = vm_ptr.allocator.create(types.Value) catch {
            vm_ptr.allocator.free(type_copy);
            vm_ptr.allocator.free(msg_copy);
            return null;
        };
        val.* = .{ .Error = .{
            .type = type_copy,
            .message = msg_copy,
            .stack_trace = null,
        }};
        return @ptrCast(val);
    };
    
    const val = vm_ptr.allocator.create(types.Value) catch return null;
    val.* = result_value;
    return @ptrCast(val);
}

/// Safe version that returns error code
export fn gene_call_safe(vm_ptr: *VM, callable_handle: GeneValueHandle, args: [*]GeneValueHandle, arg_count: usize, result: *GeneValueHandle) callconv(.C) c_int {
    const callable = @as(*types.Value, @ptrCast(@alignCast(callable_handle))).*;
    
    // Convert handles to values
    var args_values = vm_ptr.allocator.alloc(types.Value, arg_count) catch return -2; // Memory error
    defer vm_ptr.allocator.free(args_values);
    
    for (0..arg_count) |i| {
        args_values[i] = @as(*types.Value, @ptrCast(@alignCast(args[i]))).*;
    }
    
    const result_value = vm_ptr.callGeneValue(callable, args_values) catch return -1; // Execution error
    
    const val = vm_ptr.allocator.create(types.Value) catch return -2; // Memory error
    val.* = result_value;
    result.* = @ptrCast(val);
    return 0; // Success
}

/// Get the type name of a Gene value
export fn gene_type_name(handle: GeneValueHandle) callconv(.C) [*:0]const u8 {
    const value = @as(*types.Value, @ptrCast(@alignCast(handle))).*;
    return switch (value) {
        .Nil => "Nil",
        .Bool => "Bool",
        .Int => "Int",
        .Float => "Float",
        .String => "String",
        .Symbol => "Symbol",
        .Array => "Array",
        .Map => "Map",
        .Function => "Function",
        .Object => "Object",
        .Module => "Module",
        .Class => "Class",
        .Error => "Error",
        .CCallback => "CCallback",
        else => "Unknown",
    };
}

/// Free a Gene value handle (for memory management from C)
export fn gene_free_value(vm_ptr: *VM, handle: GeneValueHandle) callconv(.C) void {
    const value = @as(*types.Value, @ptrCast(@alignCast(handle)));
    value.deinit(vm_ptr.allocator);
    vm_ptr.allocator.destroy(value);
}
