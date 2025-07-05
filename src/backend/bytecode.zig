const std = @import("std");
const ast = @import("../frontend/ast.zig");
const types = @import("../core/types.zig");
const debug = @import("../core/debug.zig");

pub const Value = types.Value;

pub const OpCode = enum {
    // Register operations
    LoadConst, // Load constant into register: LoadConst Rd, #constant
    LoadVar, // Load variable into register: LoadVar Rd, var_name
    LoadParam, // Load parameter into register: LoadParam Rd, #param_index
    LoadModule, // Load module from file: LoadModule Rd, module_path
    StoreVar, // Store register to variable: StoreVar var_name, Rs
    StoreGlobal, // Store register to global: StoreGlobal global_name, Rs
    Move, // Copy register to register: Move Rd, Rs

    // Arithmetic operations (3-address code)
    Add, // Add Rs1, Rs2 -> Rd
    Sub, // Sub Rs1, Rs2 -> Rd
    Mul, // Mul Rs1, Rs2 -> Rd
    Div, // Div Rs1, Rs2 -> Rd

    // Comparison operations
    Lt, // Lt Rs1, Rs2 -> Rd (result: boolean)
    Gt, // Gt Rs1, Rs2 -> Rd
    Eq, // Eq Rs1, Rs2 -> Rd
    Ne, // Ne Rs1, Rs2 -> Rd (not equal)
    Le, // Le Rs1, Rs2 -> Rd (less than or equal)
    Ge, // Ge Rs1, Rs2 -> Rd (greater than or equal)
    
    // Logical operations
    Not, // Not Rs -> Rd (logical not)

    // I/O operations
    Print, // Print Rs

    // Control flow
    Return, // Return [Rs] (optional return value)
    Call, // Call function_id, [R0, R1, ...] -> Rd
    Jump, // Jump #offset
    JumpIfFalse, // JumpIfFalse Rs, #offset

    // Collection operations
    CreateArray, // Create array from N elements: CreateArray Rd, #count (uses N registers starting from src1)
    CreateMap, // Create map from N key-value pairs: CreateMap Rd, #count (uses N*2 registers starting from src1)
    Length, // Get length: Length Rd, Rs (string/array/map)
    ArrayGet, // Get array element: ArrayGet Rd, array_reg, index_reg
    ArraySet, // Set array element: ArraySet array_reg, index_reg, value_reg
    ArrayPush, // Push element to array: ArrayPush array_reg, value_reg -> returns new array in Rd
    ArrayPop, // Pop element from array: ArrayPop Rd, array_reg -> returns popped value in Rd
    MapGet, // Get map value: MapGet Rd, map_reg, key_reg
    MapSet, // Set map value: MapSet map_reg, key_reg, value_reg
    
    // String operations
    Substring, // Get substring: Substring Rd, str_reg, start_reg, end_reg
    ToString, // Convert value to string: ToString Rd, Rs
    
    // Class operations
    DefineClass, // Define a class: DefineClass Rd, class_name, parent_reg (optional)
    New, // Create object instance: New Rd, class_reg, [arg1, arg2, ...]
    Get, // Universal get for maps, arrays, fields
    Set, // Universal set for maps, arrays, fields
    CallMethod, // Call method: CallMethod Rd, obj_reg, method_name, [arg1, arg2, ...]
    ClassName, // Get class name: ClassName Rd, class_reg
    ClassParent, // Get parent class: ClassParent Rd, class_reg
    
    // Type checking operations
    IsArray, // Check if value is array: IsArray Rd, Rs
    IsMap, // Check if value is map: IsMap Rd, Rs
    
    // Collection contains operations
    ArrayContains, // Check if array contains value: ArrayContains Rd, array_reg, value_reg
    MapHas, // Check if map has key: MapHas Rd, map_reg, key_reg
    MapKeys, // Get map keys as array: MapKeys Rd, map_reg
    
    // Stack operations (needed for pattern matching)
    Dup, // Duplicate top of stack: Dup Rs -> push Rs value again
    Pop, // Pop and discard: Pop Rs
    
    // Namespace operations
    CreateNamespace, // Create namespace: CreateNamespace Rd (name in immediate)
    PushNamespace, // Push namespace onto context stack: PushNamespace Rs
    PopNamespace, // Pop namespace from context stack: PopNamespace Rd
    
    // Exception handling operations
    TryStart, // Start try block: TryStart catch_target
    TryEnd, // End try/catch/finally block: TryEnd
    Throw, // Throw exception: Throw Rs
    LoadException, // Load current exception: LoadException Rd
    ClearException, // Clear current exception: ClearException
    CreateCallback, // Create C callback wrapper: CreateCallback Rd, Rs1, Rs2 (func, sig)
};

// Register identifier - u16 allows for 65,536 virtual registers
pub const Reg = u16;

pub const Instruction = struct {
    op: OpCode,
    // Destination register (for operations that produce a result)
    dst: ?Reg = null,
    // Source registers (for operations that read registers)
    src1: ?Reg = null,
    src2: ?Reg = null,
    // Immediate value (for constants, offsets, etc.)
    immediate: ?Value = null,
    // Variable name (for LoadVar/StoreVar)
    var_name: ?[]const u8 = null,
    // Jump target (for control flow instructions)
    jump_target: ?usize = null,

    pub fn deinit(self: *Instruction, allocator: std.mem.Allocator) void {
        if (self.immediate) |*imm| {
            imm.deinit(allocator);
        }
        if (self.var_name) |name| {
            allocator.free(name);
        }
    }

    // Helper constructors for common instruction patterns
    pub fn loadConst(dst: Reg, value: Value) Instruction {
        return .{ .op = .LoadConst, .dst = dst, .immediate = value };
    }

    pub fn loadVar(dst: Reg, var_name: []const u8) Instruction {
        return .{ .op = .LoadVar, .dst = dst, .var_name = var_name };
    }

    pub fn add(dst: Reg, src1: Reg, src2: Reg) Instruction {
        return .{ .op = .Add, .dst = dst, .src1 = src1, .src2 = src2 };
    }

    pub fn call(dst: Reg, func_reg: Reg) Instruction {
        return .{ .op = .Call, .dst = dst, .src1 = func_reg };
    }

    pub fn jumpIfFalse(condition: Reg, offset: i32) Instruction {
        return .{ .op = .JumpIfFalse, .src1 = condition, .immediate = .{ .Int = offset } };
    }

    pub fn ret(value_reg: ?Reg) Instruction {
        return .{ .op = .Return, .src1 = value_reg };
    }
};

pub const Function = struct {
    instructions: std.ArrayList(Instruction),
    allocator: std.mem.Allocator,
    name: []const u8,
    param_count: usize,
    rest_param: ?[]const u8 = null, // Name of rest parameter if any
    // Register allocation info
    register_count: u16, // Total number of registers needed for this function
    local_count: u16, // Number of local variable registers
    temp_count: u16, // Number of temporary computation registers

    pub fn init(allocator: std.mem.Allocator) Function {
        return Function{
            .instructions = std.ArrayList(Instruction).init(allocator),
            .allocator = allocator,
            .name = "",
            .param_count = 0,
            .register_count = 0,
            .local_count = 0,
            .temp_count = 0,
        };
    }

    // Changed self to mutable *Function
    pub fn deinit(self: *Function) void {
        for (self.instructions.items) |*instr| {
            // instr is already *Instruction (mutable pointer from iterator)
            if (instr.immediate) |*immediate| {
                // immediate is *Value (mutable pointer)
                immediate.deinit(self.allocator);
            }
            // Free var_name if it exists
            if (instr.var_name) |name| {
                self.allocator.free(name);
            }
        }
        self.instructions.deinit();

        // Free the function name
        if (self.name.len > 0) {
            self.allocator.free(self.name);
        }
        
        // Free rest parameter name if present
        if (self.rest_param) |rp| {
            self.allocator.free(rp);
        }
    }
    
    pub fn clone(self: *const Function, allocator: std.mem.Allocator) !Function {
        var new_func = Function.init(allocator);
        new_func.name = try allocator.dupe(u8, self.name);
        new_func.param_count = self.param_count;
        new_func.rest_param = if (self.rest_param) |rp| try allocator.dupe(u8, rp) else null;
        new_func.register_count = self.register_count;
        new_func.local_count = self.local_count;
        new_func.temp_count = self.temp_count;
        
        // Clone instructions
        for (self.instructions.items) |instr| {
            var new_instr = instr;
            if (instr.immediate) |imm| {
                new_instr.immediate = try imm.clone(allocator);
            }
            if (instr.var_name) |name| {
                new_instr.var_name = try allocator.dupe(u8, name);
            }
            try new_func.instructions.append(new_instr);
        }
        
        return new_func;
    }
};

pub const Module = struct {
    functions: []Function,
    allocator: std.mem.Allocator,
    // Track all function objects created during deserialization for proper cleanup
    deserialized_functions: std.ArrayList(*Function),
    // Flag to indicate if this module owns the functions (for deserialized modules)
    owns_functions: bool = true,

    pub fn deinit(self: *Module) void {
        // Clean up main functions only if we own them
        if (self.owns_functions) {
            for (self.functions) |*func| {
                func.deinit();
            }
        }
        self.allocator.free(self.functions);

        // Clean up all deserialized function objects
        for (self.deserialized_functions.items) |func| {
            func.deinit();
            self.allocator.destroy(func);
        }
        self.deserialized_functions.deinit();
    }

    pub fn readFromFile(allocator: std.mem.Allocator, reader: anytype) !Module {
        // Read and verify magic number
        var magic: [4]u8 = undefined;
        try reader.readNoEof(&magic);
        if (!std.mem.eql(u8, &magic, "GENE")) {
            return error.InvalidMagicNumber;
        }

        // Read number of functions
        var buf: [4]u8 = undefined;
        try reader.readNoEof(&buf);
        const num_functions = std.mem.readInt(u32, &buf, .little);

        // Allocate functions array
        var functions = try allocator.alloc(Function, num_functions);
        errdefer allocator.free(functions);

        // Initialize deserialized functions tracker
        var deserialized_functions = std.ArrayList(*Function).init(allocator);
        errdefer {
            for (deserialized_functions.items) |func| {
                func.deinit();
                allocator.destroy(func);
            }
            deserialized_functions.deinit();
        }

        // Read each function
        var i: usize = 0;
        while (i < num_functions) : (i += 1) {
            functions[i] = try readFunctionFromFile(allocator, reader, &deserialized_functions);
        }

        return Module{
            .functions = functions,
            .allocator = allocator,
            .deserialized_functions = deserialized_functions,
        };
    }

    pub fn writeToFile(self: *const Module, writer: anytype) !void {
        // Write magic number to identify Gene bytecode files
        try writer.writeAll("GENE");

        // Write number of functions
        var buf: [4]u8 = undefined;
        std.mem.writeInt(u32, &buf, @intCast(self.functions.len), .little);
        try writer.writeAll(&buf);

        // Write each function
        for (self.functions) |func| {
            try writeFunctionToFile(&func, writer);
        }
    }

    fn writeFunctionToFile(func: *const Function, writer: anytype) !void {
        // Write function name length and name
        var len_buf: [4]u8 = undefined;
        std.mem.writeInt(u32, &len_buf, @intCast(func.name.len), .little);
        try writer.writeAll(&len_buf);
        try writer.writeAll(func.name);
        
        // Write param count
        var param_buf: [2]u8 = undefined;
        std.mem.writeInt(u16, &param_buf, @intCast(func.param_count), .little);
        try writer.writeAll(&param_buf);
        
        // Write number of instructions
        var buf: [4]u8 = undefined;
        std.mem.writeInt(u32, &buf, @intCast(func.instructions.items.len), .little);
        try writer.writeAll(&buf);

        // Write each instruction
        for (func.instructions.items) |instr| {
            // Write opcode
            try writer.writeByte(@intFromEnum(instr.op));

            // Write register fields
            // dst register
            if (instr.dst) |dst| {
                try writer.writeByte(1);
                var reg_buf: [2]u8 = undefined;
                std.mem.writeInt(u16, &reg_buf, dst, .little);
                try writer.writeAll(&reg_buf);
            } else {
                try writer.writeByte(0);
            }
            
            // src1 register
            if (instr.src1) |src1| {
                try writer.writeByte(1);
                var reg_buf: [2]u8 = undefined;
                std.mem.writeInt(u16, &reg_buf, src1, .little);
                try writer.writeAll(&reg_buf);
            } else {
                try writer.writeByte(0);
            }
            
            // src2 register
            if (instr.src2) |src2| {
                try writer.writeByte(1);
                var reg_buf: [2]u8 = undefined;
                std.mem.writeInt(u16, &reg_buf, src2, .little);
                try writer.writeAll(&reg_buf);
            } else {
                try writer.writeByte(0);
            }

            // Write immediate if present
            if (instr.immediate) |immediate| {
                try writer.writeByte(1); // Has immediate
                try writeValueToFile(immediate, writer);
            } else {
                try writer.writeByte(0); // No immediate
            }
            
            // Write var_name if present
            if (instr.var_name) |var_name| {
                try writer.writeByte(1);
                std.mem.writeInt(u32, &len_buf, @intCast(var_name.len), .little);
                try writer.writeAll(&len_buf);
                try writer.writeAll(var_name);
            } else {
                try writer.writeByte(0);
            }
        }
    }

    fn writeValueToFile(value: Value, writer: anytype) !void {
        // Write value type tag
        try writer.writeByte(@intFromEnum(value));

        var buf: [8]u8 = undefined;
        switch (value) {
            .Int => |n| {
                std.mem.writeInt(i64, &buf, n, .little);
                try writer.writeAll(buf[0..8]);
            },
            .Float => |n| {
                const bits = @as(u64, @bitCast(n));
                std.mem.writeInt(u64, &buf, bits, .little);
                try writer.writeAll(buf[0..8]);
            },
            .String => |s| {
                var len_buf: [4]u8 = undefined;
                std.mem.writeInt(u32, &len_buf, @intCast(s.len), .little);
                try writer.writeAll(&len_buf);
                try writer.writeAll(s);
            },
            .Bool => |b| try writer.writeByte(if (b) 1 else 0),
            .Nil => {},
            .Symbol => |s| {
                var len_buf: [4]u8 = undefined;
                std.mem.writeInt(u32, &len_buf, @intCast(s.len), .little);
                try writer.writeAll(&len_buf);
                try writer.writeAll(s);
            },
            .Variable => |v| {
                var len_buf: [4]u8 = undefined;
                std.mem.writeInt(u32, &len_buf, @intCast(v.name.len), .little);
                try writer.writeAll(&len_buf);
                try writer.writeAll(v.name);
            },
            .Function => |f| {
                // Write function name length and name
                var len_buf: [4]u8 = undefined;
                std.mem.writeInt(u32, &len_buf, @intCast(f.name.len), .little);
                try writer.writeAll(&len_buf);
                try writer.writeAll(f.name);

                // Write parameter count
                std.mem.writeInt(u32, &len_buf, @intCast(f.param_count), .little);
                try writer.writeAll(&len_buf);

                // Write number of instructions
                std.mem.writeInt(u32, &len_buf, @intCast(f.instructions.items.len), .little);
                try writer.writeAll(&len_buf);

                // Write each instruction
                for (f.instructions.items) |instr| {
                    // Write opcode
                    try writer.writeByte(@intFromEnum(instr.op));

                    // Write immediate presence flag
                    try writer.writeByte(if (instr.immediate != null) 1 else 0);

                    // Write immediate if present
                    if (instr.immediate) |immediate| {
                        try writeValueToFile(immediate, writer);
                    }
                }
            },
            .BuiltinOperator => |op| {
                try writer.writeByte(@intFromEnum(op));
            },
            .ReturnAddress => |addr| {
                var usize_buf: [@sizeOf(usize)]u8 = undefined;
                std.mem.writeInt(usize, &usize_buf, addr.stack_ptr, .little);
                try writer.writeAll(&usize_buf);
                std.mem.writeInt(usize, &usize_buf, addr.arg_count, .little);
                try writer.writeAll(&usize_buf);
            },
            .Array, .Map, .Class, .Object, .Module, .CPtr, .CFunction, .CStruct, .CArray, .StdlibFunction, .FileHandle, .Error, .FFIFunction, .NativeFunction, .CCallback => {
                // For now, we don't support serializing complex types like arrays, maps, classes, objects, modules, FFI types, stdlib types, and errors
                // This would require more sophisticated serialization
                return error.UnsupportedComplexType;
            },
        }
    }
};

fn readFunctionFromFile(allocator: std.mem.Allocator, reader: anytype, deserialized_functions: *std.ArrayList(*Function)) !Function {
    var func = Function.init(allocator);
    
    // Read function name
    var len_buf: [4]u8 = undefined;
    try reader.readNoEof(&len_buf);
    const name_len = std.mem.readInt(u32, &len_buf, .little);
    const name = try allocator.alloc(u8, name_len);
    try reader.readNoEof(name);
    allocator.free(func.name); // Free the default name
    func.name = name;
    
    // Read param count
    var param_buf: [2]u8 = undefined;
    try reader.readNoEof(&param_buf);
    func.param_count = std.mem.readInt(u16, &param_buf, .little);

    // Read number of instructions
    var buf: [4]u8 = undefined;
    try reader.readNoEof(&buf);
    const num_instrs = std.mem.readInt(u32, &buf, .little);

    // Read each instruction
    var i: usize = 0;
    while (i < num_instrs) : (i += 1) {
        // Read opcode
        const op = @as(OpCode, @enumFromInt(try reader.readByte()));
        
        var instr = Instruction{ .op = op };
        
        // Read dst register
        if (try reader.readByte() == 1) {
            var reg_buf: [2]u8 = undefined;
            try reader.readNoEof(&reg_buf);
            instr.dst = std.mem.readInt(u16, &reg_buf, .little);
        }
        
        // Read src1 register
        if (try reader.readByte() == 1) {
            var reg_buf: [2]u8 = undefined;
            try reader.readNoEof(&reg_buf);
            instr.src1 = std.mem.readInt(u16, &reg_buf, .little);
        }
        
        // Read src2 register
        if (try reader.readByte() == 1) {
            var reg_buf: [2]u8 = undefined;
            try reader.readNoEof(&reg_buf);
            instr.src2 = std.mem.readInt(u16, &reg_buf, .little);
        }

        // Read immediate if present
        if (try reader.readByte() == 1) {
            instr.immediate = try readValueFromFile(allocator, reader, deserialized_functions);
        }
        
        // Read var_name if present
        if (try reader.readByte() == 1) {
            try reader.readNoEof(&len_buf);
            const var_name_len = std.mem.readInt(u32, &len_buf, .little);
            const var_name = try allocator.alloc(u8, var_name_len);
            try reader.readNoEof(var_name);
            instr.var_name = var_name;
        }

        try func.instructions.append(instr);
    }

    return func;
}

fn readValueFromFile(allocator: std.mem.Allocator, reader: anytype, deserialized_functions: *std.ArrayList(*Function)) !Value {
    // Read value type tag
    const tag = try reader.readByte();

    var buf: [8]u8 = undefined;
    return switch (tag) {
        @intFromEnum(Value.Int) => {
            try reader.readNoEof(&buf);
            return Value{ .Int = std.mem.readInt(i64, &buf, .little) };
        },
        @intFromEnum(Value.Float) => {
            try reader.readNoEof(&buf);
            const bits = std.mem.readInt(u64, &buf, .little);
            return Value{ .Float = @as(f64, @bitCast(bits)) };
        },
        @intFromEnum(Value.String) => {
            // Read string length
            try reader.readNoEof(buf[0..4]);
            const len = std.mem.readInt(u32, buf[0..4], .little);

            // Read string data
            const str = try allocator.alloc(u8, len);
            errdefer allocator.free(str);
            try reader.readNoEof(str);

            return Value{ .String = str };
        },
        @intFromEnum(Value.Bool) => {
            const b = try reader.readByte();
            return Value{ .Bool = b == 1 };
        },
        @intFromEnum(Value.Nil) => Value{ .Nil = {} },
        @intFromEnum(Value.Symbol) => {
            // Read symbol length
            try reader.readNoEof(buf[0..4]);
            const len = std.mem.readInt(u32, buf[0..4], .little);

            // Read symbol data
            const sym = try allocator.alloc(u8, len);
            errdefer allocator.free(sym);
            try reader.readNoEof(sym);

            return Value{ .Symbol = sym };
        },
        @intFromEnum(Value.Variable) => {
            // Read variable name length
            try reader.readNoEof(buf[0..4]);
            const len = std.mem.readInt(u32, buf[0..4], .little);

            // Read variable name
            const name = try allocator.alloc(u8, len);
            errdefer allocator.free(name);
            try reader.readNoEof(name);

            return Value{ .Variable = .{ .name = name } };
        },
        @intFromEnum(Value.Function) => {
            // Read function name length
            try reader.readNoEof(buf[0..4]);
            const name_len = std.mem.readInt(u32, buf[0..4], .little);

            // Read function name
            const name = try allocator.alloc(u8, name_len);
            errdefer allocator.free(name);
            try reader.readNoEof(name);

            // Read parameter count
            try reader.readNoEof(buf[0..4]);
            const param_count = std.mem.readInt(u32, buf[0..4], .little);

            // Read number of instructions
            try reader.readNoEof(buf[0..4]);
            const num_instrs = std.mem.readInt(u32, buf[0..4], .little);

            // Create function and read instructions
            var func = try allocator.create(Function);
            errdefer allocator.destroy(func);
            func.* = Function.init(allocator);
            func.name = name;
            func.param_count = param_count;

            // Read each instruction
            var i: usize = 0;
            while (i < num_instrs) : (i += 1) {
                // Read opcode
                const op = @as(OpCode, @enumFromInt(try reader.readByte()));

                // Read operand presence flag
                const has_operand = try reader.readByte() == 1;

                // Read operand if present
                const operand = if (has_operand) try readValueFromFile(allocator, reader, deserialized_functions) else null;

                try func.instructions.append(.{
                    .op = op,
                    .immediate = operand,
                });
            }

            // Register the function for cleanup
            try deserialized_functions.append(func);

            return Value{ .Function = func };
        },
        @intFromEnum(Value.BuiltinOperator) => {
            const op_tag = try reader.readByte();
            const op = @as(types.BuiltinOperatorType, @enumFromInt(op_tag));
            return Value{ .BuiltinOperator = op };
        },
        @intFromEnum(Value.ReturnAddress) => {
            var usize_buf: [@sizeOf(usize)]u8 = undefined;
            try reader.readNoEof(&usize_buf);
            const stack_ptr = std.mem.readInt(usize, &usize_buf, .little);
            try reader.readNoEof(&usize_buf);
            const arg_count = std.mem.readInt(usize, &usize_buf, .little);
            return Value{ .ReturnAddress = .{ .stack_ptr = stack_ptr, .arg_count = arg_count } };
        },
        else => error.InvalidValueType,
    };
}

pub fn lowerToBytecode(allocator: std.mem.Allocator, nodes: []ast.AstNode) !Function {
    var instructions = std.ArrayList(Instruction).init(allocator);
    errdefer {
        for (instructions.items) |*instr| {
            instr.deinit(allocator);
        }
        instructions.deinit();
    }

    var i: usize = 0;
    while (i < nodes.len) : (i += 1) {
        const node = nodes[i];
        switch (node) {
            .Expression => |expr| {
                switch (expr) {
                    .Variable => |var_expr| {
                        if (std.mem.eql(u8, var_expr.name, "print")) {
                            // Skip the print function itself, the next node should be the argument
                            i += 1;
                            if (i >= nodes.len) return error.UnexpectedEOF;
                            const arg_node = nodes[i];
                            switch (arg_node) {
                                .Expression => |arg_expr| {
                                    // First lower the argument expression
                                    try lowerExpression(allocator, &instructions, arg_expr);
                                    // Then add the print instruction
                                    try instructions.append(.{ .op = .Print });
                                },
                            }
                        } else {
                            try lowerExpression(allocator, &instructions, expr);
                        }
                    },
                    else => try lowerExpression(allocator, &instructions, expr),
                }
            },
        }
    }

    try instructions.append(.{ .op = .Return });

    return Function{
        .instructions = instructions,
        .allocator = allocator,
        .name = "", // Default empty name
        .param_count = 0, // Default no parameters
    };
}

fn lowerExpression(allocator: std.mem.Allocator, instructions: *std.ArrayList(Instruction), expr: ast.Expression) !void {
    debug.log("lowerExpression: processing expression type: {s}", .{@tagName(expr)});
    debug.log("lowerExpression: current instruction count: {}", .{instructions.items.len});
    switch (expr) {
        .SimpleFuncDef => {
            // TODO: Implement proper lowering for SimpleFuncDef
            debug.log("lowerExpression: SimpleFuncDef not yet implemented", .{});
            return error.NotImplemented;
        },
        .Literal => |lit| {
            try instructions.append(.{
                .op = .LoadConst,
                .immediate = try lit.value.clone(allocator),
            });
        },
        .BinaryOp => |bin_op| {
            try lowerExpression(allocator, instructions, bin_op.left.*);
            try lowerExpression(allocator, instructions, bin_op.right.*);

            // Check the TokenKind and compare the identifier string if applicable
            switch (bin_op.op) {
                .Ident => |op_ident| {
                    if (std.mem.eql(u8, op_ident, "+")) {
                        try instructions.append(.{ .op = .Add });
                    } else if (std.mem.eql(u8, op_ident, "-")) {
                        try instructions.append(.{ .op = .Sub });
                    } else if (std.mem.eql(u8, op_ident, "<")) {
                        try instructions.append(.{ .op = .Lt });
                    } else if (std.mem.eql(u8, op_ident, ">")) {
                        try instructions.append(.{ .op = .Gt });
                    } else if (std.mem.eql(u8, op_ident, "==")) {
                        try instructions.append(.{ .op = .Eq });
                    } else {
                        std.debug.print("Unsupported binary operator identifier: {s}\n", .{op_ident});
                        return error.UnsupportedOperator;
                    }
                },
                else => {
                    // This case should ideally not happen if the parser correctly creates BinaryOp nodes
                    std.debug.print("Unexpected token kind for binary operator: {any}\n", .{bin_op.op});
                    return error.InvalidOperatorToken; // Or a more specific error
                },
            }
        },
        .Variable => |var_expr| {
            // Load the variable onto the stack
            try instructions.append(.{
                .op = .LoadVar,
                .var_name = try allocator.dupe(u8, var_expr.name),
            });
        },
        .If => |if_expr| {
            // Evaluate condition
            try lowerExpression(allocator, instructions, if_expr.condition.*);

            // Placeholder for conditional branching
            // In a real implementation, this would generate conditional jump instructions

            // For now, just evaluate the then branch
            try lowerExpression(allocator, instructions, if_expr.then_branch.*);

            // Lower the else branch (no longer optional)
            try lowerExpression(allocator, instructions, if_expr.else_branch.*);
        },
        .FuncCall => |func_call| {
            debug.log("lowerExpression: Processing function call", .{});
            // Check if it's a call to a built-in function
            if (func_call.func.* == .Variable) {
                const func_name = func_call.func.*.Variable.name;
                debug.log("lowerExpression: Function call to variable: {s}", .{func_name});

                // Handle built-in functions
                if (std.mem.eql(u8, func_name, "print")) {
                    debug.log("lowerExpression: Handling built-in print function", .{});
                    // For print, we just need to evaluate the argument and then print it
                    if (func_call.args.items.len != 1) {
                        debug.log("lowerExpression: Invalid argument count for print: {}", .{func_call.args.items.len});
                        return error.InvalidOperatorArity;
                    }

                    // For print, we just need to evaluate the argument and then print it
                    debug.log("lowerExpression: Evaluating print argument", .{});
                    try lowerExpression(allocator, instructions, func_call.args.items[0].*);
                    debug.log("lowerExpression: Print argument evaluated successfully", .{});

                    // Add the print instruction
                    try instructions.append(.{
                        .op = .Print,
                    });
                    debug.log("lowerExpression: Print instruction added", .{});

                    return;
                }
            }

            // Regular function call
            debug.log("lowerExpression: Processing regular function call with {} args", .{func_call.args.items.len});
            // First, evaluate the function expression
            debug.log("lowerExpression: Evaluating function expression", .{});
            try lowerExpression(allocator, instructions, func_call.func.*);
            debug.log("lowerExpression: Function expression evaluated successfully", .{});

            // Then evaluate arguments
            debug.log("lowerExpression: Evaluating {} function arguments", .{func_call.args.items.len});
            for (func_call.args.items, 0..) |arg, i| {
                debug.log("lowerExpression: Evaluating argument {}/{}", .{ i + 1, func_call.args.items.len });
                try lowerExpression(allocator, instructions, arg.*);
                debug.log("lowerExpression: Argument {}/{} evaluated successfully", .{ i + 1, func_call.args.items.len });
            }
            debug.log("lowerExpression: All function arguments evaluated successfully", .{});

            // Add the call instruction with the argument count
            debug.log("lowerExpression: Adding Call instruction with {} args", .{func_call.args.items.len});
            try instructions.append(.{
                .op = .Call,
                .immediate = .{ .Int = @intCast(func_call.args.items.len) },
            });
            debug.log("lowerExpression: Call instruction added successfully", .{});
        },
        .FuncDef => |func_def| {
            debug.log("lowerExpression: Processing function definition: {s}", .{func_def.name});
            debug.log("lowerExpression: Function has {} parameters", .{func_def.params.len});

            // Create a new bytecode function object
            var func_instructions = std.ArrayList(Instruction).init(allocator);
            errdefer {
                for (func_instructions.items) |*instr| {
                    instr.deinit(allocator);
                }
                func_instructions.deinit();
            }

            // Generate bytecode for the function body
            debug.log("lowerExpression: Generating bytecode for function body", .{});
            try lowerExpression(allocator, &func_instructions, func_def.body.*);
            debug.log("lowerExpression: Function body bytecode generated successfully", .{});

            // Add return instruction at the end if not present
            try func_instructions.append(.{ .op = .Return });
            debug.log("lowerExpression: Added return instruction to function", .{});

            // Create the function object
            const func_obj = try allocator.create(Function);
            errdefer allocator.destroy(func_obj);
            func_obj.* = Function{
                .instructions = func_instructions,
                .allocator = allocator,
                .name = try allocator.dupe(u8, func_def.name),
                .param_count = func_def.params.len,
            };

            // Create the function value
            const func_value = types.Value{ .Function = func_obj };
            debug.log("lowerExpression: Created function object for {s}", .{func_def.name});

            // First load the function value onto the stack
            try instructions.append(.{
                .op = .LoadConst,
                .immediate = func_value,
            });
            debug.log("lowerExpression: LoadConst instruction added for function", .{});

            // Then store the function value in a variable with the function name
            try instructions.append(.{
                .op = .StoreVar,
                .var_name = try allocator.dupe(u8, func_def.name),
            });
            debug.log("lowerExpression: Stored function {s} as Function value", .{func_def.name});

            // Load the function value again for global storage
            try instructions.append(.{
                .op = .LoadConst,
                .immediate = types.Value{ .Function = func_obj },
            });

            // Store the function in a variable with its name
            try instructions.append(.{
                .op = .StoreGlobal,
                .var_name = try allocator.dupe(u8, func_def.name),
            });
            debug.log("lowerExpression: StoreGlobal instruction added for function {s}", .{func_def.name});
        },
        .VarDecl => |var_decl| {
            // Evaluate the variable's value
            try lowerExpression(allocator, instructions, var_decl.value.*);

            // Store it in a variable
            try instructions.append(.{
                .op = .StoreGlobal,
                .var_name = try allocator.dupe(u8, var_decl.name),
            });
        },
        .ArrayLiteral => |arr_lit| {
            debug.log("lowerExpression: Processing ArrayLiteral", .{});
            for (arr_lit.elements) |element_ptr| {
                try lowerExpression(allocator, instructions, element_ptr.*);
            }
            try instructions.append(.{
                .op = .Array,
                .immediate = .{ .Int = @intCast(arr_lit.elements.len) },
            });
            debug.log("lowerExpression: Array instruction added with {} elements", .{arr_lit.elements.len});
        },
        .MapLiteral => |map_lit| {
            debug.log("lowerExpression: Processing MapLiteral", .{});
            for (map_lit.entries) |entry| {
                try lowerExpression(allocator, instructions, entry.key.*);
                try lowerExpression(allocator, instructions, entry.value.*);
            }
            try instructions.append(.{
                .op = .Map,
                .immediate = .{ .Int = @intCast(map_lit.entries.len) },
            });
            debug.log("lowerExpression: Map instruction added with {} entries", .{map_lit.entries.len});
        },
        .DoBlock => |do_block| {
            debug.log("lowerExpression: Processing DoBlock", .{});
            for (do_block.statements) |stmt_ptr| {
                try lowerExpression(allocator, instructions, stmt_ptr.*);
            }
            debug.log("lowerExpression: DoBlock statements processed", .{});
        },
    }
}
