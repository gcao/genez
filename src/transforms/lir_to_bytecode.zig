const std = @import("std");
const lir = @import("../ir/lir.zig");
const bytecode = @import("../backend/bytecode.zig");

/// Convert LIR to final bytecode
/// This stage performs final optimizations and generates executable bytecode
pub const ConversionResult = struct {
    main_func: bytecode.Function,
    created_functions: std.ArrayList(*bytecode.Function),

    pub fn deinit(self: *ConversionResult) void {
        // Clean up created functions
        for (self.created_functions.items) |func| {
            func.deinit();
            self.created_functions.allocator.destroy(func);
        }
        self.created_functions.deinit();
        
        // Note: main_func should be cleaned up by the caller
    }
};

pub fn convert(allocator: std.mem.Allocator, lir_program: *lir.LIR) !ConversionResult {
    var created_functions = std.ArrayList(*bytecode.Function).init(allocator);
    errdefer {
        for (created_functions.items) |func| {
            func.deinit();
            allocator.destroy(func);
        }
        created_functions.deinit();
    }

    // Find the main function
    var main_func: ?bytecode.Function = null;
    
    for (lir_program.functions.items) |*lir_func| {
        const bytecode_func = try convertFunction(allocator, lir_func);
        
        if (std.mem.eql(u8, lir_func.name, "main")) {
            main_func = bytecode_func;
        } else {
            // Store non-main functions
            const func_ptr = try allocator.create(bytecode.Function);
            func_ptr.* = bytecode_func;
            try created_functions.append(func_ptr);
        }
    }

    return ConversionResult{
        .main_func = main_func orelse bytecode.Function.init(allocator),
        .created_functions = created_functions,
    };
}

fn convertFunction(allocator: std.mem.Allocator, lir_func: *lir.LIR.Function) !bytecode.Function {
    var bytecode_func = bytecode.Function.init(allocator);
    bytecode_func.name = try allocator.dupe(u8, lir_func.name);
    bytecode_func.param_count = lir_func.param_count;

    // Convert each LIR instruction to bytecode
    for (lir_func.instructions.items) |lir_instr| {
        try convertInstruction(allocator, lir_instr, &bytecode_func);
    }

    return bytecode_func;
}

fn convertInstruction(allocator: std.mem.Allocator, lir_instr: lir.LIR.Instruction, bytecode_func: *bytecode.Function) !void {
    switch (lir_instr) {
        .LoadConst => |load_const| {
            // Convert LIR Value to bytecode Value
            const operand = try valueToOperand(allocator, load_const.val);
            try bytecode_func.instructions.append(.{
                .op = .LoadConst,
                .operand = operand,
            });
        },
        .LoadNil => {
            try bytecode_func.instructions.append(.{
                .op = .LoadConst,
                .operand = bytecode.Value{ .Nil = {} },
            });
        },
        .LoadVariable => |load_var| {
            // Convert LIR LoadVariable to bytecode LoadVar
            const name_value = try valueToOperand(allocator, load_var.name);
            try bytecode_func.instructions.append(.{
                .op = .LoadVar,
                .operand = name_value,
            });
        },
        .Add => {
            try bytecode_func.instructions.append(.{
                .op = .Add,
                .operand = null,
            });
        },
        .Sub => {
            try bytecode_func.instructions.append(.{
                .op = .Sub,
                .operand = null,
            });
        },
        .Mul => {
            try bytecode_func.instructions.append(.{
                .op = .Mul,
                .operand = null,
            });
        },
        .Div => {
            try bytecode_func.instructions.append(.{
                .op = .Div,
                .operand = null,
            });
        },
        .Eq => {
            try bytecode_func.instructions.append(.{
                .op = .Eq,
                .operand = null,
            });
        },
        .Lt => {
            try bytecode_func.instructions.append(.{
                .op = .Lt,
                .operand = null,
            });
        },
        .Gt => {
            try bytecode_func.instructions.append(.{
                .op = .Gt,
                .operand = null,
            });
        },
        .Call => |call| {
            // For now, convert to simple call instruction
            // In a full implementation, we'd handle the register-based calling convention
            const arg_count_operand = bytecode.Value{ .Int = @intCast(call.args.items.len) };
            
            try bytecode_func.instructions.append(.{
                .op = .Call,
                .operand = arg_count_operand,
            });
        },
        .Return => {
            try bytecode_func.instructions.append(.{
                .op = .Return,
                .operand = null,
            });
        },
        .Jump => |jump| {
            const label_operand = bytecode.Value{ .Int = @intCast(jump.target) };
            
            try bytecode_func.instructions.append(.{
                .op = .Jump,
                .operand = label_operand,
            });
        },
        .JumpIf => |jump_if| {
            // TODO: Need to handle JumpIfTrue - for now skip
            _ = jump_if;
        },
        .JumpIfNot => |jump_if_not| {
            const label_operand = bytecode.Value{ .Int = @intCast(jump_if_not.target) };
            
            try bytecode_func.instructions.append(.{
                .op = .JumpIfFalse,
                .operand = label_operand,
            });
        },
        .Move => {
            // Register moves don't directly translate to stack operations
            // This would be optimized away in a real implementation
        },
        else => {
            // Handle other instructions
            // For now, we skip instructions that don't have direct bytecode equivalents
        },
    }
}

fn valueToOperand(allocator: std.mem.Allocator, value: anytype) !bytecode.Value {
    // Properly convert LIR Value to bytecode Value with correct memory management
    return switch (value) {
        .Int => |i| bytecode.Value{ .Int = i },
        .Float => |f| bytecode.Value{ .Float = f },
        .Bool => |b| bytecode.Value{ .Bool = b },
        .Nil => bytecode.Value{ .Nil = {} },
        .String => |s| bytecode.Value{ .String = try allocator.dupe(u8, s) },
        .Symbol => |s| bytecode.Value{ .Symbol = try allocator.dupe(u8, s) },
        else => {
            // For other value types, pass through for now
            return value;
        },
    };
}