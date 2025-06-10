const std = @import("std");
const mir = @import("mir.zig");
const types = @import("../core/types.zig");

/// Low-level Intermediate Representation
/// Register-based bytecode for VM execution
pub const LIR = struct {
    allocator: std.mem.Allocator,
    functions: std.ArrayList(Function),

    pub fn init(allocator: std.mem.Allocator) LIR {
        return LIR{
            .allocator = allocator,
            .functions = std.ArrayList(Function).init(allocator),
        };
    }

    pub fn deinit(self: *LIR) void {
        for (self.functions.items) |*func| {
            func.deinit();
        }
        self.functions.deinit();
    }

    pub const Function = struct {
        allocator: std.mem.Allocator,
        name: []const u8,
        param_count: usize,
        register_count: usize, // Total number of virtual registers needed
        instructions: std.ArrayList(Instruction),
        labels: std.ArrayList(Label),

        pub fn init(allocator: std.mem.Allocator) Function {
            const default_name = allocator.dupe(u8, "main") catch @panic("Failed to allocate default function name");
            return Function{
                .allocator = allocator,
                .name = default_name,
                .param_count = 0,
                .register_count = 0,
                .instructions = std.ArrayList(Instruction).init(allocator),
                .labels = std.ArrayList(Label).init(allocator),
            };
        }

        pub fn deinit(self: *Function) void {
            self.allocator.free(self.name);
            
            // Clean up instructions
            for (self.instructions.items) |*instr| {
                instr.deinit(self.allocator);
            }
            self.instructions.deinit();
            
            // Clean up labels
            for (self.labels.items) |*label| {
                label.deinit(self.allocator);
            }
            self.labels.deinit();
        }
    };

    pub const Label = struct {
        name: []const u8,
        position: u32, // Instruction index

        pub fn deinit(self: *Label, allocator: std.mem.Allocator) void {
            allocator.free(self.name);
        }
    };

    /// Virtual register (will be mapped to physical registers or stack slots)
    pub const Reg = u16;

    /// Jump target
    pub const LabelId = u32;

    pub const Instruction = union(enum) {
        // Constants
        LoadConst: struct { dest: Reg, val: types.Value },
        LoadNil: struct { dest: Reg },
        
        // Variables
        LoadVariable: struct { dest: Reg, name: types.Value },

        // Arithmetic
        Add: struct { dest: Reg, left: Reg, right: Reg },
        Sub: struct { dest: Reg, left: Reg, right: Reg },
        Mul: struct { dest: Reg, left: Reg, right: Reg },
        Div: struct { dest: Reg, left: Reg, right: Reg },

        // Comparison
        Eq: struct { dest: Reg, left: Reg, right: Reg },
        Lt: struct { dest: Reg, left: Reg, right: Reg },
        Le: struct { dest: Reg, left: Reg, right: Reg },
        Gt: struct { dest: Reg, left: Reg, right: Reg },
        Ge: struct { dest: Reg, left: Reg, right: Reg },

        // Logical
        And: struct { dest: Reg, left: Reg, right: Reg },
        Or: struct { dest: Reg, left: Reg, right: Reg },
        Not: struct { dest: Reg, src: Reg },

        // Control flow
        Jump: struct { target: LabelId },
        JumpIf: struct { cond: Reg, target: LabelId },
        JumpIfNot: struct { cond: Reg, target: LabelId },

        // Function calls
        Call: struct { dest: Reg, func: Reg, args: std.ArrayList(Reg) },
        TailCall: struct { func: Reg, args: std.ArrayList(Reg) },
        Return: struct { value: ?Reg },

        // Object operations
        NewObject: struct { dest: Reg, class_id: u32 },
        GetField: struct { dest: Reg, obj: Reg, field_id: u32 },
        SetField: struct { obj: Reg, field_id: u32, value: Reg },

        // Method dispatch
        GetMethod: struct { dest: Reg, obj: Reg, method_id: u32 },
        CallMethod: struct { dest: Reg, obj: Reg, method_id: u32, args: std.ArrayList(Reg) },

        // Array operations
        NewArray: struct { dest: Reg, size: Reg },
        GetElement: struct { dest: Reg, arr: Reg, idx: Reg },
        SetElement: struct { arr: Reg, idx: Reg, value: Reg },
        ArrayLen: struct { dest: Reg, arr: Reg },

        // Type operations
        TypeCheck: struct { dest: Reg, value: Reg, type_id: u32 },
        Cast: struct { dest: Reg, value: Reg, type_id: u32 },

        // Memory
        Move: struct { dest: Reg, src: Reg },

        // Inline cache support (for future optimization)
        LoadIC: struct { dest: Reg, cache_id: u32 },
        StoreIC: struct { cache_id: u32, value: Reg },

        pub fn deinit(self: *Instruction, allocator: std.mem.Allocator) void {
            switch (self.*) {
                .LoadConst => |*load_const| {
                    load_const.val.deinit(allocator);
                },
                .LoadVariable => |*load_var| {
                    load_var.name.deinit(allocator);
                },
                .Call => |*call| {
                    call.args.deinit();
                },
                .TailCall => |*tail_call| {
                    tail_call.args.deinit();
                },
                .CallMethod => |*call_method| {
                    call_method.args.deinit();
                },
                else => {
                    // Most instructions don't need cleanup
                },
            }
        }
    };
};