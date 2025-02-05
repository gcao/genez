const std = @import("std");
const hir = @import("hir.zig");

pub const MIR = struct {
    allocator: std.mem.Allocator,
    functions: std.ArrayList(Function),
    globals: std.ArrayList(Global),

    pub fn init(allocator: std.mem.Allocator) MIR {
        return .{
            .allocator = allocator,
            .functions = std.ArrayList(Function).init(allocator),
            .globals = std.ArrayList(Global).init(allocator),
        };
    }

    pub fn deinit(self: *MIR) void {
        for (self.functions.items) |*func| {
            func.deinit();
        }
        self.functions.deinit();

        for (self.globals.items) |*global| {
            global.deinit();
        }
        self.globals.deinit();
    }

    pub const Function = struct {
        allocator: std.mem.Allocator,
        name: []const u8,
        params: std.ArrayList(Param),
        return_type: hir.Type,
        blocks: std.ArrayList(BasicBlock),

        pub fn init(allocator: std.mem.Allocator) Function {
            return .{
                .allocator = allocator,
                .name = "",
                .params = std.ArrayList(Param).init(allocator),
                .return_type = .void,
                .blocks = std.ArrayList(BasicBlock).init(allocator),
            };
        }

        pub fn deinit(self: *Function) void {
            for (self.blocks.items) |*block| {
                block.deinit();
            }
            self.blocks.deinit();
            self.params.deinit();
        }

        pub const Param = struct {
            name: []const u8,
            type: hir.Type,
        };
    };

    pub const Global = struct {
        name: []const u8,
        type: hir.Type,
        init: ?Instruction,

        pub fn deinit(self: *Global) void {
            if (self.init) |*instr| {
                instr.deinit();
            }
        }
    };

    pub const BasicBlock = struct {
        instructions: std.ArrayList(Instruction),

        pub fn init(allocator: std.mem.Allocator) BasicBlock {
            return .{
                .instructions = std.ArrayList(Instruction).init(allocator),
            };
        }

        pub fn deinit(self: *BasicBlock) void {
            self.instructions.deinit();
        }
    };

    pub const Instruction = union(enum) {
        LoadInt: i64,
        LoadString: []const u8,
        LoadBool: bool,
        Add,
        Print,
        Return,

        pub fn deinit(self: *Instruction) void {
            switch (self.*) {
                .LoadString => |str| self.allocator.free(str),
                else => {},
            }
        }
    };

    pub fn hirToMir(allocator: std.mem.Allocator, hir_prog: hir.HIR) !MIR {
        var mir = MIR.init(allocator);
        errdefer mir.deinit();

        // Convert each HIR function to MIR
        for (hir_prog.functions.items) |hir_func| {
            var mir_func = Function.init(allocator);
            mir_func.name = hir_func.name;
            mir_func.return_type = hir_func.return_type;

            // Create entry block
            var entry_block = BasicBlock.init(allocator);

            // Convert HIR statements to MIR instructions
            for (hir_func.body.items) |stmt| {
                try lowerStatement(&entry_block.instructions, stmt);
            }

            try mir_func.blocks.append(entry_block);
            try mir.functions.append(mir_func);
        }

        return mir;
    }

    fn lowerStatement(instructions: *std.ArrayList(Instruction), stmt: hir.HIR.Statement) !void {
        switch (stmt) {
            .Expression => |expr| try lowerExpression(instructions, expr),
            .Return => |expr| {
                if (expr) |ret_expr| {
                    try lowerExpression(instructions, ret_expr);
                }
                try instructions.append(.Return);
            },
        }
    }

    fn lowerExpression(instructions: *std.ArrayList(Instruction), expr: hir.HIR.Expression) !void {
        switch (expr.value) {
            .literal => |lit| switch (lit) {
                .int => |val| try instructions.append(.{ .LoadInt = val }),
                .string => |val| try instructions.append(.{ .LoadString = val }),
                .bool => |val| try instructions.append(.{ .LoadBool = val }),
            },
            .binary_op => |bin_op| {
                try lowerExpression(instructions, bin_op.left.*);
                try lowerExpression(instructions, bin_op.right.*);
                switch (bin_op.op) {
                    .add => try instructions.append(.Add),
                }
            },
            else => {},
        }
    }
};
