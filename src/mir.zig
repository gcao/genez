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
        return_type: Type,
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
            self.allocator.free(self.name);
        }

        pub const Param = struct {
            name: []const u8,
            type: Type,
        };
    };

    pub const Global = struct {
        allocator: std.mem.Allocator,
        name: []const u8,
        type: Type,
        value: Value,

        pub fn deinit(self: *Global) void {
            if (self.value == .string) {
                self.allocator.free(self.value.string);
            }
            self.allocator.free(self.name);
        }
    };

    pub const BasicBlock = struct {
        allocator: std.mem.Allocator,
        label: []const u8,
        instructions: std.ArrayList(Instruction),
        terminator: Terminator,

        pub fn init(allocator: std.mem.Allocator) BasicBlock {
            return .{
                .allocator = allocator,
                .label = "",
                .instructions = std.ArrayList(Instruction).init(allocator),
                .terminator = .{ .ret = .void },
            };
        }

        pub fn deinit(self: *BasicBlock) void {
            self.instructions.deinit();
            self.allocator.free(self.label);
        }
    };

    pub const Instruction = union(enum) {
        load: Load,
        store: Store,
        binary: BinaryOp,
        call: Call,

        pub const Load = struct {
            dest: []const u8,
            src: []const u8,
        };

        pub const Store = struct {
            dest: []const u8,
            value: Value,
        };
    };

    pub const Terminator = union(enum) {
        ret: Value,
        br: Branch,
        cond_br: ConditionalBranch,

        pub const Branch = struct {
            target: []const u8,
        };

        pub const ConditionalBranch = struct {
            cond: []const u8,
            true_target: []const u8,
            false_target: []const u8,
        };
    };

    pub const Call = struct {
        callee: []const u8,
        args: std.ArrayList([]const u8),
    };

    pub const BinaryOp = struct {
        op: BinaryOperator,
        lhs: []const u8,
        rhs: []const u8,
        dest: []const u8,
    };

    pub const BinaryOperator = enum {
        add,
        sub,
        mul,
        div,
        eq,
        neq,
    };

    pub const Type = enum {
        void,
        bool,
        int,
        float,
        string,
    };

    pub const Value = union(Type) {
        void: void,
        bool: bool,
        int: i64,
        float: f64,
        string: []const u8,
    };

    pub fn hirToMir(allocator: std.mem.Allocator, hir_prog: hir.HIR) !MIR {
        var mir = MIR.init(allocator);
        errdefer mir.deinit();

        // Convert each HIR function to MIR
        for (hir_prog.functions.items) |hir_fn| {
            var mir_fn = Function.init(allocator);
            mir_fn.name = try allocator.dupe(u8, hir_fn.name);
            mir_fn.return_type = switch (hir_fn.return_type) {
                .void => .void,
                .bool => .bool,
                .int => .int,
                .float => .float,
                .string => .string,
                else => return error.UnsupportedType,
            };

            // Create entry block
            var entry_block = BasicBlock.init(allocator);
            entry_block.label = try allocator.dupe(u8, "entry");

            // Convert statements
            for (hir_fn.body.items) |stmt| {
                switch (stmt) {
                    .expr => |expr| {
                        const instr = try lowerExpr(allocator, expr);
                        try entry_block.instructions.append(instr);
                    },
                    else => return error.UnsupportedStatement,
                }
            }

            // Add return terminator
            entry_block.terminator = .{ .ret = .void };
            try mir_fn.blocks.append(entry_block);
            try mir.functions.append(mir_fn);
        }

        return mir;
    }

    fn lowerExpr(allocator: std.mem.Allocator, expr: hir.HIR.Expression) !Instruction {
        switch (expr) {
            .literal => |lit| {
                const dest = try allocator.dupe(u8, "tmp");
                return .{ .store = .{ .dest = dest, .value = switch (lit) {
                    .string => |s| .{ .string = try allocator.dupe(u8, s) },
                    .int => |i| .{ .int = i },
                    .float => |f| .{ .float = f },
                    .bool => |b| .{ .bool = b },
                    else => return error.UnsupportedLiteral,
                } } };
            },
            .ident => |ident| {
                const dest = try allocator.dupe(u8, "tmp");
                return .{ .load = .{
                    .dest = dest,
                    .src = try allocator.dupe(u8, ident),
                } };
            },
            else => return error.UnsupportedExpression,
        }
    }
};
