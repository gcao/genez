const std = @import("std");
const hir = @import("hir.zig");
const ast = @import("ast.zig");
const AstNode = ast.AstNode;

pub const MIR = struct {
    allocator: std.mem.Allocator,
    functions: std.ArrayList(Function),

    pub fn init(allocator: std.mem.Allocator) MIR {
        return MIR{
            .allocator = allocator,
            .functions = std.ArrayList(Function).init(allocator),
        };
    }

    pub fn deinit(self: *MIR) void {
        for (self.functions.items) |*func| {
            func.deinit();
        }
        self.functions.deinit();
    }

    pub const Function = struct {
        allocator: std.mem.Allocator,
        name: []const u8,
        blocks: std.ArrayList(Block),

        pub fn init(allocator: std.mem.Allocator) Function {
            return Function{
                .allocator = allocator,
                .name = "main",
                .blocks = std.ArrayList(Block).init(allocator),
            };
        }

        pub fn deinit(self: *Function) void {
            self.allocator.free(self.name);
            for (self.blocks.items) |*block| {
                block.deinit();
            }
            self.blocks.deinit();
        }
    };

    pub const Block = struct {
        allocator: std.mem.Allocator,
        instructions: std.ArrayList(Instruction),

        pub fn init(allocator: std.mem.Allocator) Block {
            return Block{
                .allocator = allocator,
                .instructions = std.ArrayList(Instruction).init(allocator),
            };
        }

        pub fn deinit(self: *Block) void {
            for (self.instructions.items) |*instr| {
                switch (instr.*) {
                    .LoadString => |str| self.allocator.free(str),
                    .LoadSymbol => |sym| self.allocator.free(sym),
                    .LoadVariable => |name| self.allocator.free(name),
                    .LoadArray => |arr| {
                        for (arr) |*val| {
                            // Create a temporary node to deinit the value
                            var tmp_node = AstNode{ .tag = .{ .Expression = .{ .Literal = .{ .value = val.* } } }, .loc = .{ .start = 0, .end = 0 } };
                            tmp_node.deinit(self.allocator);
                        }
                        self.allocator.free(arr);
                    },
                    .LoadMap => |*map| {
                        var it = map.iterator();
                        while (it.next()) |entry| {
                            self.allocator.free(entry.key_ptr.*);
                            // Create a temporary node to deinit the value
                            var tmp_node = AstNode{ .tag = .{ .Expression = .{ .Literal = .{ .value = entry.value_ptr.* } } }, .loc = .{ .start = 0, .end = 0 } };
                            tmp_node.deinit(self.allocator);
                        }
                        map.deinit();
                    },
                    .DefineFunction => |func| {
                        func.deinit();
                        self.allocator.destroy(func);
                    },
                    .Call => |call| {
                        self.allocator.free(call.func);
                    },
                    else => {},
                }
            }
            self.instructions.deinit();
        }
    };

    pub const Instruction = union(enum) {
        LoadInt: i64,
        LoadFloat: f64,
        LoadBool: bool,
        LoadString: []const u8,
        LoadNil,
        LoadSymbol: []const u8,
        LoadArray: []AstNode.Value,
        LoadMap: std.StringHashMap(AstNode.Value),
        LoadVariable: []const u8,
        Add,
        Sub,
        Mul,
        Div,
        Lt,
        Gt,
        Eq,
        Print,
        Return,
        // New instructions for function definitions and calls
        DefineFunction: *Function,
        Call: struct {
            func: []const u8,
            arg_count: usize,
        },
        // New instructions for if expressions
        JumpIfFalse: struct {
            offset: usize,
        },
        Jump: struct {
            offset: usize,
        },
        Store: Expression,
    };

    pub const FunctionDefinition = struct {
        name: []const u8,
        params: []const []const u8,
        return_type: hir.Type,
        body: *Expression,

        pub fn deinit(self: *FunctionDefinition, allocator: std.mem.Allocator) void {
            for (self.params) |param| {
                allocator.free(param);
            }
            allocator.free(self.params);
            self.body.deinit(allocator);
            allocator.destroy(self.body);
        }
    };

    pub const Expression = union(enum) {
        LoadInt: i64,
        LoadFloat: f64,
        LoadBool: bool,
        LoadString: []const u8,
        LoadNil,
        LoadSymbol: []const u8,
        LoadVariable: []const u8,
        LoadArray: []AstNode.Value,
        LoadMap: std.StringHashMap(AstNode.Value),
        DefineFunction: *Function,
        BinaryOp: struct {
            left: *Expression,
            right: *Expression,
            op: hir.HIR.BinaryOpType,
        },
        FuncDef: FunctionDefinition,
        FuncCall: struct {
            func: *Expression,
            args: []*Expression,
        },
        If: struct {
            condition: *Expression,
            then_branch: *Expression,
            else_branch: ?*Expression,
        },
        Call: struct {
            func: []const u8,
            arg_count: usize,
        },

        pub fn deinit(self: *Expression, allocator: std.mem.Allocator) void {
            switch (self.*) {
                .LoadString => |str| allocator.free(str),
                .LoadSymbol => |sym| allocator.free(sym),
                .LoadVariable => |name| allocator.free(name),
                .LoadArray => |arr| {
                    for (arr) |*val| {
                        // Create a temporary node to deinit the value
                        var tmp_node = AstNode{ .tag = .{ .Expression = .{ .Literal = .{ .value = val.* } } }, .loc = .{ .start = 0, .end = 0 } };
                        tmp_node.deinit(allocator);
                    }
                    allocator.free(arr);
                },
                .LoadMap => |*map| {
                    var it = map.iterator();
                    while (it.next()) |entry| {
                        allocator.free(entry.key_ptr.*);
                        // Create a temporary node to deinit the value
                        var tmp_node = AstNode{ .tag = .{ .Expression = .{ .Literal = .{ .value = entry.value_ptr.* } } }, .loc = .{ .start = 0, .end = 0 } };
                        tmp_node.deinit(allocator);
                    }
                    map.deinit();
                },
                .LoadInt, .LoadFloat, .LoadBool, .LoadNil => {},
                .BinaryOp => |*bin_op| {
                    bin_op.left.deinit(allocator);
                    allocator.destroy(bin_op.left);
                    bin_op.right.deinit(allocator);
                    allocator.destroy(bin_op.right);
                },
                .FuncDef => |*func_def| func_def.deinit(allocator),
                .FuncCall => |*func_call| {
                    func_call.func.deinit(allocator);
                    allocator.destroy(func_call.func);
                    for (func_call.args) |*arg| {
                        arg.*.deinit(allocator);
                        allocator.destroy(arg);
                    }
                    allocator.free(func_call.args);
                },
                .If => |*if_expr| {
                    if_expr.condition.deinit(allocator);
                    allocator.destroy(if_expr.condition);
                    if_expr.then_branch.deinit(allocator);
                    allocator.destroy(if_expr.then_branch);
                    if (if_expr.else_branch) |*eb| {
                        eb.*.deinit(allocator);
                        allocator.destroy(eb);
                    }
                },
                .DefineFunction => |func| allocator.destroy(func),
                .Call => |call| {
                    allocator.free(call.func);
                },
            }
        }
    };
};
