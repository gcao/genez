const std = @import("std");

pub const AstNode = struct {
    tag: Tag,
    loc: Loc,

    pub fn deinit(self: *AstNode, allocator: std.mem.Allocator) void {
        switch (self.tag) {
            .Expression => |*expr| {
                switch (expr.*) {
                    .Literal => |*lit| {
                        switch (lit.value) {
                            .String => |str| allocator.free(str),
                            .Symbol => |sym| allocator.free(sym),
                            .Array => |arr| {
                                for (arr) |*val| {
                                    // Recursively deinit array values
                                    var tmp_node = AstNode{ .tag = .{ .Expression = .{ .Literal = .{ .value = val.* } } }, .loc = self.loc };
                                    tmp_node.deinit(allocator);
                                }
                                allocator.free(arr);
                            },
                            .Map => |*map| {
                                var it = map.iterator();
                                while (it.next()) |entry| {
                                    allocator.free(entry.key_ptr.*);
                                    // Recursively deinit map values
                                    var tmp_node = AstNode{ .tag = .{ .Expression = .{ .Literal = .{ .value = entry.value_ptr.* } } }, .loc = self.loc };
                                    tmp_node.deinit(allocator);
                                }
                                map.deinit();
                            },
                            else => {}, // No cleanup needed for other value types
                        }
                    },
                    .BinaryOp => |*bin_op| {
                        var left_node = AstNode{ .tag = .{ .Expression = bin_op.left.* }, .loc = self.loc };
                        left_node.deinit(allocator);
                        allocator.destroy(bin_op.left);

                        var right_node = AstNode{ .tag = .{ .Expression = bin_op.right.* }, .loc = self.loc };
                        right_node.deinit(allocator);
                        allocator.destroy(bin_op.right);
                    },
                    .IfExpr => |*if_expr| {
                        var cond_node = AstNode{ .tag = .{ .Expression = if_expr.condition.* }, .loc = self.loc };
                        cond_node.deinit(allocator);
                        allocator.destroy(if_expr.condition);

                        var then_node = AstNode{ .tag = .{ .Expression = if_expr.then_branch.* }, .loc = self.loc };
                        then_node.deinit(allocator);
                        allocator.destroy(if_expr.then_branch);

                        var else_node = AstNode{ .tag = .{ .Expression = if_expr.else_branch.* }, .loc = self.loc };
                        else_node.deinit(allocator);
                        allocator.destroy(if_expr.else_branch);
                    },
                    .FuncDef => |*func_def| {
                        allocator.free(func_def.name);
                        for (func_def.params.items) |param| {
                            allocator.free(param.name);
                        }
                        func_def.params.deinit();
                        var body_node = AstNode{ .tag = .{ .Expression = func_def.body.* }, .loc = self.loc };
                        body_node.deinit(allocator);
                        allocator.destroy(func_def.body);
                    },
                    .FuncCall => |*func_call| {
                        var func_node = AstNode{ .tag = .{ .Expression = func_call.func.* }, .loc = self.loc };
                        func_node.deinit(allocator);
                        allocator.destroy(func_call.func);

                        for (func_call.args.items) |arg| {
                            var arg_node = AstNode{ .tag = .{ .Expression = arg.* }, .loc = self.loc };
                            arg_node.deinit(allocator);
                            allocator.destroy(arg);
                        }
                        func_call.args.deinit();
                    },
                    .Variable => |*var_expr| {
                        allocator.free(var_expr.name);
                    },
                    .If => |*if_expr| {
                        var cond_node = AstNode{ .tag = .{ .Expression = if_expr.condition.* }, .loc = self.loc };
                        cond_node.deinit(allocator);
                        allocator.destroy(if_expr.condition);

                        var then_node = AstNode{ .tag = .{ .Expression = if_expr.then_branch.* }, .loc = self.loc };
                        then_node.deinit(allocator);
                        allocator.destroy(if_expr.then_branch);

                        if (if_expr.else_branch) |else_branch| {
                            var else_node = AstNode{ .tag = .{ .Expression = else_branch.* }, .loc = self.loc };
                            else_node.deinit(allocator);
                            allocator.destroy(else_branch);
                        }
                    },
                }
            },
        }
    }

    pub const Loc = struct {
        start: usize,
        end: usize,
    };

    pub const Tag = union(enum) {
        Expression: Expression,
        // Add other node types as needed
    };

    pub const Expression = union(enum) {
        Literal: Literal,
        Variable: Variable,
        BinaryOp: BinaryOp,
        IfExpr: IfExpr,
        FuncDef: FuncDef,
        FuncCall: FuncCall,
        If: If,
        // Add other expression types
    };

    pub const Literal = struct {
        value: Value,
    };

    pub const Variable = struct {
        name: []const u8,
    };

    pub const BinaryOp = struct {
        op: Op,
        left: *Expression,
        right: *Expression,
    };

    pub const Op = enum {
        add,
        sub,
        mul,
        div,
        lt,
        gt,
        eq,
    };

    pub const IfExpr = struct {
        condition: *Expression,
        then_branch: *Expression,
        else_branch: *Expression,
    };

    pub const FuncDef = struct {
        name: []const u8,
        params: std.ArrayList(Param),
        return_type: Type,
        body: *Expression,
    };

    pub const FuncCall = struct {
        func: *Expression,
        args: std.ArrayList(*Expression),
    };

    pub const If = struct {
        condition: *Expression,
        then_branch: *Expression,
        else_branch: ?*Expression,
    };

    pub const Param = struct {
        name: []const u8,
        param_type: Type,
    };

    pub const Type = enum {
        Int,
        Bool,
        // Add other types
    };

    pub const Value = union(enum) {
        Int: i64,
        Float: f64,
        String: []const u8,
        Bool: bool,
        Nil: void,
        Symbol: []const u8,
        Array: []Value,
        Map: std.StringHashMap(Value),

        pub fn clone(self: Value, allocator: std.mem.Allocator) !Value {
            return switch (self) {
                .Int => |val| Value{ .Int = val },
                .Float => |val| Value{ .Float = val },
                .String => |str| Value{ .String = try allocator.dupe(u8, str) },
                .Bool => |val| Value{ .Bool = val },
                .Nil => Value{ .Nil = {} },
                .Symbol => |sym| Value{ .Symbol = try allocator.dupe(u8, sym) },
                .Array => |arr| blk: {
                    var new_arr = try allocator.alloc(Value, arr.len);
                    errdefer allocator.free(new_arr);
                    for (arr, 0..) |val, i| {
                        new_arr[i] = try val.clone(allocator);
                    }
                    break :blk Value{ .Array = new_arr };
                },
                .Map => |map| blk: {
                    var new_map = std.StringHashMap(Value).init(allocator);
                    errdefer {
                        var it = new_map.iterator();
                        while (it.next()) |entry| {
                            allocator.free(entry.key_ptr.*);
                            const value = entry.value_ptr.*;
                            // Create a temporary node to deinit the value
                            var tmp_node = AstNode{ .tag = .{ .Expression = .{ .Literal = .{ .value = value } } }, .loc = .{ .start = 0, .end = 0 } };
                            tmp_node.deinit(allocator);
                        }
                        new_map.deinit();
                    }

                    var it = map.iterator();
                    while (it.next()) |entry| {
                        const key = try allocator.dupe(u8, entry.key_ptr.*);
                        errdefer allocator.free(key);
                        const value = try entry.value_ptr.*.clone(allocator);
                        errdefer {
                            // Create a temporary node to deinit the value
                            var tmp_node = AstNode{ .tag = .{ .Expression = .{ .Literal = .{ .value = value } } }, .loc = .{ .start = 0, .end = 0 } };
                            tmp_node.deinit(allocator);
                        }
                        try new_map.put(key, value);
                    }
                    break :blk Value{ .Map = new_map };
                },
            };
        }
    };
};
