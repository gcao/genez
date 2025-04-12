const std = @import("std");
const types = @import("types.zig");
const parser = @import("parser.zig");
const TokenKind = parser.TokenKind;

pub const Type = types.Type;
pub const Value = types.Value;

pub const Literal = struct {
    value: Value,

    pub fn deinit(self: *Literal, allocator: std.mem.Allocator) void {
        switch (self.value) {
            .String => |str| allocator.free(str),
            .Symbol => |sym| allocator.free(sym),
            .Array => |arr| {
                for (arr) |*val| {
                    val.deinit(allocator);
                }
                allocator.free(arr);
            },
            .Map => |*map| {
                var it = map.iterator();
                while (it.next()) |entry| {
                    allocator.free(entry.key_ptr.*);
                    entry.value_ptr.deinit(allocator);
                }
                map.deinit();
            },
            else => {},
        }
    }

    pub fn clone(self: Literal, allocator: std.mem.Allocator) error{OutOfMemory}!Literal {
        return Literal{
            .value = try self.value.clone(allocator),
        };
    }
};

pub const Variable = struct {
    name: []const u8,

    pub fn deinit(self: *Variable, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
    }

    pub fn clone(self: Variable, allocator: std.mem.Allocator) error{OutOfMemory}!Variable {
        return Variable{
            .name = try allocator.dupe(u8, self.name),
        };
    }
};

pub const BinaryOp = struct {
    op: TokenKind,
    left: *Expression,
    right: *Expression,

    pub fn deinit(self: *BinaryOp, allocator: std.mem.Allocator) void {
        // Restore recursive deinit
        self.left.deinit(allocator);
        allocator.destroy(self.left);
        self.right.deinit(allocator);
        allocator.destroy(self.right);
    }

    pub fn clone(self: BinaryOp, allocator: std.mem.Allocator) error{OutOfMemory}!BinaryOp {
        const left = try allocator.create(Expression);
        errdefer allocator.destroy(left);
        left.* = try self.left.clone(allocator);

        const right = try allocator.create(Expression);
        errdefer allocator.destroy(right);
        right.* = try self.right.clone(allocator);

        return BinaryOp{
            .op = self.op,
            .left = left,
            .right = right,
        };
    }
};

pub const FuncCall = struct {
    func: *Expression,
    args: std.ArrayList(*Expression),

    pub fn deinit(self: *FuncCall, allocator: std.mem.Allocator) void {
        // Restore recursive deinit
        self.func.deinit(allocator);
        allocator.destroy(self.func);
        for (self.args.items) |arg| {
            arg.deinit(allocator);
            allocator.destroy(arg);
        }
        self.args.deinit();
    }

    pub fn clone(self: FuncCall, allocator: std.mem.Allocator) error{OutOfMemory}!FuncCall {
        var args = std.ArrayList(*Expression).init(allocator);
        errdefer {
            for (args.items) |arg| {
                arg.deinit(allocator);
                allocator.destroy(arg);
            }
            args.deinit();
        }

        try args.ensureTotalCapacity(self.args.items.len);
        for (self.args.items) |arg| {
            const new_arg = try allocator.create(Expression);
            errdefer allocator.destroy(new_arg);
            new_arg.* = try arg.clone(allocator);
            try args.append(new_arg);
        }

        const func = try allocator.create(Expression);
        errdefer allocator.destroy(func);
        func.* = try self.func.clone(allocator);

        return FuncCall{
            .func = func,
            .args = args,
        };
    }
};

pub const If = struct {
    condition: *Expression,
    then_branch: *Expression,
    else_branch: ?*Expression,

    pub fn deinit(self: *If, allocator: std.mem.Allocator) void {
        // Restore recursive deinit
        self.condition.deinit(allocator);
        allocator.destroy(self.condition);
        self.then_branch.deinit(allocator);
        allocator.destroy(self.then_branch);
        if (self.else_branch) |else_branch| {
            else_branch.deinit(allocator);
            allocator.destroy(else_branch);
        }
    }

    pub fn clone(self: If, allocator: std.mem.Allocator) error{OutOfMemory}!If {
        const condition = try allocator.create(Expression);
        errdefer allocator.destroy(condition);
        condition.* = try self.condition.clone(allocator);

        const then_branch = try allocator.create(Expression);
        errdefer allocator.destroy(then_branch);
        then_branch.* = try self.then_branch.clone(allocator);

        var else_branch: ?*Expression = null;
        if (self.else_branch) |old_else_branch| {
            else_branch = try allocator.create(Expression);
            errdefer allocator.destroy(else_branch.?);
            else_branch.?.* = try old_else_branch.clone(allocator);
        }

        return If{
            .condition = condition,
            .then_branch = then_branch,
            .else_branch = else_branch,
        };
    }
};

pub const FuncParam = struct {
    name: []const u8,
    param_type: ?[]const u8,

    pub fn deinit(self: *FuncParam, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
        if (self.param_type) |param_type| {
            allocator.free(param_type);
        }
    }

    pub fn clone(self: FuncParam, allocator: std.mem.Allocator) error{OutOfMemory}!FuncParam {
        return FuncParam{
            .name = try allocator.dupe(u8, self.name),
            .param_type = if (self.param_type) |pt| try allocator.dupe(u8, pt) else null,
        };
    }
};

pub const FuncDef = struct {
    name: []const u8,
    params: []FuncParam,
    body: *Expression,

    pub fn deinit(self: *FuncDef, allocator: std.mem.Allocator) void {
        // Only free memory directly owned by FuncDef
        allocator.free(self.name);
        for (self.params) |*param| {
            // Call deinit on params as they contain allocated strings
            param.deinit(allocator);
        }
        allocator.free(self.params); // Free the slice itself
        // Restore recursive deinit for body
        self.body.deinit(allocator);
        allocator.destroy(self.body);
    }

    pub fn clone(self: FuncDef, allocator: std.mem.Allocator) error{OutOfMemory}!FuncDef {
        var params = try allocator.alloc(FuncParam, self.params.len);
        errdefer allocator.free(params);

        for (self.params, 0..) |param, i| {
            params[i] = try param.clone(allocator);
        }

        const body = try allocator.create(Expression);
        errdefer allocator.destroy(body);
        body.* = try self.body.clone(allocator);

        return FuncDef{
            .name = try allocator.dupe(u8, self.name),
            .params = params,
            .body = body,
        };
    }
};

pub const VarDecl = struct {
    name: []const u8,
    value: *Expression,

    pub fn deinit(self: *VarDecl, allocator: std.mem.Allocator) void {
        // Only free memory directly owned by VarDecl
        allocator.free(self.name);
        // Restore recursive deinit for value
        self.value.deinit(allocator);
        allocator.destroy(self.value);
    }

    pub fn clone(self: VarDecl, allocator: std.mem.Allocator) error{OutOfMemory}!VarDecl {
        const value = try allocator.create(Expression);
        errdefer allocator.destroy(value);
        value.* = try self.value.clone(allocator);

        return VarDecl{
            .name = try allocator.dupe(u8, self.name),
            .value = value,
        };
    }
};

pub const Expression = union(enum) {
    Literal: Literal,
    Variable: Variable,
    BinaryOp: BinaryOp,
    If: If,
    FuncCall: FuncCall,
    FuncDef: FuncDef,
    VarDecl: VarDecl,

    pub fn deinit(self: *Expression, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .Literal => |*lit| lit.deinit(allocator),
            .Variable => |*var_expr| var_expr.deinit(allocator),
            .BinaryOp => |*bin_op| bin_op.deinit(allocator),
            .If => |*if_expr| if_expr.deinit(allocator),
            .FuncCall => |*func_call| func_call.deinit(allocator),
            .FuncDef => |*func_def| func_def.deinit(allocator),
            .VarDecl => |*var_decl| var_decl.deinit(allocator),
        }
    }

    pub fn clone(self: Expression, allocator: std.mem.Allocator) error{OutOfMemory}!Expression {
        return switch (self) {
            .Literal => |lit| Expression{ .Literal = try lit.clone(allocator) },
            .Variable => |var_expr| Expression{ .Variable = try var_expr.clone(allocator) },
            .BinaryOp => |bin_op| Expression{ .BinaryOp = try bin_op.clone(allocator) },
            .If => |if_expr| Expression{ .If = try if_expr.clone(allocator) },
            .FuncCall => |func_call| Expression{ .FuncCall = try func_call.clone(allocator) },
            .FuncDef => |func_def| Expression{ .FuncDef = try func_def.clone(allocator) },
            .VarDecl => |var_decl| Expression{ .VarDecl = try var_decl.clone(allocator) },
        };
    }
};

pub const AstNode = union(enum) {
    Expression: Expression,

    pub fn deinit(self: *AstNode, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .Expression => |*expr| expr.deinit(allocator),
        }
    }

    pub fn clone(self: AstNode, allocator: std.mem.Allocator) error{OutOfMemory}!AstNode {
        return switch (self) {
            .Expression => |expr| AstNode{ .Expression = try expr.clone(allocator) },
        };
    }
};
