const std = @import("std");
const types = @import("types.zig");

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

pub const BinaryOpType = enum {
    Add,
};

pub const BinaryOp = struct {
    op: BinaryOpType,
    left: *Expression,
    right: *Expression,

    pub fn deinit(self: *BinaryOp, allocator: std.mem.Allocator) void {
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

pub const Expression = union(enum) {
    Literal: Literal,
    Variable: Variable,
    BinaryOp: BinaryOp,

    pub fn deinit(self: *Expression, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .Literal => |*lit| lit.deinit(allocator),
            .Variable => |*var_expr| var_expr.deinit(allocator),
            .BinaryOp => |*bin_op| bin_op.deinit(allocator),
        }
    }

    pub fn clone(self: Expression, allocator: std.mem.Allocator) error{OutOfMemory}!Expression {
        return switch (self) {
            .Literal => |lit| Expression{ .Literal = try lit.clone(allocator) },
            .Variable => |var_expr| Expression{ .Variable = try var_expr.clone(allocator) },
            .BinaryOp => |bin_op| Expression{ .BinaryOp = try bin_op.clone(allocator) },
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
