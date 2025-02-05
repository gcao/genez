const std = @import("std");
const types = @import("types.zig");

pub const Type = types.Type;

pub const Value = types.Value;

pub const Literal = struct {
    value: Value,
};

pub const Variable = struct {
    name: []const u8,
};

pub const BinaryOpType = enum {
    Add,
};

pub const BinaryOp = struct {
    op: BinaryOpType,
    left: *Expression,
    right: *Expression,
};

pub const Expression = union(enum) {
    Literal: Literal,
    Variable: Variable,
    BinaryOp: BinaryOp,
};

pub const AstNode = union(enum) {
    Expression: Expression,
};
