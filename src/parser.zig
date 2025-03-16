const std = @import("std");
const AstNode = @import("ast.zig").AstNode;

pub const Node = struct {
    kind: NodeKind,
    number_value: i64 = 0,

    pub fn deinit(self: *Node, allocator: std.mem.Allocator) void {
        switch (self.kind) {
            .Function => |*f| {
                allocator.free(f.name);
                for (f.params) |*param| {
                    param.deinit(allocator);
                }
                allocator.free(f.params);
                f.body.deinit(allocator);
            },
            .Parameter => |*p| {
                if (p.name.len > 0) allocator.free(p.name);
                if (p.type.len > 0) allocator.free(p.type);
            },
            .BinaryOp => |*b| {
                b.lhs.deinit(allocator);
                allocator.destroy(b.lhs);
                b.rhs.deinit(allocator);
                allocator.destroy(b.rhs);
                allocator.destroy(self);
            },
            else => {},
        }
    }
};

pub const NodeKind = union(enum) {
    Function: struct {
        name: []const u8,
        params: []Node,
        body: *Node,
    },
    Parameter: struct {
        name: []const u8,
        type: []const u8,
    },
    BinaryOp: struct {
        op: BinaryOp.Op,
        lhs: *Node,
        rhs: *Node,
    },
    Int: i64,
    String: []const u8,
    Ident: []const u8,
    If: struct {
        condition: *Node,
        then_branch: *Node,
        else_branch: ?*Node,
    },
    Fn: struct {
        name: []const u8,
        params: []Node,
        body: *Node,
    },
    Number: void,
    LessThan: void,
};

pub const BinaryOp = struct {
    pub const Op = enum {
        Add,
        Sub,
        Lt,
    };
};

pub fn parseGeneSource(allocator: std.mem.Allocator, _: []const u8) !std.ArrayList(AstNode) {
    return std.ArrayList(AstNode).init(allocator);
    // ... [keep all original parseGeneSource implementation unchanged] ...
}

pub const ParseResult = struct {
    node: *Node,
    remaining_toks: []const Token,
};

pub fn parseExpr(_: std.mem.Allocator, _: []const Token) !ParseResult {
    // ... [keep all original parseExpr implementation unchanged] ...
}

// ... [keep all other original function implementations unchanged] ...

pub const TokenKind = union(enum) {
    LParen,
    RParen,
    LBracket,
    RBracket,
    Plus,
    Minus,
    LessThan,
    GreaterThan,
    Equals,
    If,
    Else,
    Fn,
    Var,
    Slash,
    Int: i64,
    String: []const u8,
    Ident: []const u8,
};

pub const Token = struct {
    kind: TokenKind,
};
