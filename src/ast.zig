pub const std = @import("std");

// Example minimal AST structures

pub const AstNode = union(enum) {
    ClassDecl: ClassDecl,
    FnDecl: FnDecl,
    Stmt: Stmt,
    Expr: Expr,
};

pub const ClassDecl = struct {
    name: []const u8,
    props: []ClassProp,
};

pub const ClassProp = struct {
    name: []const u8,
    required: bool,
    typ: []const u8, // e.g. "Int", "String"
};

pub const FnDecl = struct {
    name: []const u8,
    // ...
};

pub const Stmt = union(enum) {
    VarDecl: VarDecl,
    ExprStmt: Expr,
    // ...
};

pub const VarDecl = struct {
    varName: []const u8,
    init: Expr,
};

pub const Expr = union(enum) {
    IntLit: i64,
    StrLit: []const u8,
    Ident: []const u8,
    // ...
};

// We might also define a top-level Program or Module
pub const GeneProgram = struct {
    nodes: []AstNode,
};
