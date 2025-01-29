const std = @import("std");
const types = @import("types.zig");
const Type = types.Type;

pub const AstNode = union(enum) {
    Program: *Program,
    Expression: *Expression,
    Statement: *Statement,
    Function: *Function,
    Class: *Class,
    Namespace: *Namespace,

    pub const Program = struct {
        statements: []*Statement,
    };

    pub const Expression = union(enum) {
        Literal: *Literal,
        Variable: *Variable,
        BinaryOp: *BinaryOp,
        UnaryOp: *UnaryOp,
        Call: *Call,
        PropertyAccess: *PropertyAccess,
        ArrayAccess: *ArrayAccess,
        New: *New,
        Cast: *Cast,
    };

    pub const Statement = union(enum) {
        Expression: *Expression,
        VariableDeclaration: *VariableDeclaration,
        Assignment: *Assignment,
        If: *If,
        Loop: *Loop,
        Return: *Return,
        Block: *Block,
    };

    pub const Function = struct {
        name: []const u8,
        params: []Param,
        return_type: ?*Type,
        body: *Block,
        is_pure: bool = false,

        pub const Param = struct {
            name: []const u8,
            type: ?*Type,
            default_value: ?*Expression,
        };
    };

    pub const Class = struct {
        name: []const u8,
        fields: []Field,
        methods: []*Function,
        parent: ?[]const u8,

        pub const Field = struct {
            name: []const u8,
            type: *Type,
            required: bool,
        };
    };

    pub const Namespace = struct {
        name: []const u8,
        types: []*Type,
        functions: []*Function,
        sub_namespaces: []*Namespace,
    };

    // Expression variants
    pub const Literal = struct {
        value: types.Value,
    };

    pub const Variable = struct {
        name: []const u8,
    };

    pub const BinaryOp = struct {
        op: BinaryOperator,
        left: *Expression,
        right: *Expression,
    };

    pub const UnaryOp = struct {
        op: UnaryOperator,
        operand: *Expression,
    };

    pub const Call = struct {
        function: *Expression,
        args: std.ArrayList(AstNode),
    };

    pub const PropertyAccess = struct {
        object: *Expression,
        property: []const u8,
    };

    pub const ArrayAccess = struct {
        array: *Expression,
        index: *Expression,
    };

    pub const New = struct {
        class_name: []const u8,
        args: []*Expression,
    };

    pub const Cast = struct {
        expression: *Expression,
        target_type: *Type,
    };

    // Statement variants
    pub const VariableDeclaration = struct {
        name: []const u8,
        type: ?*Type,
        value: ?*Expression,
    };

    pub const Assignment = struct {
        target: *Expression,
        value: *Expression,
    };

    pub const If = struct {
        condition: *Expression,
        then_branch: *Block,
        else_branch: ?*Block,
    };

    pub const Loop = struct {
        condition: *Expression,
        body: *Block,
    };

    pub const Return = struct {
        value: ?*Expression,
    };

    pub const Block = struct {
        statements: []*Statement,
    };

    pub const BinaryOperator = enum {
        Add,
        Subtract,
        Multiply,
        Divide,
        Equal,
        NotEqual,
        LessThan,
        LessEqual,
        GreaterThan,
        GreaterEqual,
        And,
        Or,
    };

    pub const UnaryOperator = enum {
        Negate,
        Not,
    };
};
