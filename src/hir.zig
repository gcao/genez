const std = @import("std");
const parser = @import("parser.zig");
const ast = @import("ast.zig");

pub const HIR = struct {
    allocator: std.mem.Allocator,
    functions: std.ArrayList(Function),
    globals: std.ArrayList(Global),

    pub fn init(allocator: std.mem.Allocator) HIR {
        return .{
            .allocator = allocator,
            .functions = std.ArrayList(Function).init(allocator),
            .globals = std.ArrayList(Global).init(allocator),
        };
    }

    pub fn deinit(self: *HIR) void {
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
        name: []const u8,
        params: std.ArrayList(Param),
        return_type: Type,
        body: std.ArrayList(Statement),

        pub fn init(allocator: std.mem.Allocator) Function {
            return .{
                .name = "",
                .params = std.ArrayList(Param).init(allocator),
                .return_type = .void,
                .body = std.ArrayList(Statement).init(allocator),
            };
        }

        pub fn deinit(self: *Function) void {
            self.params.deinit();
            self.body.deinit();
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
        }
    };

    pub const Statement = union(enum) {
        expr: Expression,
        decl: VariableDecl,
        ret: Expression,

        pub fn deinit(self: *Statement) void {
            switch (self.*) {
                .expr => |*expr| expr.deinit(),
                .decl => |*decl| decl.deinit(),
                .ret => |*expr| expr.deinit(),
            }
        }
    };

    pub const Expression = union(enum) {
        literal: Value,
        ident: []const u8,
        call: Call,
        binary: BinaryOp,

        pub fn deinit(self: *Expression) void {
            switch (self.*) {
                .literal => |*val| {
                    if (val.* == .string) {
                        self.allocator.free(val.string);
                    }
                },
                .ident => |ident| self.allocator.free(ident),
                .call => |*call| call.deinit(),
                .binary => |*bin| bin.deinit(),
            }
        }
    };

    pub const Call = struct {
        callee: []const u8,
        args: std.ArrayList(Expression),

        pub fn deinit(self: *Call) void {
            for (self.args.items) |*arg| {
                arg.deinit();
            }
            self.args.deinit();
            self.allocator.free(self.callee);
        }
    };

    pub const BinaryOp = struct {
        op: BinaryOperator,
        lhs: *Expression,
        rhs: *Expression,

        pub fn deinit(self: *BinaryOp) void {
            self.lhs.deinit();
            self.rhs.deinit();
        }
    };

    pub const BinaryOperator = enum {
        add,
        sub,
        mul,
        div,
        eq,
        neq,
    };

    pub const VariableDecl = struct {
        name: []const u8,
        type: Type,
        value: Expression,

        pub fn deinit(self: *VariableDecl) void {
            self.allocator.free(self.name);
            self.value.deinit();
        }
    };

    pub const Type = enum {
        void,
        bool,
        int,
        float,
        string,
        function,
    };

    pub const Value = union(Type) {
        void: void,
        bool: bool,
        int: i64,
        float: f64,
        string: []const u8,
        function: *const Function,
    };

    pub fn astToHir(allocator: std.mem.Allocator, nodes: []const ast.AstNode) !HIR {
        var hir = HIR.init(allocator);
        errdefer hir.deinit();

        // Create a main function to hold our statements
        var main_fn = Function.init(allocator);
        main_fn.name = try allocator.dupe(u8, "main");
        main_fn.return_type = .void;

        for (nodes) |node| {
            switch (node) {
                .Stmt => |stmt| switch (stmt) {
                    .ExprStmt => |expr| {
                        const hir_expr = try lowerExpr(allocator, expr);
                        try main_fn.body.append(.{ .expr = hir_expr });
                    },
                    else => return error.UnsupportedStatement,
                },
                else => return error.UnsupportedNode,
            }
        }

        try hir.functions.append(main_fn);
        return hir;
    }

    fn lowerExpr(allocator: std.mem.Allocator, expr: ast.Expr) !Expression {
        switch (expr) {
            .StrLit => |value| {
                const str = try allocator.dupe(u8, value);
                return .{ .literal = .{ .string = str } };
            },
            .Ident => |value| {
                const ident = try allocator.dupe(u8, value);
                return .{ .ident = ident };
            },
            else => return error.UnsupportedExpression,
        }
    }
};
