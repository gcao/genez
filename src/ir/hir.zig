const std = @import("std");
const parser = @import("../frontend/parser.zig");
const ast = @import("../frontend/ast.zig");

pub const Type = enum {
    void,
    bool,
    int,
    float,
    string,
    function,
};

pub const HIR = struct {
    allocator: std.mem.Allocator,
    functions: std.ArrayList(Function),

    pub fn init(allocator: std.mem.Allocator) HIR {
        return HIR{
            .allocator = allocator,
            .functions = std.ArrayList(Function).init(allocator),
        };
    }

    pub fn deinit(self: *HIR) void {
        for (self.functions.items) |*func| {
            func.deinit();
        }
        self.functions.deinit();
    }

    pub const Function = struct {
        name: []const u8,
        params: []FuncParam, // Added params field
        body: std.ArrayList(Statement),
        allocator: std.mem.Allocator,

        pub fn init(allocator: std.mem.Allocator) Function {
            return Function{
                .name = "main",
                .params = &[_]FuncParam{}, // Initialize as an empty slice
                .body = std.ArrayList(Statement).init(allocator),
                .allocator = allocator,
            };
        }

        pub fn deinit(self: *Function) void {
            self.allocator.free(self.name);
            for (self.params) |*param| {
                param.deinit(self.allocator);
            }
            self.allocator.free(self.params); // Free the slice itself
            for (self.body.items) |*stmt| {
                stmt.deinit(self.allocator);
            }
            self.body.deinit();
        }
    };

    pub const Statement = union(enum) {
        Expression: Expression,

        pub fn deinit(self: *Statement, allocator: std.mem.Allocator) void {
            switch (self.*) {
                .Expression => |*expr| expr.deinit(allocator),
            }
        }
    };

    pub const Expression = union(enum) {
        literal: Literal,
        variable: Variable,
        binary_op: BinaryOp,
        if_expr: If,
        func_call: FuncCall,
        func_def: FuncDef,
        function: *Function, // Changed to mutable pointer
        var_decl: VarDecl,
        array_literal: ArrayLiteral, // New
        map_literal: MapLiteral, // New
        do_block: DoBlock, // New

        pub fn deinit(self: *Expression, allocator: std.mem.Allocator) void {
            switch (self.*) {
                .literal => |*lit| lit.deinit(allocator),
                .variable => |*var_expr| var_expr.deinit(allocator),
                .binary_op => |*bin_op| bin_op.deinit(allocator),
                .if_expr => |*if_expr| if_expr.deinit(allocator),
                .func_call => |*func_call| func_call.deinit(allocator),
                .func_def => |*func_def| func_def.deinit(allocator),
                .var_decl => |*var_decl| var_decl.deinit(allocator),
                .array_literal => |*arr_lit| arr_lit.deinit(allocator), // New
                .map_literal => |*map_lit| map_lit.deinit(allocator), // New
                .do_block => |*do_block| do_block.deinit(allocator), // New
                .function => |*func_ptr| { // Changed to func_ptr
                    func_ptr.*.deinit(); // Dereference before calling deinit
                    allocator.destroy(func_ptr);
                },
            }
        }
    };

    pub const ArrayLiteral = struct {
        elements: []*Expression,

        pub fn deinit(self: *ArrayLiteral, allocator: std.mem.Allocator) void {
            for (self.elements) |element_ptr| {
                element_ptr.deinit(allocator);
                allocator.destroy(element_ptr);
            }
            allocator.free(self.elements);
        }
    };

    pub const MapEntry = struct {
        key: *Expression,
        value: *Expression,

        pub fn deinit(self: *MapEntry, allocator: std.mem.Allocator) void {
            self.key.deinit(allocator);
            allocator.destroy(self.key);
            self.value.deinit(allocator);
            allocator.destroy(self.value);
        }
    };

    pub const MapLiteral = struct {
        entries: []MapEntry,

        pub fn deinit(self: *MapLiteral, allocator: std.mem.Allocator) void {
            for (self.entries) |*entry| {
                entry.deinit(allocator);
            }
            allocator.free(self.entries);
        }
    };

    pub const DoBlock = struct {
        statements: []*Expression,

        pub fn deinit(self: *DoBlock, allocator: std.mem.Allocator) void {
            for (self.statements) |stmt_ptr| {
                stmt_ptr.deinit(allocator);
                allocator.destroy(stmt_ptr);
            }
            allocator.free(self.statements);
        }
    };

    pub const Literal = union(enum) {
        nil: void,
        bool: bool,
        int: i64,
        float: f64,
        string: []const u8,
        symbol: []const u8,
        array: []*Expression, // Changed to []*Expression
        map: std.StringHashMap(*Expression), // Changed to StringHashMap(*Expression)

        pub fn deinit(self: *Literal, allocator: std.mem.Allocator) void {
            switch (self.*) {
                .string => |str| allocator.free(str),
                .symbol => |sym| allocator.free(sym),
                .array => |arr| {
                    for (arr) |element_ptr| {
                        element_ptr.deinit(allocator);
                        allocator.destroy(element_ptr);
                    }
                    allocator.free(arr);
                },
                .map => |*map| {
                    var it = map.iterator();
                    while (it.next()) |entry| {
                        allocator.free(entry.key_ptr.*); // Free key string
                        entry.value_ptr.*.deinit(allocator); // Deinit value expression
                        allocator.destroy(entry.value_ptr); // Destroy value expression pointer
                    }
                    map.deinit();
                },
                else => {},
            }
        }
    };

    pub const Variable = struct {
        name: []const u8,

        pub fn deinit(self: *Variable, allocator: std.mem.Allocator) void {
            allocator.free(self.name);
        }
    };

    pub const BinaryOp = struct {
        op: BinaryOpType,
        left: *Expression,
        right: *Expression,

        pub fn deinit(self: *BinaryOp, allocator: std.mem.Allocator) void {
            self.left.deinit(allocator);
            self.right.deinit(allocator);
            allocator.destroy(self.left);
            allocator.destroy(self.right);
        }
    };

    pub const BinaryOpType = enum {
        add,
        sub,
        lt,
        gt, // Added greater than
        eq, // Added equals
        mul,
        div,
        // TODO: Add neq, lte, gte etc.
    };

    pub const If = struct {
        condition: *Expression,
        then_branch: *Expression,
        else_branch: ?*Expression,

        pub fn deinit(self: *If, allocator: std.mem.Allocator) void {
            self.condition.deinit(allocator);
            allocator.destroy(self.condition);
            self.then_branch.deinit(allocator);
            allocator.destroy(self.then_branch);
            if (self.else_branch) |else_branch| {
                else_branch.deinit(allocator);
                allocator.destroy(else_branch);
            }
        }
    };

    pub const FuncCall = struct {
        func: *Expression,
        args: std.ArrayList(*Expression),

        pub fn deinit(self: *FuncCall, allocator: std.mem.Allocator) void {
            self.func.deinit(allocator);
            allocator.destroy(self.func);
            for (self.args.items) |arg| {
                arg.deinit(allocator);
                allocator.destroy(arg);
            }
            self.args.deinit();
        }
    };

    pub const FuncParam = struct {
        name: []const u8,
        param_type: ?[]const u8,

        pub fn deinit(self: *FuncParam, allocator: std.mem.Allocator) void {
            allocator.free(self.name);
            if (self.param_type) |pt| allocator.free(pt);
        }
    };

    pub const FuncDef = struct {
        name: []const u8,
        params: []FuncParam,
        body: *Expression,

        pub fn deinit(self: *FuncDef, allocator: std.mem.Allocator) void {
            allocator.free(self.name);

            for (self.params) |*param| {
                param.deinit(allocator);
            }
            allocator.free(self.params);

            self.body.deinit(allocator);
            allocator.destroy(self.body);
        }
    };

    pub const VarDecl = struct {
        name: []const u8,
        value: *Expression,

        pub fn deinit(self: *VarDecl, allocator: std.mem.Allocator) void {
            allocator.free(self.name);
            self.value.deinit(allocator);
            allocator.destroy(self.value);
        }
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

        for (nodes) |node| {
            switch (node) {
                .Stmt => |stmt| switch (stmt) {
                    .ExprStmt => |expr| {
                        const hir_expr = try lowerExpr(allocator, expr);
                        try main_fn.body.append(.{ .Expression = hir_expr });
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
                return .{ .variable = .{ .name = ident } };
            },
            else => return error.UnsupportedExpression,
        }
    }
};
