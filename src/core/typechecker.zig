const std = @import("std");
const types = @import("types.zig");
const ast = @import("ast.zig");
const Type = types.Type;
const Value = types.Value;
const AstNode = ast.AstNode;

pub const TypeChecker = struct {
    allocator: std.mem.Allocator,
    type_env: std.StringHashMap(*Type),
    value_env: std.StringHashMap(Value),

    pub fn init(allocator: std.mem.Allocator) TypeChecker {
        return .{
            .allocator = allocator,
            .type_env = std.StringHashMap(*Type).init(allocator),
            .value_env = std.StringHashMap(Value).init(allocator),
        };
    }

    pub fn deinit(self: *TypeChecker) void {
        self.type_env.deinit();
        self.value_env.deinit();
    }

    pub fn checkProgram(self: *TypeChecker, program: *const AstNode) !void {
        _ = self;
        _ = program;
        // TODO: Implement program type checking
    }

    pub fn checkExpression(self: *TypeChecker, expr: *const AstNode) !*Type {
        return switch (expr.*) {
            .Literal => |lit| self.checkLiteral(lit),
            .Variable => |var_expr| self.checkVariable(var_expr),
            .BinaryOp => |bin_op| self.checkBinaryOp(bin_op),
            .UnaryOp => |un_op| self.checkUnaryOp(un_op),
            .Call => |call| self.checkCall(call),
            .PropertyAccess => |prop| self.checkPropertyAccess(prop),
            .ArrayAccess => |arr| self.checkArrayAccess(arr),
            .New => |new| self.checkNew(new),
            .Cast => |cast| self.checkCast(cast),
        };
    }

    fn checkLiteral(self: *TypeChecker, lit: *ast.Literal) !*Type {
        return switch (lit.value) {
            .Nil => &Type.Nil,
            .Bool => &Type.Bool,
            .Int => &Type.Int,
            .Float => &Type.Float,
            .String => &Type.String,
            .Symbol => &Type.Symbol,
            .Array => |arr| blk: {
                if (arr.len == 0) return &Type{ .Array = &Type.ArrayType{
                    .element_type = &Type.Any,
                } };

                const first_type = try self.inferType(arr[0]);
                for (arr[1..]) |item| {
                    const item_type = try self.inferType(item);
                    _ = try self.unifyTypes(first_type, item_type);
                }
                break :blk &Type{ .Array = &Type.ArrayType{
                    .element_type = first_type,
                } };
            },
            .Map => |map| blk: {
                if (map.count() == 0) return &Type{ .Map = &Type.MapType{
                    .key_type = &Type.Any,
                    .value_type = &Type.Any,
                } };

                var key_type: ?*Type = null;
                var value_type: ?*Type = null;
                var iter = map.iterator();

                while (iter.next()) |entry| {
                    const current_key_type = try self.inferType(Value{ .String = entry.key_ptr.* });
                    const current_value_type = try self.inferType(entry.value_ptr.*);

                    key_type = if (key_type) |kt|
                        try self.unifyTypes(kt, current_key_type)
                    else
                        current_key_type;

                    value_type = if (value_type) |vt|
                        try self.unifyTypes(vt, current_value_type)
                    else
                        current_value_type;
                }

                break :blk &Type{ .Map = &Type.MapType{
                    .key_type = key_type orelse &Type.Any,
                    .value_type = value_type orelse &Type.Any,
                } };
            },
            else => &Type.Any,
        };
    }

    pub fn checkStatement(self: *TypeChecker, stmt: *const AstNode) !void {
        _ = self;
        _ = stmt;
        // TODO: Implement statement type checking
    }

    pub fn checkFunction(self: *TypeChecker, func: *const AstNode) !*Type.FunctionType {
        _ = self;
        _ = func;
        // TODO: Implement function type checking
        return &Type.FunctionType{
            .params = &.{},
            .return_type = &Type.Any,
        };
    }

    pub fn checkClass(self: *TypeChecker, class: *const AstNode) !*Type.ClassType {
        _ = class;
        // TODO: Implement class type checking
        return &Type.ClassType{
            .name = "TODO",
            .fields = std.StringHashMap(*Type).init(self.allocator),
            .methods = std.StringHashMap(*Type.FunctionType).init(self.allocator),
            .parent = null,
        };
    }

    pub fn checkNamespace(self: *TypeChecker, ns: *const AstNode) !*Type.NamespaceType {
        _ = ns;
        // TODO: Implement namespace type checking
        return &Type.NamespaceType{
            .name = "TODO",
            .types = std.StringHashMap(*Type).init(self.allocator),
            .functions = std.StringHashMap(*Type.FunctionType).init(self.allocator),
            .sub_namespaces = std.StringHashMap(*Type.NamespaceType).init(self.allocator),
        };
    }

    pub fn unifyTypes(self: *TypeChecker, t1: *Type, t2: *Type) !*Type {
        _ = self;
        if (t1 == t2) return t1;
        return &Type.Any;
    }

    pub fn inferType(self: *TypeChecker, value: Value) !*Type {
        return switch (value) {
            .Nil => &Type.Nil,
            .Bool => &Type.Bool,
            .Int => &Type.Int,
            .Float => &Type.Float,
            .String => &Type.String,
            .Symbol => &Type.Symbol,
            .Array => |arr| blk: {
                if (arr.len == 0) return &Type{ .Array = &Type.ArrayType{
                    .element_type = &Type.Any,
                } };

                const first_type = try self.inferType(arr[0]);
                for (arr[1..]) |item| {
                    const item_type = try self.inferType(item);
                    _ = try self.unifyTypes(first_type, item_type);
                }
                break :blk &Type{ .Array = &Type.ArrayType{
                    .element_type = first_type,
                } };
            },
            .Map => |map| blk: {
                if (map.count() == 0) return &Type{ .Map = &Type.MapType{
                    .key_type = &Type.Any,
                    .value_type = &Type.Any,
                } };

                var key_type: ?*Type = null;
                var value_type: ?*Type = null;
                var iter = map.iterator();

                while (iter.next()) |entry| {
                    const current_key_type = try self.inferType(Value{ .String = entry.key_ptr.* });
                    const current_value_type = try self.inferType(entry.value_ptr.*);

                    key_type = if (key_type) |kt|
                        try self.unifyTypes(kt, current_key_type)
                    else
                        current_key_type;

                    value_type = if (value_type) |vt|
                        try self.unifyTypes(vt, current_value_type)
                    else
                        current_value_type;
                }

                break :blk &Type{ .Map = &Type.MapType{
                    .key_type = key_type orelse &Type.Any,
                    .value_type = value_type orelse &Type.Any,
                } };
            },
            else => &Type.Any,
        };
    }

    fn checkVariable(self: *TypeChecker, var_expr: *ast.Variable) !*Type {
        if (self.value_env.get(var_expr.name)) |value| {
            return try self.inferType(value);
        }
        return error.UndefinedVariable;
    }

    fn checkCall(self: *TypeChecker, call: *ast.Call) !*Type {
        const callee_type = try self.checkExpression(call.callee);

        switch (callee_type.*) {
            .Function => |func_type| {
                if (call.args.len != func_type.params.len) {
                    return error.ArgumentCountMismatch;
                }

                for (call.args, func_type.params) |arg, param| {
                    const arg_type = try self.checkExpression(arg);
                    _ = try self.unifyTypes(arg_type, param.type);
                }

                return func_type.return_type;
            },
            else => return error.NotCallable,
        }
    }

    fn checkBinaryOp(self: *TypeChecker, bin_op: *ast.BinaryOp) !*Type {
        const left_type = try self.checkExpression(bin_op.left);
        const right_type = try self.checkExpression(bin_op.right);

        return switch (bin_op.op) {
            .Add, .Subtract, .Multiply, .Divide => blk: {
                _ = try self.unifyTypes(left_type, right_type);
                break :blk left_type;
            },
            .Equal, .NotEqual, .LessThan, .LessEqual, .GreaterThan, .GreaterEqual => &Type.Bool,
            .And, .Or => blk: {
                _ = try self.unifyTypes(left_type, &Type.Bool);
                _ = try self.unifyTypes(right_type, &Type.Bool);
                break :blk &Type.Bool;
            },
        };
    }

    fn checkUnaryOp(self: *TypeChecker, un_op: *ast.UnaryOp) !*Type {
        const operand_type = try self.checkExpression(un_op.operand);

        return switch (un_op.op) {
            .Negate => blk: {
                _ = try self.unifyTypes(operand_type, &Type.Int);
                break :blk &Type.Int;
            },
            .Not => blk: {
                _ = try self.unifyTypes(operand_type, &Type.Bool);
                break :blk &Type.Bool;
            },
        };
    }

    fn checkPropertyAccess(self: *TypeChecker, prop: *ast.PropertyAccess) !*Type {
        const object_type = try self.checkExpression(prop.object);

        switch (object_type.*) {
            .Class => |class_type| {
                if (class_type.fields.get(prop.property)) |field_type| {
                    return field_type;
                }
                return error.UndefinedProperty;
            },
            else => return error.NotAnObject,
        }
    }

    fn checkArrayAccess(self: *TypeChecker, arr: *ast.ArrayAccess) !*Type {
        const array_type = try self.checkExpression(arr.array);
        const index_type = try self.checkExpression(arr.index);

        _ = try self.unifyTypes(index_type, &Type.Int);

        switch (array_type.*) {
            .Array => |arr_type| return arr_type.element_type,
            else => return error.NotAnArray,
        }
    }

    fn checkNew(self: *TypeChecker, new: *ast.New) !*Type {
        if (self.type_env.get(new.class_name)) |type_def| {
            switch (type_def.*) {
                .Class => {
                    // TODO: Check constructor arguments
                    return type_def;
                },
                else => return error.NotAClass,
            }
        }
        return error.UndefinedClass;
    }

    fn checkCast(self: *TypeChecker, cast: *ast.Cast) !*Type {
        _ = try self.checkExpression(cast.expression);
        return cast.target_type;
    }
};
