const std = @import("std");
const hir = @import("../ir/hir.zig");
const types = @import("types.zig");

pub const TypeError = error{
    TypeMismatch,
    UndefinedVariable,
    UndefinedFunction,
    ArgumentCountMismatch,
    InvalidOperandType,
    NotCallable,
    DivisionByZero,
};

pub const TypeInfo = struct {
    type_enum: hir.Type,
    is_nullable: bool = false,
};

pub const TypeEnvironment = struct {
    allocator: std.mem.Allocator,
    // Variable name -> type mapping
    variables: std.StringHashMap(TypeInfo),
    // Function name -> (param types, return type) mapping
    functions: std.StringHashMap(FunctionType),
    // Parent environment for nested scopes
    parent: ?*TypeEnvironment = null,

    pub const FunctionType = struct {
        params: []TypeInfo,
        return_type: TypeInfo,
    };

    pub fn init(allocator: std.mem.Allocator) TypeEnvironment {
        return .{
            .allocator = allocator,
            .variables = std.StringHashMap(TypeInfo).init(allocator),
            .functions = std.StringHashMap(FunctionType).init(allocator),
        };
    }

    pub fn deinit(self: *TypeEnvironment) void {
        self.variables.deinit();
        self.functions.deinit();
    }

    pub fn lookupVariable(self: *const TypeEnvironment, name: []const u8) ?TypeInfo {
        if (self.variables.get(name)) |type_info| {
            return type_info;
        }
        if (self.parent) |parent| {
            return parent.lookupVariable(name);
        }
        return null;
    }

    pub fn lookupFunction(self: *const TypeEnvironment, name: []const u8) ?FunctionType {
        if (self.functions.get(name)) |func_type| {
            return func_type;
        }
        if (self.parent) |parent| {
            return parent.lookupFunction(name);
        }
        return null;
    }
};

pub const HIRTypeChecker = struct {
    allocator: std.mem.Allocator,
    global_env: TypeEnvironment,
    errors: std.ArrayList(TypeCheckError),
    // Store allocated arrays for cleanup
    allocated_params: std.ArrayList([]TypeInfo),

    pub const TypeCheckError = struct {
        message: []const u8,
        location: ?SourceLocation = null,
    };

    pub const SourceLocation = struct {
        line: u32,
        column: u32,
    };

    pub fn init(allocator: std.mem.Allocator) HIRTypeChecker {
        var checker = HIRTypeChecker{
            .allocator = allocator,
            .global_env = TypeEnvironment.init(allocator),
            .errors = std.ArrayList(TypeCheckError).init(allocator),
            .allocated_params = std.ArrayList([]TypeInfo).init(allocator),
        };
        
        // Initialize built-in functions
        checker.initBuiltins() catch {};
        
        return checker;
    }

    pub fn deinit(self: *HIRTypeChecker) void {
        self.global_env.deinit();
        for (self.errors.items) |error_item| {
            self.allocator.free(error_item.message);
        }
        self.errors.deinit();
        
        // Free allocated parameter arrays
        for (self.allocated_params.items) |params| {
            self.allocator.free(params);
        }
        self.allocated_params.deinit();
    }

    fn initBuiltins(self: *HIRTypeChecker) !void {
        // Built-in arithmetic functions
        const int_type = TypeInfo{ .type_enum = .int };
        const bool_type = TypeInfo{ .type_enum = .bool };
        const void_type = TypeInfo{ .type_enum = .void };

        // Arithmetic operators (work with both int and float)
        const arithmetic_ops = [_][]const u8{ "+", "-", "*", "/" };
        
        // We need to allocate the params array
        var int_params = try self.allocator.alloc(TypeInfo, 2);
        int_params[0] = int_type;
        int_params[1] = int_type;
        try self.allocated_params.append(int_params);
        
        for (arithmetic_ops) |op| {
            try self.global_env.functions.put(op, .{
                .params = int_params,
                .return_type = int_type,
            });
        }

        // Comparison operators
        const comparison_ops = [_][]const u8{ "<", ">", "=", "<=", ">=" };
        for (comparison_ops) |op| {
            try self.global_env.functions.put(op, .{
                .params = int_params, // Reuse the same params array
                .return_type = bool_type,
            });
        }

        // print function
        var print_params = try self.allocator.alloc(TypeInfo, 1);
        print_params[0] = int_type;
        try self.allocated_params.append(print_params);
        
        try self.global_env.functions.put("print", .{
            .params = print_params,
            .return_type = void_type,
        });
    }

    pub fn checkProgram(self: *HIRTypeChecker, program: hir.HIR) !void {
        // Check all functions
        for (program.functions.items) |*function| {
            try self.checkFunction(function);
        }
    }

    fn checkFunction(self: *HIRTypeChecker, function: *hir.HIR.Function) !void {
        // Create a new environment for this function
        var func_env = TypeEnvironment.init(self.allocator);
        defer func_env.deinit();
        func_env.parent = &self.global_env;

        // Add function parameters to the environment
        for (function.params) |param| {
            const param_type = if (param.param_type) |type_str| blk: {
                // Parse type annotation
                if (std.mem.eql(u8, type_str, "Int")) {
                    break :blk TypeInfo{ .type_enum = .int };
                } else if (std.mem.eql(u8, type_str, "Float")) {
                    break :blk TypeInfo{ .type_enum = .float };
                } else if (std.mem.eql(u8, type_str, "Bool")) {
                    break :blk TypeInfo{ .type_enum = .bool };
                } else if (std.mem.eql(u8, type_str, "String")) {
                    break :blk TypeInfo{ .type_enum = .string };
                } else {
                    // Unknown type, default to int for now
                    break :blk TypeInfo{ .type_enum = .int };
                }
            } else TypeInfo{ .type_enum = .int }; // Default to int if no type annotation

            try func_env.variables.put(param.name, param_type);
        }

        // Check function body
        for (function.body.items) |*stmt| {
            _ = try self.checkStatement(stmt, &func_env);
        }
    }

    fn checkStatement(self: *HIRTypeChecker, stmt: *const hir.HIR.Statement, env: *TypeEnvironment) !?TypeInfo {
        switch (stmt.*) {
            .Expression => |expr| {
                return try self.checkExpression(&expr, env);
            },
        }
    }

    fn checkExpression(self: *HIRTypeChecker, expr: *const hir.HIR.Expression, env: *TypeEnvironment) !TypeInfo {
        switch (expr.*) {
            .literal => |lit| {
                return switch (lit) {
                    .int => TypeInfo{ .type_enum = .int },
                    .float => TypeInfo{ .type_enum = .float },
                    .string => TypeInfo{ .type_enum = .string },
                    .bool => TypeInfo{ .type_enum = .bool },
                    .nil => TypeInfo{ .type_enum = .void }, // Nil maps to void for now
                    .symbol => TypeInfo{ .type_enum = .string }, // Symbols are like strings
                    .array => TypeInfo{ .type_enum = .void }, // TODO: Handle array types
                    .map => TypeInfo{ .type_enum = .void }, // TODO: Handle map types
                };
            },
            .variable => |var_expr| {
                if (env.lookupVariable(var_expr.name)) |type_info| {
                    return type_info;
                } else {
                    try self.addError("Undefined variable: {s}", .{var_expr.name});
                    return TypeError.UndefinedVariable;
                }
            },
            .binary_op => |bin_op| {
                const left_type = try self.checkExpression(bin_op.left, env);
                const right_type = try self.checkExpression(bin_op.right, env);

                // Type checking for binary operations
                switch (bin_op.op) {
                    .add, .sub, .mul, .div => {
                        // Arithmetic operations require numeric types
                        if (left_type.type_enum != .int and left_type.type_enum != .float) {
                            try self.addError("Left operand of arithmetic operation must be numeric", .{});
                            return TypeError.InvalidOperandType;
                        }
                        if (right_type.type_enum != .int and right_type.type_enum != .float) {
                            try self.addError("Right operand of arithmetic operation must be numeric", .{});
                            return TypeError.InvalidOperandType;
                        }
                        
                        // Result type is float if either operand is float
                        if (left_type.type_enum == .float or right_type.type_enum == .float) {
                            return TypeInfo{ .type_enum = .float };
                        }
                        return TypeInfo{ .type_enum = .int };
                    },
                    .lt, .gt, .eq => {
                        // Comparison operations return bool
                        if (left_type.type_enum != right_type.type_enum) {
                            try self.addError("Comparison operands must have the same type", .{});
                            return TypeError.TypeMismatch;
                        }
                        return TypeInfo{ .type_enum = .bool };
                    },
                }
            },
            .if_expr => |if_expr| {
                const cond_type = try self.checkExpression(if_expr.condition, env);
                if (cond_type.type_enum != .bool) {
                    try self.addError("If condition must be boolean", .{});
                    return TypeError.TypeMismatch;
                }

                const then_type = try self.checkExpression(if_expr.then_branch, env);
                
                if (if_expr.else_branch) |else_branch| {
                    const else_type = try self.checkExpression(else_branch, env);
                    // Both branches should have compatible types
                    if (then_type.type_enum != else_type.type_enum) {
                        try self.addError("If branches must have the same type", .{});
                        return TypeError.TypeMismatch;
                    }
                }
                
                return then_type;
            },
            .func_call => |call| {
                // Handle built-in function calls
                if (call.func.* == .variable) {
                    const func_name = call.func.variable.name;
                    
                    if (env.lookupFunction(func_name)) |func_type| {
                        // Check argument count
                        if (call.args.items.len != func_type.params.len) {
                            try self.addError("Function {s} expects {} arguments, got {}", .{
                                func_name,
                                func_type.params.len,
                                call.args.items.len,
                            });
                            return TypeError.ArgumentCountMismatch;
                        }
                        
                        // Check argument types
                        for (call.args.items, func_type.params) |arg, expected_type| {
                            const arg_type = try self.checkExpression(arg, env);
                            if (arg_type.type_enum != expected_type.type_enum) {
                                try self.addError("Type mismatch in function argument", .{});
                                return TypeError.TypeMismatch;
                            }
                        }
                        
                        return func_type.return_type;
                    } else {
                        try self.addError("Undefined function: {s}", .{func_name});
                        return TypeError.UndefinedFunction;
                    }
                }
                
                // For other call types, return void for now
                return TypeInfo{ .type_enum = .void };
            },
            .var_decl => |var_decl| {
                const value_type = try self.checkExpression(var_decl.value, env);
                try env.variables.put(var_decl.name, value_type);
                return TypeInfo{ .type_enum = .void };
            },
            else => {
                // For unhandled expression types, return void
                return TypeInfo{ .type_enum = .void };
            },
        }
    }

    fn addError(self: *HIRTypeChecker, comptime fmt: []const u8, args: anytype) !void {
        const message = try std.fmt.allocPrint(self.allocator, fmt, args);
        try self.errors.append(.{ .message = message });
    }

    pub fn hasErrors(self: *const HIRTypeChecker) bool {
        return self.errors.items.len > 0;
    }

    pub fn printErrors(self: *const HIRTypeChecker) void {
        for (self.errors.items) |err| {
            std.debug.print("Type error: {s}\n", .{err.message});
        }
    }
};