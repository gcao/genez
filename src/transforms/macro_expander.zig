const std = @import("std");
const ast = @import("../frontend/ast.zig");

/// Macro environment for storing macro definitions
pub const MacroEnv = struct {
    allocator: std.mem.Allocator,
    macros: std.StringHashMap(ast.PseudoMacroDef),
    parent: ?*MacroEnv,

    pub fn init(allocator: std.mem.Allocator) MacroEnv {
        return .{
            .allocator = allocator,
            .macros = std.StringHashMap(ast.PseudoMacroDef).init(allocator),
            .parent = null,
        };
    }

    pub fn deinit(self: *MacroEnv) void {
        var iter = self.macros.iterator();
        while (iter.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            // Clone was used, so we need to clean up
            var macro_def = entry.value_ptr.*;
            macro_def.deinit(self.allocator);
        }
        self.macros.deinit();
    }

    pub fn define(self: *MacroEnv, name: []const u8, macro_def: ast.PseudoMacroDef) !void {
        const name_copy = try self.allocator.dupe(u8, name);
        const macro_clone = try macro_def.clone(self.allocator);
        try self.macros.put(name_copy, macro_clone);
    }

    pub fn lookup(self: *const MacroEnv, name: []const u8) ?*const ast.PseudoMacroDef {
        if (self.macros.getPtr(name)) |macro_def| {
            return macro_def;
        }
        if (self.parent) |parent| {
            return parent.lookup(name);
        }
        return null;
    }
};

/// Expand macros in AST nodes
pub fn expandMacros(allocator: std.mem.Allocator, nodes: []ast.AstNode) ![]ast.AstNode {
    var env = MacroEnv.init(allocator);
    defer env.deinit();

    var expanded_nodes = std.ArrayList(ast.AstNode).init(allocator);

    for (nodes) |node| {
        switch (node) {
            .Expression => |expr| {
                const expanded = try expandExpression(allocator, expr, &env);
                if (expanded) |exp_expr| {
                    try expanded_nodes.append(.{ .Expression = exp_expr });
                }
            },
        }
    }


    return expanded_nodes.toOwnedSlice();
}

/// Expand a single expression
fn expandExpression(allocator: std.mem.Allocator, expr: ast.Expression, env: *MacroEnv) !?ast.Expression {
    switch (expr) {
        .PseudoMacroDef => |macro_def| {
            // Store macro definition in environment
            try env.define(macro_def.name, macro_def);
            // Return null to indicate this node should be removed from output
            return null;
        },
        .PseudoMacroCall => |macro_call| {
            // Look up the macro
            switch (macro_call.macro.*) {
                .Variable => |var_ref| {
                    if (env.lookup(var_ref.name)) |macro_def| {
                        // Expand the macro
                        return try expandMacroCall(allocator, macro_def, macro_call.args, env);
                    } else {
                        // Not a macro, convert to regular function call
                        const func_call = try convertToFunctionCall(allocator, macro_call);
                        return func_call;
                    }
                },
                else => {
                    // Complex macro expression, convert to function call
                    const func_call = try convertToFunctionCall(allocator, macro_call);
                    return func_call;
                },
            }
        },
        .VarDecl => |var_decl| {
            // Expand the value expression
            const expanded_value = try expandExpression(allocator, var_decl.value.*, env);
            if (expanded_value) |exp_val| {
                const new_value = try allocator.create(ast.Expression);
                new_value.* = exp_val;
                return ast.Expression{ .VarDecl = .{
                    .name = try allocator.dupe(u8, var_decl.name),
                    .value = new_value,
                } };
            }
            return expr;
        },
        .FuncCall => |func_call| {
            // Check if this is a call to a macro
            switch (func_call.func.*) {
                .Variable => |var_ref| {
                    if (env.lookup(var_ref.name)) |macro_def| {
                        // This is a macro call!
                        // Convert arguments to MacroArg format
                        var macro_args = try allocator.alloc(ast.PseudoMacroCall.MacroArg, func_call.args.items.len);
                        defer allocator.free(macro_args);
                        
                        for (func_call.args.items, 0..) |arg, i| {
                            macro_args[i] = .{ .expr = arg };
                        }
                        // Expand the macro
                        return try expandMacroCall(allocator, macro_def, macro_args, env);
                    }
                },
                else => {},
            }
            
            // Not a macro call, expand arguments normally
            var new_args = std.ArrayList(*ast.Expression).init(allocator);
            for (func_call.args.items) |arg| {
                const expanded = try expandExpression(allocator, arg.*, env);
                if (expanded) |exp_arg| {
                    const arg_ptr = try allocator.create(ast.Expression);
                    arg_ptr.* = exp_arg;
                    try new_args.append(arg_ptr);
                } else {
                    const arg_ptr = try allocator.create(ast.Expression);
                    arg_ptr.* = arg.*;
                    try new_args.append(arg_ptr);
                }
            }
            
            // Clone function expression
            const func_ptr = try allocator.create(ast.Expression);
            func_ptr.* = func_call.func.*;
            
            return ast.Expression{ .FuncCall = .{
                .func = func_ptr,
                .args = new_args,
            } };
        },
        else => return expr,
    }
}

/// Expand a macro call by substituting arguments into the macro body
fn expandMacroCall(allocator: std.mem.Allocator, macro_def: *const ast.PseudoMacroDef, args: []ast.PseudoMacroCall.MacroArg, env: *MacroEnv) !ast.Expression {
    // Create a new environment for macro expansion
    var macro_env = MacroEnv.init(allocator);
    macro_env.parent = env;
    defer macro_env.deinit();

    // Bind macro parameters to arguments (as thunks)
    if (args.len != macro_def.params.len) {
        std.debug.print("Macro '{s}' arity mismatch: expected {} args, got {}\n", .{ macro_def.name, macro_def.params.len, args.len });
        std.debug.print("Macro params: ", .{});
        for (macro_def.params) |param| {
            std.debug.print("{s} ", .{param.name});
        }
        std.debug.print("\n", .{});
        return error.MacroArityMismatch;
    }

    // Store arguments as thunks (unevaluated expressions)
    for (macro_def.params, 0..) |param, i| {
        // Create a thunk that stores the unevaluated argument expression
        // When the macro encounters %param, it will substitute this expression
        const thunk_def = ast.PseudoMacroDef{
            .name = param.name,
            .params = &[_]ast.PseudoMacroDef.MacroParam{},
            .body = args[i].expr, // Store the argument expression without evaluating it
        };
        try macro_env.define(param.name, thunk_def);
    }

    // Expand the macro body with substitutions
    const expanded_body = try expandMacroBody(allocator, macro_def.body.*, &macro_env);
    
    // If the macro body has multiple statements, wrap in a do block
    // This ensures proper sequencing of side effects
    switch (macro_def.body.*) {
        .DoBlock => return expanded_body, // Already a do block
        else => {
            // Check if we need to wrap in a do block
            // For simple expressions, no wrapping needed
            return expanded_body;
        }
    }
}

/// Expand macro body, handling unquote expressions
fn expandMacroBody(allocator: std.mem.Allocator, expr: ast.Expression, env: *MacroEnv) !ast.Expression {
    switch (expr) {
        .Literal => |lit| {
            // Clone literals to ensure strings are properly allocated
            return ast.Expression{ .Literal = try lit.clone(allocator) };
        },
        .Variable => |var_ref| {
            // Check if this is an unquote expression (starts with %)
            if (var_ref.name.len > 0 and var_ref.name[0] == '%') {
                // This is an unquote - evaluate the parameter
                const param_name = var_ref.name[1..]; // Skip the %
                if (env.lookup(param_name)) |thunk| {
                    if (thunk.params.len == 0) {
                        // Return a clone of the argument expression (this will be evaluated)
                        return try thunk.body.*.clone(allocator);
                    }
                }
                // If not found, it's an error - undefined unquote
                return error.UndefinedUnquote;
            } else if (env.lookup(var_ref.name)) |thunk| {
                // This is a macro parameter WITHOUT unquote - return it as a literal
                // We need to convert the expression to a string representation
                // For now, if it's a variable, return a string literal with the variable name
                if (thunk.body.* == .Variable) {
                    const var_name = thunk.body.*.Variable.name;
                    return ast.Expression{ .Literal = .{ .value = .{ .String = try allocator.dupe(u8, var_name) } } };
                }
                // For other expressions, we'd need to serialize them to strings
                // For now, just return the expression itself
                return try thunk.body.*.clone(allocator);
            }
            // Regular variable reference - clone it
            return ast.Expression{ .Variable = try var_ref.clone(allocator) };
        },
        .DoBlock => |do_block| {
            // Expand each statement in the block
            var new_stmts = try allocator.alloc(*ast.Expression, do_block.statements.len);
            for (do_block.statements, 0..) |stmt, i| {
                const expanded = try expandMacroBody(allocator, stmt.*, env);
                const stmt_ptr = try allocator.create(ast.Expression);
                stmt_ptr.* = expanded;
                new_stmts[i] = stmt_ptr;
            }
            return ast.Expression{ .DoBlock = .{ .statements = new_stmts } };
        },
        .FuncCall => |func_call| {
            // Expand function and arguments
            const expanded_func = try expandMacroBody(allocator, func_call.func.*, env);
            const func_ptr = try allocator.create(ast.Expression);
            func_ptr.* = expanded_func;

            var new_args = std.ArrayList(*ast.Expression).init(allocator);
            for (func_call.args.items) |arg| {
                const expanded_arg = try expandMacroBody(allocator, arg.*, env);
                const arg_ptr = try allocator.create(ast.Expression);
                arg_ptr.* = expanded_arg;
                try new_args.append(arg_ptr);
            }

            return ast.Expression{ .FuncCall = .{
                .func = func_ptr,
                .args = new_args,
            } };
        },
        .If => |if_expr| {
            const cond = try allocator.create(ast.Expression);
            cond.* = try expandMacroBody(allocator, if_expr.condition.*, env);
            
            const then_branch = try allocator.create(ast.Expression);
            then_branch.* = try expandMacroBody(allocator, if_expr.then_branch.*, env);
            
            const else_branch = try allocator.create(ast.Expression);
            else_branch.* = try expandMacroBody(allocator, if_expr.else_branch.*, env);
            
            return ast.Expression{ .If = .{
                .condition = cond,
                .then_branch = then_branch,
                .else_branch = else_branch,
            } };
        },
        .BinaryOp => |bin_op| {
            const left = try allocator.create(ast.Expression);
            left.* = try expandMacroBody(allocator, bin_op.left.*, env);
            
            const right = try allocator.create(ast.Expression);
            right.* = try expandMacroBody(allocator, bin_op.right.*, env);
            
            return ast.Expression{ .BinaryOp = try bin_op.clone(allocator) };
        },
        else => {
            // For any other expression types, clone them
            return try expr.clone(allocator);
        },
    }
}

/// Convert a macro call to a regular function call
fn convertToFunctionCall(allocator: std.mem.Allocator, macro_call: ast.PseudoMacroCall) !ast.Expression {
    var args = std.ArrayList(*ast.Expression).init(allocator);
    for (macro_call.args) |arg| {
        const arg_ptr = try allocator.create(ast.Expression);
        arg_ptr.* = arg.expr.*;
        try args.append(arg_ptr);
    }
    
    const func_ptr = try allocator.create(ast.Expression);
    func_ptr.* = macro_call.macro.*;
    
    return ast.Expression{ .FuncCall = .{
        .func = func_ptr,
        .args = args,
    } };
}