const std = @import("std");
const types = @import("../core/types.zig");
const parser = @import("parser.zig");
const TokenKind = parser.TokenKind;

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

pub const BinaryOp = struct {
    op: TokenKind,
    left: *Expression,
    right: *Expression,

    pub fn deinit(self: *BinaryOp, allocator: std.mem.Allocator) void {
        // Free the operator identifier if it was allocated
        // Use switch to correctly capture the payload
        switch (self.op) {
            .Ident => |ident| allocator.free(ident),
            else => {}, // Other TokenKind variants don't need freeing here
        }
        // Restore recursive deinit
        self.left.deinit(allocator);
        allocator.destroy(self.left);
        self.right.deinit(allocator);
        allocator.destroy(self.right);
    }

    pub fn clone(self: BinaryOp, allocator: std.mem.Allocator) error{OutOfMemory}!BinaryOp {
        const left = try allocator.create(Expression);
        errdefer allocator.destroy(left); // Destroy container if left.clone fails

        left.* = try self.left.clone(allocator);
        // If left.clone succeeded, add errdefer to clean it up if right side fails
        errdefer left.deinit(allocator);

        const right = try allocator.create(Expression);
        // If right allocation fails, the two errdefers above handle 'left' cleanup
        errdefer allocator.destroy(right); // Destroy container if right.clone fails

        right.* = try self.right.clone(allocator);
        // If right.clone succeeded, add errdefer to clean it up if return fails (unlikely but safe)
        errdefer right.deinit(allocator);

        // If we reach here, all clones succeeded. Ownership is transferred on return.
        // The errdefers are cancelled upon successful return.

        // Clone the operator TokenKind if it's an Ident
        const op_clone: TokenKind = switch (self.op) {
            .Ident => |ident| .{ .Ident = try allocator.dupe(u8, ident) },
            else => self.op, // Other kinds don't need cloning
        };
        // Add errdefer to free the cloned ident if return fails
        errdefer if (op_clone == .Ident) |ident| allocator.free(ident);

        return BinaryOp{
            .op = op_clone,
            .left = left,
            .right = right,
        };
    }
};

pub const FuncCall = struct {
    func: *Expression,
    args: std.ArrayList(*Expression),

    pub fn deinit(self: *FuncCall, allocator: std.mem.Allocator) void {
        // Restore recursive deinit
        self.func.deinit(allocator);
        allocator.destroy(self.func);
        for (self.args.items) |arg| {
            arg.deinit(allocator);
            allocator.destroy(arg);
        }
        self.args.deinit();
    }

    pub fn clone(self: FuncCall, allocator: std.mem.Allocator) error{OutOfMemory}!FuncCall {
        var args_list = std.ArrayList(*Expression).init(allocator);
        // Deinit list structure itself if anything below fails
        errdefer args_list.deinit();

        // Clone arguments, ensuring cleanup on failure
        for (self.args.items) |arg| {
            const new_arg = try allocator.create(Expression);
            // If new_arg allocation fails, loop terminates, top errdefer cleans partially filled args_list

            // Use a block to scope errdefers for this specific argument clone
            {
                errdefer allocator.destroy(new_arg); // If new_arg.clone fails, destroy container
                new_arg.* = try arg.clone(allocator);
                // If new_arg.clone fails, the errdefer above destroys container, top errdefer cleans list

                errdefer new_arg.deinit(allocator); // If append fails, deinit the cloned content
                try args_list.append(new_arg); // Ownership of new_arg transferred to list
                // If append fails, the two errdefers above clean new_arg, top errdefer cleans list
            }
            // If block completes successfully, new_arg is safely in the list
        }
        // If loop completes, args_list owns all cloned args. Add errdefer for args cleanup if func clone fails
        errdefer {
            for (args_list.items) |item| {
                item.deinit(allocator);
                allocator.destroy(item);
            }
        }

        const func = try allocator.create(Expression);
        // If func allocation fails, the args cleanup errdefer runs
        errdefer allocator.destroy(func); // If func.clone fails

        func.* = try self.func.clone(allocator);
        // If func.clone fails, the errdefer above destroys container, args cleanup errdefer runs
        errdefer func.deinit(allocator); // If return fails

        // Success, transfer ownership
        return FuncCall{
            .func = func,
            .args = args_list, // Transfer ownership of list and its contents
        };
    }
};

pub const If = struct {
    condition: *Expression,
    then_branch: *Expression,
    else_branch: *Expression, // Made mandatory

    pub fn deinit(self: *If, allocator: std.mem.Allocator) void {
        // Restore recursive deinit
        self.condition.deinit(allocator);
        allocator.destroy(self.condition);
        self.then_branch.deinit(allocator);
        allocator.destroy(self.then_branch);
        self.else_branch.deinit(allocator); // No longer optional
        allocator.destroy(self.else_branch);
    }

    pub fn clone(self: If, allocator: std.mem.Allocator) error{OutOfMemory}!If {
        const condition = try allocator.create(Expression);
        errdefer allocator.destroy(condition);
        condition.* = try self.condition.clone(allocator);

        const then_branch = try allocator.create(Expression);
        errdefer allocator.destroy(then_branch);
        then_branch.* = try self.then_branch.clone(allocator);

        const else_branch = try allocator.create(Expression); // No longer optional
        errdefer allocator.destroy(else_branch);
        else_branch.* = try self.else_branch.clone(allocator);

        return If{
            .condition = condition,
            .then_branch = then_branch,
            .else_branch = else_branch,
        };
    }
};

pub const FuncParam = struct {
    name: []const u8,
    param_type: ?[]const u8,

    pub fn deinit(self: *FuncParam, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
        if (self.param_type) |param_type| {
            allocator.free(param_type);
        }
    }

    pub fn clone(self: FuncParam, allocator: std.mem.Allocator) error{OutOfMemory}!FuncParam {
        return FuncParam{
            .name = try allocator.dupe(u8, self.name),
            .param_type = if (self.param_type) |pt| try allocator.dupe(u8, pt) else null,
        };
    }
};

pub const FuncDef = struct {
    name: []const u8,
    params: []FuncParam,
    body: *Expression,

    pub fn deinit(self: *FuncDef, allocator: std.mem.Allocator) void {
        // Only free memory directly owned by FuncDef
        allocator.free(self.name);
        for (self.params) |*param| {
            // Call deinit on params as they contain allocated strings
            param.deinit(allocator);
        }
        allocator.free(self.params); // Free the slice itself
        // Restore recursive deinit for body
        self.body.deinit(allocator);
        allocator.destroy(self.body);
    }

    pub fn clone(self: FuncDef, allocator: std.mem.Allocator) error{OutOfMemory}!FuncDef {
        var params = try allocator.alloc(FuncParam, self.params.len);
        errdefer allocator.free(params);

        for (self.params, 0..) |param, i| {
            params[i] = try param.clone(allocator);
        }

        const body = try allocator.create(Expression);
        errdefer allocator.destroy(body);
        body.* = try self.body.clone(allocator);

        return FuncDef{
            .name = try allocator.dupe(u8, self.name),
            .params = params,
            .body = body,
        };
    }
};

pub const VarDecl = struct {
    name: []const u8,
    value: *Expression,

    pub fn deinit(self: *VarDecl, allocator: std.mem.Allocator) void {
        // Only free memory directly owned by VarDecl
        allocator.free(self.name);
        // Restore recursive deinit for value
        self.value.deinit(allocator);
        allocator.destroy(self.value);
    }

    pub fn clone(self: VarDecl, allocator: std.mem.Allocator) error{OutOfMemory}!VarDecl {
        const value = try allocator.create(Expression);
        errdefer allocator.destroy(value);
        value.* = try self.value.clone(allocator);

        return VarDecl{
            .name = try allocator.dupe(u8, self.name),
            .value = value,
        };
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

    pub fn clone(self: ArrayLiteral, allocator: std.mem.Allocator) error{OutOfMemory}!ArrayLiteral {
        var new_elements = try allocator.alloc(*Expression, self.elements.len);
        errdefer allocator.free(new_elements);

        for (self.elements, 0..) |element_ptr, i| {
            const new_element_ptr = try allocator.create(Expression);
            errdefer allocator.destroy(new_element_ptr);
            new_element_ptr.* = try element_ptr.clone(allocator);
            new_elements[i] = new_element_ptr;
        }

        return ArrayLiteral{ .elements = new_elements };
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

    pub fn clone(self: MapEntry, allocator: std.mem.Allocator) error{OutOfMemory}!MapEntry {
        const new_key = try allocator.create(Expression);
        errdefer allocator.destroy(new_key);
        new_key.* = try self.key.clone(allocator);

        const new_value = try allocator.create(Expression);
        errdefer allocator.destroy(new_value);
        new_value.* = try self.value.clone(allocator);

        return MapEntry{ .key = new_key, .value = new_value };
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

    pub fn clone(self: MapLiteral, allocator: std.mem.Allocator) error{OutOfMemory}!MapLiteral {
        var new_entries = try allocator.alloc(MapEntry, self.entries.len);
        errdefer allocator.free(new_entries);

        for (self.entries, 0..) |entry, i| {
            new_entries[i] = try entry.clone(allocator);
        }

        return MapLiteral{ .entries = new_entries };
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

    pub fn clone(self: DoBlock, allocator: std.mem.Allocator) error{OutOfMemory}!DoBlock {
        var new_statements = try allocator.alloc(*Expression, self.statements.len);
        errdefer allocator.free(new_statements);

        for (self.statements, 0..) |stmt_ptr, i| {
            const new_stmt_ptr = try allocator.create(Expression);
            errdefer allocator.destroy(new_stmt_ptr);
            new_stmt_ptr.* = try stmt_ptr.clone(allocator);
            new_statements[i] = new_stmt_ptr;
        }

        return DoBlock{ .statements = new_statements };
    }
};

pub const Expression = union(enum) {
    Literal: Literal,
    Variable: Variable,
    BinaryOp: BinaryOp,
    If: If,
    FuncCall: FuncCall,
    FuncDef: FuncDef,
    VarDecl: VarDecl,
    SimpleFuncDef: SimpleFuncDef,
    ArrayLiteral: ArrayLiteral, // New
    MapLiteral: MapLiteral, // New
    DoBlock: DoBlock, // New

    pub fn deinit(self: *Expression, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .Literal => |*lit| lit.deinit(allocator),
            .Variable => |*var_expr| var_expr.deinit(allocator),
            .BinaryOp => |*bin_op| bin_op.deinit(allocator),
            .If => |*if_expr| if_expr.deinit(allocator),
            .FuncCall => |*func_call| func_call.deinit(allocator),
            .FuncDef => |*func_def| func_def.deinit(allocator),
            .VarDecl => |*var_decl| var_decl.deinit(allocator),
            .SimpleFuncDef => |*func_def| func_def.deinit(allocator),
            .ArrayLiteral => |*arr_lit| arr_lit.deinit(allocator), // New
            .MapLiteral => |*map_lit| map_lit.deinit(allocator), // New
            .DoBlock => |*do_block| do_block.deinit(allocator), // New
        }
    }

    pub fn clone(self: Expression, allocator: std.mem.Allocator) error{OutOfMemory}!Expression {
        return switch (self) {
            .Literal => |lit| Expression{ .Literal = try lit.clone(allocator) },
            .Variable => |var_expr| Expression{ .Variable = try var_expr.clone(allocator) },
            .BinaryOp => |bin_op| Expression{ .BinaryOp = try bin_op.clone(allocator) },
            .If => |if_expr| Expression{ .If = try if_expr.clone(allocator) },
            .FuncCall => |func_call| Expression{ .FuncCall = try func_call.clone(allocator) },
            .FuncDef => |func_def| Expression{ .FuncDef = try func_def.clone(allocator) },
            .VarDecl => |var_decl| Expression{ .VarDecl = try var_decl.clone(allocator) },
            .SimpleFuncDef => |func_def| Expression{ .SimpleFuncDef = try func_def.clone(allocator) },
            .ArrayLiteral => |arr_lit| Expression{ .ArrayLiteral = try arr_lit.clone(allocator) }, // New
            .MapLiteral => |map_lit| Expression{ .MapLiteral = try map_lit.clone(allocator) }, // New
            .DoBlock => |do_block| Expression{ .DoBlock = try do_block.clone(allocator) }, // New
        };
    }
};

pub const SimpleFuncDef = struct {
    name: []const u8,
    param_count: usize,
    body_literal: i64,

    pub fn deinit(self: *SimpleFuncDef, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
    }

    pub fn clone(self: SimpleFuncDef, allocator: std.mem.Allocator) error{OutOfMemory}!SimpleFuncDef {
        return SimpleFuncDef{
            .name = try allocator.dupe(u8, self.name),
            .param_count = self.param_count,
            .body_literal = self.body_literal,
        };
    }

    pub fn getName(self: SimpleFuncDef) []const u8 {
        return self.name;
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
