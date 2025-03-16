const std = @import("std");
const hir = @import("hir.zig");
const mir = @import("mir.zig");
const ast = @import("ast.zig");
const AstNode = ast.AstNode;

pub fn convert(allocator: std.mem.Allocator, hir_prog: hir.HIR) !mir.MIR {
    var mir_prog = mir.MIR.init(allocator);
    errdefer mir_prog.deinit();

    // Convert each HIR function to MIR function
    for (hir_prog.functions.items) |func| {
        const mir_func = try convertFunction(allocator, func);
        try mir_prog.functions.append(mir_func);
    }

    return mir_prog;
}

fn convertFunction(allocator: std.mem.Allocator, func: hir.HIR.Function) !mir.MIR.Function {
    var mir_func = mir.MIR.Function.init(allocator);
    mir_func.name = try allocator.dupe(u8, func.name);

    // Create entry block
    var entry_block = mir.MIR.Block.init(allocator);

    // Convert HIR statements to MIR instructions
    for (func.body.items) |stmt| {
        try convertStatement(&entry_block, stmt);
    }

    mir_func.blocks.append(entry_block) catch unreachable;
    return mir_func;
}

fn convertStatement(block: *mir.MIR.Block, stmt: hir.HIR.Statement) !void {
    switch (stmt) {
        .Expression => |expr| {
            if (expr == .variable and std.mem.eql(u8, expr.variable.name, "print")) {
                // This is a print statement - first convert the argument to print
                // The argument should be the next expression in the HIR
                try block.instructions.append(.Print);
            } else {
                const result = try convertExpression(block, expr);
                try block.instructions.append(.{ .Store = result });
            }
        },
    }
}

const ConvertError = error{
    UnsupportedExpression,
    OutOfMemory,
};

fn convertExpression(block: *mir.MIR.Block, expr: hir.HIR.Expression) ConvertError!mir.MIR.Expression {
    switch (expr) {
        .literal => |lit| switch (lit) {
            .int => |val| return .{ .LoadInt = val },
            .string => |val| return .{ .LoadString = try block.allocator.dupe(u8, val) },
            .bool => |val| return .{ .LoadBool = val },
            .float => |val| return .{ .LoadFloat = val },
            .nil => return .LoadNil,
            .symbol => |val| return .{ .LoadSymbol = try block.allocator.dupe(u8, val) },
            .array => |val| {
                var new_array = try block.allocator.alloc(AstNode.Value, val.len);
                for (val, 0..) |item, i| {
                    new_array[i] = item;
                }
                return .{ .LoadArray = new_array };
            },
            .map => |val| {
                var new_map = std.StringHashMap(AstNode.Value).init(block.allocator);
                var it = val.iterator();
                while (it.next()) |entry| {
                    try new_map.put(try block.allocator.dupe(u8, entry.key_ptr.*), entry.value_ptr.*);
                }
                return .{ .LoadMap = new_map };
            },
        },
        .binary_op => |bin_op| {
            // First convert and load the left operand
            const left = try convertExpression(block, bin_op.left.*);

            // Then convert and load the right operand
            const right = try convertExpression(block, bin_op.right.*);

            // Finally add the operation
            const left_ptr = try block.allocator.create(mir.MIR.Expression);
            errdefer block.allocator.destroy(left_ptr);
            left_ptr.* = left;

            const right_ptr = try block.allocator.create(mir.MIR.Expression);
            errdefer block.allocator.destroy(right_ptr);
            right_ptr.* = right;

            return .{ .BinaryOp = .{
                .left = left_ptr,
                .right = right_ptr,
                .op = switch (bin_op.op) {
                    .add => .add,
                    .sub => .sub,
                    .mul => .mul,
                    .div => .div,
                    .lt => .lt,
                    .gt => .gt,
                    .eq => .eq,
                },
            } };
        },
        .variable => |var_expr| {
            if (std.mem.eql(u8, var_expr.name, "print")) {
                // Skip loading the print variable - it will be handled by the next expression
                return error.UnsupportedExpression;
            } else {
                const name_copy = try block.allocator.dupe(u8, var_expr.name);
                errdefer block.allocator.free(name_copy);
                return .{ .LoadVariable = name_copy };
            }
        },
        .if_expr => |if_expr| {
            // Convert condition
            const condition_expr = try convertExpression(block, if_expr.condition.*);
            const condition = try block.allocator.create(mir.MIR.Expression);
            errdefer block.allocator.destroy(condition);
            condition.* = condition_expr;

            // Add JumpIfFalse instruction - we'll patch the offset later
            const jump_if_false_pos = block.instructions.items.len;
            try block.instructions.append(.{ .JumpIfFalse = .{ .offset = 0 } });

            // Convert then branch
            const then_expr = try convertExpression(block, if_expr.then_branch.*);
            const then_branch = try block.allocator.create(mir.MIR.Expression);
            errdefer block.allocator.destroy(then_branch);
            then_branch.* = then_expr;

            // Add Jump instruction to skip else branch - we'll patch the offset later
            const jump_pos = block.instructions.items.len;
            try block.instructions.append(.{ .Jump = .{ .offset = 0 } });

            // Convert else branch if it exists
            var else_branch: ?*mir.MIR.Expression = null;
            if (if_expr.else_branch) |else_expr| {
                const else_expr_val = try convertExpression(block, else_expr.*);
                else_branch = try block.allocator.create(mir.MIR.Expression);
                errdefer block.allocator.destroy(else_branch.?);
                else_branch.?.* = else_expr_val;
            }

            // Patch the jump offsets
            block.instructions.items[jump_if_false_pos].JumpIfFalse.offset = block.instructions.items.len - jump_if_false_pos;
            block.instructions.items[jump_pos].Jump.offset = block.instructions.items.len - jump_pos;

            return .{ .If = .{ .condition = condition, .then_branch = then_branch, .else_branch = else_branch } };
        },
        .func_def => |func_def| {
            // Create a new function
            var new_func = mir.MIR.Function.init(block.allocator);
            new_func.name = try block.allocator.dupe(u8, func_def.name);

            // Create entry block for the function
            var entry_block = mir.MIR.Block.init(block.allocator);

            // Convert function body
            const body_expr = try convertExpression(&entry_block, func_def.body.*);
            try entry_block.instructions.append(.{ .Store = body_expr });

            // Add return instruction
            try entry_block.instructions.append(.{ .Return = {} });

            try new_func.blocks.append(entry_block);

            // Create a pointer to the function
            const func_ptr = try block.allocator.create(mir.MIR.Function);
            func_ptr.* = new_func;

            // Add DefineFunction instruction
            return .{ .DefineFunction = func_ptr };
        },
        .func_call => |func_call| {
            // First evaluate the function
            const func = try convertExpression(block, func_call.func.*);

            // Then evaluate all arguments
            const args = try convertExpressionList(block.allocator, func_call.args.items);

            // Extract function name from the function expression
            const func_name = switch (func) {
                .LoadVariable => |name| try block.allocator.dupe(u8, name),
                else => return error.UnsupportedExpression,
            };
            errdefer block.allocator.free(func_name);

            // Add Call instruction
            return .{ .Call = .{ .func = func_name, .arg_count = args.len } };
        },
        else => return error.UnsupportedExpression,
    }
}

fn convertExpressionList(allocator: std.mem.Allocator, exprs: []const *hir.HIR.Expression) ![]mir.MIR.Expression {
    var result = try allocator.alloc(mir.MIR.Expression, exprs.len);
    errdefer {
        for (result) |*expr| {
            expr.deinit(allocator);
        }
        allocator.free(result);
    }

    var temp_block = mir.MIR.Block.init(allocator);
    defer temp_block.deinit();

    for (exprs, 0..) |expr, i| {
        result[i] = try convertExpression(&temp_block, expr.*);
    }

    return result;
}
