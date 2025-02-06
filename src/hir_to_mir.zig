const std = @import("std");
const hir = @import("hir.zig");
const mir = @import("mir.zig");
const types = @import("types.zig");

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
                try convertExpression(block, expr);
            }
        },
    }
}

fn convertExpression(block: *mir.MIR.Block, expr: hir.HIR.Expression) !void {
    switch (expr) {
        .literal => |lit| switch (lit) {
            .int => |val| try block.instructions.append(.{ .LoadInt = val }),
            .string => |val| try block.instructions.append(.{ .LoadString = try block.allocator.dupe(u8, val) }),
            .bool => |val| try block.instructions.append(.{ .LoadBool = val }),
            .float => |val| try block.instructions.append(.{ .LoadFloat = val }),
            .nil => try block.instructions.append(.LoadNil),
            .symbol => |val| try block.instructions.append(.{ .LoadSymbol = try block.allocator.dupe(u8, val) }),
            .array => |val| {
                var new_array = try block.allocator.alloc(types.Value, val.len);
                for (val, 0..) |item, i| {
                    new_array[i] = item;
                }
                try block.instructions.append(.{ .LoadArray = new_array });
            },
            .map => |val| {
                var new_map = std.StringHashMap(types.Value).init(block.allocator);
                var it = val.iterator();
                while (it.next()) |entry| {
                    try new_map.put(try block.allocator.dupe(u8, entry.key_ptr.*), entry.value_ptr.*);
                }
                try block.instructions.append(.{ .LoadMap = new_map });
            },
        },
        .binary_op => |bin_op| {
            // First convert and load the left operand
            try convertExpression(block, bin_op.left.*);

            // Then convert and load the right operand
            try convertExpression(block, bin_op.right.*);

            // Finally add the operation
            switch (bin_op.op) {
                .add => try block.instructions.append(.Add),
            }
        },
        .variable => |var_expr| {
            if (std.mem.eql(u8, var_expr.name, "print")) {
                // Skip loading the print variable - it will be handled by the next expression
            } else {
                const name_copy = try block.allocator.dupe(u8, var_expr.name);
                errdefer block.allocator.free(name_copy);
                try block.instructions.append(.{ .LoadVariable = name_copy });
            }
        },
    }
}
