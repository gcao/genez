const std = @import("std");
const hir = @import("hir.zig");
const mir = @import("mir.zig");
const types = @import("types.zig");

pub fn convert(allocator: std.mem.Allocator, hir_prog: hir.HIR) !mir.MIR {
    std.debug.print("Converting HIR to MIR...\n", .{});
    var mir_prog = mir.MIR.init(allocator);
    errdefer mir_prog.deinit();

    // Convert each HIR function to MIR function
    for (hir_prog.functions.items) |func| {
        std.debug.print("Converting function: {s}\n", .{func.name});
        const mir_func = try convertFunction(allocator, func);
        try mir_prog.functions.append(mir_func);
    }

    return mir_prog;
}

fn convertFunction(allocator: std.mem.Allocator, func: hir.HIR.Function) !mir.MIR.Function {
    std.debug.print("Converting function body with {d} statements\n", .{func.body.items.len});
    var mir_func = mir.MIR.Function.init(allocator);
    mir_func.name = try allocator.dupe(u8, func.name);

    // Create entry block
    var entry_block = mir.MIR.Block.init(allocator);

    // Convert HIR statements to MIR instructions
    for (func.body.items) |stmt| {
        std.debug.print("Converting statement: {any}\n", .{stmt});
        convertStatement(&entry_block, stmt) catch |err| {
            std.debug.print("Error converting statement: {any}\n", .{err});
            continue;
        };
    }

    mir_func.blocks.append(entry_block) catch unreachable;
    return mir_func;
}

fn convertStatement(block: *mir.MIR.Block, stmt: hir.HIR.Statement) !void {
    std.debug.print("Converting statement type: {s}\n", .{@tagName(stmt)});
    switch (stmt) {
        .Expression => |expr| {
            std.debug.print("Converting expression type: {s}\n", .{@tagName(expr)});
            try convertExpression(block, expr);
        },
    }
}

fn convertExpression(block: *mir.MIR.Block, expr: hir.HIR.Expression) !void {
    std.debug.print("Converting expression: {any}\n", .{expr});
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
            std.debug.print("Converting binary op with left: {any}, right: {any}\n", .{ bin_op.left, bin_op.right });
            try convertExpression(block, bin_op.left.*);
            try convertExpression(block, bin_op.right.*);
            try block.instructions.append(switch (bin_op.op) {
                .add => .Add,
            });
        },
        .variable => |var_expr| {
            std.debug.print("Converting variable: {s}\n", .{var_expr.name});
            try block.instructions.append(.{ .LoadVariable = try block.allocator.dupe(u8, var_expr.name) });
        },
    }
}
