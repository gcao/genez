const std = @import("std");
const parser = @import("../frontend/parser.zig");
const bytecode = @import("../backend/bytecode.zig");
const vm = @import("../backend/vm.zig");
const types = @import("../core/types.zig");
const testing = std.testing;

fn testGeneExecution(source: []const u8, expected: types.Value) !void {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Parse the source code
    var parse_result = try parser.parseGeneSource(allocator, source);
    defer {
        // Clean up the arena after we're done with the AST
        parse_result.arena.deinit();
    }

    // Lower to bytecode
    const ast_nodes = parser.getLastParseNodes() orelse return error.NoAstNodesFound;
    const func = try bytecode.lowerToBytecode(allocator, ast_nodes);

    // Print the bytecode for debugging
    std.debug.print("\nBytecode for source: {s}\n", .{source});
    for (func.instructions.items, 0..) |instr, i| {
        std.debug.print("  {}: {s}", .{ i, @tagName(instr.op) });
        if (instr.operand) |operand| {
            switch (operand) {
                .Int => |val| std.debug.print(" {}", .{val}),
                .String => |val| std.debug.print(" \"{s}\"", .{val}),
                else => std.debug.print(" (other operand)", .{}),
            }
        }
        std.debug.print("\n", .{});
    }

    // Create and initialize the VM
    var gene_vm = vm.VM.init(allocator, std.io.getStdOut().writer());
    defer gene_vm.deinit();

    // Execute the function
    try gene_vm.execute(&func);

    // Verify the result
    try testing.expect(gene_vm.stack.items.len == 1);
    const result = gene_vm.stack.items[0];

    // Print the result for debugging
    std.debug.print("Result: ", .{});
    switch (result) {
        .String => |val| std.debug.print("\"{s}\"\n", .{val}),
        .Int => |val| std.debug.print("{}\n", .{val}),
        .Bool => |val| std.debug.print("{}\n", .{val}),
        .Float => |val| std.debug.print("{}\n", .{val}),
        .Nil => std.debug.print("nil\n", .{}),
        .Symbol => |val| std.debug.print(":{s}\n", .{val}),
        .Array => std.debug.print("(array)\n", .{}),
        .Map => std.debug.print("(map)\n", .{}),
        .Function => std.debug.print("(function)\n", .{}),
        .ReturnAddress => std.debug.print("(return address)\n", .{}),
        .Variable => |val| std.debug.print("(variable {s})\n", .{val.name}),
        .BuiltinOperator => |op| std.debug.print("(builtin operator {any})\n", .{op}),
    }

    // Check that the result matches the expected value
    switch (expected) {
        .String => |exp_str| {
            try testing.expectEqualStrings(exp_str, result.String);
        },
        .Int => |exp_int| {
            try testing.expectEqual(exp_int, result.Int);
        },
        .Bool => |exp_bool| {
            try testing.expectEqual(exp_bool, result.Bool);
        },
        .Float => |exp_float| {
            try testing.expectEqual(exp_float, result.Float);
        },
        else => unreachable,
    }
}

test "execute string literal" {
    try testGeneExecution("\"hello\"", .{ .String = "hello" });
}

test "execute integer literal" {
    try testGeneExecution("42", .{ .Int = 42 });
}

test "execute binary operation" {
    try testGeneExecution("(+ 1 2)", .{ .Int = 3 });
}

test "execute infix notation" {
    try testGeneExecution("(1 + 2)", .{ .Int = 3 });
}
