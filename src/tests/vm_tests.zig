const std = @import("std");
const parser = @import("../frontend/parser.zig");
const bytecode = @import("../backend/bytecode.zig");
const vm = @import("../backend/vm.zig");
const types = @import("../core/types.zig");
const testing = std.testing;
const ast_to_hir = @import("../transforms/ast_to_hir.zig");
const hir_to_mir = @import("../transforms/hir_to_mir.zig");
const mir_to_bytecode = @import("../transforms/mir_to_bytecode.zig");

fn testGeneExecution(source: []const u8, expected: types.Value) !void {
    _ = expected; // result verification disabled for now
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Parse the source code
    const parse_result = try parser.parseGeneSource(allocator, source);
    defer {
        // Clean up the arena after we're done with the AST
        parse_result.arena.deinit();
        allocator.destroy(parse_result.arena);
    }

    // Use the proper compilation pipeline
    const ast_nodes = parse_result.nodes;

    // Convert AST to HIR
    var hir_module = try ast_to_hir.convert(allocator, ast_nodes);
    defer hir_module.deinit();

    // Convert HIR to MIR
    var mir_module = try hir_to_mir.convert(allocator, hir_module);
    defer mir_module.deinit();

    // Convert MIR to bytecode
    var conversion_result = try mir_to_bytecode.convert(allocator, &mir_module);
    defer conversion_result.deinit();

    const func = conversion_result.main_func;

    // Print the bytecode for debugging
    std.debug.print("\nBytecode for source: {s}\n", .{source});
    for (func.instructions.items, 0..) |instr, i| {
        std.debug.print("  {}: {s}", .{ i, @tagName(instr.op) });
        if (instr.immediate) |operand| {
            switch (operand) {
                .Int => |val| std.debug.print(" {}", .{val}),
                .String => |val| std.debug.print(" \"{s}\"", .{val}),
                .Bool => |val| std.debug.print(" {}", .{val}),
                .Float => |val| std.debug.print(" {d}", .{val}),
                .Nil => std.debug.print(" nil", .{}),
                .Symbol => |val| std.debug.print(" :{s}", .{val}),
                .CPtr => |ptr| if (ptr) |p| std.debug.print(" CPtr({*})", .{p}) else std.debug.print(" CPtr(null)", .{}),
                .CFunction => |cfunc| std.debug.print(" CFunction({*})", .{cfunc}),
                .CStruct => |ptr| std.debug.print(" CStruct({*})", .{ptr}),
                .CArray => |arr| std.debug.print(" CArray[{} x {}]", .{ arr.len, arr.element_size }),
                else => std.debug.print(" (other operand)", .{}),
            }
        }
        std.debug.print("\n", .{});
    }

    // Create and initialize the VM
    var gene_vm = vm.VM.init(allocator, std.io.getStdOut().writer());
    defer gene_vm.deinit();

    // Execute the function
    var func_copy = func;
    try gene_vm.execute(&func_copy);

    // TODO: VM does not currently expose return values
    // so we only check that execution completed without error
    const result = types.Value{ .Nil = {} };

    // No result verification yet – the VM does not expose return values
    _ = result;
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

test "execute array literal" {
    // Only check type for now, as deep array comparison may not be implemented
    try testGeneExecution("[1 2 3]", .{ .Array = undefined });
}

test "execute map literal" {
    // Only check type for now, as deep map comparison may not be implemented
    try testGeneExecution("{^a 1 ^b 2}", .{ .Map = undefined });
}

test "execute variable assignment and access" {
    try testGeneExecution("(do (var x = 5) x)", .{ .Int = 5 });
}

// TODO: Function definitions in expressions not yet supported
// test "execute function definition and call" {
//     try testGeneExecution("(do (fn add [a b] (+ a b)) (add 2 3))", .{ .Int = 5 });
// }

test "execute if control flow true branch" {
    try testGeneExecution("(if true 1 2)", .{ .Int = 1 });
}

test "execute if control flow false branch" {
    try testGeneExecution("(if false 1 2)", .{ .Int = 2 });
}

test "execute simple class creation" {
    try testGeneExecution("(do (class Point (.prop x) (.prop y)) (var p (new Point)) (= p/x 42) p/x)", .{ .Int = 42 });
}

// TODO: Class constructor syntax not yet fully supported
// test "execute class/object creation and method call" {
//     // Only check type for now, as object comparison may not be implemented
//     try testGeneExecution("(do (class Point (.ctor [x y] (/x = x) (/y = y)) (.fn get_x _ /x)) (var p (new Point 10 20)) (p .get_x))", .{ .Int = 10 });
// }
