const std = @import("std");
const ast = @import("ast.zig");
const hir = @import("hir.zig");
const mir = @import("mir.zig");
const bytecode = @import("bytecode.zig");
const ast_to_hir = @import("ast_to_hir.zig");
const hir_to_mir = @import("hir_to_mir.zig");
const mir_to_bytecode = @import("mir_to_bytecode.zig");
const parser = @import("parser.zig");
const debug = @import("debug.zig");

test "compile simple expression to bytecode" {
    const allocator = std.testing.allocator;

    // Parse the source code
    const source = "(print \"Hello, world!\")";
    var ast_nodes = try parser.parseGeneSource(allocator, source);
    defer {
        for (ast_nodes.items) |*node| {
            node.deinit(allocator);
        }
        ast_nodes.deinit();
    }

    // Lower to HIR
    var hir_module = try ast_to_hir.convert(allocator, ast_nodes.items);
    defer hir_module.deinit();

    // Lower to MIR
    var mir_module = try hir_to_mir.convert(allocator, hir_module);
    defer mir_module.deinit();

    // Lower to bytecode
    var bytecode_module = try mir_to_bytecode.convert(allocator, &mir_module);
    defer bytecode_module.deinit();

    // Verify the bytecode structure
    // bytecode_module is a Function, not a Module
    const main_func = bytecode_module;

    // Check that we have the expected instructions
    try std.testing.expectEqual(@as(usize, 3), main_func.instructions.items.len);

    // First instruction should be LoadVar for "print"
    try std.testing.expectEqual(bytecode.OpCode.LoadVar, main_func.instructions.items[0].op);
    try std.testing.expectEqualStrings("print", main_func.instructions.items[0].operand.?.String);

    // Second instruction should be LoadConst for the string
    try std.testing.expectEqual(bytecode.OpCode.LoadConst, main_func.instructions.items[1].op);
    try std.testing.expectEqualStrings("Hello, world!", main_func.instructions.items[1].operand.?.String);

    // Third instruction should be Call with 1 argument
    try std.testing.expectEqual(bytecode.OpCode.Call, main_func.instructions.items[2].op);
    try std.testing.expectEqual(@as(i64, 1), main_func.instructions.items[2].operand.?.Int);
}

test "compile binary operation to bytecode" {
    const allocator = std.testing.allocator;

    // Parse the source code
    const source = "(+ 1 2)";
    var ast_nodes = try parser.parseGeneSource(allocator, source);
    defer {
        for (ast_nodes.items) |*node| {
            node.deinit(allocator);
        }
        ast_nodes.deinit();
    }

    // Lower to HIR
    var hir_module = try ast_to_hir.convert(allocator, ast_nodes.items);
    defer hir_module.deinit();

    // Lower to MIR
    var mir_module = try hir_to_mir.convert(allocator, hir_module);
    defer mir_module.deinit();

    // Lower to bytecode
    var bytecode_module = try mir_to_bytecode.convert(allocator, &mir_module);
    defer bytecode_module.deinit();

    // Verify the bytecode structure
    // bytecode_module is a Function, not a Module
    const main_func = bytecode_module;

    // Check that we have the expected instructions
    try std.testing.expectEqual(@as(usize, 4), main_func.instructions.items.len);

    // First instruction should be LoadVar for "+"
    try std.testing.expectEqual(bytecode.OpCode.LoadVar, main_func.instructions.items[0].op);
    try std.testing.expectEqualStrings("+", main_func.instructions.items[0].operand.?.String);

    // Second instruction should be LoadConst for 1
    try std.testing.expectEqual(bytecode.OpCode.LoadConst, main_func.instructions.items[1].op);
    try std.testing.expectEqual(@as(i64, 1), main_func.instructions.items[1].operand.?.Int);

    // Third instruction should be LoadConst for 2
    try std.testing.expectEqual(bytecode.OpCode.LoadConst, main_func.instructions.items[2].op);
    try std.testing.expectEqual(@as(i64, 2), main_func.instructions.items[2].operand.?.Int);

    // Fourth instruction should be Call with 2 arguments
    try std.testing.expectEqual(bytecode.OpCode.Call, main_func.instructions.items[3].op);
    try std.testing.expectEqual(@as(i64, 2), main_func.instructions.items[3].operand.?.Int);
}
