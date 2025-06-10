const std = @import("std");
const ast = @import("../frontend/ast.zig");
const hir = @import("../ir/hir.zig");
const mir = @import("../ir/mir.zig");
const bytecode = @import("../backend/bytecode.zig");
const ast_to_hir = @import("../transforms/ast_to_hir.zig");
const hir_to_mir = @import("../transforms/hir_to_mir.zig");
const mir_to_bytecode = @import("../transforms/mir_to_bytecode.zig");
const parser = @import("../frontend/parser.zig");
const debug = @import("../core/debug.zig");

test "compile simple expression to bytecode" {
    const allocator = std.testing.allocator;

    // Parse the source code
    const source = "(print \"Hello, world!\")";
    var parse_result = try parser.parseGeneSource(allocator, source);
    defer {
        // Clean up the arena after we're done with the AST
        parse_result.arena.deinit();
        allocator.destroy(parse_result.arena);
    }

    // Lower to HIR
    const ast_nodes = parse_result.nodes;
    var hir_module = try ast_to_hir.convert(allocator, ast_nodes);
    defer hir_module.deinit();

    // Lower to MIR
    var mir_module = try hir_to_mir.convert(allocator, hir_module);
    defer mir_module.deinit();

    // Lower to bytecode
    var conversion_result = try mir_to_bytecode.convert(allocator, &mir_module);
    defer conversion_result.deinit();

    // Verify the bytecode structure
    // conversion_result.main_func is a Function
    const main_func = conversion_result.main_func;

    // Check that we have the expected instructions (including Return)
    try std.testing.expectEqual(@as(usize, 4), main_func.instructions.items.len);

    // First instruction should be LoadVar for "print"
    try std.testing.expectEqual(bytecode.OpCode.LoadVar, main_func.instructions.items[0].op);
    try std.testing.expectEqualStrings("print", main_func.instructions.items[0].operand.?.String);

    // Second instruction should be LoadConst for the string
    try std.testing.expectEqual(bytecode.OpCode.LoadConst, main_func.instructions.items[1].op);
    try std.testing.expectEqualStrings("Hello, world!", main_func.instructions.items[1].operand.?.String);

    // Third instruction should be Call with 1 argument
    try std.testing.expectEqual(bytecode.OpCode.Call, main_func.instructions.items[2].op);
    try std.testing.expectEqual(@as(i64, 1), main_func.instructions.items[2].operand.?.Int);

    // Fourth instruction should be Return
    try std.testing.expectEqual(bytecode.OpCode.Return, main_func.instructions.items[3].op);
}

test "compile binary operation to bytecode" {
    const allocator = std.testing.allocator;

    // Parse the source code
    const source = "(+ 1 2)";
    var parse_result = try parser.parseGeneSource(allocator, source);
    defer {
        // Clean up the arena after we're done with the AST
        parse_result.arena.deinit();
        allocator.destroy(parse_result.arena);
    }

    // Lower to HIR
    const ast_nodes = parse_result.nodes;
    var hir_module = try ast_to_hir.convert(allocator, ast_nodes);
    defer hir_module.deinit();

    // Lower to MIR
    var mir_module = try hir_to_mir.convert(allocator, hir_module);
    defer mir_module.deinit();

    // Lower to bytecode
    var conversion_result = try mir_to_bytecode.convert(allocator, &mir_module);
    defer conversion_result.deinit();

    // Verify the bytecode structure
    // conversion_result.main_func is a Function
    const main_func = conversion_result.main_func;

    // Check that we have the expected instructions (including Return)
    try std.testing.expectEqual(@as(usize, 5), main_func.instructions.items.len);

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

    // Fifth instruction should be Return
    try std.testing.expectEqual(bytecode.OpCode.Return, main_func.instructions.items[4].op);
}
