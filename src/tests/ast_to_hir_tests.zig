const std = @import("std");
const ast = @import("../frontend/ast.zig");
const hir = @import("../ir/hir.zig");
const ast_to_hir = @import("../transforms/ast_to_hir.zig");
const parser = @import("../frontend/parser.zig");
const debug = @import("../core/debug.zig");

test "lower simple expression to HIR" {
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

    // Verify the HIR structure
    try std.testing.expectEqual(@as(usize, 1), hir_module.functions.items.len);

    // Get the main function
    const main_func = hir_module.functions.items[0];
    try std.testing.expectEqual(@as(usize, 1), main_func.body.items.len);

    const stmt = main_func.body.items[0];
    try std.testing.expect(stmt == .Expression);

    const expr = stmt.Expression;
    try std.testing.expect(expr == .func_call);

    const func_call = expr.func_call;

    // Check function expression
    try std.testing.expect(func_call.func.* == .variable);
    try std.testing.expectEqualStrings("print", func_call.func.*.variable.name);

    // Check arguments
    try std.testing.expectEqual(@as(usize, 1), func_call.args.items.len);
    try std.testing.expect(func_call.args.items[0].* == .literal);
    try std.testing.expectEqualStrings("Hello, world!", func_call.args.items[0].*.literal.string);
}

test "lower binary operation to HIR" {
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

    // Verify the HIR structure
    try std.testing.expectEqual(@as(usize, 1), hir_module.functions.items.len);

    // Get the main function
    const main_func = hir_module.functions.items[0];
    try std.testing.expectEqual(@as(usize, 1), main_func.body.items.len);

    const stmt = main_func.body.items[0];
    try std.testing.expect(stmt == .Expression);

    const expr = stmt.Expression;
    try std.testing.expect(expr == .func_call);

    const func_call = expr.func_call;

    // Check function expression
    try std.testing.expect(func_call.func.* == .variable);
    try std.testing.expectEqualStrings("+", func_call.func.*.variable.name);

    // Check arguments
    try std.testing.expectEqual(@as(usize, 2), func_call.args.items.len);
    try std.testing.expect(func_call.args.items[0].* == .literal);
    try std.testing.expectEqual(@as(i64, 1), func_call.args.items[0].*.literal.int);
    try std.testing.expect(func_call.args.items[1].* == .literal);
    try std.testing.expectEqual(@as(i64, 2), func_call.args.items[1].*.literal.int);
}

test "lower infix notation to HIR" {
    const allocator = std.testing.allocator;

    // Parse the source code
    const source = "(1 + 2)";
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

    // Verify the HIR structure
    try std.testing.expectEqual(@as(usize, 1), hir_module.functions.items.len);

    // Get the main function
    const main_func = hir_module.functions.items[0];
    try std.testing.expectEqual(@as(usize, 1), main_func.body.items.len);

    const stmt = main_func.body.items[0];
    try std.testing.expect(stmt == .Expression);

    const expr = stmt.Expression;
    try std.testing.expect(expr == .func_call);

    const func_call = expr.func_call;

    // Check function expression
    try std.testing.expect(func_call.func.* == .variable);
    try std.testing.expectEqualStrings("+", func_call.func.*.variable.name);

    // Check arguments
    try std.testing.expectEqual(@as(usize, 2), func_call.args.items.len);
    try std.testing.expect(func_call.args.items[0].* == .literal);
    try std.testing.expectEqual(@as(i64, 1), func_call.args.items[0].*.literal.int);
    try std.testing.expect(func_call.args.items[1].* == .literal);
    try std.testing.expectEqual(@as(i64, 2), func_call.args.items[1].*.literal.int);
}
