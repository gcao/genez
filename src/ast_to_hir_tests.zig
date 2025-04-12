const std = @import("std");
const ast = @import("ast.zig");
const hir = @import("hir.zig");
const ast_to_hir = @import("ast_to_hir.zig");
const parser = @import("parser.zig");
const debug = @import("debug.zig");

test "lower simple expression to HIR" {
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
    var hir_module = try ast_to_hir.lowerModule(allocator, ast_nodes);
    defer hir_module.deinit(allocator);
    
    // Verify the HIR structure
    try std.testing.expectEqual(@as(usize, 1), hir_module.statements.items.len);
    
    const stmt = hir_module.statements.items[0];
    try std.testing.expect(stmt == .expression);
    
    const expr = stmt.expression;
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
    var ast_nodes = try parser.parseGeneSource(allocator, source);
    defer {
        for (ast_nodes.items) |*node| {
            node.deinit(allocator);
        }
        ast_nodes.deinit();
    }
    
    // Lower to HIR
    var hir_module = try ast_to_hir.lowerModule(allocator, ast_nodes);
    defer hir_module.deinit(allocator);
    
    // Verify the HIR structure
    try std.testing.expectEqual(@as(usize, 1), hir_module.statements.items.len);
    
    const stmt = hir_module.statements.items[0];
    try std.testing.expect(stmt == .expression);
    
    const expr = stmt.expression;
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
    var ast_nodes = try parser.parseGeneSource(allocator, source);
    defer {
        for (ast_nodes.items) |*node| {
            node.deinit(allocator);
        }
        ast_nodes.deinit();
    }
    
    // Lower to HIR
    var hir_module = try ast_to_hir.lowerModule(allocator, ast_nodes);
    defer hir_module.deinit(allocator);
    
    // Verify the HIR structure
    try std.testing.expectEqual(@as(usize, 1), hir_module.statements.items.len);
    
    const stmt = hir_module.statements.items[0];
    try std.testing.expect(stmt == .expression);
    
    const expr = stmt.expression;
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
