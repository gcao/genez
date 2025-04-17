const std = @import("std");
const ast = @import("ast.zig");
const hir = @import("hir.zig");

/// AstToHirConverter defines the interface for converting AST to HIR
///
/// This interface provides a clear contract for the transformation from
/// AST (Abstract Syntax Tree) to HIR (High-level Intermediate Representation).
pub const AstToHirConverter = struct {
    /// Convert AST nodes to an HIR module
    ///
    /// This function takes a slice of AST nodes and converts them to an HIR module.
    /// The caller is responsible for managing the memory of the returned HIR module.
    ///
    /// Parameters:
    ///   - allocator: The allocator to use for allocating memory
    ///   - nodes: The AST nodes to convert
    ///
    /// Returns:
    ///   - An HIR module representing the converted AST nodes
    ///
    /// Errors:
    ///   - Std.mem.Allocator.Error: If memory allocation fails
    ///   - error.InvalidSyntax: If the AST contains invalid syntax
    ///   - error.UnsupportedFeature: If the AST contains unsupported features
    pub fn convert(allocator: std.mem.Allocator, nodes: []ast.AstNode) !hir.HIR {
        return @import("ast_to_hir.zig").convert(allocator, nodes);
    }
};

/// Convert AST nodes to an HIR module
///
/// This is a convenience function that delegates to AstToHirConverter.convert.
///
/// Parameters:
///   - allocator: The allocator to use for allocating memory
///   - nodes: The AST nodes to convert
///
/// Returns:
///   - An HIR module representing the converted AST nodes
///
/// Errors:
///   - Std.mem.Allocator.Error: If memory allocation fails
///   - error.InvalidSyntax: If the AST contains invalid syntax
///   - error.UnsupportedFeature: If the AST contains unsupported features
pub fn convert(allocator: std.mem.Allocator, nodes: []ast.AstNode) !hir.HIR {
    return AstToHirConverter.convert(allocator, nodes);
}
