const ast = @import("ast.zig");

pub fn TypeCheckError() type {
    return error{
        UnknownIdentifier,
        TypeMismatch,
        // ...
    };
}

pub fn checkProgram(nodes: []const ast.AstNode) !void {
    // Skeleton static checks
    // For now, do nothing or minimal:
    for (nodes) |node| {
        // match node and do checks
        _ = node;
    }
}
