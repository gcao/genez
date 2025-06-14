const std = @import("std");
const lsp = @import("lsp.zig");
const Document = @import("document.zig").Document;
const parser = @import("gene_parser");
const ast = parser.ast;

pub fn getDiagnostics(allocator: std.mem.Allocator, doc: Document) ![]lsp.Diagnostic {
    var diagnostics = std.ArrayList(lsp.Diagnostic).init(allocator);
    errdefer diagnostics.deinit();

    // Check for parse errors
    if (doc.parse_error) |err| {
        const pos = doc.getLineAndColumn(0); // TODO: Get actual error position
        const diagnostic = lsp.Diagnostic{
            .range = .{
                .start = .{ .line = pos.line, .character = pos.column },
                .end = .{ .line = pos.line, .character = pos.column + 1 },
            },
            .severity = .Error,
            .source = "gene",
            .message = try std.fmt.allocPrint(allocator, "Parse error: {}", .{err}),
        };
        try diagnostics.append(diagnostic);
    }

    // TODO: Add more diagnostics (undefined variables, type errors, etc.)

    return diagnostics.toOwnedSlice();
}

pub fn getDocumentSymbols(allocator: std.mem.Allocator, doc: Document) ![]lsp.DocumentSymbol {
    var symbols = std.ArrayList(lsp.DocumentSymbol).init(allocator);
    errdefer symbols.deinit();

    if (doc.ast_nodes) |nodes| {
        for (nodes) |node| {
            if (try extractSymbol(allocator, node, doc)) |symbol| {
                try symbols.append(symbol);
            }
        }
    }

    return symbols.toOwnedSlice();
}

fn extractSymbol(allocator: std.mem.Allocator, node: ast.AstNode, doc: Document) !?lsp.DocumentSymbol {
    switch (node) {
        .Expression => |expr| {
            switch (expr) {
                .FuncDef => |func_def| {
                    const start_pos = doc.getLineAndColumn(0); // TODO: Get actual position
                    const end_pos = doc.getLineAndColumn(0); // TODO: Get actual position
                    
                    return lsp.DocumentSymbol{
                        .name = try allocator.dupe(u8, func_def.name),
                        .detail = try std.fmt.allocPrint(allocator, "({d} params)", .{func_def.params.len}),
                        .kind = .Function,
                        .range = .{
                            .start = .{ .line = start_pos.line, .character = start_pos.column },
                            .end = .{ .line = end_pos.line, .character = end_pos.column },
                        },
                        .selectionRange = .{
                            .start = .{ .line = start_pos.line, .character = start_pos.column },
                            .end = .{ .line = start_pos.line, .character = start_pos.column + func_def.name.len },
                        },
                    };
                },
                .VarDecl => |var_decl| {
                    const start_pos = doc.getLineAndColumn(0); // TODO: Get actual position
                    const end_pos = doc.getLineAndColumn(0); // TODO: Get actual position
                    
                    return lsp.DocumentSymbol{
                        .name = try allocator.dupe(u8, var_decl.name),
                        .kind = .Variable,
                        .range = .{
                            .start = .{ .line = start_pos.line, .character = start_pos.column },
                            .end = .{ .line = end_pos.line, .character = end_pos.column },
                        },
                        .selectionRange = .{
                            .start = .{ .line = start_pos.line, .character = start_pos.column },
                            .end = .{ .line = start_pos.line, .character = start_pos.column + var_decl.name.len },
                        },
                    };
                },
                .ClassDef => |class_def| {
                    const start_pos = doc.getLineAndColumn(0); // TODO: Get actual position
                    const end_pos = doc.getLineAndColumn(0); // TODO: Get actual position
                    
                    var children = std.ArrayList(lsp.DocumentSymbol).init(allocator);
                    defer children.deinit();

                    // Add methods as children
                    for (class_def.methods) |method| {
                        if (try extractSymbol(allocator, .{ .Expression = method }, doc)) |child| {
                            try children.append(child);
                        }
                    }

                    return lsp.DocumentSymbol{
                        .name = try allocator.dupe(u8, class_def.name),
                        .kind = .Class,
                        .range = .{
                            .start = .{ .line = start_pos.line, .character = start_pos.column },
                            .end = .{ .line = end_pos.line, .character = end_pos.column },
                        },
                        .selectionRange = .{
                            .start = .{ .line = start_pos.line, .character = start_pos.column },
                            .end = .{ .line = start_pos.line, .character = start_pos.column + class_def.name.len },
                        },
                        .children = try children.toOwnedSlice(),
                    };
                },
                .Namespace => |ns| {
                    const start_pos = doc.getLineAndColumn(0); // TODO: Get actual position
                    const end_pos = doc.getLineAndColumn(0); // TODO: Get actual position
                    
                    var children = std.ArrayList(lsp.DocumentSymbol).init(allocator);
                    defer children.deinit();

                    // Add namespace contents as children
                    for (ns.body) |item| {
                        if (try extractSymbol(allocator, .{ .Expression = item }, doc)) |child| {
                            try children.append(child);
                        }
                    }

                    return lsp.DocumentSymbol{
                        .name = try allocator.dupe(u8, ns.name),
                        .kind = .Namespace,
                        .range = .{
                            .start = .{ .line = start_pos.line, .character = start_pos.column },
                            .end = .{ .line = end_pos.line, .character = end_pos.column },
                        },
                        .selectionRange = .{
                            .start = .{ .line = start_pos.line, .character = start_pos.column },
                            .end = .{ .line = start_pos.line, .character = start_pos.column + ns.name.len },
                        },
                        .children = try children.toOwnedSlice(),
                    };
                },
                else => return null,
            }
        },
    }
}

pub fn getSemanticTokens(allocator: std.mem.Allocator, doc: Document) !lsp.SemanticTokens {
    var tokens = std.ArrayList(u32).init(allocator);
    errdefer tokens.deinit();

    if (doc.ast_nodes) |nodes| {
        var last_line: u32 = 0;
        var last_col: u32 = 0;

        for (nodes) |node| {
            try collectSemanticTokens(&tokens, node, doc, &last_line, &last_col);
        }
    }

    return lsp.SemanticTokens{
        .data = try tokens.toOwnedSlice(),
    };
}

fn collectSemanticTokens(
    tokens: *std.ArrayList(u32),
    node: ast.AstNode,
    doc: Document,
    last_line: *u32,
    last_col: *u32,
) !void {
    switch (node) {
        .Expression => |expr| {
            switch (expr) {
                .Literal => |lit| {
                    switch (lit.value) {
                        .Int, .Float => {
                            // Add number token
                            try addSemanticToken(tokens, doc, 0, .number, last_line, last_col);
                        },
                        .String => {
                            // Add string token
                            try addSemanticToken(tokens, doc, 0, .string, last_line, last_col);
                        },
                        .Symbol => {
                            // Add variable/symbol token
                            try addSemanticToken(tokens, doc, 0, .variable, last_line, last_col);
                        },
                        else => {},
                    }
                },
                .FuncDef => |func_def| {
                    // Add function name token
                    _ = func_def;
                    try addSemanticToken(tokens, doc, 0, .function, last_line, last_col);
                    
                    // TODO: Add parameter tokens
                    // TODO: Recursively process function body
                },
                .VarDecl => |var_decl| {
                    // Add var keyword token
                    try addSemanticToken(tokens, doc, 0, .keyword, last_line, last_col);
                    
                    // Add variable name token
                    _ = var_decl;
                    try addSemanticToken(tokens, doc, 0, .variable, last_line, last_col);
                    
                    // TODO: Recursively process initial value
                },
                else => {
                    // TODO: Handle other expression types
                },
            }
        },
    }
}

fn addSemanticToken(
    tokens: *std.ArrayList(u32),
    doc: Document,
    offset: usize,
    token_type: lsp.SemanticTokenType,
    last_line: *u32,
    last_col: *u32,
) !void {
    const pos = doc.getLineAndColumn(offset);
    
    // Semantic tokens are encoded as:
    // [deltaLine, deltaStartChar, length, tokenType, tokenModifiers]
    const delta_line = pos.line - last_line.*;
    const delta_col = if (delta_line == 0) pos.column - last_col.* else pos.column;
    
    try tokens.append(delta_line);
    try tokens.append(delta_col);
    try tokens.append(1); // TODO: Calculate actual token length
    try tokens.append(@intFromEnum(token_type));
    try tokens.append(0); // No modifiers for now
    
    last_line.* = pos.line;
    last_col.* = pos.column;
}