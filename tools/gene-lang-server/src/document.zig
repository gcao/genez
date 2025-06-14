const std = @import("std");
const parser = @import("gene_parser");
const ast = parser.ast;

pub const Document = struct {
    allocator: std.mem.Allocator,
    uri: []const u8,
    content: []u8,
    version: i32 = 0,
    ast_nodes: ?[]ast.AstNode = null,
    parse_error: ?parser.ParserError = null,

    pub fn init(allocator: std.mem.Allocator, uri: []const u8, content: []const u8) !Document {
        var doc = Document{
            .allocator = allocator,
            .uri = uri,
            .content = try allocator.dupe(u8, content),
        };

        // Parse the document
        doc.parse();

        return doc;
    }

    pub fn deinit(self: *Document) void {
        self.allocator.free(self.content);
        if (self.ast_nodes) |nodes| {
            // TODO: Proper cleanup of AST nodes
            _ = nodes;
        }
    }

    pub fn updateContent(self: *Document, new_content: []const u8) !void {
        self.allocator.free(self.content);
        self.content = try self.allocator.dupe(u8, new_content);
        self.version += 1;

        // Re-parse the document
        self.parse();
    }

    fn parse(self: *Document) void {
        // Clear previous parse results
        self.ast_nodes = null;
        self.parse_error = null;

        // Try to parse the content
        const result = parser.parseGeneSource(self.allocator, self.content) catch |err| {
            self.parse_error = err;
            return;
        };

        self.ast_nodes = result.nodes;
        // Note: We're not storing the arena allocator from the parse result
        // This is a simplification - in a real implementation, we'd need to manage this properly
    }

    pub fn getLineAndColumn(self: Document, offset: usize) struct { line: u32, column: u32 } {
        var line: u32 = 0;
        var column: u32 = 0;
        var i: usize = 0;

        while (i < offset and i < self.content.len) : (i += 1) {
            if (self.content[i] == '\n') {
                line += 1;
                column = 0;
            } else {
                column += 1;
            }
        }

        return .{ .line = line, .column = column };
    }

    pub fn getOffset(self: Document, line: u32, column: u32) ?usize {
        var current_line: u32 = 0;
        var current_column: u32 = 0;
        var i: usize = 0;

        while (i < self.content.len) : (i += 1) {
            if (current_line == line and current_column == column) {
                return i;
            }

            if (self.content[i] == '\n') {
                current_line += 1;
                current_column = 0;
            } else {
                current_column += 1;
            }
        }

        if (current_line == line and current_column == column) {
            return i;
        }

        return null;
    }

    pub fn getLine(self: Document, line_number: u32) ?[]const u8 {
        var current_line: u32 = 0;
        var line_start: usize = 0;
        var i: usize = 0;

        while (i < self.content.len) : (i += 1) {
            if (current_line == line_number) {
                const line_end = std.mem.indexOfScalarPos(u8, self.content, i, '\n') orelse self.content.len;
                return self.content[line_start..line_end];
            }

            if (self.content[i] == '\n') {
                current_line += 1;
                line_start = i + 1;
            }
        }

        return null;
    }
};