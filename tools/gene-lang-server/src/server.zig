const std = @import("std");
const lsp = @import("lsp.zig");
const Document = @import("document.zig").Document;
const analysis = @import("analysis.zig");

pub const Server = struct {
    allocator: std.mem.Allocator,
    documents: std.StringHashMap(Document),
    initialized: bool = false,
    shutdown_requested: bool = false,
    stdin: std.fs.File.Reader,
    stdout: std.fs.File.Writer,

    pub fn init(allocator: std.mem.Allocator) !Server {
        return Server{
            .allocator = allocator,
            .documents = std.StringHashMap(Document).init(allocator),
            .stdin = std.io.getStdIn().reader(),
            .stdout = std.io.getStdOut().writer(),
        };
    }

    pub fn deinit(self: *Server) void {
        var iter = self.documents.iterator();
        while (iter.next()) |entry| {
            entry.value_ptr.deinit();
        }
        self.documents.deinit();
    }

    pub fn run(self: *Server) !void {
        while (!self.shutdown_requested) {
            // Read LSP message header
            const header = try self.readHeader();
            if (header.content_length == 0) continue;

            // Read message content
            const content = try self.allocator.alloc(u8, header.content_length);
            defer self.allocator.free(content);
            _ = try self.stdin.readAll(content);

            // Parse JSON-RPC message
            const parsed = try std.json.parseFromSlice(lsp.Message, self.allocator, content, .{});
            defer parsed.deinit();

            // Handle the message
            try self.handleMessage(parsed.value);
        }
    }

    const Header = struct {
        content_length: usize = 0,
    };

    fn readHeader(self: *Server) !Header {
        var header = Header{};
        var buf: [1024]u8 = undefined;

        while (true) {
            const line = try self.stdin.readUntilDelimiterOrEof(&buf, '\n') orelse return header;
            const trimmed = std.mem.trim(u8, line, "\r\n");
            
            if (trimmed.len == 0) {
                // Empty line indicates end of headers
                return header;
            }

            if (std.mem.startsWith(u8, trimmed, "Content-Length: ")) {
                const len_str = trimmed["Content-Length: ".len..];
                header.content_length = try std.fmt.parseInt(usize, len_str, 10);
            }
        }
    }

    fn handleMessage(self: *Server, msg: lsp.Message) !void {
        if (msg.method) |method| {
            if (msg.id) |id| {
                // Request
                try self.handleRequest(id, method, msg.params);
            } else {
                // Notification
                try self.handleNotification(method, msg.params);
            }
        } else if (msg.id) |_| {
            // Response (we don't send requests yet, so this shouldn't happen)
            std.log.warn("Received unexpected response", .{});
        }
    }

    fn handleRequest(self: *Server, id: lsp.Id, method: []const u8, params: ?std.json.Value) !void {
        if (std.mem.eql(u8, method, "initialize")) {
            try self.handleInitialize(id, params);
        } else if (std.mem.eql(u8, method, "shutdown")) {
            self.shutdown_requested = true;
            try self.sendResult(id, null);
        } else if (!self.initialized) {
            try self.sendError(id, lsp.ErrorCode.ServerNotInitialized, "Server not initialized");
        } else if (std.mem.eql(u8, method, "textDocument/completion")) {
            try self.handleCompletion(id, params);
        } else if (std.mem.eql(u8, method, "textDocument/hover")) {
            try self.handleHover(id, params);
        } else if (std.mem.eql(u8, method, "textDocument/documentSymbol")) {
            try self.handleDocumentSymbol(id, params);
        } else if (std.mem.eql(u8, method, "textDocument/semanticTokens/full")) {
            try self.handleSemanticTokens(id, params);
        } else {
            try self.sendError(id, lsp.ErrorCode.MethodNotFound, "Method not found");
        }
    }

    fn handleNotification(self: *Server, method: []const u8, params: ?std.json.Value) !void {
        if (std.mem.eql(u8, method, "initialized")) {
            self.initialized = true;
        } else if (std.mem.eql(u8, method, "exit")) {
            std.process.exit(if (self.shutdown_requested) 0 else 1);
        } else if (std.mem.eql(u8, method, "textDocument/didOpen")) {
            try self.handleDidOpen(params);
        } else if (std.mem.eql(u8, method, "textDocument/didChange")) {
            try self.handleDidChange(params);
        } else if (std.mem.eql(u8, method, "textDocument/didClose")) {
            try self.handleDidClose(params);
        }
    }

    fn handleInitialize(self: *Server, id: lsp.Id, params: ?std.json.Value) !void {
        _ = params; // TODO: Parse InitializeParams

        const result = lsp.InitializeResult{
            .capabilities = .{
                .textDocumentSync = .{
                    .openClose = true,
                    .change = .Full,
                    .save = .{ .includeText = false },
                },
                .completionProvider = .{
                    .triggerCharacters = &[_][]const u8{ ".", "/" },
                },
                .hoverProvider = true,
                .definitionProvider = true,
                .documentSymbolProvider = true,
                .semanticTokensProvider = .{
                    .legend = .{
                        .tokenTypes = &[_][]const u8{
                            "namespace", "type", "class", "enum", "interface",
                            "struct", "typeParameter", "parameter", "variable",
                            "property", "enumMember", "event", "function",
                            "method", "macro", "keyword", "modifier",
                            "comment", "string", "number", "regexp", "operator",
                        },
                        .tokenModifiers = &[_][]const u8{
                            "declaration", "definition", "readonly", "static",
                            "deprecated", "abstract", "async", "modification",
                            "documentation", "defaultLibrary",
                        },
                    },
                    .full = true,
                },
            },
            .serverInfo = .{
                .name = "Gene Language Server",
                .version = "0.1.0",
            },
        };

        try self.sendResult(id, result);
    }

    fn handleDidOpen(self: *Server, params: ?std.json.Value) !void {
        if (params == null) return;
        
        // TODO: Properly parse DidOpenTextDocumentParams
        const obj = params.?.object;
        const text_doc = obj.get("textDocument") orelse return;
        const uri = text_doc.object.get("uri") orelse return;
        const text = text_doc.object.get("text") orelse return;

        const uri_str = try self.allocator.dupe(u8, uri.string);
        errdefer self.allocator.free(uri_str);

        const doc = try Document.init(self.allocator, uri_str, text.string);
        try self.documents.put(uri_str, doc);

        // Send diagnostics
        try self.publishDiagnostics(uri_str);
    }

    fn handleDidChange(self: *Server, params: ?std.json.Value) !void {
        if (params == null) return;

        // TODO: Properly parse DidChangeTextDocumentParams
        const obj = params.?.object;
        const text_doc = obj.get("textDocument") orelse return;
        const uri = text_doc.object.get("uri") orelse return;
        const changes = obj.get("contentChanges") orelse return;

        if (self.documents.get(uri.string)) |*doc| {
            // For now, we only support full document sync
            if (changes.array.items.len > 0) {
                const change = changes.array.items[0];
                const new_text = change.object.get("text") orelse return;
                try doc.updateContent(new_text.string);
            }

            // Send updated diagnostics
            try self.publishDiagnostics(uri.string);
        }
    }

    fn handleDidClose(self: *Server, params: ?std.json.Value) !void {
        if (params == null) return;

        // TODO: Properly parse DidCloseTextDocumentParams
        const obj = params.?.object;
        const text_doc = obj.get("textDocument") orelse return;
        const uri = text_doc.object.get("uri") orelse return;

        if (self.documents.fetchRemove(uri.string)) |entry| {
            self.allocator.free(entry.key);
            entry.value.deinit();
        }
    }

    fn handleCompletion(self: *Server, id: lsp.Id, params: ?std.json.Value) !void {
        _ = params; // TODO: Parse CompletionParams

        // For now, return some basic completions
        const items = [_]lsp.CompletionItem{
            .{
                .label = "fn",
                .kind = .Keyword,
                .detail = "Function definition",
                .insertText = "fn ${1:name} [${2:params}]\n  $0",
                .insertTextFormat = .Snippet,
            },
            .{
                .label = "var",
                .kind = .Keyword,
                .detail = "Variable declaration",
                .insertText = "var ${1:name} = $0",
                .insertTextFormat = .Snippet,
            },
            .{
                .label = "if",
                .kind = .Keyword,
                .detail = "If expression",
                .insertText = "if ${1:condition}\n  ${2:then}\nelse\n  ${3:else}",
                .insertTextFormat = .Snippet,
            },
            .{
                .label = "match",
                .kind = .Keyword,
                .detail = "Pattern matching",
                .insertText = "match ${1:expr}\n  (${2:pattern} ${3:result})\n  (_ ${4:default})",
                .insertTextFormat = .Snippet,
            },
            .{
                .label = "class",
                .kind = .Keyword,
                .detail = "Class definition",
                .insertText = "class ${1:Name}\n  $0",
                .insertTextFormat = .Snippet,
            },
        };

        const result = lsp.CompletionList{
            .isIncomplete = false,
            .items = &items,
        };

        try self.sendResult(id, result);
    }

    fn handleHover(self: *Server, id: lsp.Id, params: ?std.json.Value) !void {
        _ = params; // TODO: Parse HoverParams and implement hover

        // For now, return null (no hover)
        try self.sendResult(id, null);
    }

    fn handleDocumentSymbol(self: *Server, id: lsp.Id, params: ?std.json.Value) !void {
        if (params == null) {
            try self.sendResult(id, null);
            return;
        }

        // TODO: Parse DocumentSymbolParams
        const obj = params.?.object;
        const text_doc = obj.get("textDocument") orelse {
            try self.sendResult(id, null);
            return;
        };
        const uri = text_doc.object.get("uri") orelse {
            try self.sendResult(id, null);
            return;
        };

        const doc = self.documents.get(uri.string) orelse {
            try self.sendResult(id, null);
            return;
        };

        // Get symbols from the document
        const symbols = try analysis.getDocumentSymbols(self.allocator, doc);
        defer self.allocator.free(symbols);

        try self.sendResult(id, symbols);
    }

    fn handleSemanticTokens(self: *Server, id: lsp.Id, params: ?std.json.Value) !void {
        if (params == null) {
            try self.sendResult(id, null);
            return;
        }

        // TODO: Parse SemanticTokensParams
        const obj = params.?.object;
        const text_doc = obj.get("textDocument") orelse {
            try self.sendResult(id, null);
            return;
        };
        const uri = text_doc.object.get("uri") orelse {
            try self.sendResult(id, null);
            return;
        };

        const doc = self.documents.get(uri.string) orelse {
            try self.sendResult(id, null);
            return;
        };

        // Get semantic tokens from the document
        const tokens = try analysis.getSemanticTokens(self.allocator, doc);
        defer self.allocator.free(tokens.data);

        try self.sendResult(id, tokens);
    }

    fn publishDiagnostics(self: *Server, uri: []const u8) !void {
        const doc = self.documents.get(uri) orelse return;
        
        // Get diagnostics from document analysis
        const diagnostics = try analysis.getDiagnostics(self.allocator, doc);
        defer self.allocator.free(diagnostics);

        // Create notification
        const params = .{
            .uri = uri,
            .diagnostics = diagnostics,
        };

        try self.sendNotification("textDocument/publishDiagnostics", params);
    }

    fn sendResult(self: *Server, id: lsp.Id, result: anytype) !void {
        const msg = lsp.Message{
            .id = id,
            .result = if (@TypeOf(result) == @TypeOf(null)) 
                null 
            else 
                try std.json.parseFromValue(std.json.Value, self.allocator, result, .{}),
        };

        try self.sendMessage(msg);
    }

    fn sendError(self: *Server, id: lsp.Id, code: i32, message: []const u8) !void {
        const msg = lsp.Message{
            .id = id,
            .@"error" = .{
                .code = code,
                .message = message,
            },
        };

        try self.sendMessage(msg);
    }

    fn sendNotification(self: *Server, method: []const u8, params: anytype) !void {
        const msg = lsp.Message{
            .method = method,
            .params = try std.json.parseFromValue(std.json.Value, self.allocator, params, .{}),
        };

        try self.sendMessage(msg);
    }

    fn sendMessage(self: *Server, msg: lsp.Message) !void {
        // Serialize message to JSON
        var buffer = std.ArrayList(u8).init(self.allocator);
        defer buffer.deinit();

        try std.json.stringify(msg, .{}, buffer.writer());

        // Send header
        try self.stdout.print("Content-Length: {d}\r\n\r\n", .{buffer.items.len});

        // Send content
        try self.stdout.writeAll(buffer.items);
    }
};