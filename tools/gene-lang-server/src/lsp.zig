// Language Server Protocol types and constants
const std = @import("std");

// LSP Message structure
pub const Message = struct {
    jsonrpc: []const u8 = "2.0",
    id: ?Id = null,
    method: ?[]const u8 = null,
    params: ?std.json.Value = null,
    result: ?std.json.Value = null,
    @"error": ?Error = null,
};

pub const Id = union(enum) {
    number: i64,
    string: []const u8,
};

pub const Error = struct {
    code: i32,
    message: []const u8,
    data: ?std.json.Value = null,
};

// Error codes
pub const ErrorCode = struct {
    pub const ParseError = -32700;
    pub const InvalidRequest = -32600;
    pub const MethodNotFound = -32601;
    pub const InvalidParams = -32602;
    pub const InternalError = -32603;
    pub const ServerNotInitialized = -32002;
    pub const UnknownErrorCode = -32001;
};

// Initialize request
pub const InitializeParams = struct {
    processId: ?i64 = null,
    clientInfo: ?struct {
        name: []const u8,
        version: ?[]const u8 = null,
    } = null,
    rootUri: ?[]const u8 = null,
    capabilities: ClientCapabilities,
    trace: ?[]const u8 = null,
    workspaceFolders: ?[]const WorkspaceFolder = null,
};

pub const ClientCapabilities = struct {
    textDocument: ?TextDocumentClientCapabilities = null,
    workspace: ?WorkspaceClientCapabilities = null,
};

pub const TextDocumentClientCapabilities = struct {
    synchronization: ?struct {
        dynamicRegistration: ?bool = null,
        willSave: ?bool = null,
        willSaveWaitUntil: ?bool = null,
        didSave: ?bool = null,
    } = null,
    completion: ?struct {
        dynamicRegistration: ?bool = null,
        completionItem: ?struct {
            snippetSupport: ?bool = null,
            commitCharactersSupport: ?bool = null,
            documentationFormat: ?[]const []const u8 = null,
        } = null,
    } = null,
    hover: ?struct {
        dynamicRegistration: ?bool = null,
        contentFormat: ?[]const []const u8 = null,
    } = null,
    definition: ?struct {
        dynamicRegistration: ?bool = null,
    } = null,
};

pub const WorkspaceClientCapabilities = struct {
    applyEdit: ?bool = null,
    workspaceEdit: ?struct {
        documentChanges: ?bool = null,
    } = null,
    symbol: ?struct {
        dynamicRegistration: ?bool = null,
    } = null,
};

pub const WorkspaceFolder = struct {
    uri: []const u8,
    name: []const u8,
};

// Initialize result
pub const InitializeResult = struct {
    capabilities: ServerCapabilities,
    serverInfo: ?struct {
        name: []const u8,
        version: ?[]const u8 = null,
    } = null,
};

pub const ServerCapabilities = struct {
    textDocumentSync: ?TextDocumentSyncOptions = null,
    completionProvider: ?CompletionOptions = null,
    hoverProvider: ?bool = null,
    definitionProvider: ?bool = null,
    referencesProvider: ?bool = null,
    documentSymbolProvider: ?bool = null,
    workspaceSymbolProvider: ?bool = null,
    documentFormattingProvider: ?bool = null,
    documentRangeFormattingProvider: ?bool = null,
    renameProvider: ?bool = null,
    foldingRangeProvider: ?bool = null,
    semanticTokensProvider: ?SemanticTokensOptions = null,
};

pub const TextDocumentSyncOptions = struct {
    openClose: ?bool = null,
    change: ?TextDocumentSyncKind = null,
    save: ?struct {
        includeText: ?bool = null,
    } = null,
};

pub const TextDocumentSyncKind = enum(u8) {
    None = 0,
    Full = 1,
    Incremental = 2,
};

pub const CompletionOptions = struct {
    triggerCharacters: ?[]const []const u8 = null,
    resolveProvider: ?bool = null,
};

pub const SemanticTokensOptions = struct {
    legend: SemanticTokensLegend,
    full: ?bool = null,
    range: ?bool = null,
};

pub const SemanticTokensLegend = struct {
    tokenTypes: []const []const u8,
    tokenModifiers: []const []const u8,
};

// Text document notifications
pub const DidOpenTextDocumentParams = struct {
    textDocument: TextDocumentItem,
};

pub const TextDocumentItem = struct {
    uri: []const u8,
    languageId: []const u8,
    version: i32,
    text: []const u8,
};

pub const DidChangeTextDocumentParams = struct {
    textDocument: VersionedTextDocumentIdentifier,
    contentChanges: []const TextDocumentContentChangeEvent,
};

pub const VersionedTextDocumentIdentifier = struct {
    uri: []const u8,
    version: i32,
};

pub const TextDocumentContentChangeEvent = struct {
    range: ?Range = null,
    rangeLength: ?u32 = null,
    text: []const u8,
};

pub const DidCloseTextDocumentParams = struct {
    textDocument: TextDocumentIdentifier,
};

pub const TextDocumentIdentifier = struct {
    uri: []const u8,
};

// Common types
pub const Position = struct {
    line: u32,
    character: u32,
};

pub const Range = struct {
    start: Position,
    end: Position,
};

pub const Location = struct {
    uri: []const u8,
    range: Range,
};

pub const Diagnostic = struct {
    range: Range,
    severity: ?DiagnosticSeverity = null,
    code: ?union(enum) {
        number: i32,
        string: []const u8,
    } = null,
    source: ?[]const u8 = null,
    message: []const u8,
    relatedInformation: ?[]const DiagnosticRelatedInformation = null,
};

pub const DiagnosticSeverity = enum(u8) {
    Error = 1,
    Warning = 2,
    Information = 3,
    Hint = 4,
};

pub const DiagnosticRelatedInformation = struct {
    location: Location,
    message: []const u8,
};

// Completion
pub const CompletionParams = struct {
    textDocument: TextDocumentIdentifier,
    position: Position,
    context: ?CompletionContext = null,
};

pub const CompletionContext = struct {
    triggerKind: CompletionTriggerKind,
    triggerCharacter: ?[]const u8 = null,
};

pub const CompletionTriggerKind = enum(u8) {
    Invoked = 1,
    TriggerCharacter = 2,
    TriggerForIncompleteCompletions = 3,
};

pub const CompletionList = struct {
    isIncomplete: bool,
    items: []const CompletionItem,
};

pub const CompletionItem = struct {
    label: []const u8,
    kind: ?CompletionItemKind = null,
    detail: ?[]const u8 = null,
    documentation: ?[]const u8 = null,
    insertText: ?[]const u8 = null,
    insertTextFormat: ?InsertTextFormat = null,
};

pub const CompletionItemKind = enum(u8) {
    Text = 1,
    Method = 2,
    Function = 3,
    Constructor = 4,
    Field = 5,
    Variable = 6,
    Class = 7,
    Interface = 8,
    Module = 9,
    Property = 10,
    Unit = 11,
    Value = 12,
    Enum = 13,
    Keyword = 14,
    Snippet = 15,
    Color = 16,
    File = 17,
    Reference = 18,
    Folder = 19,
    EnumMember = 20,
    Constant = 21,
    Struct = 22,
    Event = 23,
    Operator = 24,
    TypeParameter = 25,
};

pub const InsertTextFormat = enum(u8) {
    PlainText = 1,
    Snippet = 2,
};

// Hover
pub const HoverParams = struct {
    textDocument: TextDocumentIdentifier,
    position: Position,
};

pub const Hover = struct {
    contents: MarkupContent,
    range: ?Range = null,
};

pub const MarkupContent = struct {
    kind: MarkupKind,
    value: []const u8,
};

pub const MarkupKind = enum {
    plaintext,
    markdown,
};

// Document symbols
pub const DocumentSymbolParams = struct {
    textDocument: TextDocumentIdentifier,
};

pub const DocumentSymbol = struct {
    name: []const u8,
    detail: ?[]const u8 = null,
    kind: SymbolKind,
    range: Range,
    selectionRange: Range,
    children: ?[]const DocumentSymbol = null,
};

pub const SymbolKind = enum(u8) {
    File = 1,
    Module = 2,
    Namespace = 3,
    Package = 4,
    Class = 5,
    Method = 6,
    Property = 7,
    Field = 8,
    Constructor = 9,
    Enum = 10,
    Interface = 11,
    Function = 12,
    Variable = 13,
    Constant = 14,
    String = 15,
    Number = 16,
    Boolean = 17,
    Array = 18,
    Object = 19,
    Key = 20,
    Null = 21,
    EnumMember = 22,
    Struct = 23,
    Event = 24,
    Operator = 25,
    TypeParameter = 26,
};

// Semantic tokens
pub const SemanticTokensParams = struct {
    textDocument: TextDocumentIdentifier,
};

pub const SemanticTokens = struct {
    resultId: ?[]const u8 = null,
    data: []const u32,
};

pub const SemanticTokenType = enum {
    namespace,
    type,
    class,
    enum_,
    interface,
    struct_,
    typeParameter,
    parameter,
    variable,
    property,
    enumMember,
    event,
    function,
    method,
    macro,
    keyword,
    modifier,
    comment,
    string,
    number,
    regexp,
    operator,
};

pub const SemanticTokenModifier = enum {
    declaration,
    definition,
    readonly,
    static,
    deprecated,
    abstract,
    @"async",
    modification,
    documentation,
    defaultLibrary,
};