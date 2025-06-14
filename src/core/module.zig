const std = @import("std");
const types = @import("types.zig");
const ast = @import("../frontend/ast.zig");
const debug = @import("debug.zig");

/// Represents a Gene module
pub const Module = struct {
    /// Module name (e.g., "math/utils")
    name: []const u8,
    
    /// Source file path (if loaded from file)
    file_path: ?[]const u8,
    
    /// Module's namespace
    namespace: *Namespace,
    
    /// Imported modules
    imports: std.StringHashMap(*Module),
    
    /// Import aliases (import name -> local name)
    import_aliases: std.StringHashMap([]const u8),
    
    /// Allocator for this module
    allocator: std.mem.Allocator,
    
    /// Whether this module has been loaded/executed
    loaded: bool,
    
    /// Module's top-level AST nodes (for lazy evaluation)
    ast_nodes: ?[]ast.AstNode,
    
    pub fn init(allocator: std.mem.Allocator, name: []const u8, file_path: ?[]const u8) !*Module {
        const module = try allocator.create(Module);
        const namespace = try Namespace.init(allocator, name, null);
        
        module.* = Module{
            .name = try allocator.dupe(u8, name),
            .file_path = if (file_path) |fp| try allocator.dupe(u8, fp) else null,
            .namespace = namespace,
            .imports = std.StringHashMap(*Module).init(allocator),
            .import_aliases = std.StringHashMap([]const u8).init(allocator),
            .allocator = allocator,
            .loaded = false,
            .ast_nodes = null,
        };
        
        return module;
    }
    
    pub fn deinit(self: *Module) void {
        self.allocator.free(self.name);
        if (self.file_path) |fp| {
            self.allocator.free(fp);
        }
        
        self.imports.deinit();
        
        var alias_iter = self.import_aliases.iterator();
        while (alias_iter.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            self.allocator.free(entry.value_ptr.*);
        }
        self.import_aliases.deinit();
        
        self.namespace.deinit();
        
        if (self.ast_nodes) |nodes| {
            for (nodes) |*node| {
                node.deinit(self.allocator);
            }
            self.allocator.free(nodes);
        }
        
        self.allocator.destroy(self);
    }
    
    /// Import another module
    pub fn importModule(self: *Module, other: *Module, alias: ?[]const u8) !void {
        const import_name = if (alias) |a| try self.allocator.dupe(u8, a) else try self.allocator.dupe(u8, other.name);
        try self.imports.put(import_name, other);
    }
    
    /// Import specific symbols from another module
    pub fn importSymbols(self: *Module, other: *Module, symbols: []const ImportSpec) !void {
        for (symbols) |spec| {
            // Check if symbol exists in other module
            if (other.namespace.lookup(spec.name)) |value| {
                const local_name = spec.alias orelse spec.name;
                
                // Check for conflicts
                if (self.namespace.bindings.contains(local_name)) {
                    return error.NameAlreadyDefined;
                }
                
                // Add to our namespace
                try self.namespace.define(local_name, value);
                
                // Track import alias if different
                if (spec.alias) |alias| {
                    try self.import_aliases.put(
                        try self.allocator.dupe(u8, alias),
                        try self.allocator.dupe(u8, spec.name)
                    );
                }
            } else {
                return error.SymbolNotFound;
            }
        }
    }
};

/// Import specification for a single symbol
pub const ImportSpec = struct {
    name: []const u8,
    alias: ?[]const u8 = null,
};

/// Represents a namespace (can be nested)
pub const Namespace = struct {
    /// Namespace name
    name: []const u8,
    
    /// Parent namespace (null for top-level)
    parent: ?*Namespace,
    
    /// Bindings in this namespace
    bindings: std.StringHashMap(types.Value),
    
    /// Child namespaces
    children: std.StringHashMap(*Namespace),
    
    /// Allocator
    allocator: std.mem.Allocator,
    
    pub fn init(allocator: std.mem.Allocator, name: []const u8, parent: ?*Namespace) !*Namespace {
        const ns = try allocator.create(Namespace);
        ns.* = Namespace{
            .name = try allocator.dupe(u8, name),
            .parent = parent,
            .bindings = std.StringHashMap(types.Value).init(allocator),
            .children = std.StringHashMap(*Namespace).init(allocator),
            .allocator = allocator,
        };
        return ns;
    }
    
    pub fn deinit(self: *Namespace) void {
        self.allocator.free(self.name);
        
        // Clean up bindings
        var binding_iter = self.bindings.iterator();
        while (binding_iter.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            entry.value_ptr.deinit(self.allocator);
        }
        self.bindings.deinit();
        
        // Clean up child namespaces
        var child_iter = self.children.iterator();
        while (child_iter.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            entry.value_ptr.*.deinit();
        }
        self.children.deinit();
        
        self.allocator.destroy(self);
    }
    
    /// Define a binding in this namespace
    pub fn define(self: *Namespace, name: []const u8, value: types.Value) !void {
        const name_copy = try self.allocator.dupe(u8, name);
        try self.bindings.put(name_copy, value);
    }
    
    /// Look up a name in this namespace (and parents)
    pub fn lookup(self: *const Namespace, name: []const u8) ?types.Value {
        // Check this namespace first
        if (self.bindings.get(name)) |value| {
            return value;
        }
        
        // Check parent namespace
        if (self.parent) |parent| {
            return parent.lookup(name);
        }
        
        return null;
    }
    
    /// Look up a qualified name (e.g., "geometry/Point")
    pub fn lookupQualified(self: *const Namespace, qualified_name: []const u8) ?types.Value {
        // Split on first /
        if (std.mem.indexOf(u8, qualified_name, "/")) |slash_pos| {
            const namespace_part = qualified_name[0..slash_pos];
            const rest = qualified_name[slash_pos + 1..];
            
            // Look for child namespace
            if (self.children.get(namespace_part)) |child_ns| {
                return child_ns.lookupQualified(rest);
            }
            
            return null;
        } else {
            // No more slashes, look up in this namespace
            return self.lookup(qualified_name);
        }
    }
    
    /// Create or get a child namespace
    pub fn getOrCreateChild(self: *Namespace, name: []const u8) !*Namespace {
        if (self.children.get(name)) |child| {
            return child;
        }
        
        const child = try Namespace.init(self.allocator, name, self);
        const name_copy = try self.allocator.dupe(u8, name);
        try self.children.put(name_copy, child);
        return child;
    }
};

/// Module loader manages all loaded modules
pub const ModuleLoader = struct {
    /// All loaded modules
    modules: std.StringHashMap(*Module),
    
    /// Global namespace (available everywhere)
    global_namespace: *Namespace,
    
    /// Module search paths
    search_paths: std.ArrayList([]const u8),
    
    /// Allocator
    allocator: std.mem.Allocator,
    
    pub fn init(allocator: std.mem.Allocator) !*ModuleLoader {
        const loader = try allocator.create(ModuleLoader);
        const global_ns = try Namespace.init(allocator, "global", null);
        
        loader.* = ModuleLoader{
            .modules = std.StringHashMap(*Module).init(allocator),
            .global_namespace = global_ns,
            .search_paths = std.ArrayList([]const u8).init(allocator),
            .allocator = allocator,
        };
        
        // Add built-in functions to global namespace
        try loader.initializeGlobalNamespace();
        
        return loader;
    }
    
    pub fn deinit(self: *ModuleLoader) void {
        // Clean up modules
        var module_iter = self.modules.iterator();
        while (module_iter.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            entry.value_ptr.*.deinit();
        }
        self.modules.deinit();
        
        // Clean up search paths
        for (self.search_paths.items) |path| {
            self.allocator.free(path);
        }
        self.search_paths.deinit();
        
        self.global_namespace.deinit();
        self.allocator.destroy(self);
    }
    
    /// Initialize global namespace with built-ins
    fn initializeGlobalNamespace(self: *ModuleLoader) !void {
        // Add built-in operators and functions
        try self.global_namespace.define("print", .{ .BuiltinOperator = .Print });
        try self.global_namespace.define("+", .{ .BuiltinOperator = .Add });
        try self.global_namespace.define("-", .{ .BuiltinOperator = .Sub });
        try self.global_namespace.define("*", .{ .BuiltinOperator = .Mul });
        try self.global_namespace.define("/", .{ .BuiltinOperator = .Div });
        try self.global_namespace.define("<", .{ .BuiltinOperator = .LessThan });
        try self.global_namespace.define(">", .{ .BuiltinOperator = .GreaterThan });
        try self.global_namespace.define("==", .{ .BuiltinOperator = .Eq });
        // Add more built-ins as needed
    }
    
    /// Add a module search path
    pub fn addSearchPath(self: *ModuleLoader, path: []const u8) !void {
        const path_copy = try self.allocator.dupe(u8, path);
        try self.search_paths.append(path_copy);
    }
    
    /// Load a module by name
    pub fn loadModule(self: *ModuleLoader, name: []const u8) !*Module {
        // Check if already loaded
        if (self.modules.get(name)) |module| {
            return module;
        }
        
        // Try to find module file
        const file_path = try self.findModuleFile(name);
        
        // Create and register module
        const module = try Module.init(self.allocator, name, file_path);
        const name_copy = try self.allocator.dupe(u8, name);
        try self.modules.put(name_copy, module);
        
        // TODO: Parse and load module contents
        
        return module;
    }
    
    /// Find module file in search paths
    fn findModuleFile(self: *ModuleLoader, name: []const u8) !?[]const u8 {
        // Convert module name to file path (e.g., "math/utils" -> "math/utils.gene")
        const file_name = try std.fmt.allocPrint(self.allocator, "{s}.gene", .{name});
        defer self.allocator.free(file_name);
        
        // Check search paths
        for (self.search_paths.items) |search_path| {
            const full_path = try std.fs.path.join(self.allocator, &[_][]const u8{ search_path, file_name });
            defer self.allocator.free(full_path);
            
            // Check if file exists
            if (std.fs.cwd().access(full_path, .{})) |_| {
                return try self.allocator.dupe(u8, full_path);
            } else |_| {
                continue;
            }
        }
        
        // Check current directory
        if (std.fs.cwd().access(file_name, .{})) |_| {
            return try self.allocator.dupe(u8, file_name);
        } else |_| {}
        
        return null;
    }
    
    /// Create a module from source string
    pub fn createModuleFromSource(self: *ModuleLoader, name: []const u8, source: []const u8) !*Module {
        _ = source; // TODO: Use source to parse module
        
        const module = try Module.init(self.allocator, name, null);
        const name_copy = try self.allocator.dupe(u8, name);
        try self.modules.put(name_copy, module);
        
        return module;
    }
};