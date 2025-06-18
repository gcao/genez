const std = @import("std");
const ast = @import("../frontend/ast.zig");
const parser = @import("../frontend/parser.zig");
const types = @import("types.zig");
const debug = @import("debug.zig");
const module_resolver = @import("module_resolver.zig");
const pipeline = @import("../pipeline.zig");
const compiler = @import("../compiler.zig");

/// Represents a loaded module
pub const Module = struct {
    /// Module identifier (import path)
    id: []const u8,
    /// Absolute file path
    path: []const u8,
    /// Module properties (from ^module etc.)
    properties: std.StringHashMap(types.Value),
    /// Exported symbols
    exports: std.StringHashMap(types.Value),
    /// Module namespace (if any)
    namespace: ?[]const u8,
    /// Whether module is fully loaded
    is_loaded: bool,
    /// Compiled module result (if loaded)
    compiled: ?pipeline.CompiledResult,
    
    pub fn init(allocator: std.mem.Allocator, id: []const u8, path: []const u8) Module {
        return Module{
            .id = id,
            .path = path,
            .properties = std.StringHashMap(types.Value).init(allocator),
            .exports = std.StringHashMap(types.Value).init(allocator),
            .namespace = null,
            .is_loaded = false,
            .compiled = null,
        };
    }
    
    pub fn deinit(self: *Module, allocator: std.mem.Allocator) void {
        // Free properties
        var prop_it = self.properties.iterator();
        while (prop_it.next()) |entry| {
            allocator.free(entry.key_ptr.*);
            // Note: Value cleanup depends on implementation
        }
        self.properties.deinit();
        
        // Free exports
        var export_it = self.exports.iterator();
        while (export_it.next()) |entry| {
            allocator.free(entry.key_ptr.*);
        }
        self.exports.deinit();
        
        if (self.namespace) |ns| {
            allocator.free(ns);
        }
        
        if (self.compiled) |*comp| {
            comp.deinit();
        }
        
        allocator.free(self.id);
        allocator.free(self.path);
    }
};

/// Module system manages all loaded modules
pub const ModuleSystem = struct {
    allocator: std.mem.Allocator,
    /// Module resolver for finding modules
    resolver: module_resolver.ModuleResolver,
    /// All loaded modules by ID
    modules: std.StringHashMap(*Module),
    /// Current module being executed (for relative imports)
    current_module: ?*Module,
    /// Compiler options
    compiler_options: compiler.CompilerOptions,
    
    pub fn init(allocator: std.mem.Allocator, compiler_options: compiler.CompilerOptions) !ModuleSystem {
        return ModuleSystem{
            .allocator = allocator,
            .resolver = try module_resolver.ModuleResolver.init(allocator),
            .modules = std.StringHashMap(*Module).init(allocator),
            .current_module = null,
            .compiler_options = compiler_options,
        };
    }
    
    pub fn deinit(self: *ModuleSystem) void {
        // Free all modules
        var it = self.modules.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.*.deinit(self.allocator);
            self.allocator.destroy(entry.value_ptr.*);
        }
        self.modules.deinit();
        
        self.resolver.deinit();
    }
    
    /// Import a module and return it
    pub fn import(self: *ModuleSystem, import_path: []const u8, current_file: ?[]const u8) !*Module {
        debug.log("Importing module: {s}", .{import_path});
        
        // Resolve the module path
        const resolved = try self.resolver.resolve(import_path, current_file);
        defer resolved.deinit(self.allocator);
        
        // Check if already loaded
        if (self.modules.get(resolved.module_id)) |module| {
            debug.log("Module already loaded: {s}", .{resolved.module_id});
            return module;
        }
        
        // Check if already being loaded (circular import)
        for (self.resolver.loading_stack.items) |loading_id| {
            if (std.mem.eql(u8, loading_id, resolved.module_id)) {
                return error.CircularImport;
            }
        }
        
        // Load the module
        return try self.loadModule(resolved);
    }
    
    /// Load a module from disk
    fn loadModule(self: *ModuleSystem, resolved: module_resolver.ResolvedModule) !*Module {
        debug.log("Loading module from: {s}", .{resolved.absolute_path});
        
        // Mark as loading
        try self.resolver.beginLoading(resolved.module_id);
        defer self.resolver.endLoading(resolved.module_id);
        
        // Create module
        const module = try self.allocator.create(Module);
        errdefer self.allocator.destroy(module);
        
        module.* = Module.init(
            self.allocator,
            try self.allocator.dupe(u8, resolved.module_id),
            try self.allocator.dupe(u8, resolved.absolute_path),
        );
        errdefer module.deinit(self.allocator);
        
        // Add to modules map
        try self.modules.put(module.id, module);
        
        // Read and parse the file
        const file = try std.fs.cwd().openFile(resolved.absolute_path, .{});
        defer file.close();
        
        const source = try file.readToEndAlloc(self.allocator, std.math.maxInt(usize));
        defer self.allocator.free(source);
        
        // Parse the module
        const parse_result = try parser.parseGeneSourceWithFilename(self.allocator, source, resolved.absolute_path);
        defer {
            parse_result.arena.deinit();
            self.allocator.destroy(parse_result.arena);
        }
        
        // Extract module properties and expressions
        try self.processModuleAST(module, parse_result.nodes);
        
        // Compile the module
        const old_current = self.current_module;
        self.current_module = module;
        defer self.current_module = old_current;
        
        // TODO: Actually compile the module
        // For now, just mark as loaded
        module.is_loaded = true;
        
        debug.log("Module loaded successfully: {s}", .{module.id});
        return module;
    }
    
    /// Process module AST to extract properties and prepare for compilation
    fn processModuleAST(self: *ModuleSystem, module: *Module, nodes: []ast.AstNode) !void {
        if (nodes.len == 0) return;
        
        // Check if first node is a properties map
        var start_index: usize = 0;
        if (nodes[0] == .Expression and nodes[0].Expression == .MapLiteral) {
            const map = nodes[0].Expression.MapLiteral;
            
            // Extract properties
            for (map.entries) |entry| {
                const key_expr = entry.key.*;
                const key_str = switch (key_expr) {
                    .Literal => |lit| switch (lit.value) {
                        .String => |s| s,
                        else => continue,
                    },
                    else => continue,
                };
                
                // Store property
                // For now, just track that we have this property
                // TODO: Convert AST expression to runtime value
                const key_copy = try self.allocator.dupe(u8, key_str);
                try module.properties.put(key_copy, .Nil);
                
                // Special handling for module metadata
                if (std.mem.eql(u8, key_str, "module")) {
                    if (entry.value.* == .Literal and entry.value.Literal.value == .String) {
                        module.namespace = try self.allocator.dupe(u8, entry.value.Literal.value.String);
                    }
                } else if (std.mem.eql(u8, key_str, "exports")) {
                    // TODO: Process exports list
                }
            }
            
            start_index = 1;
        }
        
        // Process remaining nodes (module body)
        // TODO: Actually compile these expressions
        // For now, we just acknowledge they exist
        _ = start_index;
    }
    
    /// Get an exported value from a module
    pub fn getExport(self: *ModuleSystem, module: *Module, name: []const u8) ?types.Value {
        return module.exports.get(name);
    }
    
    /// Add an export to the current module
    pub fn addExport(self: *ModuleSystem, name: []const u8, value: types.Value) !void {
        if (self.current_module) |module| {
            const name_copy = try self.allocator.dupe(u8, name);
            try module.exports.put(name_copy, value);
        }
    }
};