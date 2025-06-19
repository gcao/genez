const std = @import("std");
const types = @import("types.zig");
const bytecode = @import("../backend/bytecode.zig");
const debug = @import("debug.zig");

/// Represents a compiled module with its namespace
pub const CompiledModule = struct {
    /// Module identifier (e.g., "math", "utils/helpers")
    id: []const u8,
    
    /// Absolute path to the module file
    path: []const u8,
    
    /// Root namespace containing all module members
    namespace: *Namespace,
    
    /// Compiled bytecode functions
    functions: std.ArrayList(*bytecode.Function),
    
    /// Module-level initialization code (if any)
    init_function: ?*bytecode.Function,
    
    /// Whether the module has been initialized
    initialized: bool,
    
    /// Allocator for this module
    allocator: std.mem.Allocator,
    
    pub fn init(allocator: std.mem.Allocator, id: []const u8, path: []const u8) !*CompiledModule {
        const module = try allocator.create(CompiledModule);
        const namespace = try Namespace.init(allocator, id);
        
        module.* = CompiledModule{
            .id = try allocator.dupe(u8, id),
            .path = try allocator.dupe(u8, path),
            .namespace = namespace,
            .functions = std.ArrayList(*bytecode.Function).init(allocator),
            .init_function = null,
            .initialized = false,
            .allocator = allocator,
        };
        
        return module;
    }
    
    pub fn deinit(self: *CompiledModule) void {
        self.allocator.free(self.id);
        self.allocator.free(self.path);
        self.namespace.deinit();
        
        for (self.functions.items) |func| {
            func.deinit();
            self.allocator.destroy(func);
        }
        self.functions.deinit();
        
        if (self.init_function) |func| {
            func.deinit();
            self.allocator.destroy(func);
        }
        
        self.allocator.destroy(self);
    }
    
    /// Add a function to the module
    pub fn addFunction(self: *CompiledModule, func: *bytecode.Function) !void {
        try self.functions.append(func);
        
        // Also add to namespace for lookup
        const func_value = types.Value{ .Function = func };
        try self.namespace.define(func.name, func_value);
    }
    
    /// Get a member from the module's namespace
    pub fn getMember(self: *const CompiledModule, name: []const u8) ?types.Value {
        return self.namespace.lookup(name);
    }
};

/// Namespace holds module members
pub const Namespace = struct {
    /// Namespace name
    name: []const u8,
    
    /// Members in this namespace
    members: std.StringHashMap(types.Value),
    
    /// Allocator
    allocator: std.mem.Allocator,
    
    pub fn init(allocator: std.mem.Allocator, name: []const u8) !*Namespace {
        const ns = try allocator.create(Namespace);
        ns.* = Namespace{
            .name = try allocator.dupe(u8, name),
            .members = std.StringHashMap(types.Value).init(allocator),
            .allocator = allocator,
        };
        return ns;
    }
    
    pub fn deinit(self: *Namespace) void {
        self.allocator.free(self.name);
        
        var it = self.members.iterator();
        while (it.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            entry.value_ptr.deinit(self.allocator);
        }
        self.members.deinit();
        
        self.allocator.destroy(self);
    }
    
    /// Define a member in this namespace
    pub fn define(self: *Namespace, name: []const u8, value: types.Value) !void {
        const name_copy = try self.allocator.dupe(u8, name);
        errdefer self.allocator.free(name_copy);
        
        var value_copy = try value.clone(self.allocator);
        errdefer value_copy.deinit(self.allocator);
        
        try self.members.put(name_copy, value_copy);
    }
    
    /// Look up a member in this namespace
    pub fn lookup(self: *const Namespace, name: []const u8) ?types.Value {
        return self.members.get(name);
    }
};

/// Module registry manages all loaded modules
pub const ModuleRegistry = struct {
    /// All loaded modules by ID
    modules: std.StringHashMap(*CompiledModule),
    
    /// Allocator
    allocator: std.mem.Allocator,
    
    pub fn init(allocator: std.mem.Allocator) ModuleRegistry {
        return ModuleRegistry{
            .modules = std.StringHashMap(*CompiledModule).init(allocator),
            .allocator = allocator,
        };
    }
    
    pub fn deinit(self: *ModuleRegistry) void {
        var it = self.modules.iterator();
        while (it.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            entry.value_ptr.*.deinit();
        }
        self.modules.deinit();
    }
    
    /// Register a new module
    pub fn registerModule(self: *ModuleRegistry, module: *CompiledModule) !void {
        const id_copy = try self.allocator.dupe(u8, module.id);
        errdefer self.allocator.free(id_copy);
        
        try self.modules.put(id_copy, module);
        debug.log("Registered module: {s}", .{module.id});
    }
    
    /// Get a module by ID
    pub fn getModule(self: *const ModuleRegistry, id: []const u8) ?*CompiledModule {
        return self.modules.get(id);
    }
    
    /// Resolve a module member access (e.g., "math/square")
    pub fn resolveMember(self: *const ModuleRegistry, module_id: []const u8, member_name: []const u8) ?types.Value {
        if (self.getModule(module_id)) |module| {
            return module.getMember(member_name);
        }
        return null;
    }
};