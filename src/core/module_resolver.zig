const std = @import("std");
const debug = @import("debug.zig");

/// Module resolution errors
pub const ModuleError = error{
    ModuleNotFound,
    CircularImport,
    InvalidModulePath,
    PackageNotFound,
    InvalidPackageFile,
    OutOfMemory,
};

/// Represents a resolved module path
pub const ResolvedModule = struct {
    /// Absolute path to the module file
    absolute_path: []const u8,
    /// Module identifier (for caching)
    module_id: []const u8,
    /// Whether this is a system module
    is_system: bool,
    
    pub fn deinit(self: *ResolvedModule, allocator: std.mem.Allocator) void {
        allocator.free(self.absolute_path);
        allocator.free(self.module_id);
    }
};

/// Module resolver handles finding and loading modules
pub const ModuleResolver = struct {
    allocator: std.mem.Allocator,
    /// Current working directory
    cwd: []const u8,
    /// Project root directory (where package.gene is)
    project_root: ?[]const u8,
    /// Source directories from package.gene
    src_dirs: std.ArrayList([]const u8),
    /// Cache of loaded modules to prevent circular imports
    loaded_modules: std.StringHashMap(void),
    /// Stack of modules being loaded (for circular detection)
    loading_stack: std.ArrayList([]const u8),
    
    pub fn init(allocator: std.mem.Allocator) !ModuleResolver {
        const cwd = try std.process.getCwdAlloc(allocator);
        errdefer allocator.free(cwd);
        
        const project_root = try findProjectRoot(allocator, cwd);
        
        var resolver = ModuleResolver{
            .allocator = allocator,
            .cwd = cwd,
            .project_root = project_root,
            .src_dirs = std.ArrayList([]const u8).init(allocator),
            .loaded_modules = std.StringHashMap(void).init(allocator),
            .loading_stack = std.ArrayList([]const u8).init(allocator),
        };
        
        // Load source directories from package.gene if it exists
        if (project_root) |root| {
            try resolver.loadPackageConfig(root);
        }
        
        // Always add current working directory as a source directory
        try resolver.src_dirs.append(try allocator.dupe(u8, cwd));
        
        return resolver;
    }
    
    pub fn deinit(self: *ModuleResolver) void {
        for (self.src_dirs.items) |dir| {
            self.allocator.free(dir);
        }
        self.src_dirs.deinit();
        
        var it = self.loaded_modules.iterator();
        while (it.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
        }
        self.loaded_modules.deinit();
        
        for (self.loading_stack.items) |path| {
            self.allocator.free(path);
        }
        self.loading_stack.deinit();
        
        if (self.project_root) |root| {
            self.allocator.free(root);
        }
        self.allocator.free(self.cwd);
    }
    
    /// Find project root by looking for package.gene
    fn findProjectRoot(allocator: std.mem.Allocator, start_dir: []const u8) !?[]const u8 {
        var current_dir = try allocator.dupe(u8, start_dir);
        defer allocator.free(current_dir);
        
        while (true) {
            const package_path = try std.fs.path.join(allocator, &[_][]const u8{ current_dir, "package.gene" });
            defer allocator.free(package_path);
            
            // Check if package.gene exists
            if (std.fs.cwd().access(package_path, .{})) |_| {
                return try allocator.dupe(u8, current_dir);
            } else |_| {}
            
            // Move up one directory
            const parent = std.fs.path.dirname(current_dir);
            if (parent == null or std.mem.eql(u8, parent.?, current_dir)) {
                // Reached root directory
                return null;
            }
            
            const new_dir = try allocator.dupe(u8, parent.?);
            allocator.free(current_dir);
            current_dir = new_dir;
        }
    }
    
    /// Load package configuration from package.gene
    fn loadPackageConfig(self: *ModuleResolver, project_root: []const u8) !void {
        const package_path = try std.fs.path.join(self.allocator, &[_][]const u8{ project_root, "package.gene" });
        defer self.allocator.free(package_path);
        
        // For now, just add default source directories
        // TODO: Actually parse package.gene and extract src-dirs
        const default_dirs = [_][]const u8{ "src", "lib" };
        for (default_dirs) |dir| {
            const full_path = try std.fs.path.join(self.allocator, &[_][]const u8{ project_root, dir });
            
            // Check if directory exists
            if (std.fs.cwd().access(full_path, .{})) |_| {
                try self.src_dirs.append(full_path);
            } else |_| {
                self.allocator.free(full_path);
            }
        }
    }
    
    /// Resolve a module import path to an absolute file path
    pub fn resolve(self: *ModuleResolver, import_path: []const u8, current_file: ?[]const u8) !ResolvedModule {
        debug.log("Resolving module: {s} from {?s}", .{ import_path, current_file });
        
        // Check if we're already loading this module (circular import)
        for (self.loading_stack.items) |loading_path| {
            if (std.mem.eql(u8, loading_path, import_path)) {
                return error.CircularImport;
            }
        }
        
        // Determine import type
        if (std.mem.startsWith(u8, import_path, "./") or std.mem.startsWith(u8, import_path, "../")) {
            // Relative import
            return try self.resolveRelative(import_path, current_file);
        } else if (std.mem.startsWith(u8, import_path, "/")) {
            // Absolute import
            return try self.resolveAbsolute(import_path);
        } else {
            // Package import
            return try self.resolvePackage(import_path);
        }
    }
    
    /// Resolve relative import path
    fn resolveRelative(self: *ModuleResolver, import_path: []const u8, current_file: ?[]const u8) !ResolvedModule {
        debug.log("resolveRelative: import_path={s}, current_file={?s}", .{import_path, current_file});
        if (current_file == null) {
            return error.InvalidModulePath;
        }
        
        const dir = std.fs.path.dirname(current_file.?) orelse ".";
        debug.log("resolveRelative: dir={s}", .{dir});
        
        // Try with .gene extension
        const with_ext = try std.fmt.allocPrint(self.allocator, "{s}.gene", .{import_path});
        defer self.allocator.free(with_ext);
        
        const full_path = try std.fs.path.resolve(self.allocator, &[_][]const u8{ dir, with_ext });
        errdefer self.allocator.free(full_path);
        
        // Check if file exists
        if (std.fs.cwd().access(full_path, .{})) |_| {
            const module_id = try self.allocator.dupe(u8, full_path);
            return ResolvedModule{
                .absolute_path = full_path,
                .module_id = module_id,
                .is_system = false,
            };
        } else |_| {}
        
        // Try without extension
        const full_path_no_ext = try std.fs.path.resolve(self.allocator, &[_][]const u8{ dir, import_path });
        defer self.allocator.free(full_path_no_ext);
        
        if (std.fs.cwd().access(full_path_no_ext, .{})) |_| {
            const module_id = try self.allocator.dupe(u8, full_path_no_ext);
            return ResolvedModule{
                .absolute_path = try self.allocator.dupe(u8, full_path_no_ext),
                .module_id = module_id,
                .is_system = false,
            };
        } else |_| {}
        
        return error.ModuleNotFound;
    }
    
    /// Resolve absolute import path
    fn resolveAbsolute(self: *ModuleResolver, import_path: []const u8) !ResolvedModule {
        // For now, treat absolute paths as relative to project root
        if (self.project_root) |root| {
            const relative_path = import_path[1..]; // Remove leading /
            
            // Try with .gene extension
            const with_ext = try std.fmt.allocPrint(self.allocator, "{s}.gene", .{relative_path});
            defer self.allocator.free(with_ext);
            
            const full_path = try std.fs.path.join(self.allocator, &[_][]const u8{ root, with_ext });
            errdefer self.allocator.free(full_path);
            
            if (std.fs.cwd().access(full_path, .{})) |_| {
                const module_id = try std.fmt.allocPrint(self.allocator, "/{s}", .{relative_path});
                return ResolvedModule{
                    .absolute_path = full_path,
                    .module_id = module_id,
                    .is_system = true,
                };
            } else |_| {
                self.allocator.free(full_path);
            }
        }
        
        return error.ModuleNotFound;
    }
    
    /// Resolve package import path
    fn resolvePackage(self: *ModuleResolver, import_path: []const u8) !ResolvedModule {
        debug.log("Resolving package import: {s}", .{import_path});
        debug.log("Source directories: {d}", .{self.src_dirs.items.len});
        
        // Search in source directories
        for (self.src_dirs.items) |src_dir| {
            debug.log("Checking source dir: {s}", .{src_dir});
            
            // Try with .gene extension
            const with_ext = try std.fmt.allocPrint(self.allocator, "{s}.gene", .{import_path});
            defer self.allocator.free(with_ext);
            
            const full_path = try std.fs.path.join(self.allocator, &[_][]const u8{ src_dir, with_ext });
            errdefer self.allocator.free(full_path);
            
            debug.log("Checking path: {s}", .{full_path});
            
            if (std.fs.cwd().access(full_path, .{})) |_| {
                debug.log("Found module at: {s}", .{full_path});
                const module_id = try self.allocator.dupe(u8, import_path);
                return ResolvedModule{
                    .absolute_path = full_path,
                    .module_id = module_id,
                    .is_system = false,
                };
            } else |_| {
                debug.log("Not found at: {s}", .{full_path});
                self.allocator.free(full_path);
            }
            
            // Try without extension (directory with index.gene)
            const index_path = try std.fs.path.join(self.allocator, &[_][]const u8{ src_dir, import_path, "index.gene" });
            errdefer self.allocator.free(index_path);
            
            if (std.fs.cwd().access(index_path, .{})) |_| {
                const module_id = try self.allocator.dupe(u8, import_path);
                return ResolvedModule{
                    .absolute_path = index_path,
                    .module_id = module_id,
                    .is_system = false,
                };
            } else |_| {
                self.allocator.free(index_path);
            }
        }
        
        return error.ModuleNotFound;
    }
    
    /// Mark a module as being loaded (for circular import detection)
    pub fn beginLoading(self: *ModuleResolver, module_id: []const u8) !void {
        const id_copy = try self.allocator.dupe(u8, module_id);
        try self.loading_stack.append(id_copy);
    }
    
    /// Mark a module as finished loading
    pub fn endLoading(self: *ModuleResolver, module_id: []const u8) void {
        // Remove from loading stack
        var i: usize = self.loading_stack.items.len;
        while (i > 0) {
            i -= 1;
            if (std.mem.eql(u8, self.loading_stack.items[i], module_id)) {
                const removed = self.loading_stack.orderedRemove(i);
                self.allocator.free(removed);
                break;
            }
        }
        
        // Add to loaded modules cache
        const id_copy = self.allocator.dupe(u8, module_id) catch return;
        self.loaded_modules.put(id_copy, {}) catch {
            self.allocator.free(id_copy);
        };
    }
    
    /// Check if a module has already been loaded
    pub fn isLoaded(self: *ModuleResolver, module_id: []const u8) bool {
        return self.loaded_modules.contains(module_id);
    }
};