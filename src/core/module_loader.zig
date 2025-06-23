const std = @import("std");
const debug = @import("debug.zig");

/// Module loader handles finding and loading module files
pub const ModuleLoader = struct {
    /// Module search paths
    search_paths: std.ArrayList([]const u8),
    
    /// Allocator
    allocator: std.mem.Allocator,
    
    pub fn init(allocator: std.mem.Allocator) !*ModuleLoader {
        const loader = try allocator.create(ModuleLoader);
        
        loader.* = ModuleLoader{
            .search_paths = std.ArrayList([]const u8).init(allocator),
            .allocator = allocator,
        };
        
        // Add default search paths
        try loader.search_paths.append(".");
        try loader.search_paths.append("./lib");
        try loader.search_paths.append("./src");
        
        return loader;
    }
    
    pub fn deinit(self: *ModuleLoader) void {
        for (self.search_paths.items) |path| {
            self.allocator.free(path);
        }
        self.search_paths.deinit();
        self.allocator.destroy(self);
    }
    
    /// Find a module file given an import path
    pub fn findModule(self: *ModuleLoader, import_path: []const u8, current_file: ?[]const u8) !?[]const u8 {
        // Handle relative imports
        if (std.mem.startsWith(u8, import_path, "./") or std.mem.startsWith(u8, import_path, "../")) {
            if (current_file) |cf| {
                const dir = std.fs.path.dirname(cf) orelse ".";
                const path = try std.fs.path.join(self.allocator, &[_][]const u8{ dir, import_path });
                
                // Try with .gene extension
                const gene_path = try std.fmt.allocPrint(self.allocator, "{s}.gene", .{path});
                defer self.allocator.free(gene_path);
                
                if (std.fs.cwd().access(gene_path, .{})) |_| {
                    return try self.allocator.dupe(u8, gene_path);
                } else |_| {}
                
                // Try without extension
                if (std.fs.cwd().access(path, .{})) |_| {
                    return path;
                } else |_| {
                    self.allocator.free(path);
                }
            }
        }
        
        // Handle absolute paths
        if (std.mem.startsWith(u8, import_path, "/")) {
            // Try with .gene extension
            const gene_path = try std.fmt.allocPrint(self.allocator, "{s}.gene", .{import_path});
            defer self.allocator.free(gene_path);
            
            if (std.fs.cwd().access(gene_path, .{})) |_| {
                return try self.allocator.dupe(u8, gene_path);
            } else |_| {}
            
            // Try without extension
            if (std.fs.cwd().access(import_path, .{})) |_| {
                return try self.allocator.dupe(u8, import_path);
            } else |_| {}
        }
        
        // Search in search paths
        for (self.search_paths.items) |search_path| {
            const path = try std.fs.path.join(self.allocator, &[_][]const u8{ search_path, import_path });
            
            // Try with .gene extension
            const gene_path = try std.fmt.allocPrint(self.allocator, "{s}.gene", .{path});
            defer self.allocator.free(gene_path);
            
            if (std.fs.cwd().access(gene_path, .{})) |_| {
                self.allocator.free(path);
                return try self.allocator.dupe(u8, gene_path);
            } else |_| {}
            
            // Try without extension
            if (std.fs.cwd().access(path, .{})) |_| {
                return path;
            } else |_| {
                self.allocator.free(path);
            }
        }
        
        return null;
    }
};