const std = @import("std");
const types = @import("types.zig");
const debug = @import("debug.zig");

/// Garbage collector implementation for Gene
/// Uses a simple mark-and-sweep algorithm as the initial implementation
pub const GC = struct {
    allocator: std.mem.Allocator,
    
    /// All allocated objects that need GC
    objects: std.ArrayList(*GCObject),
    
    /// GC statistics
    stats: GCStats,
    
    /// GC threshold - collect when allocated bytes exceed this
    threshold: usize,
    
    /// Current allocated bytes
    allocated_bytes: usize,
    
    /// GC enabled flag
    enabled: bool,
    
    /// Root set for marking
    roots: std.ArrayList(*types.Value),
    
    pub const GCStats = struct {
        collections: u64 = 0,
        total_allocated: u64 = 0,
        total_freed: u64 = 0,
        last_collection_freed: u64 = 0,
        last_collection_time_ms: u64 = 0,
    };
    
    /// Object header for GC tracking
    pub const GCObject = struct {
        /// The actual value
        value: types.Value,
        
        /// GC mark bit
        marked: bool = false,
        
        /// Size of this object in bytes
        size: usize,
        
        /// Next object in the linked list
        next: ?*GCObject = null,
    };
    
    pub fn init(allocator: std.mem.Allocator) !*GC {
        const gc = try allocator.create(GC);
        gc.* = GC{
            .allocator = allocator,
            .objects = std.ArrayList(*GCObject).init(allocator),
            .stats = GCStats{},
            .threshold = 1024 * 1024, // 1MB initial threshold
            .allocated_bytes = 0,
            .enabled = true,
            .roots = std.ArrayList(*types.Value).init(allocator),
        };
        return gc;
    }
    
    pub fn deinit(self: *GC) void {
        // Free all remaining objects
        for (self.objects.items) |obj| {
            obj.value.deinit(self.allocator);
            self.allocator.destroy(obj);
        }
        self.objects.deinit();
        self.roots.deinit();
        self.allocator.destroy(self);
    }
    
    /// Allocate a new GC-managed value
    pub fn allocValue(self: *GC, value: types.Value) !*types.Value {
        // Calculate size of the value
        const size = self.sizeOfValue(value);
        
        // Check if we need to collect
        if (self.enabled and self.allocated_bytes + size > self.threshold) {
            try self.collect();
        }
        
        // Create GC object
        const obj = try self.allocator.create(GCObject);
        obj.* = GCObject{
            .value = value,
            .marked = false,
            .size = size,
            .next = null,
        };
        
        try self.objects.append(obj);
        self.allocated_bytes += size;
        self.stats.total_allocated += size;
        
        return &obj.value;
    }
    
    /// Add a root for GC marking
    pub fn addRoot(self: *GC, root: *types.Value) !void {
        try self.roots.append(root);
    }
    
    /// Remove a root
    pub fn removeRoot(self: *GC, root: *types.Value) void {
        for (self.roots.items, 0..) |r, i| {
            if (r == root) {
                _ = self.roots.swapRemove(i);
                break;
            }
        }
    }
    
    /// Perform garbage collection
    pub fn collect(self: *GC) !void {
        if (!self.enabled) return;
        
        const start_time = std.time.milliTimestamp();
        debug.log("GC: Starting collection, allocated={} threshold={}", .{ self.allocated_bytes, self.threshold });
        
        // Mark phase
        self.markRoots();
        
        // Sweep phase
        const freed = self.sweep();
        
        // Update statistics
        self.stats.collections += 1;
        self.stats.last_collection_freed = freed;
        self.stats.last_collection_time_ms = @intCast(std.time.milliTimestamp() - start_time);
        self.stats.total_freed += freed;
        
        // Adjust threshold based on collection results
        if (freed < self.allocated_bytes / 4) {
            // If we freed less than 25%, increase threshold
            self.threshold = self.threshold * 2;
        }
        
        debug.log("GC: Completed collection, freed={} bytes in {} ms", .{ freed, self.stats.last_collection_time_ms });
    }
    
    /// Mark all reachable objects starting from roots
    fn markRoots(self: *GC) void {
        // Clear all marks
        for (self.objects.items) |obj| {
            obj.marked = false;
        }
        
        // Mark from roots
        for (self.roots.items) |root| {
            self.markValue(root);
        }
    }
    
    /// Mark a value and all values it references
    fn markValue(self: *GC, value: *types.Value) void {
        // Find the GCObject for this value
        const obj = self.findObject(value) orelse return;
        
        // Already marked?
        if (obj.marked) return;
        
        // Mark it
        obj.marked = true;
        
        // Mark referenced values
        switch (value.*) {
            .Array => |arr| {
                for (arr) |*elem| {
                    self.markValue(elem);
                }
            },
            .Map => |*map| {
                var it = map.iterator();
                while (it.next()) |entry| {
                    self.markValue(entry.value_ptr);
                }
            },
            .Object => {
                // Object is just an ID - we need the VM's object pool to traverse it
                // For now, skip object traversal
                // TODO: Pass VM reference to GC for object pool access
            },
            .Function => {
                // Functions are typically shared and managed separately
                // Don't traverse into them
            },
            else => {
                // Primitive values don't reference other values
            },
        }
    }
    
    /// Find the GCObject for a given value pointer
    fn findObject(self: *GC, value: *types.Value) ?*GCObject {
        for (self.objects.items) |obj| {
            if (&obj.value == value) {
                return obj;
            }
        }
        return null;
    }
    
    /// Sweep unmarked objects
    fn sweep(self: *GC) u64 {
        var freed: u64 = 0;
        var i: usize = 0;
        
        while (i < self.objects.items.len) {
            const obj = self.objects.items[i];
            if (!obj.marked) {
                // Free this object
                freed += obj.size;
                self.allocated_bytes -= obj.size;
                
                obj.value.deinit(self.allocator);
                self.allocator.destroy(obj);
                
                _ = self.objects.swapRemove(i);
                // Don't increment i since we removed an element
            } else {
                i += 1;
            }
        }
        
        return freed;
    }
    
    /// Calculate the size of a value in bytes
    fn sizeOfValue(self: *GC, value: types.Value) usize {
        _ = self;
        return switch (value) {
            .String => |s| @sizeOf(types.Value) + s.len,
            .Symbol => |s| @sizeOf(types.Value) + s.len,
            .Array => |a| @sizeOf(types.Value) + @sizeOf(types.Value) * a.len,
            .Map => |m| @sizeOf(types.Value) + @sizeOf(types.Value) * m.count() * 2,
            .Object => @sizeOf(types.Value) + @sizeOf(types.ClassDefinition),
            else => @sizeOf(types.Value),
        };
    }
    
    /// Disable GC temporarily
    pub fn disable(self: *GC) void {
        self.enabled = false;
    }
    
    /// Enable GC
    pub fn enable(self: *GC) void {
        self.enabled = true;
    }
    
    /// Get GC statistics
    pub fn getStats(self: *const GC) GCStats {
        return self.stats;
    }
};

/// Scoped GC disable - automatically re-enables GC when scope exits
pub const GCScope = struct {
    gc: *GC,
    was_enabled: bool,
    
    pub fn init(gc: *GC) GCScope {
        const was_enabled = gc.enabled;
        gc.disable();
        return GCScope{ .gc = gc, .was_enabled = was_enabled };
    }
    
    pub fn deinit(self: *GCScope) void {
        if (self.was_enabled) {
            self.gc.enable();
        }
    }
};