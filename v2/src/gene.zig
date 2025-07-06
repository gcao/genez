const std = @import("std");
const value_mod = @import("value.zig");
const Value = value_mod.Value;
const ObjectHeader = value_mod.ObjectHeader;
const TypeTag = value_mod.TypeTag;

/// The fundamental Gene type that represents all values in the unified format
/// Format: (head ^prop1 value1 ^prop2 value2 child1 child2)
pub const Gene = struct {
    /// Object header for GC and type information
    header: ObjectHeader,
    
    /// The head/type of this Gene (e.g., Int, String, Call, etc.)
    head: *Value,
    
    /// Properties attached to this Gene
    props: PropertyStorage,
    
    /// Children/arguments of this Gene
    children: ChildrenStorage,

    /// Create a new Gene with just a head
    pub fn init(allocator: std.mem.Allocator, head: *Value) !*Gene {
        const gene = try allocator.create(Gene);
        gene.* = .{
            .header = .{
                .gc_bits = 0,
                .type_tag = .Gene,
                .size = @sizeOf(Gene),
            },
            .head = head,
            .props = .None,
            .children = .None,
        };
        return gene;
    }

    /// Create a Gene with properties and children
    pub fn initFull(
        allocator: std.mem.Allocator,
        head: *Value,
        props: []const Property,
        children: []const *Value,
    ) !*Gene {
        const gene = try allocator.create(Gene);
        
        // Initialize property storage with optimization for small counts
        const prop_storage = try PropertyStorage.init(allocator, props);
        const child_storage = try ChildrenStorage.init(allocator, children);

        gene.* = .{
            .header = .{
                .gc_bits = 0,
                .type_tag = .Gene,
                .size = @sizeOf(Gene),
            },
            .head = head,
            .props = prop_storage,
            .children = child_storage,
        };
        return gene;
    }

    /// Get a property value by key (O(1) for small, O(1) amortized for large)
    pub fn getProp(self: *const Gene, key: []const u8) ?*Value {
        return self.props.get(key);
    }

    /// Set a property (returns new Gene for immutability by default)
    pub fn setProp(self: *Gene, allocator: std.mem.Allocator, key: []const u8, value: *Value) !*Gene {
        // For v2, we'll start with immutable semantics
        const props = try self.props.toArray(allocator);
        defer allocator.free(props);
        
        // Check if property exists
        var new_props = std.ArrayList(Property).init(allocator);
        defer new_props.deinit();
        
        var found = false;
        for (props) |prop| {
            if (std.mem.eql(u8, prop.key, key)) {
                try new_props.append(.{ .key = key, .value = value });
                found = true;
            } else {
                try new_props.append(prop);
            }
        }
        
        if (!found) {
            try new_props.append(.{ .key = key, .value = value });
        }
        
        return Gene.initFull(allocator, self.head, new_props.items, try self.children.toArray(allocator));
    }

    /// Get a child by index (O(1))
    pub fn getChild(self: *const Gene, index: usize) ?*Value {
        return self.children.get(index);
    }

    /// Get the number of children
    pub fn childCount(self: *const Gene) usize {
        return self.children.count();
    }

    /// Add a child (returns new Gene for immutability)
    pub fn addChild(self: *Gene, allocator: std.mem.Allocator, child: *Value) !*Gene {
        const children = try self.children.toArray(allocator);
        defer allocator.free(children);
        
        var new_children = std.ArrayList(*Value).init(allocator);
        defer new_children.deinit();
        
        try new_children.appendSlice(children);
        try new_children.append(child);
        
        return Gene.initFull(allocator, self.head, try self.props.toArray(allocator), new_children.items);
    }

    /// Check if this Gene matches a pattern (for pattern matching)
    pub fn matches(self: *const Gene, pattern: *const Gene) bool {
        // Check head match
        if (!self.head.equals(pattern.head)) return false;
        
        // Check properties match
        // TODO: Implement property pattern matching
        
        // Check children match
        if (self.childCount() != pattern.childCount()) return false;
        
        return true;
    }

    /// Convert to human-readable string
    pub fn toString(self: *const Gene, allocator: std.mem.Allocator) ![]u8 {
        var buffer = std.ArrayList(u8).init(allocator);
        errdefer buffer.deinit();

        try buffer.append('(');
        
        // Write head
        const head_str = try self.head.toString(allocator);
        defer allocator.free(head_str);
        try buffer.appendSlice(head_str);

        // Write properties
        const props = try self.props.toArray(allocator);
        defer allocator.free(props);
        
        for (props) |prop| {
            try buffer.appendSlice(" ^");
            try buffer.appendSlice(prop.key);
            try buffer.append(' ');
            const val_str = try prop.value.toString(allocator);
            defer allocator.free(val_str);
            try buffer.appendSlice(val_str);
        }

        // Write children
        const children = try self.children.toArray(allocator);
        defer allocator.free(children);
        
        for (children) |child| {
            try buffer.append(' ');
            const child_str = try child.toString(allocator);
            defer allocator.free(child_str);
            try buffer.appendSlice(child_str);
        }

        try buffer.append(')');
        return buffer.toOwnedSlice();
    }
};

/// Property key-value pair
pub const Property = struct {
    key: []const u8,
    value: *Value,
};

/// Optimized storage for properties
pub const PropertyStorage = union(enum) {
    /// No properties
    None,
    /// Single property (common case)
    Single: Property,
    /// Few properties (optimize for 2-4)
    Few: struct {
        items: [4]?Property,
        count: u8,
    },
    /// Many properties (hash map)
    Many: std.StringHashMap(*Value),

    pub fn init(allocator: std.mem.Allocator, props: []const Property) !PropertyStorage {
        if (props.len == 0) {
            return .None;
        } else if (props.len == 1) {
            return PropertyStorage{ .Single = props[0] };
        } else if (props.len <= 4) {
            var few = PropertyStorage{ .Few = .{
                .items = .{null} ** 4,
                .count = @intCast(props.len),
            }};
            for (props, 0..) |prop, i| {
                few.Few.items[i] = prop;
            }
            return few;
        } else {
            var map = std.StringHashMap(*Value).init(allocator);
            for (props) |prop| {
                try map.put(prop.key, prop.value);
            }
            return PropertyStorage{ .Many = map };
        }
    }

    pub fn get(self: *const PropertyStorage, key: []const u8) ?*Value {
        switch (self.*) {
            .None => return null,
            .Single => |prop| {
                if (std.mem.eql(u8, prop.key, key)) return prop.value;
                return null;
            },
            .Few => |few| {
                for (few.items[0..few.count]) |maybe_prop| {
                    if (maybe_prop) |prop| {
                        if (std.mem.eql(u8, prop.key, key)) return prop.value;
                    }
                }
                return null;
            },
            .Many => |map| return map.get(key),
        }
    }

    pub fn toArray(self: *const PropertyStorage, allocator: std.mem.Allocator) ![]Property {
        switch (self.*) {
            .None => return allocator.alloc(Property, 0),
            .Single => |prop| {
                const arr = try allocator.alloc(Property, 1);
                arr[0] = prop;
                return arr;
            },
            .Few => |few| {
                const arr = try allocator.alloc(Property, few.count);
                for (few.items[0..few.count], 0..) |maybe_prop, i| {
                    if (maybe_prop) |prop| arr[i] = prop;
                }
                return arr;
            },
            .Many => |map| {
                const arr = try allocator.alloc(Property, map.count());
                var iter = map.iterator();
                var i: usize = 0;
                while (iter.next()) |entry| : (i += 1) {
                    arr[i] = .{ .key = entry.key_ptr.*, .value = entry.value_ptr.* };
                }
                return arr;
            },
        }
    }
};

/// Optimized storage for children
pub const ChildrenStorage = union(enum) {
    /// No children
    None,
    /// Single child (common for unary operations)
    Single: *Value,
    /// Few children (optimize for 2-4)
    Few: struct {
        items: [4]*Value,
        count: u8,
    },
    /// Many children (dynamic array)
    Many: []*Value,

    pub fn init(allocator: std.mem.Allocator, children: []const *Value) !ChildrenStorage {
        if (children.len == 0) {
            return .None;
        } else if (children.len == 1) {
            return ChildrenStorage{ .Single = children[0] };
        } else if (children.len <= 4) {
            var few = ChildrenStorage{ .Few = .{
                .items = undefined,
                .count = @intCast(children.len),
            }};
            for (children, 0..) |child, i| {
                few.Few.items[i] = child;
            }
            return few;
        } else {
            const arr = try allocator.alloc(*Value, children.len);
            std.mem.copyForwards(*Value, arr, children);
            return ChildrenStorage{ .Many = arr };
        }
    }

    pub fn get(self: *const ChildrenStorage, index: usize) ?*Value {
        switch (self.*) {
            .None => return null,
            .Single => |child| if (index == 0) return child else return null,
            .Few => |few| {
                if (index < few.count) return few.items[index];
                return null;
            },
            .Many => |children| {
                if (index < children.len) return children[index];
                return null;
            },
        }
    }

    pub fn count(self: *const ChildrenStorage) usize {
        return switch (self.*) {
            .None => 0,
            .Single => 1,
            .Few => |few| few.count,
            .Many => |children| children.len,
        };
    }

    pub fn toArray(self: *const ChildrenStorage, allocator: std.mem.Allocator) ![]*Value {
        switch (self.*) {
            .None => return allocator.alloc(*Value, 0),
            .Single => |child| {
                const arr = try allocator.alloc(*Value, 1);
                arr[0] = child;
                return arr;
            },
            .Few => |few| {
                const arr = try allocator.alloc(*Value, few.count);
                std.mem.copyForwards(*Value, arr, few.items[0..few.count]);
                return arr;
            },
            .Many => |children| {
                const arr = try allocator.alloc(*Value, children.len);
                std.mem.copyForwards(*Value, arr, children);
                return arr;
            },
        }
    }
};
