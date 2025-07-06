const std = @import("std");
const types = @import("types.zig");

/// The fundamental Gene type that can represent any value in the unified format
/// Format: (head ^prop1 value1 ^prop2 value2 child1 child2)
pub const Gene = struct {
    head: types.Value,
    props: PropertyStorage,
    children: ChildrenStorage,
    allocator: std.mem.Allocator,

    /// Create a new Gene with the given head
    pub fn init(allocator: std.mem.Allocator, head: types.Value) !*Gene {
        const gene = try allocator.create(Gene);
        gene.* = .{
            .head = head,
            .props = .None,
            .children = .None,
            .allocator = allocator,
        };
        return gene;
    }

    /// Create a Gene with properties and children
    pub fn initFull(
        allocator: std.mem.Allocator,
        head: types.Value,
        props: []const Property,
        children: []const types.Value,
    ) !*Gene {
        const gene = try allocator.create(Gene);
        
        // Initialize property storage
        const prop_storage = if (props.len == 0)
            PropertyStorage.None
        else if (props.len == 1)
            PropertyStorage{ .Single = .{
                .key = try allocator.dupe(u8, props[0].key),
                .value = try props[0].value.clone(allocator),
            }}
        else if (props.len <= 4) blk: {
            var few: [4]?KeyValue = .{null} ** 4;
            for (props, 0..) |prop, i| {
                few[i] = .{
                    .key = try allocator.dupe(u8, prop.key),
                    .value = try prop.value.clone(allocator),
                };
            }
            break :blk PropertyStorage{ .Few = few };
        } else blk: {
            var map = std.StringHashMap(types.Value).init(allocator);
            for (props) |prop| {
                try map.put(try allocator.dupe(u8, prop.key), try prop.value.clone(allocator));
            }
            break :blk PropertyStorage{ .Many = map };
        };

        // Initialize children storage
        const child_storage = if (children.len == 0)
            ChildrenStorage.None
        else if (children.len == 1)
            ChildrenStorage{ .Single = try children[0].clone(allocator) }
        else if (children.len <= 4) blk: {
            var few: [4]types.Value = undefined;
            for (children, 0..) |child, i| {
                few[i] = try child.clone(allocator);
            }
            break :blk ChildrenStorage{ .Few = .{
                .items = few,
                .count = @intCast(children.len),
            }};
        } else blk: {
            var arr = try allocator.alloc(types.Value, children.len);
            for (children, 0..) |child, i| {
                arr[i] = try child.clone(allocator);
            }
            break :blk ChildrenStorage{ .Many = arr };
        };

        gene.* = .{
            .head = try head.clone(allocator),
            .props = prop_storage,
            .children = child_storage,
            .allocator = allocator,
        };
        return gene;
    }

    pub fn deinit(self: *Gene) void {
        // Clean up head
        self.head.deinit(self.allocator);

        // Clean up properties
        switch (self.props) {
            .None => {},
            .Single => |kv| {
                self.allocator.free(kv.key);
                var value = kv.value;
                value.deinit(self.allocator);
            },
            .Few => |kvs| {
                for (kvs) |maybe_kv| {
                    if (maybe_kv) |kv| {
                        self.allocator.free(kv.key);
                        var value = kv.value;
                        value.deinit(self.allocator);
                    }
                }
            },
            .Many => |map| {
                var iter = map.iterator();
                while (iter.next()) |entry| {
                    self.allocator.free(entry.key_ptr.*);
                    entry.value_ptr.deinit(self.allocator);
                }
                map.deinit();
            },
        }

        // Clean up children
        switch (self.children) {
            .None => {},
            .Single => |*child| child.deinit(self.allocator),
            .Few => |few| {
                for (few.items[0..few.count]) |*child| {
                    child.deinit(self.allocator);
                }
            },
            .Many => |children| {
                for (children) |*child| {
                    child.deinit(self.allocator);
                }
                self.allocator.free(children);
            },
        }

        self.allocator.destroy(self);
    }

    /// Get a property value by key
    pub fn getProp(self: *const Gene, key: []const u8) ?types.Value {
        switch (self.props) {
            .None => return null,
            .Single => |kv| {
                if (std.mem.eql(u8, kv.key, key)) return kv.value;
                return null;
            },
            .Few => |kvs| {
                for (kvs) |maybe_kv| {
                    if (maybe_kv) |kv| {
                        if (std.mem.eql(u8, kv.key, key)) return kv.value;
                    }
                }
                return null;
            },
            .Many => |map| {
                return map.get(key);
            },
        }
    }

    /// Set a property value (returns new Gene for immutability)
    pub fn setProp(self: *const Gene, key: []const u8, value: types.Value) !*Gene {
        // For now, implement mutable update
        // TODO: Implement immutable update with structural sharing
        
        const existing = self.getProp(key);
        if (existing != null) {
            // Update existing property
            switch (self.props) {
                .Single => |*kv| {
                    if (std.mem.eql(u8, kv.key, key)) {
                        kv.value.deinit(self.allocator);
                        kv.value = try value.clone(self.allocator);
                    }
                },
                .Few => |*kvs| {
                    for (kvs) |*maybe_kv| {
                        if (maybe_kv.*) |*kv| {
                            if (std.mem.eql(u8, kv.key, key)) {
                                kv.value.deinit(self.allocator);
                                kv.value = try value.clone(self.allocator);
                                break;
                            }
                        }
                    }
                },
                .Many => |*map| {
                    const entry = map.getPtr(key).?;
                    entry.deinit(self.allocator);
                    entry.* = try value.clone(self.allocator);
                },
                else => unreachable,
            }
        } else {
            // Add new property
            // This requires converting storage format if needed
            // TODO: Implement storage upgrade
            return error.NotImplemented;
        }
        
        // For mutable update, return self
        return @constCast(self);
    }

    /// Get a child by index
    pub fn getChild(self: *const Gene, index: usize) ?types.Value {
        switch (self.children) {
            .None => return null,
            .Single => |child| {
                if (index == 0) return child;
                return null;
            },
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

    /// Get the number of children
    pub fn childCount(self: *const Gene) usize {
        return switch (self.children) {
            .None => 0,
            .Single => 1,
            .Few => |few| few.count,
            .Many => |children| children.len,
        };
    }

    /// Add a child (returns new Gene for immutability)
    pub fn addChild(self: *const Gene, child: types.Value) !*Gene {
        // TODO: Implement immutable update with structural sharing
        return error.NotImplemented;
    }

    /// Convert to human-readable string
    pub fn toString(self: *const Gene, allocator: std.mem.Allocator) ![]u8 {
        var buffer = std.ArrayList(u8).init(allocator);
        defer buffer.deinit();

        try buffer.append('(');
        
        // Write head
        const head_str = try self.head.toString(allocator);
        defer allocator.free(head_str);
        try buffer.appendSlice(head_str);

        // Write properties
        switch (self.props) {
            .None => {},
            .Single => |kv| {
                try buffer.appendSlice(" ^");
                try buffer.appendSlice(kv.key);
                try buffer.append(' ');
                const val_str = try kv.value.toString(allocator);
                defer allocator.free(val_str);
                try buffer.appendSlice(val_str);
            },
            .Few => |kvs| {
                for (kvs) |maybe_kv| {
                    if (maybe_kv) |kv| {
                        try buffer.appendSlice(" ^");
                        try buffer.appendSlice(kv.key);
                        try buffer.append(' ');
                        const val_str = try kv.value.toString(allocator);
                        defer allocator.free(val_str);
                        try buffer.appendSlice(val_str);
                    }
                }
            },
            .Many => |map| {
                var iter = map.iterator();
                while (iter.next()) |entry| {
                    try buffer.appendSlice(" ^");
                    try buffer.appendSlice(entry.key_ptr.*);
                    try buffer.append(' ');
                    const val_str = try entry.value_ptr.toString(allocator);
                    defer allocator.free(val_str);
                    try buffer.appendSlice(val_str);
                }
            },
        }

        // Write children
        const count = self.childCount();
        var i: usize = 0;
        while (i < count) : (i += 1) {
            if (self.getChild(i)) |child| {
                try buffer.append(' ');
                const child_str = try child.toString(allocator);
                defer allocator.free(child_str);
                try buffer.appendSlice(child_str);
            }
        }

        try buffer.append(')');
        return buffer.toOwnedSlice();
    }
};

/// Property key-value pair
pub const Property = struct {
    key: []const u8,
    value: types.Value,
};

const KeyValue = struct {
    key: []const u8,
    value: types.Value,
};

/// Optimized storage for properties
pub const PropertyStorage = union(enum) {
    None,
    Single: KeyValue,
    Few: [4]?KeyValue,
    Many: std.StringHashMap(types.Value),
};

/// Optimized storage for children
pub const ChildrenStorage = union(enum) {
    None,
    Single: types.Value,
    Few: struct {
        items: [4]types.Value,
        count: u8,
    },
    Many: []types.Value,
};

// Tests
test "Gene creation" {
    const allocator = std.testing.allocator;

    // Simple gene with just a head
    const g1 = try Gene.init(allocator, .{ .Symbol = "div" });
    defer g1.deinit();

    try std.testing.expectEqual(g1.head, .{ .Symbol = "div" });
    try std.testing.expectEqual(g1.childCount(), 0);
    try std.testing.expect(g1.getProp("class") == null);

    // Gene with properties and children
    const props = [_]Property{
        .{ .key = "class", .value = .{ .String = "container" } },
        .{ .key = "id", .value = .{ .String = "main" } },
    };
    const children = [_]types.Value{
        .{ .String = "Hello" },
        .{ .String = "World" },
    };
    
    const g2 = try Gene.initFull(allocator, .{ .Symbol = "div" }, &props, &children);
    defer g2.deinit();

    try std.testing.expectEqual(g2.childCount(), 2);
    try std.testing.expectEqualStrings(g2.getProp("class").?.String, "container");
    try std.testing.expectEqualStrings(g2.getProp("id").?.String, "main");
    try std.testing.expectEqualStrings(g2.getChild(0).?.String, "Hello");
    try std.testing.expectEqualStrings(g2.getChild(1).?.String, "World");
}

test "Gene toString" {
    const allocator = std.testing.allocator;

    const props = [_]Property{
        .{ .key = "debug", .value = .{ .Bool = true } },
    };
    const children = [_]types.Value{
        .{ .Int = 1 },
        .{ .Int = 2 },
    };
    
    const gene = try Gene.initFull(allocator, .{ .Symbol = "add" }, &props, &children);
    defer gene.deinit();

    const str = try gene.toString(allocator);
    defer allocator.free(str);

    try std.testing.expectEqualStrings(str, "(add ^debug true 1 2)");
}