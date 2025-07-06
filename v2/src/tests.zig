const std = @import("std");
const testing = std.testing;
const gene_mod = @import("gene.zig");
const Gene = gene_mod.Gene;
const PropertyStorage = gene_mod.PropertyStorage;
const ChildrenStorage = gene_mod.ChildrenStorage;
const Property = gene_mod.Property;
const value_mod = @import("value.zig");
const Value = value_mod.Value;
const Symbol = value_mod.Symbol;
const SmallStr = value_mod.SmallStr;

test "Gene creation" {
    const allocator = testing.allocator;
    
    // Create a simple Gene representing: (Int)
    const symbol = try Symbol.init(allocator, "Int");
    defer symbol.deinit(allocator);
    
    const int_symbol = try allocator.create(Value);
    defer allocator.destroy(int_symbol);
    int_symbol.* = .{ .Symbol = symbol };
    
    const gene = try Gene.init(allocator, int_symbol);
    defer allocator.destroy(gene);
    
    try testing.expectEqual(gene.childCount(), 0);
    try testing.expect(gene.getProp("value") == null);
}

test "Gene with properties" {
    const allocator = testing.allocator;
    
    // Create: (Int ^value 42)
    const symbol = try Symbol.init(allocator, "Int");
    defer symbol.deinit(allocator);
    
    const int_symbol = try allocator.create(Value);
    defer allocator.destroy(int_symbol);
    int_symbol.* = .{ .Symbol = symbol };
    
    const value_42 = try allocator.create(Value);
    defer allocator.destroy(value_42);
    value_42.* = .{ .Int = 42 };
    
    const props = [_]Property{
        .{ .key = "value", .value = value_42 },
    };
    
    const gene = try Gene.initFull(allocator, int_symbol, &props, &.{});
    defer allocator.destroy(gene);
    
    const val = gene.getProp("value");
    try testing.expect(val != null);
    try testing.expectEqual(val.?.Int, 42);
}

test "Gene with children" {
    const allocator = testing.allocator;
    
    // Create: (Array 1 2 3)
    const symbol = try Symbol.init(allocator, "Array");
    defer symbol.deinit(allocator);
    
    const array_symbol = try allocator.create(Value);
    defer allocator.destroy(array_symbol);
    array_symbol.* = .{ .Symbol = symbol };
    
    const one = try allocator.create(Value);
    defer allocator.destroy(one);
    one.* = .{ .Int = 1 };
    
    const two = try allocator.create(Value);
    defer allocator.destroy(two);
    two.* = .{ .Int = 2 };
    
    const three = try allocator.create(Value);
    defer allocator.destroy(three);
    three.* = .{ .Int = 3 };
    
    const children = [_]*Value{ one, two, three };
    
    const gene = try Gene.initFull(allocator, array_symbol, &.{}, &children);
    defer allocator.destroy(gene);
    
    try testing.expectEqual(gene.childCount(), 3);
    try testing.expectEqual(gene.getChild(0).?.Int, 1);
    try testing.expectEqual(gene.getChild(1).?.Int, 2);
    try testing.expectEqual(gene.getChild(2).?.Int, 3);
    try testing.expect(gene.getChild(3) == null);
}

test "Gene toString" {
    const allocator = testing.allocator;
    
    // Create: (add ^checked true 1 2)
    const symbol = try Symbol.init(allocator, "add");
    defer symbol.deinit(allocator);
    
    const add_symbol = try allocator.create(Value);
    defer allocator.destroy(add_symbol);
    add_symbol.* = .{ .Symbol = symbol };
    
    const checked_true = try allocator.create(Value);
    defer allocator.destroy(checked_true);
    checked_true.* = .{ .Bool = true };
    
    const one = try allocator.create(Value);
    defer allocator.destroy(one);
    one.* = .{ .Int = 1 };
    
    const two = try allocator.create(Value);
    defer allocator.destroy(two);
    two.* = .{ .Int = 2 };
    
    const props = [_]Property{
        .{ .key = "checked", .value = checked_true },
    };
    const children = [_]*Value{ one, two };
    
    const gene = try Gene.initFull(allocator, add_symbol, &props, &children);
    defer allocator.destroy(gene);
    
    const str = try gene.toString(allocator);
    defer allocator.free(str);
    
    try testing.expectEqualStrings("(:add ^checked true 1 2)", str);
}

test "PropertyStorage optimization levels" {
    const allocator = testing.allocator;
    
    // Test None
    {
        const storage = try PropertyStorage.init(allocator, &.{});
        try testing.expect(storage == .None);
        try testing.expect(storage.get("any") == null);
    }
    
    // Test Single
    {
        const val = try allocator.create(Value);
        defer allocator.destroy(val);
        val.* = .{ .Int = 1 };
        
        const props = [_]Property{
            .{ .key = "a", .value = val },
        };
        const storage = try PropertyStorage.init(allocator, &props);
        try testing.expect(storage == .Single);
        try testing.expect(storage.get("a") != null);
        try testing.expect(storage.get("b") == null);
    }
    
    // Test Few
    {
        const val1 = try allocator.create(Value);
        defer allocator.destroy(val1);
        val1.* = .{ .Int = 1 };
        
        const val2 = try allocator.create(Value);
        defer allocator.destroy(val2);
        val2.* = .{ .Int = 2 };
        
        const props = [_]Property{
            .{ .key = "a", .value = val1 },
            .{ .key = "b", .value = val2 },
        };
        const storage = try PropertyStorage.init(allocator, &props);
        try testing.expect(storage == .Few);
        try testing.expectEqual(storage.get("a").?.Int, 1);
        try testing.expectEqual(storage.get("b").?.Int, 2);
    }
}

test "ChildrenStorage optimization levels" {
    const allocator = testing.allocator;
    
    // Test None
    {
        const storage = try ChildrenStorage.init(allocator, &.{});
        try testing.expect(storage == .None);
        try testing.expectEqual(storage.count(), 0);
    }
    
    // Test Single
    {
        const val = try allocator.create(Value);
        defer allocator.destroy(val);
        val.* = .{ .Int = 1 };
        
        const children = [_]*Value{val};
        const storage = try ChildrenStorage.init(allocator, &children);
        try testing.expect(storage == .Single);
        try testing.expectEqual(storage.count(), 1);
        try testing.expectEqual(storage.get(0).?.Int, 1);
    }
    
    // Test Few
    {
        const val1 = try allocator.create(Value);
        defer allocator.destroy(val1);
        val1.* = .{ .Int = 1 };
        
        const val2 = try allocator.create(Value);
        defer allocator.destroy(val2);
        val2.* = .{ .Int = 2 };
        
        const val3 = try allocator.create(Value);
        defer allocator.destroy(val3);
        val3.* = .{ .Int = 3 };
        
        const children = [_]*Value{ val1, val2, val3 };
        const storage = try ChildrenStorage.init(allocator, &children);
        try testing.expect(storage == .Few);
        try testing.expectEqual(storage.count(), 3);
        try testing.expectEqual(storage.get(2).?.Int, 3);
    }
}

test "SmallString" {
    const allocator = testing.allocator;
    
    // Test small string
    {
        const small = SmallStr.init("hello");
        try testing.expectEqual(small.len, 5);
        try testing.expect(small.equals(SmallStr.init("hello")));
        try testing.expect(!small.equals(SmallStr.init("world")));
        
        const str = try small.toString(allocator);
        defer allocator.free(str);
        try testing.expectEqualStrings("hello", str);
    }
    
    // Test empty string
    {
        const empty = SmallStr.init("");
        try testing.expectEqual(empty.len, 0);
    }
    
    // Test max size
    {
        const max = SmallStr.init("123456");
        try testing.expectEqual(max.len, 6);
    }
}