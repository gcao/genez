const std = @import("std");
const gene_mod = @import("gene.zig");
const Gene = gene_mod.Gene;
const Property = gene_mod.Property;
const value_mod = @import("value.zig");
const Value = value_mod.Value;
const Symbol = value_mod.Symbol;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    const stdout = std.io.getStdOut().writer();
    try stdout.print("Gene v2 Benchmarks\n\n", .{});
    
    try benchGeneCreation(allocator);
    try benchPropertyAccess(allocator);
    try benchChildAccess(allocator);
}

fn benchGeneCreation(allocator: std.mem.Allocator) !void {
    const stdout = std.io.getStdOut().writer();
    try stdout.print("=== Gene Creation ===\n", .{});
    
    const iterations = 1_000_000;
    var timer = try std.time.Timer.start();
    
    // Benchmark empty Gene creation
    {
        timer.reset();
        const symbol = try allocator.create(Value);
        defer allocator.destroy(symbol);
        symbol.* = .{ .Symbol = try Symbol.init(allocator, "Test") };
        
        var i: usize = 0;
        while (i < iterations) : (i += 1) {
            const gene = try Gene.init(allocator, symbol);
            allocator.destroy(gene);
        }
        
        const ns = timer.read();
        try stdout.print("  Empty Gene: {} ns/op\n", .{ns / iterations});
    }
    
    // Benchmark Gene with properties
    {
        timer.reset();
        const symbol = try allocator.create(Value);
        defer allocator.destroy(symbol);
        symbol.* = .{ .Symbol = try Symbol.init(allocator, "Test") };
        
        const val = try allocator.create(Value);
        defer allocator.destroy(val);
        val.* = .{ .Int = 42 };
        
        const props = [_]Property{
            .{ .key = "value", .value = val },
        };
        
        var i: usize = 0;
        while (i < iterations) : (i += 1) {
            const gene = try Gene.initFull(allocator, symbol, &props, &.{});
            allocator.destroy(gene);
        }
        
        const ns = timer.read();
        try stdout.print("  Gene with 1 property: {} ns/op\n", .{ns / iterations});
    }
    
    try stdout.print("\n", .{});
}

fn benchPropertyAccess(allocator: std.mem.Allocator) !void {
    const stdout = std.io.getStdOut().writer();
    try stdout.print("=== Property Access ===\n", .{});
    
    const iterations = 10_000_000;
    var timer = try std.time.Timer.start();
    
    // Single property access
    {
        const symbol = try allocator.create(Value);
        defer allocator.destroy(symbol);
        symbol.* = .{ .Symbol = try Symbol.init(allocator, "Test") };
        
        const val = try allocator.create(Value);
        defer allocator.destroy(val);
        val.* = .{ .Int = 42 };
        
        const props = [_]Property{
            .{ .key = "value", .value = val },
        };
        
        const gene = try Gene.initFull(allocator, symbol, &props, &.{});
        defer allocator.destroy(gene);
        
        timer.reset();
        var sum: i64 = 0;
        var i: usize = 0;
        while (i < iterations) : (i += 1) {
            if (gene.getProp("value")) |v| {
                sum += v.Int;
            }
        }
        
        const ns = timer.read();
        try stdout.print("  Single property: {} ns/op (sum={})\n", .{ ns / iterations, sum });
    }
    
    // Few properties access
    {
        const symbol = try allocator.create(Value);
        defer allocator.destroy(symbol);
        symbol.* = .{ .Symbol = try Symbol.init(allocator, "Test") };
        
        var values: [4]*Value = undefined;
        for (&values, 0..) |*v, i| {
            v.* = try allocator.create(Value);
            v.*.* = .{ .Int = @intCast(i) };
        }
        defer for (values) |v| allocator.destroy(v);
        
        const props = [_]Property{
            .{ .key = "a", .value = values[0] },
            .{ .key = "b", .value = values[1] },
            .{ .key = "c", .value = values[2] },
            .{ .key = "d", .value = values[3] },
        };
        
        const gene = try Gene.initFull(allocator, symbol, &props, &.{});
        defer allocator.destroy(gene);
        
        timer.reset();
        var sum: i64 = 0;
        var i: usize = 0;
        while (i < iterations) : (i += 1) {
            if (gene.getProp("c")) |v| {
                sum += v.Int;
            }
        }
        
        const ns = timer.read();
        try stdout.print("  Few properties (4): {} ns/op (sum={})\n", .{ ns / iterations, sum });
    }
    
    try stdout.print("\n", .{});
}

fn benchChildAccess(allocator: std.mem.Allocator) !void {
    const stdout = std.io.getStdOut().writer();
    try stdout.print("=== Child Access ===\n", .{});
    
    const iterations = 10_000_000;
    var timer = try std.time.Timer.start();
    
    // Few children access
    {
        const symbol = try allocator.create(Value);
        defer allocator.destroy(symbol);
        symbol.* = .{ .Symbol = try Symbol.init(allocator, "Test") };
        
        var children: [3]*Value = undefined;
        for (&children, 0..) |*v, i| {
            v.* = try allocator.create(Value);
            v.*.* = .{ .Int = @intCast(i) };
        }
        defer for (children) |v| allocator.destroy(v);
        
        const gene = try Gene.initFull(allocator, symbol, &.{}, &children);
        defer allocator.destroy(gene);
        
        timer.reset();
        var sum: i64 = 0;
        var i: usize = 0;
        while (i < iterations) : (i += 1) {
            if (gene.getChild(1)) |v| {
                sum += v.Int;
            }
        }
        
        const ns = timer.read();
        try stdout.print("  Few children (3): {} ns/op (sum={})\n", .{ ns / iterations, sum });
    }
    
    try stdout.print("\n", .{});
}