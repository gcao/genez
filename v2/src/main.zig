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

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        try printUsage();
        return;
    }

    const command = args[1];
    
    if (std.mem.eql(u8, command, "repl")) {
        try runRepl(allocator);
    } else if (std.mem.eql(u8, command, "run") and args.len >= 3) {
        try runFile(allocator, args[2]);
    } else if (std.mem.eql(u8, command, "test")) {
        try runTests(allocator);
    } else {
        try printUsage();
    }
}

fn printUsage() !void {
    const stdout = std.io.getStdOut().writer();
    try stdout.print(
        \\Gene v2 - Unified Data Format Implementation
        \\
        \\Usage:
        \\  gene repl              Start interactive REPL
        \\  gene run <file>        Run a Gene source file  
        \\  gene test              Run self-tests
        \\  gene help              Show this help
        \\
    , .{});
}

fn runRepl(allocator: std.mem.Allocator) !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();
    
    try stdout.print("Gene v2 REPL - Type 'exit' to quit\n", .{});
    
    var buf: [1024]u8 = undefined;
    while (true) {
        try stdout.print("> ", .{});
        
        if (try stdin.readUntilDelimiterOrEof(&buf, '\n')) |line| {
            if (std.mem.eql(u8, line, "exit")) break;
            
            // For now, just echo back
            try stdout.print("You entered: {s}\n", .{line});
            
            // TODO: Parse and evaluate expressions using allocator
            _ = allocator;
        } else {
            break;
        }
    }
}

fn runFile(allocator: std.mem.Allocator, path: []const u8) !void {
    _ = allocator;
    const stdout = std.io.getStdOut().writer();
    try stdout.print("Running file: {s}\n", .{path});
    // TODO: Implement file execution
}

fn runTests(allocator: std.mem.Allocator) !void {
    const stdout = std.io.getStdOut().writer();
    try stdout.print("Running Gene v2 tests...\n", .{});
    
    // Test Gene creation
    {
        const int_class = try allocator.create(Value);
        int_class.* = .{ .Symbol = try Symbol.init(allocator, "Int") };
        
        const gene = try Gene.init(allocator, int_class);
        defer allocator.destroy(gene);
        
        const str = try gene.toString(allocator);
        defer allocator.free(str);
        
        try stdout.print("Empty Gene: {s}\n", .{str});
    }
    
    // Test Gene with properties and children
    {
        const call_symbol = try allocator.create(Value);
        call_symbol.* = .{ .Symbol = try Symbol.init(allocator, "Call") };
        
        const debug_prop = try allocator.create(Value);
        debug_prop.* = .{ .Bool = true };
        
        const child1 = try allocator.create(Value);
        child1.* = .{ .Int = 1 };
        
        const child2 = try allocator.create(Value);
        child2.* = .{ .Int = 2 };
        
        const props = [_]Property{
            .{ .key = "debug", .value = debug_prop },
        };
        const children = [_]*Value{ child1, child2 };
        
        const gene = try Gene.initFull(allocator, call_symbol, &props, &children);
        defer allocator.destroy(gene);
        
        const str = try gene.toString(allocator);
        defer allocator.free(str);
        
        try stdout.print("Gene with props: {s}\n", .{str});
    }
    
    try stdout.print("All tests passed!\n", .{});
}