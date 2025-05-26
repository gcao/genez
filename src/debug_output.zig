const std = @import("std");
const ast = @import("ast.zig");
const hir = @import("hir.zig");
const mir = @import("mir.zig");
const bytecode = @import("bytecode.zig");

// Re-use existing serialization implementations
const ast_serialize = @import("ast_serialize.zig");
const hir_serialize = @import("hir_serialize.zig");
const mir_serialize = @import("mir_serialize.zig");
const bytecode_serialize = @import("bytecode_serialize.zig");

/// Unified debug output interface for all compilation stages
pub const DebugOutput = struct {
    writer: std.fs.File.Writer,
    enabled: bool,
    
    pub fn init(writer: std.fs.File.Writer, enabled: bool) DebugOutput {
        return .{
            .writer = writer,
            .enabled = enabled,
        };
    }
    
    /// Write AST nodes with optional header
    pub fn writeAST(self: DebugOutput, nodes: []ast.AstNode, comptime header: ?[]const u8) !void {
        if (!self.enabled) return;
        
        if (header) |h| {
            std.debug.print("\n=== {s} ===\n", .{h});
        }
        
        for (nodes) |node| {
            try ast_serialize.serializeNode(self.writer, node, 0);
            std.debug.print("\n", .{});
        }
    }
    
    /// Write HIR module with optional header
    pub fn writeHIR(self: DebugOutput, hir_module: hir.HIR, comptime header: ?[]const u8) !void {
        if (!self.enabled) return;
        
        if (header) |h| {
            std.debug.print("\n=== {s} ===\n", .{h});
        }
        
        try hir_serialize.serializeModule(self.writer, hir_module, 0);
        std.debug.print("\n", .{});
    }
    
    /// Write MIR module with optional header
    pub fn writeMIR(self: DebugOutput, mir_module: mir.MIR, comptime header: ?[]const u8) !void {
        if (!self.enabled) return;
        
        if (header) |h| {
            std.debug.print("\n=== {s} ===\n", .{h});
        }
        
        try mir_serialize.serializeModule(self.writer, mir_module, 0);
        std.debug.print("\n", .{});
    }
    
    /// Write bytecode function with optional header
    pub fn writeBytecode(self: DebugOutput, func: bytecode.Function, comptime header: ?[]const u8) !void {
        if (!self.enabled) return;
        
        if (header) |h| {
            std.debug.print("\n=== {s} ===\n", .{h});
        }
        
        try bytecode_serialize.serializeFunction(self.writer, func, 0);
        std.debug.print("\n", .{});
    }
    
    /// Write a simple debug message
    pub fn writeMessage(self: DebugOutput, comptime format: []const u8, args: anytype) void {
        if (!self.enabled) return;
        std.debug.print(format, args);
    }
    
    /// Write bytecode instructions in a simple format (used by runtime)
    pub fn writeBytecodeInstructions(self: DebugOutput, func: *bytecode.Function) void {
        if (!self.enabled) return;
        
        std.debug.print("\n[DEBUG] === Bytecode ===\n", .{});
        for (func.instructions.items, 0..) |instr, i| {
            std.debug.print("[{}] {s}", .{ i, @tagName(instr.op) });
            if (instr.operand) |operand| {
                switch (operand) {
                    .Int => |val| std.debug.print(" {}", .{val}),
                    .String => |val| std.debug.print(" \"{s}\"", .{val}),
                    else => std.debug.print(" (other operand)", .{}),
                }
            }
            std.debug.print("\n", .{});
        }
    }
};