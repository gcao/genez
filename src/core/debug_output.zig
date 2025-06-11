const std = @import("std");
const ast = @import("../frontend/ast.zig");
const hir = @import("../ir/hir.zig");
const mir = @import("../ir/mir.zig");
const lir = @import("../ir/lir.zig");
const bytecode = @import("../backend/bytecode.zig");

// Re-use existing serialization implementations
const ast_serialize = @import("../frontend/ast_serialize.zig");
const hir_serialize = @import("../ir/hir_serialize.zig");
const mir_serialize = @import("../ir/mir_serialize.zig");
const lir_serialize = @import("../ir/lir_serialize.zig");
const bytecode_serialize = @import("../backend/bytecode_serialize.zig");

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
    
    /// Write LIR module with optional header
    pub fn writeLIR(self: DebugOutput, lir_module: lir.LIR, comptime header: ?[]const u8) !void {
        if (!self.enabled) return;
        
        if (header) |h| {
            std.debug.print("\n=== {s} ===\n", .{h});
        }
        
        try lir_serialize.serialize(self.writer, lir_module, "");
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
            if (instr.immediate) |immediate| {
                switch (immediate) {
                    .Int => |val| std.debug.print(" {}", .{val}),
                    .String => |val| std.debug.print(" \"{s}\"", .{val}),
                    else => std.debug.print(" (other immediate)", .{}),
                }
            }
            if (instr.var_name) |var_name| {
                std.debug.print(" var:{s}", .{var_name});
            }
            std.debug.print("\n", .{});
        }
    }
};