const std = @import("std");
const ast = @import("ast.zig");
const vm = @import("vm.zig");
const parser = @import("parser.zig");
const bytecode = @import("bytecode.zig");
const hir = @import("hir.zig");
const mir = @import("mir.zig");
const compiler = @import("compiler.zig");

pub const Runtime = struct {
    allocator: std.mem.Allocator,
    debug_mode: bool,

    pub fn init(allocator: std.mem.Allocator, debug_mode: bool) Runtime {
        return Runtime{
            .allocator = allocator,
            .debug_mode = debug_mode,
        };
    }

    fn debugPrint(self: *const Runtime, comptime fmt: []const u8, args: anytype) void {
        if (self.debug_mode) {
            std.debug.print(fmt, args);
        }
    }

    fn debugSection(self: *const Runtime, comptime title: []const u8) void {
        if (self.debug_mode) {
            std.debug.print("\n[DEBUG] === {s} ===\n", .{title});
        }
    }

    pub fn runFile(self: *const Runtime, file_path: []const u8) !void {
        const file = try std.fs.cwd().openFile(file_path, .{});
        defer file.close();

        const source = try file.readToEndAlloc(self.allocator, std.math.maxInt(usize));
        defer self.allocator.free(source);

        // Parse source into AST
        const nodes = parser.parseGeneSource(self.allocator, source) catch |err| {
            std.debug.print("Error parsing source: {}\n", .{err});
            return err;
        };
        defer {
            for (nodes) |node| {
                switch (node) {
                    .Expression => |expr| switch (expr) {
                        .Literal => |lit| switch (lit.value) {
                            .String => |str| self.allocator.free(str),
                            else => {},
                        },
                        .BinaryOp => |bin_op| {
                            self.allocator.destroy(bin_op.left);
                            self.allocator.destroy(bin_op.right);
                        },
                        .Variable => |var_expr| self.allocator.free(var_expr.name),
                    },
                }
            }
            self.allocator.free(nodes);
        }

        if (self.debug_mode) {
            self.debugSection("AST");
            for (nodes) |node| {
                std.debug.print("{any}\n", .{node});
            }
        }

        // Lower AST to bytecode
        var func = try bytecode.lowerToBytecode(self.allocator, nodes);
        defer func.deinit();

        if (self.debug_mode) {
            self.debugSection("Bytecode");
            for (func.instructions) |instr| {
                std.debug.print("{any}\n", .{instr});
            }
        }

        // Execute bytecode
        var gene_vm = try vm.VM.init(self.allocator);
        defer gene_vm.deinit();

        try gene_vm.execute(func);
    }

    pub fn compileFile(self: *const Runtime, file_path: []const u8) !void {
        // Read input file
        const input = try std.fs.cwd().readFileAlloc(self.allocator, file_path, std.math.maxInt(usize));
        defer self.allocator.free(input);

        // Parse source
        const parsed = try parser.parseGeneSource(self.allocator, input);
        defer {
            for (parsed) |node| {
                switch (node) {
                    .Expression => |expr| switch (expr) {
                        .Literal => |lit| switch (lit.value) {
                            .String => |str| self.allocator.free(str),
                            else => {},
                        },
                        .BinaryOp => |bin_op| {
                            self.allocator.destroy(bin_op.left);
                            self.allocator.destroy(bin_op.right);
                        },
                        else => {},
                    },
                }
            }
            self.allocator.free(parsed);
        }

        // Convert to bytecode
        var module = try bytecode.lowerToBytecode(self.allocator, parsed);
        defer module.deinit();

        if (self.debug_mode) {
            self.debugSection("Compilation");
            std.debug.print("Compilation successful.\n", .{});
        }
    }
};
