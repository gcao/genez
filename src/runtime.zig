const std = @import("std");
const types = @import("types.zig");
const parser = @import("parser.zig");
const compiler = @import("compiler.zig");
const bytecode = @import("bytecode.zig");
const vm = @import("vm.zig");

pub const Runtime = struct {
    allocator: std.mem.Allocator,
    debug_mode: bool,
    stdout: std.fs.File.Writer,

    pub fn init(allocator: std.mem.Allocator, debug_mode: bool, stdout: std.fs.File.Writer) Runtime {
        return Runtime{
            .allocator = allocator,
            .debug_mode = debug_mode,
            .stdout = stdout,
        };
    }

    pub fn deinit(self: *Runtime) void {
        _ = self;
    }

    pub fn eval(self: *Runtime, source: []const u8) !void {
        const options = compiler.CompilerOptions{
            .debug_mode = self.debug_mode,
            .optimize = false,
        };
        const ctx = compiler.CompilationContext.init(self.allocator, options);

        // Parse source into AST using arena allocator
        if (self.debug_mode) {
            std.debug.print("[DEBUG] About to call parseGeneSource\n", .{});
        }
        const parse_result = try parser.parseGeneSource(self.allocator, source);
        if (self.debug_mode) {
            std.debug.print("[DEBUG] Back in runtime after parseGeneSource\n", .{});
        }

        // Get the parsed nodes
        const nodes = parser.getLastParseNodes() orelse {
            return error.NoNodesReturned;
        };

        // Immediate debug after parse returns
        if (self.debug_mode) {
            std.debug.print("[DEBUG] parseGeneSource returned successfully\n", .{});
            std.debug.print("[DEBUG] Arena pointer: {*}\n", .{parse_result.arena});
            std.debug.print("[DEBUG] Number of nodes: {}\n", .{nodes.len});
        }

        // Add debug output
        if (self.debug_mode) {
            std.debug.print("[DEBUG] Parse completed successfully. Starting compilation...\n", .{});
        }

        defer {
            // Clean up the arena after we're done with the AST
            // Arena cleanup handles everything including nodes and strings
            if (self.debug_mode) {
                std.debug.print("[DEBUG] About to deinit arena\n", .{});
            }
            parse_result.arena.deinit();
            if (self.debug_mode) {
                std.debug.print("[DEBUG] Arena deinit completed, about to destroy\n", .{});
            }
            self.allocator.destroy(parse_result.arena);
            if (self.debug_mode) {
                std.debug.print("[DEBUG] Arena destroy completed\n", .{});
            }
            // No need to destroy parse_result since it's on the stack now
        }

        // Compile AST to bytecode
        if (self.debug_mode) {
            std.debug.print("[DEBUG] Starting compilation...\n", .{});
        }
        var conversion_result = try compiler.compile(ctx, nodes);
        defer conversion_result.deinit();
        if (self.debug_mode) {
            std.debug.print("[DEBUG] Compilation completed successfully\n", .{});
        }

        // Execute bytecode
        if (self.debug_mode) {
            std.debug.print("[DEBUG] Starting execution...\n", .{});
        }
        try self.execute(@constCast(&conversion_result.main_func));
        if (self.debug_mode) {
            std.debug.print("[DEBUG] Execution completed successfully\n", .{});
        }
    }

    fn runBytecodeFile(self: *Runtime, path: []const u8) !void {
        const file = try std.fs.cwd().openFile(path, .{});
        defer file.close();

        // Read bytecode module from file
        var module = try bytecode.Module.readFromFile(self.allocator, file.reader());
        defer module.deinit();

        // Execute the first function in the module
        if (module.functions.len == 0) {
            return error.NoFunctionsInModule;
        }
        try self.execute(&module.functions[0]);
    }

    pub fn runFile(self: *Runtime, path: []const u8) !void {
        // Check if file is a bytecode file
        if (std.mem.endsWith(u8, path, ".gbc")) {
            try self.runBytecodeFile(path);
            return;
        }

        // Handle regular Gene source file
        const file = if (comptime @import("builtin").target.cpu.arch == .wasm32)
            try std.fs.cwd().openFile(path, .{})
        else
            try std.fs.cwd().openFile(path, .{});
        defer file.close();

        const source = try file.readToEndAlloc(self.allocator, std.math.maxInt(usize));
        defer self.allocator.free(source);

        const parse_result = try parser.parseGeneSource(self.allocator, source);
        if (self.debug_mode) {
            std.debug.print("[DEBUG] Back in runtime after parseGeneSource\n", .{});
        }

        // Get the parsed nodes
        const nodes = parser.getLastParseNodes() orelse {
            return error.NoNodesReturned;
        };

        // Immediate debug after parse returns
        if (self.debug_mode) {
            std.debug.print("[DEBUG] parseGeneSource returned successfully\n", .{});
            std.debug.print("[DEBUG] Arena pointer: {*}\n", .{parse_result.arena});
            std.debug.print("[DEBUG] Number of nodes: {}\n", .{nodes.len});
        }

        defer {
            // Clean up the arena after we're done with the AST
            if (self.debug_mode) {
                std.debug.print("[DEBUG] About to deinit arena\n", .{});
            }
            parse_result.arena.deinit();
            if (self.debug_mode) {
                std.debug.print("[DEBUG] Arena deinit completed, about to destroy\n", .{});
            }
            self.allocator.destroy(parse_result.arena);
            if (self.debug_mode) {
                std.debug.print("[DEBUG] Arena destroy completed\n", .{});
            }
        }

        // Now compile and execute!
        if (self.debug_mode) {
            std.debug.print("[DEBUG] Starting compilation of {} nodes...\n", .{nodes.len});
        }

        const options = compiler.CompilerOptions{
            .debug_mode = self.debug_mode,
            .optimize = false,
        };
        const ctx = compiler.CompilationContext.init(self.allocator, options);
        var conversion_result = try compiler.compile(ctx, nodes);
        defer conversion_result.deinit();

        if (self.debug_mode) {
            std.debug.print("[DEBUG] Compilation completed, starting execution...\n", .{});
        }

        try self.execute(@constCast(&conversion_result.main_func));

        if (self.debug_mode) {
            std.debug.print("[DEBUG] File execution completed successfully!\n", .{});
        }
    }

    fn execute(self: *Runtime, func: *bytecode.Function) !void {
        // Print bytecode if debug mode
        if (self.debug_mode) {
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

        // Create VM and execute bytecode
        var gene_vm = vm.VM.init(self.allocator, self.stdout);
        defer gene_vm.deinit();

        try gene_vm.execute(func);
    }

    pub fn compileFile(self: *Runtime, file_path: []const u8) !void {
        _ = self;
        _ = file_path;
        return error.NotImplemented;
    }
};
