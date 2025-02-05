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
            .debug_mode = false,
            .optimize = false,
        };
        const ctx = compiler.CompilationContext.init(self.allocator, options);

        // Parse source into AST
        const nodes = try parser.parseGeneSource(self.allocator, source);
        defer {
            for (nodes) |*node| {
                node.deinit(self.allocator);
            }
            self.allocator.free(nodes);
        }

        // Compile AST to bytecode
        var func = try compiler.compile(ctx, nodes);
        defer func.deinit();

        // Execute bytecode
        try self.execute(&func);
    }

    pub fn runFile(self: *Runtime, filename: []const u8) !void {
        const options = compiler.CompilerOptions{
            .debug_mode = self.debug_mode,
            .optimize = false,
        };
        const ctx = compiler.CompilationContext.init(self.allocator, options);

        // Read source file
        const source = try std.fs.cwd().readFileAlloc(self.allocator, filename, 1024 * 1024);
        defer self.allocator.free(source);

        // Parse source into AST
        const nodes = try parser.parseGeneSource(self.allocator, source);
        defer {
            for (nodes) |*node| {
                node.deinit(self.allocator);
            }
            self.allocator.free(nodes);
        }

        // Compile AST to bytecode
        var func = try compiler.compile(ctx, nodes);
        defer func.deinit();

        // Execute bytecode
        try self.execute(&func);
    }

    fn execute(self: *Runtime, func: *bytecode.Function) !void {
        // Print bytecode if debug mode
        if (self.debug_mode) {
            std.debug.print("\n[DEBUG] === Bytecode ===\n", .{});
            for (func.instructions.items) |instr| {
                std.debug.print("{any}\n", .{instr});
            }
        }

        // Create VM and execute bytecode
        var gene_vm = vm.VM.init(self.allocator, self.stdout);
        defer gene_vm.deinit();

        try gene_vm.execute(func);
    }

    pub fn compileFile(self: *const Runtime, file_path: []const u8) !void {
        const options = compiler.CompilerOptions{
            .debug_mode = self.debug_mode,
            .optimize = false,
        };
        const ctx = compiler.CompilationContext.init(self.allocator, options);

        // Read source file
        const source = try std.fs.cwd().readFileAlloc(self.allocator, file_path, 1024 * 1024);
        defer self.allocator.free(source);

        // Parse source into AST
        const nodes = try parser.parseGeneSource(self.allocator, source);
        defer {
            for (nodes) |*node| {
                node.deinit(self.allocator);
            }
            self.allocator.free(nodes);
        }

        // Compile AST to bytecode
        var func = try compiler.compile(ctx, nodes);
        defer func.deinit();

        // Print bytecode
        for (func.instructions.items) |instr| {
            std.debug.print("{any}\n", .{instr});
        }
    }
};
