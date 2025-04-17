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
        var parse_result = try parser.parseGeneSource(self.allocator, source);
        defer {
            // Clean up the arena after we're done with the AST
            parse_result.arena.deinit();
            parse_result.nodes.deinit();
        }

        // Compile AST to bytecode
        var func = try compiler.compile(ctx, parse_result.nodes.items);

        // Execute bytecode
        try self.execute(&func);
        func.deinit(); // Pass mutable func (already was mutable 'var func')
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

        const ctx = compiler.CompilationContext.init(self.allocator, compiler.CompilerOptions{
            .debug_mode = self.debug_mode,
            .optimize = false,
        });

        var parse_result = try parser.parseGeneSource(self.allocator, source);
        defer {
            // Clean up the arena after we're done with the AST
            parse_result.arena.deinit();
            parse_result.nodes.deinit();
        }

        // Compile AST to bytecode
        var func = try compiler.compile(ctx, parse_result.nodes.items);
        // std.debug.print("[DEBUG_TRACE] Received bytecode function from compiler.compile\n", .{}); // TRACE 6

        // Execute bytecode
        // std.debug.print("[DEBUG_TRACE] Calling self.execute\n", .{}); // TRACE 7
        try self.execute(&func);
        // std.debug.print("[DEBUG_TRACE] Returned from self.execute\n", .{}); // TRACE 8
        func.deinit();
        // std.debug.print("[DEBUG_TRACE] Deinitialized bytecode function\n", .{}); // TRACE 9
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
        const source = try std.fs.cwd().readFileAlloc(self.allocator, file_path, 1024 * 1024);
        defer self.allocator.free(source);

        const options = compiler.CompilerOptions{
            .debug_mode = self.debug_mode,
            .optimize = false,
        };
        const ctx = compiler.CompilationContext.init(self.allocator, options);

        var parse_result = try parser.parseGeneSource(self.allocator, source);
        defer {
            // Clean up the arena after we're done with the AST
            parse_result.arena.deinit();
            parse_result.nodes.deinit();
        }

        // Compile AST to bytecode
        const func = try compiler.compile(ctx, parse_result.nodes.items);

        // Create module with the function
        const functions = try self.allocator.alloc(bytecode.Function, 1);
        functions[0] = func;

        var module = bytecode.Module{
            .functions = functions,
            .allocator = self.allocator,
        };

        // Create output file with .gbc extension
        const output_path = if (std.mem.endsWith(u8, file_path, ".gene"))
            try std.fmt.allocPrint(self.allocator, "{s}", .{file_path[0 .. file_path.len - 5]})
        else
            try std.fmt.allocPrint(self.allocator, "{s}", .{file_path});
        defer self.allocator.free(output_path);

        const gbc_path = try std.fmt.allocPrint(self.allocator, "{s}.gbc", .{output_path});
        defer self.allocator.free(gbc_path);

        const output_file = try std.fs.cwd().createFile(gbc_path, .{});
        defer output_file.close();

        // Write bytecode to file
        try module.writeToFile(output_file.writer());

        // Deinit module after writing to file
        // Module.deinit already takes *Module (mutable)
        module.deinit();

        if (self.debug_mode) {
            std.debug.print("Compiled {s} to {s}\n", .{ file_path, gbc_path });
        }
    }
};
