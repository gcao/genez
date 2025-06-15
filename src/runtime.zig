const std = @import("std");
const types = @import("core/types.zig");
const bytecode = @import("backend/bytecode.zig");
const vm = @import("backend/vm.zig");
const pipeline = @import("pipeline.zig");
const compiler = @import("compiler.zig");
const debug_output = @import("core/debug_output.zig");
const debug = @import("core/debug.zig");

pub const Runtime = struct {
    allocator: std.mem.Allocator,
    debug_mode: bool,
    stdout: std.fs.File.Writer,

    pub fn init(allocator: std.mem.Allocator, debug_mode: bool, stdout: std.fs.File.Writer) Runtime {
        // Set the global debug flag
        debug.setDebugEnabled(debug_mode);

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
            .type_check = false, // Type checking disabled - needs more work to handle all language features
        };

        // Use pipeline to compile source with special eval flag
        var result = try pipeline.compileSource(self.allocator, source, options);
        defer result.deinit();

        // For eval mode, wrap the last expression with a print if it's not already a print
        // This is a temporary solution - ideally we'd modify the VM to return the last value
        const needs_print = !std.mem.containsAtLeast(u8, source, 1, "print");
        
        if (needs_print) {
            // HACK: Re-compile with print wrapper
            const wrapped_source = try std.fmt.allocPrint(self.allocator, "(print {s})", .{source});
            defer self.allocator.free(wrapped_source);
            
            result.deinit();
            result = try pipeline.compileSource(self.allocator, wrapped_source, options);
        }

        // Execute bytecode
        debug.log("Starting execution...", .{});
        try self.execute(@constCast(&result.main_func));
        debug.log("Execution completed successfully", .{});
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

        const options = compiler.CompilerOptions{
            .debug_mode = self.debug_mode,
            .optimize = false,
            .type_check = false, // Type checking disabled - needs more work to handle all language features
        };

        // Use pipeline to compile file
        var result = try pipeline.compileFile(self.allocator, path, options);
        defer result.deinit();

        debug.log("Compilation completed, starting execution...", .{});

        try self.executeWithFunctions(@constCast(&result.main_func), result.created_functions.items);

        debug.log("File execution completed successfully!", .{});
    }

    /// Run Gene source code provided as a string.
    pub fn runSource(self: *Runtime, source: []const u8) !void {
        const options = compiler.CompilerOptions{
            .debug_mode = self.debug_mode,
            .optimize = false,
            .type_check = false,
        };

        var result = try pipeline.compileSource(self.allocator, source, options);
        defer result.deinit();

        debug.log("Compilation completed, starting execution...", .{});

        try self.executeWithFunctions(@constCast(&result.main_func), result.created_functions.items);

        debug.log("Source execution completed successfully!", .{});
    }

    fn execute(self: *Runtime, func: *bytecode.Function) !void {
        try self.executeWithFunctions(func, &[_]*bytecode.Function{});
    }

    fn executeWithFunctions(self: *Runtime, func: *bytecode.Function, created_functions: []*bytecode.Function) !void {
        // Use debug output for bytecode display
        const debug_out = debug_output.DebugOutput.init(self.stdout, self.debug_mode);
        debug_out.writeBytecodeInstructions(func);

        // Create VM and execute bytecode
        var gene_vm = vm.VM.init(self.allocator, self.stdout);
        defer gene_vm.deinit();

        // Register user-defined functions as global variables
        for (created_functions) |user_func| {
            const func_value = types.Value{ .Function = user_func };
            try gene_vm.setVariable(user_func.name, func_value);
        }

        try gene_vm.execute(func);
    }

    pub fn compileFile(self: *Runtime, file_path: []const u8) !void {
        const options = compiler.CompilerOptions{
            .debug_mode = self.debug_mode,
            .optimize = false,
            .type_check = false, // Type checking disabled - needs more work to handle all language features
        };

        // Use pipeline to compile file
        var result = try pipeline.compileFile(self.allocator, file_path, options);
        defer result.deinit();

        debug.log("Compilation completed, writing bytecode...", .{});

        // Generate output filename by replacing .gene with .gbc
        const output_path = if (std.mem.endsWith(u8, file_path, ".gene"))
            try std.fmt.allocPrint(self.allocator, "{s}.gbc", .{file_path[0 .. file_path.len - 5]}) // Replace .gene with .gbc
        else
            try std.fmt.allocPrint(self.allocator, "{s}.gbc", .{file_path});
        defer self.allocator.free(output_path);

        // Create output file
        const output_file = try std.fs.cwd().createFile(output_path, .{});
        defer output_file.close();

        // Create a module with the main function and any created functions
        var functions = try self.allocator.alloc(bytecode.Function, 1 + result.created_functions.items.len);
        // Note: Don't defer free functions here since they contain shallow copies
        // The actual function data is owned by CompiledResult and will be freed by result.deinit()

        // Copy main function
        functions[0] = result.main_func;

        // Copy created functions
        for (result.created_functions.items, 0..) |func, i| {
            functions[i + 1] = func.*;
        }

        // Create module
        var module = bytecode.Module{
            .functions = functions,
            .allocator = self.allocator,
            .deserialized_functions = std.ArrayList(*bytecode.Function).init(self.allocator),
            .owns_functions = false, // Don't own the functions - they're owned by CompiledResult
        };
        defer module.deinit();

        // Write module to file
        try module.writeToFile(output_file.writer());

        debug.log("Bytecode written to: {s}", .{output_path});
        if (!self.debug_mode) {
            try self.stdout.print("Compiled {s} -> {s}\n", .{ file_path, output_path });
        }
    }

    /// Compile Gene source code from a string and write bytecode to `output_path`.
    pub fn compileSource(self: *Runtime, source: []const u8, output_path: []const u8) !void {
        const options = compiler.CompilerOptions{
            .debug_mode = self.debug_mode,
            .optimize = false,
            .type_check = false,
        };

        var result = try pipeline.compileSource(self.allocator, source, options);
        defer result.deinit();

        debug.log("Compilation completed, writing bytecode...", .{});

        const output_file = try std.fs.cwd().createFile(output_path, .{});
        defer output_file.close();

        var functions = try self.allocator.alloc(bytecode.Function, 1 + result.created_functions.items.len);
        functions[0] = result.main_func;
        for (result.created_functions.items, 0..) |func, i| {
            functions[i + 1] = func.*;
        }

        var module = bytecode.Module{
            .functions = functions,
            .allocator = self.allocator,
            .deserialized_functions = std.ArrayList(*bytecode.Function).init(self.allocator),
            .owns_functions = false,
        };
        defer module.deinit();

        try module.writeToFile(output_file.writer());

        debug.log("Bytecode written to: {s}", .{output_path});
        if (!self.debug_mode) {
            try self.stdout.print("Compiled stdin -> {s}\n", .{output_path});
        }
    }

    pub fn runRepl(self: *Runtime) !void {
        try self.stdout.print("Gene REPL v{s}\n", .{@import("main.zig").VERSION});
        try self.stdout.print("Type 'exit' or 'quit' to exit.\n", .{});

        var buf: [1024]u8 = undefined;
        var reader = std.io.getStdIn().reader();

        while (true) {
            try self.stdout.print("gene> ", .{});
            const line_or_null = try reader.readUntilDelimiterOrEof(&buf, '\n');

            if (line_or_null == null) {
                // EOF (Ctrl-D) received
                break;
            }
            const line = line_or_null.?;

            if (std.mem.eql(u8, line, "exit") or std.mem.eql(u8, line, "quit")) {
                break;
            }

            if (line.len == 0) {
                continue;
            }

            errdefer std.debug.print("Error in REPL: {any}\n", .{std.err.getLastError()});
            if (self.eval(line)) |_| {
                // Success
            } else |err| {
                std.debug.print("Error: {any}\n", .{err});
            }
        }
        try self.stdout.print("Exiting REPL.\n", .{});
    }
};
