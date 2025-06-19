const std = @import("std");
const types = @import("core/types.zig");
const bytecode = @import("backend/bytecode.zig");
const vm = @import("backend/vm.zig");
const pipeline = @import("pipeline.zig");
const compiler = @import("compiler.zig");
const debug_output = @import("core/debug_output.zig");
const debug = @import("core/debug.zig");
const ast = @import("frontend/ast.zig");

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
            .type_check = false, // Type checking disabled - too strict for current language features
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
            .type_check = false, // Type checking disabled - too strict for current language features
        };

        // Use pipeline to compile file
        var result = try pipeline.compileFile(self.allocator, path, options);
        defer result.deinit();

        debug.log("Compilation completed, starting execution...", .{});

        try self.executeWithFunctionsAndRegistry(@constCast(&result.main_func), result.created_functions.items, result.module_registry);

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
        self.executeWithFunctionsAndRegistry(func, created_functions, null) catch |err| return err;
    }
    
    fn executeWithFunctionsAndRegistry(self: *Runtime, func: *bytecode.Function, created_functions: []*bytecode.Function, module_registry: ?*@import("core/module_registry.zig").ModuleRegistry) !void {
        // Use debug output for bytecode display
        const debug_out = debug_output.DebugOutput.init(self.stdout, self.debug_mode);
        debug_out.writeBytecodeInstructions(func);

        // Create VM and execute bytecode
        var gene_vm = vm.VM.init(self.allocator, self.stdout);
        defer gene_vm.deinit();

        // Set module registry if available and register module namespaces
        if (module_registry) |registry| {
            gene_vm.setModuleRegistry(registry);
            
            // Create namespace objects for each module
            var it = registry.modules.iterator();
            while (it.next()) |entry| {
                const module_id = entry.key_ptr.*;
                const module = entry.value_ptr.*;
                
                // Create a Module value for the module namespace
                const module_value = types.Value{ .Module = module };
                
                // Register with full module ID
                try gene_vm.setVariable(module_id, module_value);
                
                // Also register with base name for convenience
                // Extract base name from path (e.g., "./math" -> "math")
                var alias = module_id;
                if (std.mem.lastIndexOfScalar(u8, module_id, '/')) |slash_idx| {
                    alias = module_id[slash_idx + 1..];
                }
                
                // Remove .gene extension if present
                if (std.mem.endsWith(u8, alias, ".gene")) {
                    alias = alias[0..alias.len - 5];
                }
                
                try gene_vm.setVariable(alias, module_value);
                
                debug.log("Registered module namespace: {s} (alias: {s})", .{module_id, alias});
            }
        }

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
            .type_check = false, // Type checking disabled - too strict for current language features
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

    /// Parse a package.gene file and output the parsed structure
    pub fn parsePackageFile(self: *Runtime, path: []const u8) !void {
        try self.parsePackageFileWithFormat(path, false);
    }
    
    /// Parse a .gene file with optional pretty formatting
    pub fn parsePackageFileWithFormat(self: *Runtime, path: []const u8, pretty: bool) !void {
        const parser = @import("frontend/parser.zig");
        const ast_serialize = @import("frontend/ast_serialize.zig");
        
        // Read file
        const file = try std.fs.cwd().openFile(path, .{});
        defer file.close();
        
        const source = try file.readToEndAlloc(self.allocator, std.math.maxInt(usize));
        defer self.allocator.free(source);
        
        // Parse with filename
        const parse_result = try parser.parseGeneSourceWithFilename(self.allocator, source, path);
        defer {
            parse_result.arena.deinit();
            self.allocator.destroy(parse_result.arena);
        }
        
        // Serialize and print the AST
        if (self.debug_mode) {
            try self.stdout.print("\n=== Parsed Document Structure ===\n", .{});
        }
        
        if (pretty) {
            // Pretty print the document structure
            try self.prettyPrintDocument(parse_result.nodes);
        } else {
            // Raw AST output - if first node is a map (properties), show it separately
            var buffer = std.ArrayList(u8).init(self.allocator);
            defer buffer.deinit();
            
            if (parse_result.nodes.len > 0) {
                const first_node = parse_result.nodes[0];
                if (first_node == .Expression and first_node.Expression == .MapLiteral) {
                    // This is a document with properties
                    try self.stdout.print("(GeneDocument\n", .{});
                    try self.stdout.print("  properties: ", .{});
                    try ast_serialize.serializeNode(buffer.writer(), first_node, 0);
                    try self.stdout.print("{s}\n", .{buffer.items});
                    
                    // Print expressions if any
                    if (parse_result.nodes.len > 1) {
                        try self.stdout.print("  expressions:\n", .{});
                        for (parse_result.nodes[1..]) |node| {
                            buffer.clearRetainingCapacity();
                            try ast_serialize.serializeNode(buffer.writer(), node, 0);
                            try self.stdout.print("    {s}\n", .{buffer.items});
                        }
                    }
                    try self.stdout.print(")\n", .{});
                } else {
                    // No properties, just expressions
                    for (parse_result.nodes) |node| {
                        buffer.clearRetainingCapacity();
                        try ast_serialize.serializeNode(buffer.writer(), node, 0);
                        try self.stdout.print("{s}\n", .{buffer.items});
                    }
                }
            }
        }
    }

    /// Pretty print a parsed document with properties and expressions
    fn prettyPrintDocument(self: *Runtime, nodes: []ast.AstNode) !void {
        if (nodes.len == 0) return;
        
        // Check if first node is a map (properties)
        const first_node = nodes[0];
        if (first_node == .Expression and first_node.Expression == .MapLiteral) {
            const map = first_node.Expression.MapLiteral;
            
            // Print properties
            try self.stdout.print("=== Properties ===\n", .{});
            for (map.entries) |entry| {
                // Get the key as a string
                const key_expr = entry.key.*;
                const key_str = switch (key_expr) {
                    .Literal => |lit| switch (lit.value) {
                        .String => |s| s,
                        else => continue,
                    },
                    else => continue,
                };
                
                // Print the key
                try self.stdout.print("{s}: ", .{key_str});
                
                // Print the value based on its type
                try self.prettyPrintValue(entry.value.*, 0);
                try self.stdout.print("\n", .{});
            }
            
            // Print expressions if any
            if (nodes.len > 1) {
                try self.stdout.print("\n=== Expressions ===\n", .{});
                for (nodes[1..]) |node| {
                    try self.prettyPrintNode(node, 0);
                    try self.stdout.print("\n", .{});
                }
            }
        } else {
            // No properties, just print expressions
            for (nodes) |node| {
                try self.prettyPrintNode(node, 0);
                try self.stdout.print("\n", .{});
            }
        }
    }
    
    /// Pretty print a single AST node
    fn prettyPrintNode(self: *Runtime, node: ast.AstNode, indent: usize) !void {
        switch (node) {
            .Expression => |expr| try self.prettyPrintExpression(expr, indent),
        }
    }
    
    /// Pretty print an expression in a readable format
    fn prettyPrintExpression(self: *Runtime, expr: ast.Expression, indent: usize) !void {
        // Add indentation
        var i: usize = 0;
        while (i < indent) : (i += 1) {
            try self.stdout.print("  ", .{});
        }
        
        switch (expr) {
            .FuncCall => |call| {
                try self.stdout.print("(", .{});
                try self.prettyPrintExpression(call.func.*, 0);
                for (call.args.items) |arg| {
                    try self.stdout.print(" ", .{});
                    try self.prettyPrintExpression(arg.*, 0);
                }
                try self.stdout.print(")", .{});
            },
            .VarDecl => |decl| {
                try self.stdout.print("(var {s} ", .{decl.name});
                try self.prettyPrintExpression(decl.value.*, 0);
                try self.stdout.print(")", .{});
            },
            .FuncDef => |def| {
                try self.stdout.print("(fn {s} [", .{def.name});
                for (def.params, 0..) |param, j| {
                    if (j > 0) try self.stdout.print(" ", .{});
                    try self.stdout.print("{s}", .{param.name});
                }
                try self.stdout.print("] ...)", .{});
            },
            .Literal => |lit| try self.prettyPrintLiteral(lit),
            .Variable => |v| try self.stdout.print("{s}", .{v.name}),
            .BinaryOp => |op| {
                try self.stdout.print("(", .{});
                switch (op.op) {
                    .Ident => |ident| try self.stdout.print("{s} ", .{ident}),
                    else => try self.stdout.print("op ", .{}),
                }
                try self.prettyPrintExpression(op.left.*, 0);
                try self.stdout.print(" ", .{});
                try self.prettyPrintExpression(op.right.*, 0);
                try self.stdout.print(")", .{});
            },
            else => try self.stdout.print("<{s}>", .{@tagName(expr)}),
        }
    }
    
    /// Pretty print a literal value
    fn prettyPrintLiteral(self: *Runtime, lit: ast.Literal) !void {
        switch (lit.value) {
            .String => |s| try self.stdout.print("\"{s}\"", .{s}),
            .Int => |i| try self.stdout.print("{d}", .{i}),
            .Float => |f| try self.stdout.print("{d}", .{f}),
            .Bool => |b| try self.stdout.print("{s}", .{if (b) "true" else "false"}),
            .Nil => try self.stdout.print("nil", .{}),
            .Symbol => |s| try self.stdout.print(":{s}", .{s}),
            else => try self.stdout.print("<literal>", .{}),
        }
    }
    
    /// Helper to pretty print values recursively
    fn prettyPrintValue(self: *Runtime, expr: ast.Expression, indent: usize) !void {
        switch (expr) {
            .Literal => |lit| {
                switch (lit.value) {
                    .String => |s| try self.stdout.print("\"{s}\"", .{s}),
                    .Int => |i| try self.stdout.print("{d}", .{i}),
                    .Float => |f| try self.stdout.print("{d}", .{f}),
                    .Bool => |b| try self.stdout.print("{s}", .{if (b) "true" else "false"}),
                    .Nil => try self.stdout.print("nil", .{}),
                    .Symbol => |s| try self.stdout.print(":{s}", .{s}),
                    else => try self.stdout.print("<unknown>", .{}),
                }
            },
            .ArrayLiteral => |arr| {
                try self.stdout.print("[", .{});
                for (arr.elements, 0..) |elem, i| {
                    if (i > 0) try self.stdout.print(", ", .{});
                    try self.prettyPrintValue(elem.*, indent);
                }
                try self.stdout.print("]", .{});
            },
            .MapLiteral => |map| {
                try self.stdout.print("{{", .{});
                if (map.entries.len > 0) {
                    try self.stdout.print("\n", .{});
                    for (map.entries) |entry| {
                        // Indent
                        var j: usize = 0;
                        while (j < indent + 2) : (j += 1) {
                            try self.stdout.print(" ", .{});
                        }
                        
                        // Print key
                        const key_expr = entry.key.*;
                        const key_str = switch (key_expr) {
                            .Literal => |lit| switch (lit.value) {
                                .String => |s| s,
                                else => "<key>",
                            },
                            else => "<key>",
                        };
                        try self.stdout.print("{s}: ", .{key_str});
                        
                        // Print value
                        try self.prettyPrintValue(entry.value.*, indent + 2);
                        try self.stdout.print("\n", .{});
                    }
                    
                    // Closing indent
                    var j: usize = 0;
                    while (j < indent) : (j += 1) {
                        try self.stdout.print(" ", .{});
                    }
                }
                try self.stdout.print("}}", .{});
            },
            else => try self.stdout.print("<expr>", .{}),
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
