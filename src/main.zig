const std = @import("std");
pub const vm = @import("vm.zig");
pub const parser = @import("parser.zig");
pub const bytecode = @import("bytecode.zig");

pub fn main() !void {
    std.debug.print("Gene VM starting...\n", .{});
    
    // Initialize VM
    var my_vm = vm.VM.init();

    // Create an allocator for parser
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var allocator = gpa.allocator();

    // Example usage of parser and VM
    const input = "1 + 2 * 3";
    const parsed = try parser.parseGeneSource(&allocator, input);
    defer allocator.free(parsed);
    
    // Convert AST to bytecode
    const module = try bytecode.lowerToBytecode(&allocator);
    if (module.functions.len == 0) {
        return error.NoFunctionsGenerated;
    }
    
    // Run the first function
    try my_vm.runFunction(module.functions[0]);

    std.debug.print("Program completed.\n", .{});
}
