const std = @import("std");
const ast = @import("frontend/ast.zig");
const bytecode = @import("backend/bytecode.zig");
const debug_output = @import("core/debug_output.zig");

// Stage transformation modules (direct imports, no interfaces)
const ast_to_hir = @import("transforms/ast_to_hir.zig");
const hir_to_mir = @import("transforms/hir_to_mir.zig");
const mir_to_lir = @import("transforms/mir_to_lir.zig");
const lir_to_bytecode = @import("transforms/lir_to_bytecode.zig");
const mir_to_bytecode = @import("transforms/mir_to_bytecode.zig");
const hir_typechecker = @import("core/hir_typechecker.zig");
const macro_expander = @import("transforms/macro_expander.zig");

pub const CompilerOptions = struct {
    debug_mode: bool = false,
    optimize: bool = false,
    type_check: bool = true,
};

pub const CompilationContext = struct {
    allocator: std.mem.Allocator,
    options: CompilerOptions,

    pub fn init(allocator: std.mem.Allocator, options: CompilerOptions) CompilationContext {
        return .{
            .allocator = allocator,
            .options = options,
        };
    }
};

pub fn compile(ctx: CompilationContext, nodes: []ast.AstNode) !mir_to_bytecode.ConversionResult {
    // Initialize debug output
    const debug = debug_output.DebugOutput.init(std.io.getStdOut().writer(), ctx.options.debug_mode);
    
    // Display AST
    try debug.writeAST(nodes, "AST");

    // Macro expansion
    debug.writeMessage("\n=== Macro Expansion ===\n", .{});
    const expanded_nodes = try macro_expander.expandMacros(ctx.allocator, nodes);
    defer ctx.allocator.free(expanded_nodes);
    
    // Display expanded AST if macros were expanded
    if (ctx.options.debug_mode) {
        try debug.writeAST(expanded_nodes, "Expanded AST");
    }

    // AST -> HIR
    debug.writeMessage("\n=== AST to HIR ===\n", .{});
    var hir_prog = try ast_to_hir.convert(ctx.allocator, expanded_nodes);
    defer hir_prog.deinit();

    // Display HIR
    try debug.writeHIR(hir_prog, "HIR");

    // Type checking (if enabled)
    if (ctx.options.type_check) {
        debug.writeMessage("\n=== Type Checking ===\n", .{});
        var type_checker = hir_typechecker.HIRTypeChecker.init(ctx.allocator);
        defer type_checker.deinit();
        
        type_checker.checkProgram(hir_prog) catch |err| {
            if (type_checker.hasErrors()) {
                type_checker.printErrors();
            }
            return err;
        };
        
        if (type_checker.hasErrors()) {
            type_checker.printErrors();
            return error.TypeCheckFailed;
        }
        
        debug.writeMessage("Type checking passed\n", .{});
    }

    // HIR -> MIR
    debug.writeMessage("\n=== HIR to MIR ===\n", .{});
    var mir_prog = try hir_to_mir.convert(ctx.allocator, hir_prog);
    defer mir_prog.deinit();

    // Display MIR
    try debug.writeMIR(mir_prog, "MIR");

    // MIR -> Bytecode (bypassing LIR for now until function handling is fixed)
    debug.writeMessage("\n=== MIR to Bytecode ===\n", .{});
    const conversion_result = try mir_to_bytecode.convert(ctx.allocator, &mir_prog);

    // Display Bytecode
    try debug.writeBytecode(conversion_result.main_func, "Bytecode");

    return conversion_result;
}
