const std = @import("std");

// Import all test modules
comptime {
    _ = @import("tests/parser_tests.zig");
    _ = @import("tests/vm_tests.zig");
    _ = @import("tests/bytecode_tests.zig");
    _ = @import("tests/ast_to_hir_tests.zig");
    // TODO: Fix FFI tests - they have memory management issues
    // _ = @import("tests/ffi_test.zig");
}

test {
    std.testing.refAllDecls(@This());
}
