const std = @import("std");
const mir = @import("mir.zig");
const bytecode = @import("bytecode.zig");

/// MirToBytecodeConverter defines the interface for converting MIR to Bytecode
///
/// This interface provides a clear contract for the transformation from
/// MIR (Mid-level Intermediate Representation) to Bytecode.
pub const MirToBytecodeConverter = struct {
    /// Convert an MIR module to a bytecode function
    ///
    /// This function takes an MIR module and converts it to a bytecode function.
    /// The caller is responsible for managing the memory of the returned bytecode function.
    ///
    /// Parameters:
    ///   - allocator: The allocator to use for allocating memory
    ///   - mir_module: The MIR module to convert
    ///
    /// Returns:
    ///   - A bytecode function representing the converted MIR module
    ///
    /// Errors:
    ///   - Std.mem.Allocator.Error: If memory allocation fails
    ///   - error.InvalidMIR: If the MIR contains invalid constructs
    ///   - error.UnsupportedFeature: If the MIR contains unsupported features
    pub fn convert(allocator: std.mem.Allocator, mir_module: *mir.MIR) !bytecode.Function {
        return @import("mir_to_bytecode.zig").convert(allocator, mir_module);
    }
};

/// Convert an MIR module to a bytecode function
///
/// This is a convenience function that delegates to MirToBytecodeConverter.convert.
///
/// Parameters:
///   - allocator: The allocator to use for allocating memory
///   - mir_module: The MIR module to convert
///
/// Returns:
///   - A bytecode function representing the converted MIR module
///
/// Errors:
///   - Std.mem.Allocator.Error: If memory allocation fails
///   - error.InvalidMIR: If the MIR contains invalid constructs
///   - error.UnsupportedFeature: If the MIR contains unsupported features
pub fn convert(allocator: std.mem.Allocator, mir_module: *mir.MIR) !bytecode.Function {
    return MirToBytecodeConverter.convert(allocator, mir_module);
}
