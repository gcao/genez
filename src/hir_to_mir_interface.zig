const std = @import("std");
const hir = @import("hir.zig");
const mir = @import("mir.zig");

/// HirToMirConverter defines the interface for converting HIR to MIR
///
/// This interface provides a clear contract for the transformation from
/// HIR (High-level Intermediate Representation) to MIR (Mid-level Intermediate Representation).
pub const HirToMirConverter = struct {
    /// Convert an HIR module to an MIR module
    ///
    /// This function takes an HIR module and converts it to an MIR module.
    /// The caller is responsible for managing the memory of the returned MIR module.
    ///
    /// Parameters:
    ///   - allocator: The allocator to use for allocating memory
    ///   - hir_module: The HIR module to convert
    ///
    /// Returns:
    ///   - An MIR module representing the converted HIR module
    ///
    /// Errors:
    ///   - Std.mem.Allocator.Error: If memory allocation fails
    ///   - error.InvalidHIR: If the HIR contains invalid constructs
    ///   - error.UnsupportedFeature: If the HIR contains unsupported features
    pub fn convert(allocator: std.mem.Allocator, hir_module: hir.HIR) !mir.MIR {
        return @import("hir_to_mir.zig").convert(allocator, hir_module);
    }
};

/// Convert an HIR module to an MIR module
///
/// This is a convenience function that delegates to HirToMirConverter.convert.
///
/// Parameters:
///   - allocator: The allocator to use for allocating memory
///   - hir_module: The HIR module to convert
///
/// Returns:
///   - An MIR module representing the converted HIR module
///
/// Errors:
///   - Std.mem.Allocator.Error: If memory allocation fails
///   - error.InvalidHIR: If the HIR contains invalid constructs
///   - error.UnsupportedFeature: If the HIR contains unsupported features
pub fn convert(allocator: std.mem.Allocator, hir_module: hir.HIR) !mir.MIR {
    return HirToMirConverter.convert(allocator, hir_module);
}
