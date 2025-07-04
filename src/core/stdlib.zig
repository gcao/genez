const std = @import("std");
const types = @import("types.zig");
const debug = @import("debug.zig");

// File handle type
pub const FileHandle = struct {
    file: std.fs.File,
    allocator: std.mem.Allocator,
    path: []const u8,
    
    pub fn init(file: std.fs.File, allocator: std.mem.Allocator, path: []const u8) !FileHandle {
        return FileHandle{
            .file = file,
            .allocator = allocator,
            .path = try allocator.dupe(u8, path),
        };
    }
    
    pub fn deinit(self: *FileHandle) void {
        self.allocator.free(self.path);
        self.file.close();
    }
};

// Standard library functions
pub const StdlibFunction = enum {
    // File I/O
    FileOpen,
    FileClose,
    FileRead,
    FileWrite,
    FileReadAll,
    FileWriteAll,
    FileExists,
    
    // Directory operations
    DirList,
    DirCreate,
    DirRemove,
    
    // Path operations
    PathJoin,
    PathBasename,
    PathDirname,
    PathAbs,
    
    // System operations
    Exit,
    GetEnv,
    SetEnv,
    Args,
    
    // String operations
    StringSplit,
    StringJoin,
    StringTrim,
    StringReplace,
    StringStartsWith,
    StringEndsWith,
    StringConcat,
    
    // Math operations
    MathSqrt,
    MathSin,
    MathCos,
    MathTan,
    MathAbs,
    MathCeil,
    MathFloor,
    MathRound,
    MathPow,
    MathLog,
    
    // Type conversion
    ParseInt,
    ParseFloat,
    
    // Time operations
    TimeNow,
    TimeSleep,
    
    // Error handling
    Throw,
    ErrorNew,
    ErrorType,
    ErrorMessage,
};

pub fn registerStdlibFunctions(vm: anytype) !void {
    // Register file I/O functions
    try vm.registerGlobalBuiltin("file_open", .{ .StdlibFunction = .FileOpen });
    try vm.registerGlobalBuiltin("file_close", .{ .StdlibFunction = .FileClose });
    try vm.registerGlobalBuiltin("file_read", .{ .StdlibFunction = .FileRead });
    try vm.registerGlobalBuiltin("file_write", .{ .StdlibFunction = .FileWrite });
    try vm.registerGlobalBuiltin("file_read_all", .{ .StdlibFunction = .FileReadAll });
    try vm.registerGlobalBuiltin("file_write_all", .{ .StdlibFunction = .FileWriteAll });
    try vm.registerGlobalBuiltin("file_exists", .{ .StdlibFunction = .FileExists });
    
    // Register directory operations
    try vm.registerGlobalBuiltin("dir_list", .{ .StdlibFunction = .DirList });
    try vm.registerGlobalBuiltin("dir_create", .{ .StdlibFunction = .DirCreate });
    try vm.registerGlobalBuiltin("dir_remove", .{ .StdlibFunction = .DirRemove });
    
    // Register path operations
    try vm.registerGlobalBuiltin("path_join", .{ .StdlibFunction = .PathJoin });
    try vm.registerGlobalBuiltin("path_basename", .{ .StdlibFunction = .PathBasename });
    try vm.registerGlobalBuiltin("path_dirname", .{ .StdlibFunction = .PathDirname });
    try vm.registerGlobalBuiltin("path_abs", .{ .StdlibFunction = .PathAbs });
    
    // Register system operations
    try vm.registerGlobalBuiltin("exit", .{ .StdlibFunction = .Exit });
    try vm.registerGlobalBuiltin("get_env", .{ .StdlibFunction = .GetEnv });
    try vm.registerGlobalBuiltin("set_env", .{ .StdlibFunction = .SetEnv });
    try vm.registerGlobalBuiltin("args", .{ .StdlibFunction = .Args });
    
    // Register string operations
    try vm.registerGlobalBuiltin("string_split", .{ .StdlibFunction = .StringSplit });
    try vm.registerGlobalBuiltin("string_join", .{ .StdlibFunction = .StringJoin });
    try vm.registerGlobalBuiltin("string_trim", .{ .StdlibFunction = .StringTrim });
    try vm.registerGlobalBuiltin("string_replace", .{ .StdlibFunction = .StringReplace });
    try vm.registerGlobalBuiltin("string_starts_with", .{ .StdlibFunction = .StringStartsWith });
    try vm.registerGlobalBuiltin("string_ends_with", .{ .StdlibFunction = .StringEndsWith });
    
    // Register math operations
    try vm.registerGlobalBuiltin("math_sqrt", .{ .StdlibFunction = .MathSqrt });
    try vm.registerGlobalBuiltin("math_sin", .{ .StdlibFunction = .MathSin });
    try vm.registerGlobalBuiltin("math_cos", .{ .StdlibFunction = .MathCos });
    try vm.registerGlobalBuiltin("math_tan", .{ .StdlibFunction = .MathTan });
    try vm.registerGlobalBuiltin("math_abs", .{ .StdlibFunction = .MathAbs });
    try vm.registerGlobalBuiltin("math_ceil", .{ .StdlibFunction = .MathCeil });
    try vm.registerGlobalBuiltin("math_floor", .{ .StdlibFunction = .MathFloor });
    try vm.registerGlobalBuiltin("math_round", .{ .StdlibFunction = .MathRound });
    try vm.registerGlobalBuiltin("math_pow", .{ .StdlibFunction = .MathPow });
    try vm.registerGlobalBuiltin("math_log", .{ .StdlibFunction = .MathLog });
    
    // Register type conversion
    try vm.registerGlobalBuiltin("parse_int", .{ .StdlibFunction = .ParseInt });
    try vm.registerGlobalBuiltin("parse_float", .{ .StdlibFunction = .ParseFloat });
    
    // Register time operations
    try vm.registerGlobalBuiltin("time_now", .{ .StdlibFunction = .TimeNow });
    try vm.registerGlobalBuiltin("time_sleep", .{ .StdlibFunction = .TimeSleep });
}