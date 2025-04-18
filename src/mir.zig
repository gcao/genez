const std = @import("std");
const hir = @import("hir.zig");
const types = @import("types.zig");

pub const MIR = struct {
    allocator: std.mem.Allocator,
    functions: std.ArrayList(Function),

    pub fn init(allocator: std.mem.Allocator) MIR {
        return MIR{
            .allocator = allocator,
            .functions = std.ArrayList(Function).init(allocator),
        };
    }

    pub fn deinit(self: *MIR) void {
        for (self.functions.items) |*func| {
            func.deinit();
        }
        self.functions.deinit();
    }

    pub const Function = struct {
        allocator: std.mem.Allocator,
        name: []const u8,
        blocks: std.ArrayList(Block),
        param_count: usize = 0,
        param_names: std.ArrayList([]const u8),

        pub fn init(allocator: std.mem.Allocator) Function {
            return Function{
                .allocator = allocator,
                .name = "main",
                .blocks = std.ArrayList(Block).init(allocator),
                .param_names = std.ArrayList([]const u8).init(allocator),
            };
        }

        pub fn deinit(self: *Function) void {
            self.allocator.free(self.name);
            for (self.blocks.items) |*block| {
                block.deinit();
            }
            self.blocks.deinit();

            // Free parameter names
            for (self.param_names.items) |name| {
                self.allocator.free(name);
            }
            self.param_names.deinit();
        }
    };

    pub const Block = struct {
        allocator: std.mem.Allocator,
        instructions: std.ArrayList(Instruction),

        pub fn init(allocator: std.mem.Allocator) Block {
            return Block{
                .allocator = allocator,
                .instructions = std.ArrayList(Instruction).init(allocator),
            };
        }

        pub fn deinit(self: *Block) void {
            for (self.instructions.items) |*instr| {
                switch (instr.*) {
                    .LoadString => |str| self.allocator.free(str),
                    .LoadSymbol => |sym| self.allocator.free(sym),
                    .LoadVariable => |name| self.allocator.free(name),
                    .LoadArray => |arr| {
                        for (arr) |*val| {
                            val.deinit(self.allocator);
                        }
                        self.allocator.free(arr);
                    },
                    .LoadMap => |*map| {
                        var it = map.iterator();
                        while (it.next()) |entry| {
                            self.allocator.free(entry.key_ptr.*);
                            entry.value_ptr.deinit(self.allocator);
                        }
                        map.deinit();
                    },
                    .StoreVariable => |name| self.allocator.free(name), // Free the stored name
                    .LoadFunction => |func_ptr| {
                        // Deallocate the function object pointed to
                        func_ptr.deinit();
                        self.allocator.destroy(func_ptr);
                    },
                    else => {},
                }
            }
            self.instructions.deinit();
        }
    };

    pub const Instruction = union(enum) {
        LoadInt: i64,
        LoadFloat: f64,
        LoadBool: bool,
        LoadString: []const u8,
        LoadNil,
        LoadSymbol: []const u8,
        LoadArray: []types.Value,
        LoadMap: std.StringHashMap(types.Value),
        LoadVariable: []const u8,
        LoadParameter: usize, // Load parameter by index
        LoadFunction: *Function,
        StoreVariable: []const u8,
        Add,
        Sub,
        LessThan,
        GreaterThan, // Added GreaterThan instruction
        Equal, // Added Equal instruction
        Jump: usize,
        JumpIfFalse: usize,
        Call: usize,
        Print,
        Return,
    };
};
