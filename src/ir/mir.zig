const std = @import("std");
const hir = @import("hir.zig");
const types = @import("../core/types.zig");

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
            // Allocate the default name properly to avoid freeing string literals
            const default_name = allocator.dupe(u8, "main") catch @panic("Failed to allocate default function name");
            return Function{
                .allocator = allocator,
                .name = default_name,
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
                    .DefineClass => |*class_def| {
                        self.allocator.free(class_def.name);
                        if (class_def.parent_name) |parent| {
                            self.allocator.free(parent);
                        }
                        for (class_def.fields) |field| {
                            self.allocator.free(field);
                        }
                        self.allocator.free(class_def.fields);
                        
                        var method_iter = class_def.methods.iterator();
                        while (method_iter.next()) |entry| {
                            self.allocator.free(entry.key_ptr.*);
                            entry.value_ptr.*.deinit();
                            self.allocator.destroy(entry.value_ptr.*);
                        }
                        class_def.methods.deinit();
                    },
                    .CreateInstance => |*inst_creation| self.allocator.free(inst_creation.class_name),
                    .GetField => |field_name| self.allocator.free(field_name),
                    .SetField => |field_name| self.allocator.free(field_name),
                    .CallMethod => |*method_call| {
                        self.allocator.free(method_call.method_name);
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
        Mul: void,
        Div: void,
        LessThan,
        GreaterThan, // Added GreaterThan instruction
        Equal, // Added Equal instruction
        Jump: usize,
        JumpIfFalse: usize,
        Call: usize,
        Print,
        Return,
        // Class-related instructions
        DefineClass: ClassDefinition,
        CreateInstance: InstanceCreation,
        GetField: []const u8, // Field name
        SetField: []const u8, // Field name
        CallMethod: MethodCall,
        
        // Pattern matching support
        Length, // Get length of array/map/string
        ArrayGet, // Get array element (index on stack)
        MapGet, // Get map value (key on stack)
        Duplicate, // Duplicate top of stack
        Pop, // Pop and discard top of stack
        IsArray, // Check if value is array
        IsMap, // Check if value is map
    };
    
    pub const ClassDefinition = struct {
        name: []const u8,
        parent_name: ?[]const u8,
        fields: [][]const u8,
        methods: std.StringHashMap(*Function),
    };
    
    pub const MethodCall = struct {
        method_name: []const u8,
        arg_count: usize,
    };
    
    pub const InstanceCreation = struct {
        class_name: []const u8,
        arg_count: usize,
    };
};
