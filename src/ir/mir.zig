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
        rest_param: ?[]const u8 = null,

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
            
            // Free rest parameter name if present
            if (self.rest_param) |rp| {
                self.allocator.free(rp);
            }
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
                    .LoadModule => |path| self.allocator.free(path),
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
        CreateArray: usize, // Create array from top N stack elements
        CreateMap: usize, // Create map from top N*2 stack elements (key-value pairs)
        LoadVariable: []const u8,
        LoadParameter: usize, // Load parameter by index
        LoadFunction: *Function,
        LoadModule: []const u8, // Load module by path
        StoreVariable: []const u8,
        Add,
        Sub,
        Mul: void,
        Div: void,
        LessThan,
        GreaterThan, // Added GreaterThan instruction
        Equal, // Added Equal instruction
        NotEqual, // Not equal comparison
        LessEqual, // Less than or equal comparison
        GreaterEqual, // Greater than or equal comparison
        LogicalAnd, // Logical AND (&&)
        LogicalOr, // Logical OR (||)
        LogicalNot, // Logical NOT (!)
        Jump: usize,
        JumpIfFalse: usize,
        Call: usize,
        Print,
        Return,
        // Class-related instructions
        DefineClass: ClassDefinition,
        CreateInstance: InstanceCreation,
        Get, // Universal get for maps, arrays, fields
        Set, // Universal set for maps, arrays, fields
        CallMethod: MethodCall,

        // Pattern matching support
        Length, // Get length of array/map/string
        ArrayGet, // Get array element (index on stack)
        MapGet, // Get map value (key on stack)
        Duplicate, // Duplicate top of stack
        Pop, // Pop and discard top of stack
        IsArray, // Check if value is array
        IsMap, // Check if value is map
        CreateNamespace, // Create a new namespace with name from stack
        PushNamespace, // Push namespace onto namespace stack
        PopNamespace, // Pop namespace from stack and return it
        
        // Module instructions
        CreateModule, // Create a new module with name from stack
        PushModule, // Push module onto module stack for evaluation
        PopModule, // Pop module from stack and return it
        MarkExport, // Mark a name (from stack) for export in current module
        Export, // Export a value with a name in current module
        
        // Exception handling instructions
        TryStart: usize, // Start of try block (jump to catch on exception)
        TryEnd, // End of try/catch/finally block
        Throw, // Throw exception from stack
        LoadException, // Load current exception onto stack
        ClearException, // Clear current exception
        CreateCallback, // Create a C callback wrapper from function and signature on stack
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
        is_super: bool = false,
    };

    pub const InstanceCreation = struct {
        class_name: []const u8,
        arg_count: usize,
    };
};
