const std = @import("std");
const types = @import("types.zig");
const bytecode = @import("../backend/bytecode.zig");

// Core class hierarchy implementation
pub const CoreClasses = struct {
    allocator: std.mem.Allocator,

    // Metaclasses
    class_class: *types.ClassDefinition, // Class metaclass

    // Root classes
    any_class: *types.ClassDefinition, // Root of all classes

    // Type classes
    number_class: *types.ClassDefinition, // Abstract number class
    int_class: *types.ClassDefinition, // Integer class
    float_class: *types.ClassDefinition, // Float class
    string_class: *types.ClassDefinition, // String class
    bool_class: *types.ClassDefinition, // Boolean class
    nil_class: *types.ClassDefinition, // Nil class

    // Function classes
    fn_class: *types.ClassDefinition, // Function class
    builtin_fn_class: *types.ClassDefinition, // Built-in function class
    macro_class: *types.ClassDefinition, // Macro class

    // Collection classes
    array_class: *types.ClassDefinition, // Array class
    map_class: *types.ClassDefinition, // Map/Dictionary class

    // Other core classes
    symbol_class: *types.ClassDefinition, // Symbol class

    pub fn init(allocator: std.mem.Allocator) !CoreClasses {
        var self: CoreClasses = undefined;
        self.allocator = allocator;

        // Create Any class (root of hierarchy)
        self.any_class = try createClass(allocator, "Any", null);

        // Create Class metaclass (instance of itself)
        self.class_class = try createClass(allocator, "Class", null);

        // Create Number class (abstract)
        self.number_class = try createClass(allocator, "Number", self.any_class);

        // Create numeric classes
        self.int_class = try createClass(allocator, "Int", self.number_class);
        self.float_class = try createClass(allocator, "Float", self.number_class);

        // Create other primitive classes
        self.string_class = try createClass(allocator, "String", self.any_class);
        self.bool_class = try createClass(allocator, "Bool", self.any_class);
        self.nil_class = try createClass(allocator, "Nil", self.any_class);
        self.symbol_class = try createClass(allocator, "Symbol", self.any_class);

        // Create function classes
        self.fn_class = try createClass(allocator, "Fn", self.any_class);
        self.builtin_fn_class = try createClass(allocator, "BuiltinFn", self.fn_class);
        self.macro_class = try createClass(allocator, "Macro", self.any_class);

        // Create collection classes
        self.array_class = try createClass(allocator, "Array", self.any_class);
        self.map_class = try createClass(allocator, "Map", self.any_class);

        // Add core methods to classes
        try self.addCoreMethods();

        return self;
    }

    fn createClass(allocator: std.mem.Allocator, name: []const u8, parent: ?*types.ClassDefinition) !*types.ClassDefinition {
        const class = try allocator.create(types.ClassDefinition);
        class.* = types.ClassDefinition.init(allocator, try allocator.dupe(u8, name));
        class.parent = parent;
        return class;
    }

    fn addCoreMethods(self: *CoreClasses) !void {
        // Add methods to Any class (inherited by all)
        try self.addMethod(self.any_class, "toString", createToStringMethod);
        try self.addMethod(self.any_class, "class", createClassMethod);
        try self.addMethod(self.any_class, "==", createEqualsMethod);
        try self.addMethod(self.any_class, "!=", createNotEqualsMethod);

        // Add methods to Number class
        try self.addMethod(self.number_class, "+", createAddMethod);
        try self.addMethod(self.number_class, "-", createSubMethod);
        try self.addMethod(self.number_class, "*", createMulMethod);
        try self.addMethod(self.number_class, "/", createDivMethod);
        try self.addMethod(self.number_class, "<", createLessThanMethod);
        try self.addMethod(self.number_class, ">", createGreaterThanMethod);
        try self.addMethod(self.number_class, "<=", createLessEqualMethod);
        try self.addMethod(self.number_class, ">=", createGreaterEqualMethod);

        // Add methods to String class
        try self.addMethod(self.string_class, "length", createLengthMethod);
        try self.addMethod(self.string_class, "++", createConcatMethod);
        try self.addMethod(self.string_class, "substring", createSubstringMethod);

        // Add methods to Array class
        try self.addMethod(self.array_class, "length", createLengthMethod);
        try self.addMethod(self.array_class, "push", createPushMethod);
        try self.addMethod(self.array_class, "pop", createPopMethod);
        try self.addMethod(self.array_class, "at", createAtMethod);

        // Add methods to Map class
        try self.addMethod(self.map_class, "get", createGetMethod);
        try self.addMethod(self.map_class, "put", createPutMethod);
        try self.addMethod(self.map_class, "has", createHasMethod);
        try self.addMethod(self.map_class, "keys", createKeysMethod);

        // Add methods to Class metaclass
        try self.addMethod(self.class_class, "new", createNewMethod);
        try self.addMethod(self.class_class, "name", createNameMethod);
        try self.addMethod(self.class_class, "parent", createParentMethod);
    }

    fn addMethod(self: *CoreClasses, class: *types.ClassDefinition, name: []const u8, create_fn: fn (std.mem.Allocator) anyerror!*bytecode.Function) !void {
        const method = try create_fn(self.allocator);
        const method_name = try self.allocator.dupe(u8, name);
        try class.methods.put(method_name, method);
    }

    pub fn getClassForValue(self: *const CoreClasses, value: types.Value) *types.ClassDefinition {
        return switch (value) {
            .Int => self.int_class,
            .Float => self.float_class,
            .String => self.string_class,
            .Bool => self.bool_class,
            .Nil => self.nil_class,
            .Symbol => self.symbol_class,
            .Array => self.array_class,
            .Map => self.map_class,
            .Function => self.fn_class,
            .BuiltinOperator => self.builtin_fn_class,
            .Class => self.class_class,
            .Object => {
                // For objects, we need the VM to look up the actual class
                // This is a placeholder - in practice, objects know their class
                return self.any_class;
            },
            else => self.any_class,
        };
    }

    pub fn registerInVM(self: *CoreClasses, variables: *std.StringArrayHashMap(types.Value)) !void {
        // Register all core classes as global variables
        try variables.put("Class", .{ .Class = self.class_class });
        try variables.put("Any", .{ .Class = self.any_class });
        try variables.put("Number", .{ .Class = self.number_class });
        try variables.put("Int", .{ .Class = self.int_class });
        try variables.put("Float", .{ .Class = self.float_class });
        try variables.put("String", .{ .Class = self.string_class });
        try variables.put("Bool", .{ .Class = self.bool_class });
        try variables.put("Nil", .{ .Class = self.nil_class });
        try variables.put("Symbol", .{ .Class = self.symbol_class });
        try variables.put("Fn", .{ .Class = self.fn_class });
        try variables.put("BuiltinFn", .{ .Class = self.builtin_fn_class });
        try variables.put("Macro", .{ .Class = self.macro_class });
        try variables.put("Array", .{ .Class = self.array_class });
        try variables.put("Map", .{ .Class = self.map_class });
    }

    pub fn deinit(_: *CoreClasses) void {
        // Classes are managed by the VM's allocated_classes list
        // So we don't need to free them here
    }
};

// Helper to create a function with given parameters
fn createFunction(allocator: std.mem.Allocator, name: []const u8, param_count: u16, register_count: u16) !*bytecode.Function {
    const func = try allocator.create(bytecode.Function);
    func.* = bytecode.Function.init(allocator);
    func.name = try allocator.dupe(u8, name);
    func.param_count = param_count;
    func.register_count = register_count;
    return func;
}

// Method implementations as built-in functions
// These create bytecode.Function objects that wrap built-in behavior

fn createToStringMethod(allocator: std.mem.Allocator) !*bytecode.Function {
    const func = try createFunction(allocator, "toString", 1, 0);
    // This would be a built-in function that converts any value to string
    // For now, it's a placeholder
    try func.instructions.append(.{ .op = .Return, .src1 = 0 });
    return func;
}

fn createClassMethod(allocator: std.mem.Allocator) !*bytecode.Function {
    const func = try createFunction(allocator, "class", 1, 0);
    // Returns the class of the object
    try func.instructions.append(.{ .op = .Return, .src1 = 0 });
    return func;
}

fn createEqualsMethod(allocator: std.mem.Allocator) !*bytecode.Function {
    const func = try createFunction(allocator, "==", 2, 3);
    // Compare two values for equality
    try func.instructions.append(.{ .op = .Eq, .src1 = 0, .src2 = 1, .dst = 2 });
    try func.instructions.append(.{ .op = .Return, .src1 = 2 });
    return func;
}

fn createNotEqualsMethod(allocator: std.mem.Allocator) !*bytecode.Function {
    const func = try createFunction(allocator, "!=", 2, 3);
    // Compare two values for inequality
    try func.instructions.append(.{ .op = .Eq, .src1 = 0, .src2 = 1, .dst = 2 });
    // TODO: Need a Not instruction
    try func.instructions.append(.{ .op = .Return, .src1 = 2 });
    return func;
}

// Arithmetic methods
fn createAddMethod(allocator: std.mem.Allocator) !*bytecode.Function {
    const func = try createFunction(allocator, "+", 2, 3);
    try func.instructions.append(.{ .op = .Add, .src1 = 0, .src2 = 1, .dst = 2 });
    try func.instructions.append(.{ .op = .Return, .src1 = 2 });
    return func;
}

fn createSubMethod(allocator: std.mem.Allocator) !*bytecode.Function {
    const func = try createFunction(allocator, "-", 2, 3);
    try func.instructions.append(.{ .op = .Sub, .src1 = 0, .src2 = 1, .dst = 2 });
    try func.instructions.append(.{ .op = .Return, .src1 = 2 });
    return func;
}

fn createMulMethod(allocator: std.mem.Allocator) !*bytecode.Function {
    const func = try createFunction(allocator, "*", 2, 3);
    try func.instructions.append(.{ .op = .Mul, .src1 = 0, .src2 = 1, .dst = 2 });
    try func.instructions.append(.{ .op = .Return, .src1 = 2 });
    return func;
}

fn createDivMethod(allocator: std.mem.Allocator) !*bytecode.Function {
    const func = try createFunction(allocator, "/", 2, 3);
    try func.instructions.append(.{ .op = .Div, .src1 = 0, .src2 = 1, .dst = 2 });
    try func.instructions.append(.{ .op = .Return, .src1 = 2 });
    return func;
}

// Comparison methods
fn createLessThanMethod(allocator: std.mem.Allocator) !*bytecode.Function {
    const func = try createFunction(allocator, "<", 2, 3);
    try func.instructions.append(.{ .op = .Lt, .src1 = 0, .src2 = 1, .dst = 2 });
    try func.instructions.append(.{ .op = .Return, .src1 = 2 });
    return func;
}

fn createGreaterThanMethod(allocator: std.mem.Allocator) !*bytecode.Function {
    const func = try createFunction(allocator, ">", 2, 3);
    try func.instructions.append(.{ .op = .Gt, .src1 = 0, .src2 = 1, .dst = 2 });
    try func.instructions.append(.{ .op = .Return, .src1 = 2 });
    return func;
}

fn createLessEqualMethod(allocator: std.mem.Allocator) !*bytecode.Function {
    const func = try createFunction(allocator, "<=", 2, 1);
    // TODO: Need Le instruction
    try func.instructions.append(.{ .op = .Return, .src1 = 0 });
    return func;
}

fn createGreaterEqualMethod(allocator: std.mem.Allocator) !*bytecode.Function {
    const func = try createFunction(allocator, ">=", 2, 1);
    // TODO: Need Ge instruction
    try func.instructions.append(.{ .op = .Return, .src1 = 0 });
    return func;
}

// String methods
fn createLengthMethod(allocator: std.mem.Allocator) !*bytecode.Function {
    const func = try createFunction(allocator, "length", 1, 2);
    // Get length of string/array (self is in register 0)
    try func.instructions.append(.{ .op = .Length, .src1 = 0, .dst = 1 });
    try func.instructions.append(.{ .op = .Return, .src1 = 1 });
    return func;
}

fn createConcatMethod(allocator: std.mem.Allocator) !*bytecode.Function {
    const func = try createFunction(allocator, "++", 2, 3);
    // String concatenation
    try func.instructions.append(.{ .op = .Add, .src1 = 0, .src2 = 1, .dst = 2 });
    try func.instructions.append(.{ .op = .Return, .src1 = 2 });
    return func;
}

fn createSubstringMethod(allocator: std.mem.Allocator) !*bytecode.Function {
    const func = try createFunction(allocator, "substring", 3, 4);
    // Get substring (self in r0, start in r1, end in r2)
    try func.instructions.append(.{ .op = .Substring, .dst = 3, .src1 = 0, .src2 = 1, .immediate = .{ .Int = 2 } });
    try func.instructions.append(.{ .op = .Return, .src1 = 3 });
    return func;
}

// Array methods
fn createPushMethod(allocator: std.mem.Allocator) !*bytecode.Function {
    const func = try createFunction(allocator, "push", 2, 0);
    // TODO: Need ArrayPush instruction
    try func.instructions.append(.{ .op = .Return });
    return func;
}

fn createPopMethod(allocator: std.mem.Allocator) !*bytecode.Function {
    const func = try createFunction(allocator, "pop", 1, 1);
    // TODO: Need ArrayPop instruction
    try func.instructions.append(.{ .op = .Return, .src1 = 0 });
    return func;
}

fn createAtMethod(allocator: std.mem.Allocator) !*bytecode.Function {
    const func = try createFunction(allocator, "at", 2, 3);
    // Get array element (self in r0, index in r1)
    try func.instructions.append(.{ .op = .ArrayGet, .dst = 2, .src1 = 0, .src2 = 1 });
    try func.instructions.append(.{ .op = .Return, .src1 = 2 });
    return func;
}

// Map methods
fn createGetMethod(allocator: std.mem.Allocator) !*bytecode.Function {
    const func = try createFunction(allocator, "get", 2, 3);
    // Get map value (self in r0, key in r1)
    try func.instructions.append(.{ .op = .MapGet, .dst = 2, .src1 = 0, .src2 = 1 });
    try func.instructions.append(.{ .op = .Return, .src1 = 2 });
    return func;
}

fn createPutMethod(allocator: std.mem.Allocator) !*bytecode.Function {
    const func = try createFunction(allocator, "put", 3, 0);
    // Set map value (self in r0, key in r1, value in r2)
    try func.instructions.append(.{ .op = .MapSet, .src1 = 0, .src2 = 1, .immediate = .{ .Int = 2 } });
    try func.instructions.append(.{ .op = .Return, .src1 = 0 }); // Return self for chaining
    return func;
}

fn createHasMethod(allocator: std.mem.Allocator) !*bytecode.Function {
    const func = try createFunction(allocator, "has", 2, 1);
    // TODO: Need MapHas instruction
    try func.instructions.append(.{ .op = .Return, .src1 = 0 });
    return func;
}

fn createKeysMethod(allocator: std.mem.Allocator) !*bytecode.Function {
    const func = try createFunction(allocator, "keys", 1, 1);
    // TODO: Need MapKeys instruction
    try func.instructions.append(.{ .op = .Return, .src1 = 0 });
    return func;
}

// Class methods
fn createNewMethod(allocator: std.mem.Allocator) !*bytecode.Function {
    const func = try createFunction(allocator, "new", 1, 1);
    // This is handled specially by the VM
    try func.instructions.append(.{ .op = .Return, .src1 = 0 });
    return func;
}

fn createNameMethod(allocator: std.mem.Allocator) !*bytecode.Function {
    const func = try createFunction(allocator, "name", 1, 1);
    // TODO: Need ClassName instruction
    try func.instructions.append(.{ .op = .Return, .src1 = 0 });
    return func;
}

fn createParentMethod(allocator: std.mem.Allocator) !*bytecode.Function {
    const func = try createFunction(allocator, "parent", 1, 1);
    // TODO: Need ClassParent instruction
    try func.instructions.append(.{ .op = .Return, .src1 = 0 });
    return func;
}
