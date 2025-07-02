const std = @import("std");
const types = @import("../core/types.zig");
const parser = @import("parser.zig");
const TokenKind = parser.TokenKind;

pub const Type = types.Type;
pub const Value = types.Value;

pub const Literal = struct {
    value: Value,

    pub fn deinit(self: *Literal, allocator: std.mem.Allocator) void {
        switch (self.value) {
            .String => |str| allocator.free(str),
            .Symbol => |sym| allocator.free(sym),
            .Array => |arr| {
                for (arr) |*val| {
                    val.deinit(allocator);
                }
                allocator.free(arr);
            },
            .Map => |*map| {
                var it = map.iterator();
                while (it.next()) |entry| {
                    allocator.free(entry.key_ptr.*);
                    entry.value_ptr.deinit(allocator);
                }
                map.deinit();
            },
            else => {},
        }
    }

    pub fn clone(self: Literal, allocator: std.mem.Allocator) error{OutOfMemory}!Literal {
        return Literal{
            .value = try self.value.clone(allocator),
        };
    }
};

pub const Variable = struct {
    name: []const u8,

    pub fn deinit(self: *Variable, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
    }

    pub fn clone(self: Variable, allocator: std.mem.Allocator) error{OutOfMemory}!Variable {
        return Variable{
            .name = try allocator.dupe(u8, self.name),
        };
    }
};

pub const BinaryOp = struct {
    op: TokenKind,
    left: *Expression,
    right: *Expression,

    pub fn deinit(self: *BinaryOp, allocator: std.mem.Allocator) void {
        // Free the operator identifier if it was allocated
        // Use switch to correctly capture the payload
        switch (self.op) {
            .Ident => |ident| allocator.free(ident),
            else => {}, // Other TokenKind variants don't need freeing here
        }
        // Restore recursive deinit
        self.left.deinit(allocator);
        allocator.destroy(self.left);
        self.right.deinit(allocator);
        allocator.destroy(self.right);
    }

    pub fn clone(self: BinaryOp, allocator: std.mem.Allocator) error{OutOfMemory}!BinaryOp {
        const left = try allocator.create(Expression);
        errdefer allocator.destroy(left); // Destroy container if left.clone fails

        left.* = try self.left.clone(allocator);
        // If left.clone succeeded, add errdefer to clean it up if right side fails
        errdefer left.deinit(allocator);

        const right = try allocator.create(Expression);
        // If right allocation fails, the two errdefers above handle 'left' cleanup
        errdefer allocator.destroy(right); // Destroy container if right.clone fails

        right.* = try self.right.clone(allocator);
        // If right.clone succeeded, add errdefer to clean it up if return fails (unlikely but safe)
        errdefer right.deinit(allocator);

        // If we reach here, all clones succeeded. Ownership is transferred on return.
        // The errdefers are cancelled upon successful return.

        // Clone the operator TokenKind if it's an Ident
        const op_clone: TokenKind = switch (self.op) {
            .Ident => |ident| .{ .Ident = try allocator.dupe(u8, ident) },
            else => self.op, // Other kinds don't need cloning
        };
        // Add errdefer to free the cloned ident if return fails
        errdefer if (op_clone == .Ident) |ident| allocator.free(ident);

        return BinaryOp{
            .op = op_clone,
            .left = left,
            .right = right,
        };
    }
};

pub const FuncCall = struct {
    func: *Expression,
    args: std.ArrayList(*Expression),

    pub fn deinit(self: *FuncCall, allocator: std.mem.Allocator) void {
        // Restore recursive deinit
        self.func.deinit(allocator);
        allocator.destroy(self.func);
        for (self.args.items) |arg| {
            arg.deinit(allocator);
            allocator.destroy(arg);
        }
        self.args.deinit();
    }

    pub fn clone(self: FuncCall, allocator: std.mem.Allocator) error{OutOfMemory}!FuncCall {
        var args_list = std.ArrayList(*Expression).init(allocator);
        // Deinit list structure itself if anything below fails
        errdefer args_list.deinit();

        // Clone arguments, ensuring cleanup on failure
        for (self.args.items) |arg| {
            const new_arg = try allocator.create(Expression);
            // If new_arg allocation fails, loop terminates, top errdefer cleans partially filled args_list

            // Use a block to scope errdefers for this specific argument clone
            {
                errdefer allocator.destroy(new_arg); // If new_arg.clone fails, destroy container
                new_arg.* = try arg.clone(allocator);
                // If new_arg.clone fails, the errdefer above destroys container, top errdefer cleans list

                errdefer new_arg.deinit(allocator); // If append fails, deinit the cloned content
                try args_list.append(new_arg); // Ownership of new_arg transferred to list
                // If append fails, the two errdefers above clean new_arg, top errdefer cleans list
            }
            // If block completes successfully, new_arg is safely in the list
        }
        // If loop completes, args_list owns all cloned args. Add errdefer for args cleanup if func clone fails
        errdefer {
            for (args_list.items) |item| {
                item.deinit(allocator);
                allocator.destroy(item);
            }
        }

        const func = try allocator.create(Expression);
        // If func allocation fails, the args cleanup errdefer runs
        errdefer allocator.destroy(func); // If func.clone fails

        func.* = try self.func.clone(allocator);
        // If func.clone fails, the errdefer above destroys container, args cleanup errdefer runs
        errdefer func.deinit(allocator); // If return fails

        // Success, transfer ownership
        return FuncCall{
            .func = func,
            .args = args_list, // Transfer ownership of list and its contents
        };
    }
};

pub const If = struct {
    condition: *Expression,
    then_branch: *Expression,
    else_branch: *Expression, // Made mandatory

    pub fn deinit(self: *If, allocator: std.mem.Allocator) void {
        // Restore recursive deinit
        self.condition.deinit(allocator);
        allocator.destroy(self.condition);
        self.then_branch.deinit(allocator);
        allocator.destroy(self.then_branch);
        self.else_branch.deinit(allocator); // No longer optional
        allocator.destroy(self.else_branch);
    }

    pub fn clone(self: If, allocator: std.mem.Allocator) error{OutOfMemory}!If {
        const condition = try allocator.create(Expression);
        errdefer allocator.destroy(condition);
        condition.* = try self.condition.clone(allocator);

        const then_branch = try allocator.create(Expression);
        errdefer allocator.destroy(then_branch);
        then_branch.* = try self.then_branch.clone(allocator);

        const else_branch = try allocator.create(Expression); // No longer optional
        errdefer allocator.destroy(else_branch);
        else_branch.* = try self.else_branch.clone(allocator);

        return If{
            .condition = condition,
            .then_branch = then_branch,
            .else_branch = else_branch,
        };
    }
};

pub const FuncParam = struct {
    name: []const u8,
    param_type: ?[]const u8,

    pub fn deinit(self: *FuncParam, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
        if (self.param_type) |param_type| {
            allocator.free(param_type);
        }
    }

    pub fn clone(self: FuncParam, allocator: std.mem.Allocator) error{OutOfMemory}!FuncParam {
        return FuncParam{
            .name = try allocator.dupe(u8, self.name),
            .param_type = if (self.param_type) |pt| try allocator.dupe(u8, pt) else null,
        };
    }
};

pub const FuncDef = struct {
    name: []const u8,
    params: []FuncParam,
    body: *Expression,

    pub fn deinit(self: *FuncDef, allocator: std.mem.Allocator) void {
        // Only free memory directly owned by FuncDef
        allocator.free(self.name);
        for (self.params) |*param| {
            // Call deinit on params as they contain allocated strings
            param.deinit(allocator);
        }
        allocator.free(self.params); // Free the slice itself
        // Restore recursive deinit for body
        self.body.deinit(allocator);
        allocator.destroy(self.body);
    }

    pub fn clone(self: FuncDef, allocator: std.mem.Allocator) error{OutOfMemory}!FuncDef {
        var params = try allocator.alloc(FuncParam, self.params.len);
        errdefer allocator.free(params);

        for (self.params, 0..) |param, i| {
            params[i] = try param.clone(allocator);
        }

        const body = try allocator.create(Expression);
        errdefer allocator.destroy(body);
        body.* = try self.body.clone(allocator);

        return FuncDef{
            .name = try allocator.dupe(u8, self.name),
            .params = params,
            .body = body,
        };
    }
};

pub const VarDecl = struct {
    name: []const u8,
    value: *Expression,

    pub fn deinit(self: *VarDecl, allocator: std.mem.Allocator) void {
        // Only free memory directly owned by VarDecl
        allocator.free(self.name);
        // Restore recursive deinit for value
        self.value.deinit(allocator);
        allocator.destroy(self.value);
    }

    pub fn clone(self: VarDecl, allocator: std.mem.Allocator) error{OutOfMemory}!VarDecl {
        const value = try allocator.create(Expression);
        errdefer allocator.destroy(value);
        value.* = try self.value.clone(allocator);

        return VarDecl{
            .name = try allocator.dupe(u8, self.name),
            .value = value,
        };
    }
};

pub const ArrayLiteral = struct {
    elements: []*Expression,

    pub fn deinit(self: *ArrayLiteral, allocator: std.mem.Allocator) void {
        for (self.elements) |element_ptr| {
            element_ptr.deinit(allocator);
            allocator.destroy(element_ptr);
        }
        allocator.free(self.elements);
    }

    pub fn clone(self: ArrayLiteral, allocator: std.mem.Allocator) error{OutOfMemory}!ArrayLiteral {
        var new_elements = try allocator.alloc(*Expression, self.elements.len);
        errdefer allocator.free(new_elements);

        for (self.elements, 0..) |element_ptr, i| {
            const new_element_ptr = try allocator.create(Expression);
            errdefer allocator.destroy(new_element_ptr);
            new_element_ptr.* = try element_ptr.clone(allocator);
            new_elements[i] = new_element_ptr;
        }

        return ArrayLiteral{ .elements = new_elements };
    }
};

pub const MapEntry = struct {
    key: *Expression,
    value: *Expression,

    pub fn deinit(self: *MapEntry, allocator: std.mem.Allocator) void {
        self.key.deinit(allocator);
        allocator.destroy(self.key);
        self.value.deinit(allocator);
        allocator.destroy(self.value);
    }

    pub fn clone(self: MapEntry, allocator: std.mem.Allocator) error{OutOfMemory}!MapEntry {
        const new_key = try allocator.create(Expression);
        errdefer allocator.destroy(new_key);
        new_key.* = try self.key.clone(allocator);

        const new_value = try allocator.create(Expression);
        errdefer allocator.destroy(new_value);
        new_value.* = try self.value.clone(allocator);

        return MapEntry{ .key = new_key, .value = new_value };
    }
};

pub const MapLiteral = struct {
    entries: []MapEntry,

    pub fn deinit(self: *MapLiteral, allocator: std.mem.Allocator) void {
        for (self.entries) |*entry| {
            entry.deinit(allocator);
        }
        allocator.free(self.entries);
    }

    pub fn clone(self: MapLiteral, allocator: std.mem.Allocator) error{OutOfMemory}!MapLiteral {
        var new_entries = try allocator.alloc(MapEntry, self.entries.len);
        errdefer allocator.free(new_entries);

        for (self.entries, 0..) |entry, i| {
            new_entries[i] = try entry.clone(allocator);
        }

        return MapLiteral{ .entries = new_entries };
    }
};

pub const DoBlock = struct {
    statements: []*Expression,

    pub fn deinit(self: *DoBlock, allocator: std.mem.Allocator) void {
        for (self.statements) |stmt_ptr| {
            stmt_ptr.deinit(allocator);
            allocator.destroy(stmt_ptr);
        }
        allocator.free(self.statements);
    }

    pub fn clone(self: DoBlock, allocator: std.mem.Allocator) error{OutOfMemory}!DoBlock {
        var new_statements = try allocator.alloc(*Expression, self.statements.len);
        errdefer allocator.free(new_statements);

        for (self.statements, 0..) |stmt_ptr, i| {
            const new_stmt_ptr = try allocator.create(Expression);
            errdefer allocator.destroy(new_stmt_ptr);
            new_stmt_ptr.* = try stmt_ptr.clone(allocator);
            new_statements[i] = new_stmt_ptr;
        }

        return DoBlock{ .statements = new_statements };
    }
};

pub const ClassDef = struct {
    name: []const u8,
    fields: []ClassField,
    methods: []ClassMethod,
    parent_class: ?[]const u8, // Name of parent class for inheritance
    traits: [][]const u8, // Names of traits this class implements
    
    pub const ClassField = struct {
        name: []const u8,
        type_annotation: ?[]const u8, // Optional type annotation
        default_value: ?*Expression, // Optional default value
        is_public: bool,
    };
    
    pub const ClassMethod = struct {
        name: []const u8,
        params: []Parameter,
        return_type: ?[]const u8, // Optional return type annotation
        body: *Expression,
        visibility: Visibility,
        method_type: MethodType,
        is_virtual: bool, // Can be overridden
        is_abstract: bool, // Must be implemented by subclasses
        
        pub const Visibility = enum {
            Public,    // Default - accessible everywhere
            Private,   // .-fn - only accessible within the class
            Protected, // .#fn - accessible in class and subclasses
        };
        
        pub const MethodType = enum {
            Regular,    // .fn - standard evaluation with implicit self
            Macro,      // .macro - lazy evaluation with implicit self
            Static,     // .static fn - class method without self
            Function,   // fn - explicit self parameter (unified system)
        };
    };
    
    pub const Parameter = struct {
        name: []const u8,
        type_annotation: ?[]const u8,
        default_value: ?*Expression,
    };
    
    pub fn deinit(self: *ClassDef, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
        
        // Free fields
        for (self.fields) |*field| {
            allocator.free(field.name);
            if (field.type_annotation) |type_ann| {
                allocator.free(type_ann);
            }
            if (field.default_value) |default_val| {
                default_val.deinit(allocator);
                allocator.destroy(default_val);
            }
        }
        allocator.free(self.fields);
        
        // Free methods
        for (self.methods) |*method| {
            allocator.free(method.name);
            if (method.return_type) |ret_type| {
                allocator.free(ret_type);
            }
            
            // Free parameters
            for (method.params) |*param| {
                allocator.free(param.name);
                if (param.type_annotation) |type_ann| {
                    allocator.free(type_ann);
                }
                if (param.default_value) |default_val| {
                    default_val.deinit(allocator);
                    allocator.destroy(default_val);
                }
            }
            allocator.free(method.params);
            
            method.body.deinit(allocator);
            allocator.destroy(method.body);
        }
        allocator.free(self.methods);
        
        if (self.parent_class) |parent| {
            allocator.free(parent);
        }
        
        // Free traits
        for (self.traits) |trait_name| {
            allocator.free(trait_name);
        }
        allocator.free(self.traits);
    }
    
    pub fn clone(self: ClassDef, allocator: std.mem.Allocator) !ClassDef {
        const new_name = try allocator.dupe(u8, self.name);
        
        // Clone fields
        var new_fields = try allocator.alloc(ClassField, self.fields.len);
        errdefer allocator.free(new_fields);
        for (self.fields, 0..) |field, i| {
            new_fields[i] = ClassField{
                .name = try allocator.dupe(u8, field.name),
                .type_annotation = if (field.type_annotation) |type_ann| try allocator.dupe(u8, type_ann) else null,
                .default_value = if (field.default_value) |default_val| blk: {
                    const new_default = try allocator.create(Expression);
                    new_default.* = try default_val.clone(allocator);
                    break :blk new_default;
                } else null,
                .is_public = field.is_public,
            };
        }
        
        // Clone methods
        var new_methods = try allocator.alloc(ClassMethod, self.methods.len);
        errdefer allocator.free(new_methods);
        for (self.methods, 0..) |method, i| {
            // Clone parameters
            var new_params = try allocator.alloc(Parameter, method.params.len);
            errdefer allocator.free(new_params);
            for (method.params, 0..) |param, j| {
                new_params[j] = Parameter{
                    .name = try allocator.dupe(u8, param.name),
                    .type_annotation = if (param.type_annotation) |type_ann| try allocator.dupe(u8, type_ann) else null,
                    .default_value = if (param.default_value) |default_val| blk: {
                        const new_default = try allocator.create(Expression);
                        new_default.* = try default_val.clone(allocator);
                        break :blk new_default;
                    } else null,
                };
            }
            
            const new_body = try allocator.create(Expression);
            new_body.* = try method.body.clone(allocator);
            
            new_methods[i] = ClassMethod{
                .name = try allocator.dupe(u8, method.name),
                .params = new_params,
                .return_type = if (method.return_type) |ret_type| try allocator.dupe(u8, ret_type) else null,
                .body = new_body,
                .visibility = method.visibility,
                .method_type = method.method_type,
                .is_virtual = method.is_virtual,
                .is_abstract = method.is_abstract,
            };
        }
        
        const new_parent_class = if (self.parent_class) |parent| try allocator.dupe(u8, parent) else null;
        
        // Clone traits
        var new_traits = try allocator.alloc([]const u8, self.traits.len);
        errdefer allocator.free(new_traits);
        for (self.traits, 0..) |trait_name, i| {
            new_traits[i] = try allocator.dupe(u8, trait_name);
        }
        
        return ClassDef{
            .name = new_name,
            .fields = new_fields,
            .methods = new_methods,
            .parent_class = new_parent_class,
            .traits = new_traits,
        };
    }
};

pub const MatchExpr = struct {
    scrutinee: *Expression, // The value being matched
    arms: []MatchArm, // Pattern arms
    
    pub const MatchArm = struct {
        pattern: Pattern,
        guard: ?*Expression, // Optional guard clause (when condition)
        body: *Expression, // Expression to execute if pattern matches
    };
    
    pub const Pattern = union(enum) {
        // Literal patterns - match exact values
        Literal: LiteralPattern,
        
        // Variable patterns - bind to variables
        Variable: VariablePattern,
        
        // Wildcard pattern - matches anything
        Wildcard,
        
        // Constructor patterns - match specific types/classes
        Constructor: ConstructorPattern,
        
        // Array patterns - destructure arrays
        Array: ArrayPattern,
        
        // Map patterns - destructure maps/objects  
        Map: MapPattern,
        
        // Or patterns - match any of several patterns
        Or: OrPattern,
        
        // Range patterns - match within ranges
        Range: RangePattern,
        
        pub const LiteralPattern = struct {
            value: *Expression, // The literal value to match
        };
        
        pub const VariablePattern = struct {
            name: []const u8, // Variable name to bind to
            type_annotation: ?[]const u8, // Optional type constraint
        };
        
        pub const ConstructorPattern = struct {
            constructor: []const u8, // Constructor/type name
            fields: []Pattern, // Nested patterns for fields
        };
        
        pub const ArrayPattern = struct {
            elements: []Pattern, // Patterns for array elements
            rest: ?[]const u8, // Optional rest pattern (..rest)
        };
        
        pub const MapPattern = struct {
            fields: []MapFieldPattern, // Key-value patterns
            rest: ?[]const u8, // Optional rest pattern
            
            pub const MapFieldPattern = struct {
                key: []const u8, // Map key
                pattern: Pattern, // Pattern for the value
            };
        };
        
        pub const OrPattern = struct {
            patterns: []Pattern, // Alternative patterns
        };
        
        pub const RangePattern = struct {
            start: *Expression, // Range start
            end: *Expression, // Range end
            inclusive: bool, // Whether end is inclusive
        };
        
        pub fn deinit(self: *Pattern, allocator: std.mem.Allocator) void {
            switch (self.*) {
                .Literal => |*lit| {
                    lit.value.deinit(allocator);
                    allocator.destroy(lit.value);
                },
                .Variable => |*var_pat| {
                    allocator.free(var_pat.name);
                    if (var_pat.type_annotation) |type_ann| {
                        allocator.free(type_ann);
                    }
                },
                .Wildcard => {},
                .Constructor => |*ctor| {
                    allocator.free(ctor.constructor);
                    for (ctor.fields) |*field| {
                        field.deinit(allocator);
                    }
                    allocator.free(ctor.fields);
                },
                .Array => |*arr| {
                    for (arr.elements) |*elem| {
                        elem.deinit(allocator);
                    }
                    allocator.free(arr.elements);
                    if (arr.rest) |rest| {
                        allocator.free(rest);
                    }
                },
                .Map => |*map| {
                    for (map.fields) |*field| {
                        allocator.free(field.key);
                        field.pattern.deinit(allocator);
                    }
                    allocator.free(map.fields);
                    if (map.rest) |rest| {
                        allocator.free(rest);
                    }
                },
                .Or => |*or_pat| {
                    for (or_pat.patterns) |*pattern| {
                        pattern.deinit(allocator);
                    }
                    allocator.free(or_pat.patterns);
                },
                .Range => |*range| {
                    range.start.deinit(allocator);
                    allocator.destroy(range.start);
                    range.end.deinit(allocator);
                    allocator.destroy(range.end);
                },
            }
        }
        
        pub fn clone(self: Pattern, allocator: std.mem.Allocator) !Pattern {
            return switch (self) {
                .Literal => |lit| blk: {
                    const new_value = try allocator.create(Expression);
                    new_value.* = try lit.value.clone(allocator);
                    break :blk Pattern{ .Literal = .{ .value = new_value } };
                },
                .Variable => |var_pat| Pattern{ .Variable = .{
                    .name = try allocator.dupe(u8, var_pat.name),
                    .type_annotation = if (var_pat.type_annotation) |type_ann| try allocator.dupe(u8, type_ann) else null,
                } },
                .Wildcard => Pattern{ .Wildcard = {} },
                .Constructor => |ctor| blk: {
                    var new_fields = try allocator.alloc(Pattern, ctor.fields.len);
                    errdefer allocator.free(new_fields);
                    for (ctor.fields, 0..) |field, i| {
                        new_fields[i] = try field.clone(allocator);
                    }
                    break :blk Pattern{ .Constructor = .{
                        .constructor = try allocator.dupe(u8, ctor.constructor),
                        .fields = new_fields,
                    } };
                },
                .Array => |arr| blk: {
                    var new_elements = try allocator.alloc(Pattern, arr.elements.len);
                    errdefer allocator.free(new_elements);
                    for (arr.elements, 0..) |elem, i| {
                        new_elements[i] = try elem.clone(allocator);
                    }
                    break :blk Pattern{ .Array = .{
                        .elements = new_elements,
                        .rest = if (arr.rest) |rest| try allocator.dupe(u8, rest) else null,
                    } };
                },
                .Map => |map| blk: {
                    var new_fields = try allocator.alloc(MapPattern.MapFieldPattern, map.fields.len);
                    errdefer allocator.free(new_fields);
                    for (map.fields, 0..) |field, i| {
                        new_fields[i] = MapPattern.MapFieldPattern{
                            .key = try allocator.dupe(u8, field.key),
                            .pattern = try field.pattern.clone(allocator),
                        };
                    }
                    break :blk Pattern{ .Map = .{
                        .fields = new_fields,
                        .rest = if (map.rest) |rest| try allocator.dupe(u8, rest) else null,
                    } };
                },
                .Or => |or_pat| blk: {
                    var new_patterns = try allocator.alloc(Pattern, or_pat.patterns.len);
                    errdefer allocator.free(new_patterns);
                    for (or_pat.patterns, 0..) |pattern, i| {
                        new_patterns[i] = try pattern.clone(allocator);
                    }
                    break :blk Pattern{ .Or = .{ .patterns = new_patterns } };
                },
                .Range => |range| blk: {
                    const new_start = try allocator.create(Expression);
                    new_start.* = try range.start.clone(allocator);
                    const new_end = try allocator.create(Expression);
                    new_end.* = try range.end.clone(allocator);
                    break :blk Pattern{ .Range = .{
                        .start = new_start,
                        .end = new_end,
                        .inclusive = range.inclusive,
                    } };
                },
            };
        }
    };
    
    pub fn deinit(self: *MatchExpr, allocator: std.mem.Allocator) void {
        self.scrutinee.deinit(allocator);
        allocator.destroy(self.scrutinee);
        
        for (self.arms) |*arm| {
            arm.pattern.deinit(allocator);
            if (arm.guard) |guard| {
                guard.deinit(allocator);
                allocator.destroy(guard);
            }
            arm.body.deinit(allocator);
            allocator.destroy(arm.body);
        }
        allocator.free(self.arms);
    }
    
    pub fn clone(self: MatchExpr, allocator: std.mem.Allocator) !MatchExpr {
        const new_scrutinee = try allocator.create(Expression);
        new_scrutinee.* = try self.scrutinee.clone(allocator);
        
        var new_arms = try allocator.alloc(MatchArm, self.arms.len);
        errdefer allocator.free(new_arms);
        
        for (self.arms, 0..) |arm, i| {
            const new_guard = if (arm.guard) |guard| blk: {
                const new_guard_expr = try allocator.create(Expression);
                new_guard_expr.* = try guard.clone(allocator);
                break :blk new_guard_expr;
            } else null;
            
            const new_body = try allocator.create(Expression);
            new_body.* = try arm.body.clone(allocator);
            
            new_arms[i] = MatchArm{
                .pattern = try arm.pattern.clone(allocator),
                .guard = new_guard,
                .body = new_body,
            };
        }
        
        return MatchExpr{
            .scrutinee = new_scrutinee,
            .arms = new_arms,
        };
    }
};

pub const InstanceCreation = struct {
    class_name: []const u8,
    args: std.ArrayList(*Expression),
    
    pub fn deinit(self: *InstanceCreation, allocator: std.mem.Allocator) void {
        allocator.free(self.class_name);
        for (self.args.items) |arg| {
            arg.deinit(allocator);
            allocator.destroy(arg);
        }
        self.args.deinit();
    }
    
    pub fn clone(self: InstanceCreation, allocator: std.mem.Allocator) !InstanceCreation {
        const new_class_name = try allocator.dupe(u8, self.class_name);
        
        var new_args = std.ArrayList(*Expression).init(allocator);
        errdefer new_args.deinit();
        
        for (self.args.items) |arg| {
            const new_arg = try allocator.create(Expression);
            new_arg.* = try arg.clone(allocator);
            try new_args.append(new_arg);
        }
        
        return InstanceCreation{
            .class_name = new_class_name,
            .args = new_args,
        };
    }
};

// FieldAccess and FieldAssignment have been replaced by PathAccess and PathAssignment

pub const MethodCall = struct {
    object: *Expression,
    method_name: []const u8,
    args: std.ArrayList(*Expression),
    
    pub fn deinit(self: *MethodCall, allocator: std.mem.Allocator) void {
        self.object.deinit(allocator);
        allocator.destroy(self.object);
        allocator.free(self.method_name);
        for (self.args.items) |arg| {
            arg.deinit(allocator);
            allocator.destroy(arg);
        }
        self.args.deinit();
    }
    
    pub fn clone(self: MethodCall, allocator: std.mem.Allocator) !MethodCall {
        const new_object = try allocator.create(Expression);
        new_object.* = try self.object.clone(allocator);
        
        var new_args = std.ArrayList(*Expression).init(allocator);
        errdefer new_args.deinit();
        
        for (self.args.items) |arg| {
            const new_arg = try allocator.create(Expression);
            new_arg.* = try arg.clone(allocator);
            try new_args.append(new_arg);
        }
        
        return MethodCall{
            .object = new_object,
            .method_name = try allocator.dupe(u8, self.method_name),
            .args = new_args,
        };
    }
};

pub const ModuleDef = struct {
    name: []const u8, // Module name
    body: *Expression, // Module body (usually a DoBlock)
    exports: [][]const u8, // Explicitly exported symbols
    imports: []ImportSpec, // Import specifications
    
    pub const ImportSpec = struct {
        module_path: []const u8, // Path to the module
        alias: ?[]const u8, // Optional alias for the module
        items: ?[]ImportItem, // Specific items to import (None = import all)
        
        pub const ImportItem = struct {
            name: []const u8, // Name in source module
            alias: ?[]const u8, // Local alias
        };
    };
    
    pub fn deinit(self: *ModuleDef, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
        self.body.deinit(allocator);
        allocator.destroy(self.body);
        
        for (self.exports) |export_name| {
            allocator.free(export_name);
        }
        allocator.free(self.exports);
        
        for (self.imports) |*import_spec| {
            allocator.free(import_spec.module_path);
            if (import_spec.alias) |alias| {
                allocator.free(alias);
            }
            if (import_spec.items) |items| {
                for (items) |*item| {
                    allocator.free(item.name);
                    if (item.alias) |alias| {
                        allocator.free(alias);
                    }
                }
                allocator.free(items);
            }
        }
        allocator.free(self.imports);
    }
    
    pub fn clone(self: ModuleDef, allocator: std.mem.Allocator) !ModuleDef {
        const new_body = try allocator.create(Expression);
        new_body.* = try self.body.clone(allocator);
        
        var new_exports = try allocator.alloc([]const u8, self.exports.len);
        errdefer allocator.free(new_exports);
        for (self.exports, 0..) |export_name, i| {
            new_exports[i] = try allocator.dupe(u8, export_name);
        }
        
        var new_imports = try allocator.alloc(ImportSpec, self.imports.len);
        errdefer allocator.free(new_imports);
        for (self.imports, 0..) |import_spec, i| {
            const new_items = if (import_spec.items) |items| blk: {
                var items_copy = try allocator.alloc(ImportSpec.ImportItem, items.len);
                for (items, 0..) |item, j| {
                    items_copy[j] = ImportSpec.ImportItem{
                        .name = try allocator.dupe(u8, item.name),
                        .alias = if (item.alias) |alias| try allocator.dupe(u8, alias) else null,
                    };
                }
                break :blk items_copy;
            } else null;
            
            new_imports[i] = ImportSpec{
                .module_path = try allocator.dupe(u8, import_spec.module_path),
                .alias = if (import_spec.alias) |alias| try allocator.dupe(u8, alias) else null,
                .items = new_items,
            };
        }
        
        return ModuleDef{
            .name = try allocator.dupe(u8, self.name),
            .body = new_body,
            .exports = new_exports,
            .imports = new_imports,
        };
    }
};

pub const ImportStmt = struct {
    module_path: []const u8, // Path to the module to import
    alias: ?[]const u8, // Optional alias for the entire module
    items: ?[]ImportItem, // Specific items to import
    
    pub const ImportItem = struct {
        name: []const u8, // Name in source module  
        alias: ?[]const u8, // Local alias
    };
    
    pub fn deinit(self: *ImportStmt, allocator: std.mem.Allocator) void {
        allocator.free(self.module_path);
        if (self.alias) |alias| {
            allocator.free(alias);
        }
        if (self.items) |items| {
            for (items) |*item| {
                allocator.free(item.name);
                if (item.alias) |alias| {
                    allocator.free(alias);
                }
            }
            allocator.free(items);
        }
    }
    
    pub fn clone(self: ImportStmt, allocator: std.mem.Allocator) !ImportStmt {
        const new_items = if (self.items) |items| blk: {
            var items_copy = try allocator.alloc(ImportItem, items.len);
            for (items, 0..) |item, i| {
                items_copy[i] = ImportItem{
                    .name = try allocator.dupe(u8, item.name),
                    .alias = if (item.alias) |alias| try allocator.dupe(u8, alias) else null,
                };
            }
            break :blk items_copy;
        } else null;
        
        return ImportStmt{
            .module_path = try allocator.dupe(u8, self.module_path),
            .alias = if (self.alias) |alias| try allocator.dupe(u8, alias) else null,
            .items = new_items,
        };
    }
};

pub const PseudoMacroDef = struct {
    name: []const u8,
    params: []MacroParam,
    body: *Expression,
    
    pub const MacroParam = struct {
        name: []const u8,
        is_variadic: bool, // For rest parameters
    };
    
    pub fn deinit(self: *PseudoMacroDef, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
        for (self.params) |*param| {
            allocator.free(param.name);
        }
        allocator.free(self.params);
        self.body.deinit(allocator);
        allocator.destroy(self.body);
    }
    
    pub fn clone(self: PseudoMacroDef, allocator: std.mem.Allocator) !PseudoMacroDef {
        const new_name = try allocator.dupe(u8, self.name);
        
        var new_params = try allocator.alloc(MacroParam, self.params.len);
        errdefer allocator.free(new_params);
        for (self.params, 0..) |param, i| {
            new_params[i] = MacroParam{
                .name = try allocator.dupe(u8, param.name),
                .is_variadic = param.is_variadic,
            };
        }
        
        const new_body = try allocator.create(Expression);
        new_body.* = try self.body.clone(allocator);
        
        return PseudoMacroDef{
            .name = new_name,
            .params = new_params,
            .body = new_body,
        };
    }
};

pub const PseudoMacroCall = struct {
    macro: *Expression, // The macro to call (can be a method macro)
    args: []MacroArg, // Arguments (not evaluated at call time)
    
    pub const MacroArg = struct {
        expr: *Expression, // The expression (stored for lazy evaluation)
    };
    
    pub fn deinit(self: *PseudoMacroCall, allocator: std.mem.Allocator) void {
        self.macro.deinit(allocator);
        allocator.destroy(self.macro);
        for (self.args) |*arg| {
            arg.expr.deinit(allocator);
            allocator.destroy(arg.expr);
        }
        allocator.free(self.args);
    }
    
    pub fn clone(self: PseudoMacroCall, allocator: std.mem.Allocator) !PseudoMacroCall {
        const new_macro = try allocator.create(Expression);
        new_macro.* = try self.macro.clone(allocator);
        
        var new_args = try allocator.alloc(MacroArg, self.args.len);
        errdefer allocator.free(new_args);
        for (self.args, 0..) |arg, i| {
            const new_expr = try allocator.create(Expression);
            new_expr.* = try arg.expr.clone(allocator);
            new_args[i] = MacroArg{ .expr = new_expr };
        }
        
        return PseudoMacroCall{
            .macro = new_macro,
            .args = new_args,
        };
    }
};

pub const CExternDecl = struct {
    name: []const u8, // Function name in Gene
    params: []CParam,
    return_type: ?[]const u8, // null for void
    lib: []const u8, // Library name
    symbol: ?[]const u8, // Optional actual symbol name in C
    calling_convention: ?[]const u8, // Optional calling convention
    is_variadic: bool,
    
    pub const CParam = struct {
        name: []const u8,
        c_type: []const u8,
    };
    
    pub fn deinit(self: *CExternDecl, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
        for (self.params) |*param| {
            allocator.free(param.name);
            allocator.free(param.c_type);
        }
        allocator.free(self.params);
        if (self.return_type) |ret| allocator.free(ret);
        allocator.free(self.lib);
        if (self.symbol) |sym| allocator.free(sym);
        if (self.calling_convention) |cc| allocator.free(cc);
    }
    
    pub fn clone(self: CExternDecl, allocator: std.mem.Allocator) !CExternDecl {
        const new_name = try allocator.dupe(u8, self.name);
        
        var new_params = try allocator.alloc(CParam, self.params.len);
        errdefer allocator.free(new_params);
        for (self.params, 0..) |param, i| {
            new_params[i] = CParam{
                .name = try allocator.dupe(u8, param.name),
                .c_type = try allocator.dupe(u8, param.c_type),
            };
        }
        
        return CExternDecl{
            .name = new_name,
            .params = new_params,
            .return_type = if (self.return_type) |ret| try allocator.dupe(u8, ret) else null,
            .lib = try allocator.dupe(u8, self.lib),
            .symbol = if (self.symbol) |sym| try allocator.dupe(u8, sym) else null,
            .calling_convention = if (self.calling_convention) |cc| try allocator.dupe(u8, cc) else null,
            .is_variadic = self.is_variadic,
        };
    }
};

pub const CStructDecl = struct {
    name: []const u8,
    fields: []CField,
    is_packed: bool,
    alignment: ?usize,
    
    pub const CField = struct {
        name: []const u8,
        c_type: []const u8,
        bit_size: ?u8, // For bit fields
    };
    
    pub fn deinit(self: *CStructDecl, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
        for (self.fields) |*field| {
            allocator.free(field.name);
            allocator.free(field.c_type);
        }
        allocator.free(self.fields);
    }
    
    pub fn clone(self: CStructDecl, allocator: std.mem.Allocator) !CStructDecl {
        const new_name = try allocator.dupe(u8, self.name);
        
        var new_fields = try allocator.alloc(CField, self.fields.len);
        errdefer allocator.free(new_fields);
        for (self.fields, 0..) |field, i| {
            new_fields[i] = CField{
                .name = try allocator.dupe(u8, field.name),
                .c_type = try allocator.dupe(u8, field.c_type),
                .bit_size = field.bit_size,
            };
        }
        
        return CStructDecl{
            .name = new_name,
            .fields = new_fields,
            .is_packed = self.is_packed,
            .alignment = self.alignment,
        };
    }
};

pub const CTypeDecl = struct {
    name: []const u8, // Type alias name
    c_type: []const u8, // Underlying C type
    
    pub fn deinit(self: *CTypeDecl, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
        allocator.free(self.c_type);
    }
    
    pub fn clone(self: CTypeDecl, allocator: std.mem.Allocator) !CTypeDecl {
        return CTypeDecl{
            .name = try allocator.dupe(u8, self.name),
            .c_type = try allocator.dupe(u8, self.c_type),
        };
    }
};

pub const ExportStmt = struct {
    items: []ExportItem, // Items to export
    
    pub const ExportItem = struct {
        name: []const u8, // Name to export
        alias: ?[]const u8, // Optional export alias
    };
    
    pub fn deinit(self: *ExportStmt, allocator: std.mem.Allocator) void {
        for (self.items) |*item| {
            allocator.free(item.name);
            if (item.alias) |alias| {
                allocator.free(alias);
            }
        }
        allocator.free(self.items);
    }
    
    pub fn clone(self: ExportStmt, allocator: std.mem.Allocator) !ExportStmt {
        var new_items = try allocator.alloc(ExportItem, self.items.len);
        errdefer allocator.free(new_items);
        for (self.items, 0..) |item, i| {
            new_items[i] = ExportItem{
                .name = try allocator.dupe(u8, item.name),
                .alias = if (item.alias) |alias| try allocator.dupe(u8, alias) else null,
            };
        }
        
        return ExportStmt{
            .items = new_items,
        };
    }
};

pub const NamespaceDecl = struct {
    name: []const u8, // Namespace path (e.g., "x" or "x/y")
    body: *Expression, // Namespace body (usually a DoBlock)
    
    pub fn deinit(self: *NamespaceDecl, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
        self.body.deinit(allocator);
        allocator.destroy(self.body);
    }
    
    pub fn clone(self: NamespaceDecl, allocator: std.mem.Allocator) !NamespaceDecl {
        const new_body = try allocator.create(Expression);
        new_body.* = try self.body.clone(allocator);
        
        return NamespaceDecl{
            .name = try allocator.dupe(u8, self.name),
            .body = new_body,
        };
    }
};

pub const Return = struct {
    value: ?*Expression, // Optional return value (null for bare return)
    
    pub fn deinit(self: *Return, allocator: std.mem.Allocator) void {
        if (self.value) |val| {
            val.deinit(allocator);
            allocator.destroy(val);
        }
    }
    
    pub fn clone(self: Return, allocator: std.mem.Allocator) !Return {
        if (self.value) |val| {
            const new_value = try allocator.create(Expression);
            new_value.* = try val.clone(allocator);
            return Return{ .value = new_value };
        }
        return Return{ .value = null };
    }
};

pub const ForLoop = struct {
    iterator: []const u8, // Variable name for the iterator
    iterable: *Expression, // The collection to iterate over
    body: *Expression, // Loop body
    
    pub fn deinit(self: *ForLoop, allocator: std.mem.Allocator) void {
        allocator.free(self.iterator);
        self.iterable.deinit(allocator);
        allocator.destroy(self.iterable);
        self.body.deinit(allocator);
        allocator.destroy(self.body);
    }
    
    pub fn clone(self: ForLoop, allocator: std.mem.Allocator) !ForLoop {
        const new_iterable = try allocator.create(Expression);
        new_iterable.* = try self.iterable.clone(allocator);
        
        const new_body = try allocator.create(Expression);
        new_body.* = try self.body.clone(allocator);
        
        return ForLoop{
            .iterator = try allocator.dupe(u8, self.iterator),
            .iterable = new_iterable,
            .body = new_body,
        };
    }
};

pub const PathAccess = struct {
    object: *Expression, // The object being accessed
    path: *Expression,   // The path or key used for access

    pub fn deinit(self: *PathAccess, allocator: std.mem.Allocator) void {
        self.object.deinit(allocator);
        allocator.destroy(self.object);
        self.path.deinit(allocator);
        allocator.destroy(self.path);
    }

    pub fn clone(self: PathAccess, allocator: std.mem.Allocator) !PathAccess {
        const new_object = try allocator.create(Expression);
        new_object.* = try self.object.clone(allocator);

        const new_path = try allocator.create(Expression);
        new_path.* = try self.path.clone(allocator);

        return PathAccess{
            .object = new_object,
            .path = new_path,
        };
    }
};

pub const PathAssignment = struct {
    path: *Expression, // The path expression to assign to
    value: *Expression,

    pub fn deinit(self: *PathAssignment, allocator: std.mem.Allocator) void {
        self.path.deinit(allocator);
        allocator.destroy(self.path);
        self.value.deinit(allocator);
        allocator.destroy(self.value);
    }

    pub fn clone(self: PathAssignment, allocator: std.mem.Allocator) !PathAssignment {
        const new_path = try allocator.create(Expression);
        new_path.* = try self.path.clone(allocator);

        const new_value = try allocator.create(Expression);
        new_value.* = try self.value.clone(allocator);

        return PathAssignment{
            .path = new_path,
            .value = new_value,
        };
    }
};

pub const TryExpr = struct {
    body: *Expression, // The try block
    catch_clauses: []CatchClause, // List of catch clauses
    finally_block: ?*Expression, // Optional finally block
    
    pub const CatchClause = struct {
        error_var: ?[]const u8, // Optional variable name to bind the error
        error_type: ?[]const u8, // Optional error type to match
        body: *Expression, // The catch block body
        
        pub fn deinit(self: *CatchClause, allocator: std.mem.Allocator) void {
            if (self.error_var) |var_name| {
                allocator.free(var_name);
            }
            if (self.error_type) |type_name| {
                allocator.free(type_name);
            }
            self.body.deinit(allocator);
            allocator.destroy(self.body);
        }
        
        pub fn clone(self: CatchClause, allocator: std.mem.Allocator) !CatchClause {
            const new_body = try allocator.create(Expression);
            new_body.* = try self.body.clone(allocator);
            
            return CatchClause{
                .error_var = if (self.error_var) |var_name| try allocator.dupe(u8, var_name) else null,
                .error_type = if (self.error_type) |type_name| try allocator.dupe(u8, type_name) else null,
                .body = new_body,
            };
        }
    };
    
    pub fn deinit(self: *TryExpr, allocator: std.mem.Allocator) void {
        self.body.deinit(allocator);
        allocator.destroy(self.body);
        
        for (self.catch_clauses) |*clause| {
            clause.deinit(allocator);
        }
        allocator.free(self.catch_clauses);
        
        if (self.finally_block) |finally| {
            finally.deinit(allocator);
            allocator.destroy(finally);
        }
    }
    
    pub fn clone(self: TryExpr, allocator: std.mem.Allocator) !TryExpr {
        const new_body = try allocator.create(Expression);
        new_body.* = try self.body.clone(allocator);
        
        var new_catch_clauses = try allocator.alloc(CatchClause, self.catch_clauses.len);
        errdefer allocator.free(new_catch_clauses);
        
        for (self.catch_clauses, 0..) |clause, i| {
            new_catch_clauses[i] = try clause.clone(allocator);
        }
        
        const new_finally = if (self.finally_block) |finally| blk: {
            const new_finally_expr = try allocator.create(Expression);
            new_finally_expr.* = try finally.clone(allocator);
            break :blk new_finally_expr;
        } else null;
        
        return TryExpr{
            .body = new_body,
            .catch_clauses = new_catch_clauses,
            .finally_block = new_finally,
        };
    }
};

pub const ThrowExpr = struct {
    value: *Expression, // The value/error to throw
    
    pub fn deinit(self: *ThrowExpr, allocator: std.mem.Allocator) void {
        self.value.deinit(allocator);
        allocator.destroy(self.value);
    }
    
    pub fn clone(self: ThrowExpr, allocator: std.mem.Allocator) !ThrowExpr {
        const new_value = try allocator.create(Expression);
        new_value.* = try self.value.clone(allocator);
        
        return ThrowExpr{
            .value = new_value,
        };
    }
};

pub const Expression = union(enum) {
    Literal: Literal,
    Variable: Variable,
    BinaryOp: BinaryOp,
    If: If,
    FuncCall: FuncCall,
    FuncDef: FuncDef,
    VarDecl: VarDecl,
    SimpleFuncDef: SimpleFuncDef,
    ArrayLiteral: ArrayLiteral, // New
    MapLiteral: MapLiteral, // New
    DoBlock: DoBlock, // New
    ClassDef: ClassDef, // New - Class definitions for OOP
    InstanceCreation: InstanceCreation, // New - Object instantiation
    PathAccess: PathAccess, // New - Field access with /
    PathAssignment: PathAssignment, // New - Field assignment with /
    MethodCall: MethodCall, // New - Method calls with .
    MatchExpr: MatchExpr, // New - Pattern matching expressions
    ModuleDef: ModuleDef, // New - Module definitions
    ImportStmt: ImportStmt, // New - Import statements
    ExportStmt: ExportStmt, // New - Export statements
    PseudoMacroDef: PseudoMacroDef, // New - Pseudo macro definitions
    PseudoMacroCall: PseudoMacroCall, // New - Pseudo macro calls
    CExternDecl: CExternDecl, // New - FFI external function declaration
    CStructDecl: CStructDecl, // New - FFI struct declaration
    CTypeDecl: CTypeDecl, // New - FFI type declaration
    NamespaceDecl: NamespaceDecl, // New - Namespace declaration
    ForLoop: ForLoop, // New - For-in loops
    Return: Return, // New - Return statement
    TryExpr: TryExpr, // New - Try/catch/finally expression
    ThrowExpr: ThrowExpr, // New - Throw expression

    pub fn deinit(self: *Expression, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .Literal => |*lit| lit.deinit(allocator),
            .Variable => |*var_expr| var_expr.deinit(allocator),
            .BinaryOp => |*bin_op| bin_op.deinit(allocator),
            .If => |*if_expr| if_expr.deinit(allocator),
            .FuncCall => |*func_call| func_call.deinit(allocator),
            .FuncDef => |*func_def| func_def.deinit(allocator),
            .VarDecl => |*var_decl| var_decl.deinit(allocator),
            .SimpleFuncDef => |*func_def| func_def.deinit(allocator),
            .ArrayLiteral => |*arr_lit| arr_lit.deinit(allocator), // New
            .MapLiteral => |*map_lit| map_lit.deinit(allocator), // New
            .DoBlock => |*do_block| do_block.deinit(allocator), // New
            .ClassDef => |*class_def| class_def.deinit(allocator), // New
            .InstanceCreation => |*inst| inst.deinit(allocator), // New
            .PathAccess => |*path| path.deinit(allocator), // New
            .PathAssignment => |*assign| assign.deinit(allocator), // New
            .MethodCall => |*call| call.deinit(allocator), // New
            .MatchExpr => |*match_expr| match_expr.deinit(allocator), // New
            .ModuleDef => |*module_def| module_def.deinit(allocator), // New
            .ImportStmt => |*import_stmt| import_stmt.deinit(allocator), // New
            .ExportStmt => |*export_stmt| export_stmt.deinit(allocator), // New
            .PseudoMacroDef => |*macro_def| macro_def.deinit(allocator), // New
            .PseudoMacroCall => |*macro_call| macro_call.deinit(allocator), // New
            .CExternDecl => |*extern_decl| extern_decl.deinit(allocator), // New
            .CStructDecl => |*struct_decl| struct_decl.deinit(allocator), // New
            .CTypeDecl => |*type_decl| type_decl.deinit(allocator), // New
            .NamespaceDecl => |*ns_decl| ns_decl.deinit(allocator), // New
            .ForLoop => |*for_loop| for_loop.deinit(allocator), // New
            .Return => |*ret| ret.deinit(allocator), // New
            .TryExpr => |*try_expr| try_expr.deinit(allocator), // New
            .ThrowExpr => |*throw_expr| throw_expr.deinit(allocator), // New
        }
    }

    pub fn clone(self: Expression, allocator: std.mem.Allocator) error{OutOfMemory}!Expression {
        return switch (self) {
            .Literal => |lit| Expression{ .Literal = try lit.clone(allocator) },
            .Variable => |var_expr| Expression{ .Variable = try var_expr.clone(allocator) },
            .BinaryOp => |bin_op| Expression{ .BinaryOp = try bin_op.clone(allocator) },
            .If => |if_expr| Expression{ .If = try if_expr.clone(allocator) },
            .FuncCall => |func_call| Expression{ .FuncCall = try func_call.clone(allocator) },
            .FuncDef => |func_def| Expression{ .FuncDef = try func_def.clone(allocator) },
            .VarDecl => |var_decl| Expression{ .VarDecl = try var_decl.clone(allocator) },
            .SimpleFuncDef => |func_def| Expression{ .SimpleFuncDef = try func_def.clone(allocator) },
            .ArrayLiteral => |arr_lit| Expression{ .ArrayLiteral = try arr_lit.clone(allocator) }, // New
            .MapLiteral => |map_lit| Expression{ .MapLiteral = try map_lit.clone(allocator) }, // New
            .DoBlock => |do_block| Expression{ .DoBlock = try do_block.clone(allocator) }, // New
            .ClassDef => |class_def| Expression{ .ClassDef = try class_def.clone(allocator) }, // New
            .InstanceCreation => |inst| Expression{ .InstanceCreation = try inst.clone(allocator) }, // New
            .PathAccess => |path| Expression{ .PathAccess = try path.clone(allocator) }, // New
            .PathAssignment => |assign| Expression{ .PathAssignment = try assign.clone(allocator) }, // New
            .MethodCall => |call| Expression{ .MethodCall = try call.clone(allocator) }, // New
            .MatchExpr => |match_expr| Expression{ .MatchExpr = try match_expr.clone(allocator) }, // New
            .ModuleDef => |module_def| Expression{ .ModuleDef = try module_def.clone(allocator) }, // New
            .ImportStmt => |import_stmt| Expression{ .ImportStmt = try import_stmt.clone(allocator) }, // New
            .ExportStmt => |export_stmt| Expression{ .ExportStmt = try export_stmt.clone(allocator) }, // New
            .PseudoMacroDef => |macro_def| Expression{ .PseudoMacroDef = try macro_def.clone(allocator) }, // New
            .PseudoMacroCall => |macro_call| Expression{ .PseudoMacroCall = try macro_call.clone(allocator) }, // New
            .CExternDecl => |extern_decl| Expression{ .CExternDecl = try extern_decl.clone(allocator) }, // New
            .CStructDecl => |struct_decl| Expression{ .CStructDecl = try struct_decl.clone(allocator) }, // New
            .CTypeDecl => |type_decl| Expression{ .CTypeDecl = try type_decl.clone(allocator) }, // New
            .NamespaceDecl => |ns_decl| Expression{ .NamespaceDecl = try ns_decl.clone(allocator) }, // New
            .ForLoop => |for_loop| Expression{ .ForLoop = try for_loop.clone(allocator) }, // New
            .Return => |ret| Expression{ .Return = try ret.clone(allocator) }, // New
            .TryExpr => |try_expr| Expression{ .TryExpr = try try_expr.clone(allocator) }, // New
            .ThrowExpr => |throw_expr| Expression{ .ThrowExpr = try throw_expr.clone(allocator) }, // New
        };
    }
};

pub const SimpleFuncDef = struct {
    name: []const u8,
    param_count: usize,
    body_literal: i64,

    pub fn deinit(self: *SimpleFuncDef, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
    }

    pub fn clone(self: SimpleFuncDef, allocator: std.mem.Allocator) error{OutOfMemory}!SimpleFuncDef {
        return SimpleFuncDef{
            .name = try allocator.dupe(u8, self.name),
            .param_count = self.param_count,
            .body_literal = self.body_literal,
        };
    }

    pub fn getName(self: SimpleFuncDef) []const u8 {
        return self.name;
    }
};

pub const AstNode = union(enum) {
    Expression: Expression,

    pub fn deinit(self: *AstNode, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .Expression => |*expr| expr.deinit(allocator),
        }
    }

    pub fn clone(self: AstNode, allocator: std.mem.Allocator) error{OutOfMemory}!AstNode {
        return switch (self) {
            .Expression => |expr| AstNode{ .Expression = try expr.clone(allocator) },
        };
    }
};
