const std = @import("std");
const parser = @import("../frontend/parser.zig");
const ast = @import("../frontend/ast.zig");

pub const Type = enum {
    void,
    bool,
    int,
    float,
    string,
    function,
    any, // Type that matches any value
};

pub const HIR = struct {
    allocator: std.mem.Allocator,
    functions: std.ArrayList(Function),
    imports: std.ArrayList(*ImportStmt),
    ffi_functions: std.ArrayList(*FFIFunction),
    ffi_structs: std.ArrayList(*FFIStruct),
    ffi_types: std.ArrayList(*FFIType),

    pub fn init(allocator: std.mem.Allocator) HIR {
        return HIR{
            .allocator = allocator,
            .functions = std.ArrayList(Function).init(allocator),
            .imports = std.ArrayList(*ImportStmt).init(allocator),
            .ffi_functions = std.ArrayList(*FFIFunction).init(allocator),
            .ffi_structs = std.ArrayList(*FFIStruct).init(allocator),
            .ffi_types = std.ArrayList(*FFIType).init(allocator),
        };
    }

    /// Transfer ownership of FFI functions to the caller
    pub fn takeFFIFunctions(self: *HIR) std.ArrayList(*FFIFunction) {
        const ffi_funcs = self.ffi_functions;
        self.ffi_functions = std.ArrayList(*FFIFunction).init(self.allocator);
        return ffi_funcs;
    }
    
    pub fn deinit(self: *HIR) void {
        for (self.functions.items) |*func| {
            func.deinit();
        }
        self.functions.deinit();
        
        for (self.imports.items) |import| {
            import.deinit(self.allocator);
            self.allocator.destroy(import);
        }
        self.imports.deinit();
        
        for (self.ffi_functions.items) |ffi_func| {
            ffi_func.deinit(self.allocator);
            self.allocator.destroy(ffi_func);
        }
        self.ffi_functions.deinit();
        
        for (self.ffi_structs.items) |ffi_struct| {
            ffi_struct.deinit(self.allocator);
            self.allocator.destroy(ffi_struct);
        }
        self.ffi_structs.deinit();
        
        for (self.ffi_types.items) |ffi_type| {
            ffi_type.deinit(self.allocator);
            self.allocator.destroy(ffi_type);
        }
        self.ffi_types.deinit();
    }

    pub const Function = struct {
        name: []const u8,
        params: []FuncParam, // Added params field
        rest_param: ?[]const u8 = null,
        body: std.ArrayList(Statement),
        allocator: std.mem.Allocator,

        pub fn init(allocator: std.mem.Allocator) Function {
            return Function{
                .name = "main",
                .params = &[_]FuncParam{}, // Initialize as an empty slice
                .body = std.ArrayList(Statement).init(allocator),
                .allocator = allocator,
            };
        }

        pub fn deinit(self: *Function) void {
            self.allocator.free(self.name);
            for (self.params) |*param| {
                param.deinit(self.allocator);
            }
            self.allocator.free(self.params); // Free the slice itself
            if (self.rest_param) |rp| {
                self.allocator.free(rp);
            }
            for (self.body.items) |*stmt| {
                stmt.deinit(self.allocator);
            }
            self.body.deinit();
        }
    };

    pub const Statement = union(enum) {
        Expression: Expression,

        pub fn deinit(self: *Statement, allocator: std.mem.Allocator) void {
            switch (self.*) {
                .Expression => |*expr| expr.deinit(allocator),
            }
        }
    };

    pub const Expression = union(enum) {
        literal: Literal,
        variable: Variable,
        binary_op: BinaryOp,
        if_expr: If,
        func_call: FuncCall,
        func_def: FuncDef,
        function: *Function, // Changed to mutable pointer
        var_decl: VarDecl,
        array_literal: ArrayLiteral, // New
        map_literal: MapLiteral, // New
        do_block: DoBlock, // New
        class_def: *ClassDef, // Class definition
        instance_creation: InstanceCreation, // Creating class instances
        method_call: MethodCall, // Calling methods on instances
        field_access: FieldAccess, // Accessing fields on instances
        path_assignment: PathAssignment, // Assigning fields on instances
        match_expr: *MatchExpr, // Pattern matching expression
        case_expr: *CaseExpr, // Case expression for conditional branching
        macro_def: *MacroDef, // Macro definition
        macro_call: MacroCall, // Macro call
        for_loop: *ForLoop, // For-in loop
        while_loop: *WhileLoop, // While loop
        return_expr: *ReturnExpr, // Return statement
        import_stmt: *ImportStmt, // Import statement
        module_access: ModuleAccess, // Access to module member
        module_def: *ModuleDef, // Module definition
        export_stmt: *ExportStmt, // Export statement
        namespace_decl: *NamespaceDecl, // Namespace declaration
        try_expr: *TryExpr, // Try/catch/finally expression
        throw_expr: *ThrowExpr, // Throw expression
        c_callback: *CCallback, // FFI callback wrapper

        pub fn deinit(self: *Expression, allocator: std.mem.Allocator) void {
            switch (self.*) {
                .literal => |*lit| lit.deinit(allocator),
                .variable => |*var_expr| var_expr.deinit(allocator),
                .binary_op => |*bin_op| bin_op.deinit(allocator),
                .if_expr => |*if_expr| if_expr.deinit(allocator),
                .func_call => |*func_call| func_call.deinit(allocator),
                .func_def => |*func_def| func_def.deinit(allocator),
                .var_decl => |*var_decl| var_decl.deinit(allocator),
                .array_literal => |*arr_lit| arr_lit.deinit(allocator), // New
                .map_literal => |*map_lit| map_lit.deinit(allocator), // New
                .do_block => |*do_block| do_block.deinit(allocator), // New
                .function => |func_ptr| {
                    // Free the function pointer and its contents
                    func_ptr.deinit();
                    allocator.destroy(func_ptr);
                },
                .class_def => |class_ptr| {
                    class_ptr.deinit(allocator);
                    allocator.destroy(class_ptr);
                },
                .instance_creation => |*inst_creation| inst_creation.deinit(allocator),
                .method_call => |*method_call| method_call.deinit(allocator),
                .field_access => |*field_access| field_access.deinit(allocator),
                .path_assignment => |*path_assign| path_assign.deinit(allocator),
                .match_expr => |match_ptr| {
                    match_ptr.deinit(allocator);
                    allocator.destroy(match_ptr);
                },
                .case_expr => |case_ptr| {
                    case_ptr.deinit(allocator);
                    allocator.destroy(case_ptr);
                },
                .macro_def => |macro_ptr| {
                    macro_ptr.deinit(allocator);
                    allocator.destroy(macro_ptr);
                },
                .macro_call => |*macro_call| macro_call.deinit(allocator),
                .for_loop => |for_ptr| {
                    for_ptr.deinit(allocator);
                    allocator.destroy(for_ptr);
                },
                .while_loop => |while_ptr| {
                    while_ptr.deinit(allocator);
                    allocator.destroy(while_ptr);
                },
                .return_expr => |ret_ptr| {
                    ret_ptr.deinit(allocator);
                    allocator.destroy(ret_ptr);
                },
                .import_stmt => |import_ptr| {
                    import_ptr.deinit(allocator);
                    allocator.destroy(import_ptr);
                },
                .module_access => |*mod_access| mod_access.deinit(allocator),
                .module_def => |mod_ptr| {
                    mod_ptr.deinit(allocator);
                    allocator.destroy(mod_ptr);
                },
                .export_stmt => |export_ptr| {
                    export_ptr.deinit(allocator);
                    allocator.destroy(export_ptr);
                },
                .namespace_decl => |ns_ptr| {
                    ns_ptr.deinit(allocator);
                    allocator.destroy(ns_ptr);
                },
                .try_expr => |try_ptr| {
                    try_ptr.deinit(allocator);
                    allocator.destroy(try_ptr);
                },
                .throw_expr => |throw_ptr| {
                    throw_ptr.deinit(allocator);
                    allocator.destroy(throw_ptr);
                },
                .c_callback => |cb_ptr| {
                    cb_ptr.deinit(allocator);
                    allocator.destroy(cb_ptr);
                },
            }
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
    };

    pub const MapLiteral = struct {
        entries: []MapEntry,

        pub fn deinit(self: *MapLiteral, allocator: std.mem.Allocator) void {
            for (self.entries) |*entry| {
                entry.deinit(allocator);
            }
            allocator.free(self.entries);
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
    };

    pub const Literal = union(enum) {
        nil: void,
        bool: bool,
        int: i64,
        float: f64,
        string: []const u8,
        symbol: []const u8,
        array: []*Expression, // Changed to []*Expression
        map: std.StringHashMap(*Expression), // Changed to StringHashMap(*Expression)

        pub fn deinit(self: *Literal, allocator: std.mem.Allocator) void {
            switch (self.*) {
                .string => |str| allocator.free(str),
                .symbol => |sym| allocator.free(sym),
                .array => |arr| {
                    for (arr) |element_ptr| {
                        element_ptr.deinit(allocator);
                        allocator.destroy(element_ptr);
                    }
                    allocator.free(arr);
                },
                .map => |*map| {
                    var it = map.iterator();
                    while (it.next()) |entry| {
                        allocator.free(entry.key_ptr.*); // Free key string
                        entry.value_ptr.*.deinit(allocator); // Deinit value expression
                        allocator.destroy(entry.value_ptr); // Destroy value expression pointer
                    }
                    map.deinit();
                },
                else => {},
            }
        }
    };

    pub const Variable = struct {
        name: []const u8,

        pub fn deinit(self: *Variable, allocator: std.mem.Allocator) void {
            allocator.free(self.name);
        }
    };

    pub const BinaryOp = struct {
        op: BinaryOpType,
        left: *Expression,
        right: *Expression,

        pub fn deinit(self: *BinaryOp, allocator: std.mem.Allocator) void {
            self.left.deinit(allocator);
            self.right.deinit(allocator);
            allocator.destroy(self.left);
            allocator.destroy(self.right);
        }
    };

    pub const BinaryOpType = enum {
        add,
        sub,
        lt,
        gt, // Added greater than
        eq, // Added equals
        ne, // Not equal
        le, // Less than or equal
        ge, // Greater than or equal
        mul,
        div,
        mod, // Modulo
        // Logical operators
        and_op, // Logical AND (&&)
        or_op,  // Logical OR (||)
    };

    pub const If = struct {
        condition: *Expression,
        then_branch: *Expression,
        else_branch: ?*Expression,

        pub fn deinit(self: *If, allocator: std.mem.Allocator) void {
            self.condition.deinit(allocator);
            allocator.destroy(self.condition);
            self.then_branch.deinit(allocator);
            allocator.destroy(self.then_branch);
            if (self.else_branch) |else_branch| {
                else_branch.deinit(allocator);
                allocator.destroy(else_branch);
            }
        }
    };

    pub const FuncCall = struct {
        func: *Expression,
        args: std.ArrayList(*Expression),

        pub fn deinit(self: *FuncCall, allocator: std.mem.Allocator) void {
            self.func.deinit(allocator);
            allocator.destroy(self.func);
            for (self.args.items) |arg| {
                arg.deinit(allocator);
                allocator.destroy(arg);
            }
            self.args.deinit();
        }
    };

    pub const FuncParam = struct {
        name: []const u8,
        param_type: ?[]const u8,
        default_value: ?*Expression = null,

        pub fn deinit(self: *FuncParam, allocator: std.mem.Allocator) void {
            allocator.free(self.name);
            if (self.param_type) |pt| allocator.free(pt);
            if (self.default_value) |dv| {
                dv.deinit(allocator);
                allocator.destroy(dv);
            }
        }
    };

    pub const FuncDef = struct {
        name: []const u8,
        params: []FuncParam,
        rest_param: ?[]const u8 = null,
        body: *Expression,

        pub fn deinit(self: *FuncDef, allocator: std.mem.Allocator) void {
            allocator.free(self.name);

            for (self.params) |*param| {
                param.deinit(allocator);
            }
            allocator.free(self.params);
            
            if (self.rest_param) |rp| {
                allocator.free(rp);
            }

            self.body.deinit(allocator);
            allocator.destroy(self.body);
        }
    };

    pub const VarDecl = struct {
        name: []const u8,
        value: *Expression,

        pub fn deinit(self: *VarDecl, allocator: std.mem.Allocator) void {
            allocator.free(self.name);
            self.value.deinit(allocator);
            allocator.destroy(self.value);
        }
    };

    pub const Value = union(Type) {
        void: void,
        bool: bool,
        int: i64,
        float: f64,
        string: []const u8,
        function: *const Function,
    };

    // Class-related HIR structures
    pub const ClassDef = struct {
        name: []const u8,
        parent_class: ?[]const u8, // Name of parent class for inheritance
        traits: [][]const u8, // Names of implemented traits
        fields: []ClassField,
        methods: []ClassMethod,
        allocator: std.mem.Allocator,

        pub fn deinit(self: *ClassDef, allocator: std.mem.Allocator) void {
            allocator.free(self.name);
            
            if (self.parent_class) |parent| {
                allocator.free(parent);
            }
            
            for (self.traits) |trait_name| {
                allocator.free(trait_name);
            }
            allocator.free(self.traits);
            
            for (self.fields) |*field| {
                field.deinit(allocator);
            }
            allocator.free(self.fields);
            
            for (self.methods) |*method| {
                method.deinit(allocator);
            }
            allocator.free(self.methods);
        }
    };
    
    pub const ClassField = struct {
        name: []const u8,
        type_annotation: ?[]const u8,
        is_public: bool,
        default_value: ?*Expression,
        
        pub fn deinit(self: *ClassField, allocator: std.mem.Allocator) void {
            allocator.free(self.name);
            if (self.type_annotation) |type_ann| {
                allocator.free(type_ann);
            }
            if (self.default_value) |default_val| {
                default_val.deinit(allocator);
                allocator.destroy(default_val);
            }
        }
    };
    
    pub const ClassMethod = struct {
        name: []const u8,
        params: []FuncParam,
        body: *Expression,
        is_public: bool,
        is_virtual: bool,
        is_abstract: bool,
        is_static: bool,
        
        pub fn deinit(self: *ClassMethod, allocator: std.mem.Allocator) void {
            allocator.free(self.name);
            
            for (self.params) |*param| {
                param.deinit(allocator);
            }
            allocator.free(self.params);
            
            self.body.deinit(allocator);
            allocator.destroy(self.body);
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
    };
    
    pub const MethodCall = struct {
        object: *Expression, // The object instance
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
    };
    
    pub const FieldAccess = struct {
        object: ?*Expression, // The object instance (null for implicit self)
        field_name: []const u8,
        
        pub fn deinit(self: *FieldAccess, allocator: std.mem.Allocator) void {
            if (self.object) |obj| {
                obj.deinit(allocator);
                allocator.destroy(obj);
            }
            allocator.free(self.field_name);
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
    };
    
    // Pattern matching HIR structures
    pub const MatchExpr = struct {
        scrutinee: *Expression, // The value being matched
        arms: []MatchArm, // Pattern matching arms
        
        pub fn deinit(self: *MatchExpr, allocator: std.mem.Allocator) void {
            self.scrutinee.deinit(allocator);
            allocator.destroy(self.scrutinee);
            
            for (self.arms) |*arm| {
                arm.deinit(allocator);
            }
            allocator.free(self.arms);
        }
    };
    
    pub const MatchArm = struct {
        pattern: Pattern,
        guard: ?*Expression, // Optional guard clause
        body: *Expression, // Expression to execute if pattern matches
        
        pub fn deinit(self: *MatchArm, allocator: std.mem.Allocator) void {
            self.pattern.deinit(allocator);
            
            if (self.guard) |guard| {
                guard.deinit(allocator);
                allocator.destroy(guard);
            }
            
            self.body.deinit(allocator);
            allocator.destroy(self.body);
        }
    };
    
    pub const Pattern = union(enum) {
        // Literal patterns - match exact values
        literal: LiteralPattern,
        
        // Variable patterns - bind to variables
        variable: VariablePattern,
        
        // Wildcard pattern - matches anything
        wildcard,
        
        // Constructor patterns - match specific types/classes
        constructor: ConstructorPattern,
        
        // Array patterns - destructure arrays
        array: ArrayPattern,
        
        // Map patterns - destructure maps/objects
        map: MapPattern,
        
        // Or patterns - match any of several patterns
        or_pattern: OrPattern,
        
        // Range patterns - match within ranges
        range: RangePattern,
        
        pub const LiteralPattern = struct {
            value: *Expression, // The literal value to match
            
            pub fn deinit(self: *LiteralPattern, allocator: std.mem.Allocator) void {
                self.value.deinit(allocator);
                allocator.destroy(self.value);
            }
        };
        
        pub const VariablePattern = struct {
            name: []const u8, // Variable name to bind to
            type_annotation: ?[]const u8, // Optional type constraint
            
            pub fn deinit(self: *VariablePattern, allocator: std.mem.Allocator) void {
                allocator.free(self.name);
                if (self.type_annotation) |type_ann| {
                    allocator.free(type_ann);
                }
            }
        };
        
        pub const ConstructorPattern = struct {
            constructor: []const u8, // Constructor/type name
            fields: []Pattern, // Nested patterns for fields
            
            pub fn deinit(self: *ConstructorPattern, allocator: std.mem.Allocator) void {
                allocator.free(self.constructor);
                for (self.fields) |*field| {
                    field.deinit(allocator);
                }
                allocator.free(self.fields);
            }
        };
        
        pub const ArrayPattern = struct {
            elements: []Pattern, // Patterns for array elements
            rest: ?[]const u8, // Optional rest pattern (..rest)
            
            pub fn deinit(self: *ArrayPattern, allocator: std.mem.Allocator) void {
                for (self.elements) |*elem| {
                    elem.deinit(allocator);
                }
                allocator.free(self.elements);
                if (self.rest) |rest| {
                    allocator.free(rest);
                }
            }
        };
        
        pub const MapPattern = struct {
            fields: []MapFieldPattern, // Key-value patterns
            rest: ?[]const u8, // Optional rest pattern
            
            pub const MapFieldPattern = struct {
                key: []const u8, // Map key
                pattern: Pattern, // Pattern for the value
                
                pub fn deinit(self: *MapFieldPattern, allocator: std.mem.Allocator) void {
                    allocator.free(self.key);
                    // Note: pattern.deinit() will be called by the parent MapPattern.deinit()
                }
            };
            
            pub fn deinit(self: *MapPattern, allocator: std.mem.Allocator) void {
                for (self.fields) |*field| {
                    field.deinit(allocator);
                    field.pattern.deinit(allocator); // Deinit the pattern explicitly
                }
                allocator.free(self.fields);
                if (self.rest) |rest| {
                    allocator.free(rest);
                }
            }
        };
        
        pub const OrPattern = struct {
            patterns: []Pattern, // Alternative patterns
            
            pub fn deinit(self: *OrPattern, allocator: std.mem.Allocator) void {
                for (self.patterns) |*pattern| {
                    pattern.deinit(allocator);
                }
                allocator.free(self.patterns);
            }
        };
        
        pub const RangePattern = struct {
            start: *Expression, // Range start
            end: *Expression, // Range end
            inclusive: bool, // Whether end is inclusive
            
            pub fn deinit(self: *RangePattern, allocator: std.mem.Allocator) void {
                self.start.deinit(allocator);
                allocator.destroy(self.start);
                self.end.deinit(allocator);
                allocator.destroy(self.end);
            }
        };
        
        pub fn deinit(self: *Pattern, allocator: std.mem.Allocator) void {
            switch (self.*) {
                .literal => |*lit| lit.deinit(allocator),
                .variable => |*var_pat| var_pat.deinit(allocator),
                .wildcard => {},
                .constructor => |*ctor| ctor.deinit(allocator),
                .array => |*arr| arr.deinit(allocator),
                .map => |*map| map.deinit(allocator),
                .or_pattern => |*or_pat| or_pat.deinit(allocator),
                .range => |*range| range.deinit(allocator),
            }
        }
    };
    
    // Case expression HIR structures
    pub const CaseExpr = struct {
        scrutinee: *Expression, // The value being examined
        branches: []CaseBranch, // When branches
        else_branch: ?*Expression, // Optional else branch
        
        pub fn deinit(self: *CaseExpr, allocator: std.mem.Allocator) void {
            self.scrutinee.deinit(allocator);
            allocator.destroy(self.scrutinee);
            
            for (self.branches) |*branch| {
                branch.deinit(allocator);
            }
            allocator.free(self.branches);
            
            if (self.else_branch) |else_br| {
                else_br.deinit(allocator);
                allocator.destroy(else_br);
            }
        }
    };
    
    pub const CaseBranch = struct {
        condition: *Expression, // The condition to check
        body: *Expression, // Expression to execute if condition is true
        
        pub fn deinit(self: *CaseBranch, allocator: std.mem.Allocator) void {
            self.condition.deinit(allocator);
            allocator.destroy(self.condition);
            
            self.body.deinit(allocator);
            allocator.destroy(self.body);
        }
    };

    pub fn astToHir(allocator: std.mem.Allocator, nodes: []const ast.AstNode) !HIR {
        var hir = HIR.init(allocator);
        errdefer hir.deinit();

        // Create a main function to hold our statements
        var main_fn = Function.init(allocator);
        main_fn.name = try allocator.dupe(u8, "main");

        for (nodes) |node| {
            switch (node) {
                .Stmt => |stmt| switch (stmt) {
                    .ExprStmt => |expr| {
                        const hir_expr = try lowerExpr(allocator, expr);
                        try main_fn.body.append(.{ .Expression = hir_expr });
                    },
                    else => return error.UnsupportedStatement,
                },
                else => return error.UnsupportedNode,
            }
        }

        try hir.functions.append(main_fn);
        return hir;
    }

    fn lowerExpr(allocator: std.mem.Allocator, expr: ast.Expr) !Expression {
        switch (expr) {
            .StrLit => |value| {
                const str = try allocator.dupe(u8, value);
                return .{ .literal = .{ .string = str } };
            },
            .Ident => |value| {
                const ident = try allocator.dupe(u8, value);
                return .{ .variable = .{ .name = ident } };
            },
            else => return error.UnsupportedExpression,
        }
    }
    
    pub const MacroDef = struct {
        name: []const u8,
        params: []MacroParam,
        body: *Expression,
        
        pub const MacroParam = struct {
            name: []const u8,
            is_variadic: bool,
            
            pub fn deinit(self: *MacroParam, allocator: std.mem.Allocator) void {
                allocator.free(self.name);
            }
        };
        
        pub fn deinit(self: *MacroDef, allocator: std.mem.Allocator) void {
            allocator.free(self.name);
            for (self.params) |*param| {
                param.deinit(allocator);
            }
            allocator.free(self.params);
            self.body.deinit(allocator);
            allocator.destroy(self.body);
        }
    };
    
    pub const MacroCall = struct {
        macro: *Expression, // The macro to call
        args: []*Expression, // Arguments (stored as thunks for lazy evaluation)
        
        pub fn deinit(self: *MacroCall, allocator: std.mem.Allocator) void {
            self.macro.deinit(allocator);
            allocator.destroy(self.macro);
            for (self.args) |arg| {
                arg.deinit(allocator);
                allocator.destroy(arg);
            }
            allocator.free(self.args);
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
    };
    
    pub const WhileLoop = struct {
        condition: *Expression, // Loop condition
        body: *Expression, // Loop body
        
        pub fn deinit(self: *WhileLoop, allocator: std.mem.Allocator) void {
            self.condition.deinit(allocator);
            allocator.destroy(self.condition);
            self.body.deinit(allocator);
            allocator.destroy(self.body);
        }
    };
    
    pub const ReturnExpr = struct {
        value: ?*Expression, // Optional return value (null for bare return)
        
        pub fn deinit(self: *ReturnExpr, allocator: std.mem.Allocator) void {
            if (self.value) |val| {
                val.deinit(allocator);
                allocator.destroy(val);
            }
        }
    };
    
    pub const ImportStmt = struct {
        module_path: []const u8,
        alias: ?[]const u8,
        items: ?[]ImportItem,
        
        pub const ImportItem = struct {
            name: []const u8,
            alias: ?[]const u8,
        };
        
        pub fn deinit(self: *ImportStmt, allocator: std.mem.Allocator) void {
            allocator.free(self.module_path);
            if (self.alias) |a| allocator.free(a);
            if (self.items) |items| {
                for (items) |*item| {
                    allocator.free(item.name);
                    if (item.alias) |a| allocator.free(a);
                }
                allocator.free(items);
            }
        }
    };
    
    pub const ModuleDef = struct {
        name: []const u8,
        body: std.ArrayList(Statement),
        exports: [][]const u8,
        
        pub fn init(allocator: std.mem.Allocator) ModuleDef {
            return ModuleDef{
                .name = "",
                .body = std.ArrayList(Statement).init(allocator),
                .exports = &[_][]const u8{},
            };
        }
        
        pub fn deinit(self: *ModuleDef, allocator: std.mem.Allocator) void {
            allocator.free(self.name);
            for (self.body.items) |*stmt| {
                stmt.deinit(allocator);
            }
            self.body.deinit();
            for (self.exports) |export_name| {
                allocator.free(export_name);
            }
            if (self.exports.len > 0) {
                allocator.free(self.exports);
            }
        }
    };
    
    pub const ExportStmt = struct {
        items: []ExportItem,
        
        pub const ExportItem = struct {
            name: []const u8,
            alias: ?[]const u8,
        };
        
        pub fn deinit(self: *ExportStmt, allocator: std.mem.Allocator) void {
            for (self.items) |*item| {
                allocator.free(item.name);
                if (item.alias) |a| allocator.free(a);
            }
            allocator.free(self.items);
        }
    };
    
    pub const ModuleAccess = struct {
        module: []const u8, // Module name or alias
        member: []const u8, // Member being accessed
        
        pub fn deinit(self: *ModuleAccess, allocator: std.mem.Allocator) void {
            allocator.free(self.module);
            allocator.free(self.member);
        }
    };

    pub const NamespaceDecl = struct {
        name: []const u8, // Namespace path (e.g., "geometry/shapes")
        body: *Expression, // Namespace body (usually a DoBlock)
        
        pub fn deinit(self: *NamespaceDecl, allocator: std.mem.Allocator) void {
            allocator.free(self.name);
            self.body.deinit(allocator);
            allocator.destroy(self.body);
        }
    };
    
    pub const TryExpr = struct {
        body: *Expression, // The body to try
        catch_clauses: []CatchClause, // Catch clauses
        finally_block: ?*Expression, // Optional finally block
        
        pub const CatchClause = struct {
            error_var: ?[]const u8, // Optional variable to bind caught error
            error_type: ?[]const u8, // Optional error type to catch
            body: *Expression, // Code to execute when error is caught
            
            pub fn deinit(self: *CatchClause, allocator: std.mem.Allocator) void {
                if (self.error_var) |var_name| allocator.free(var_name);
                if (self.error_type) |type_name| allocator.free(type_name);
                self.body.deinit(allocator);
                allocator.destroy(self.body);
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
    };
    
    pub const ThrowExpr = struct {
        value: *Expression, // The error value to throw
        
        pub fn deinit(self: *ThrowExpr, allocator: std.mem.Allocator) void {
            self.value.deinit(allocator);
            allocator.destroy(self.value);
        }
    };
    
    // FFI declarations
    pub const FFIFunction = struct {
        name: []const u8,
        params: []FFIParam,
        return_type: ?[]const u8,
        lib: []const u8,
        symbol: ?[]const u8,
        calling_convention: ?[]const u8,
        is_variadic: bool,
        
        pub const FFIParam = struct {
            name: []const u8,
            c_type: []const u8,
            
            pub fn deinit(self: *FFIParam, allocator: std.mem.Allocator) void {
                allocator.free(self.name);
                allocator.free(self.c_type);
            }
        };
        
        pub fn deinit(self: *FFIFunction, allocator: std.mem.Allocator) void {
            allocator.free(self.name);
            for (self.params) |*param| {
                param.deinit(allocator);
            }
            allocator.free(self.params);
            if (self.return_type) |ret| allocator.free(ret);
            allocator.free(self.lib);
            if (self.symbol) |sym| allocator.free(sym);
            if (self.calling_convention) |cc| allocator.free(cc);
        }
    };
    
    pub const FFIStruct = struct {
        name: []const u8,
        fields: []FFIField,
        is_packed: bool,
        alignment: ?usize,
        
        pub const FFIField = struct {
            name: []const u8,
            c_type: []const u8,
            bit_size: ?u8,
            
            pub fn deinit(self: *FFIField, allocator: std.mem.Allocator) void {
                allocator.free(self.name);
                allocator.free(self.c_type);
            }
        };
        
        pub fn deinit(self: *FFIStruct, allocator: std.mem.Allocator) void {
            allocator.free(self.name);
            for (self.fields) |*field| {
                field.deinit(allocator);
            }
            allocator.free(self.fields);
        }
    };
    
    pub const FFIType = struct {
        name: []const u8,
        c_type: []const u8,
        
        pub fn deinit(self: *FFIType, allocator: std.mem.Allocator) void {
            allocator.free(self.name);
            allocator.free(self.c_type);
        }
    };
    
    pub const CCallback = struct {
        function: *Expression, // The Gene function to wrap
        signature: ?[]const u8, // Optional C signature
        
        pub fn deinit(self: *CCallback, allocator: std.mem.Allocator) void {
            self.function.deinit(allocator);
            allocator.destroy(self.function);
            if (self.signature) |sig| {
                allocator.free(sig);
            }
        }
    };
};
