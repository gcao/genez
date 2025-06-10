# Design of Pseudo Macros for Gene Language

## Overview

This document outlines the design and implementation strategy for "pseudo macros" in the Gene programming language. Pseudo macros are a unique metaprogramming feature that bridges the gap between regular functions and compile-time macros.

## Core Concept

**Pseudo Macro Properties:**
1. **Lazy Evaluation**: Arguments passed to pseudo macros are not evaluated at call time
2. **Lexical Scoping**: Arguments are evaluated in the caller's lexical environment when needed
3. **On-Demand Resolution**: Inside the macro body, argument references trigger evaluation in the original context

Unlike traditional Lisp macros that operate at compile-time, pseudo macros work at runtime but with delayed evaluation semantics. This provides powerful metaprogramming capabilities while maintaining the simplicity of Gene's runtime model.

## Pipeline Integration

### 1. AST Level (`src/frontend/ast.zig`)

Add new AST nodes for pseudo macro support:

```zig
// New pseudo macro definition
pub const PseudoMacroDef = struct {
    name: []const u8,
    params: []FuncParam,  // Reuse existing param structure
    body: *Expression,
    
    pub fn deinit(self: *PseudoMacroDef, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
        for (self.params) |*param| {
            param.deinit(allocator);
        }
        allocator.free(self.params);
        self.body.deinit(allocator);
        allocator.destroy(self.body);
    }
    
    pub fn clone(self: PseudoMacroDef, allocator: std.mem.Allocator) !PseudoMacroDef {
        var params = try allocator.alloc(FuncParam, self.params.len);
        errdefer allocator.free(params);
        
        for (self.params, 0..) |param, i| {
            params[i] = try param.clone(allocator);
        }
        
        const body = try allocator.create(Expression);
        errdefer allocator.destroy(body);
        body.* = try self.body.clone(allocator);
        
        return PseudoMacroDef{
            .name = try allocator.dupe(u8, self.name),
            .params = params,
            .body = body,
        };
    }
};

// New pseudo macro call (distinct from FuncCall)
pub const PseudoMacroCall = struct {
    macro: *Expression,
    args: std.ArrayList(*Expression), // Stored as AST, not evaluated
    call_context: ?CallContext,       // Captures caller's lexical environment
    
    pub fn deinit(self: *PseudoMacroCall, allocator: std.mem.Allocator) void {
        self.macro.deinit(allocator);
        allocator.destroy(self.macro);
        for (self.args.items) |arg| {
            arg.deinit(allocator);
            allocator.destroy(arg);
        }
        self.args.deinit();
        if (self.call_context) |*ctx| {
            ctx.deinit(allocator);
        }
    }
    
    pub fn clone(self: PseudoMacroCall, allocator: std.mem.Allocator) !PseudoMacroCall {
        const macro = try allocator.create(Expression);
        errdefer allocator.destroy(macro);
        macro.* = try self.macro.clone(allocator);
        
        var args = std.ArrayList(*Expression).init(allocator);
        errdefer {
            for (args.items) |arg| {
                arg.deinit(allocator);
                allocator.destroy(arg);
            }
            args.deinit();
        }
        
        for (self.args.items) |arg| {
            const new_arg = try allocator.create(Expression);
            errdefer allocator.destroy(new_arg);
            new_arg.* = try arg.clone(allocator);
            try args.append(new_arg);
        }
        
        return PseudoMacroCall{
            .macro = macro,
            .args = args,
            .call_context = if (self.call_context) |ctx| try ctx.clone(allocator) else null,
        };
    }
};

// Capture lexical environment at call site
pub const CallContext = struct {
    variables: std.HashMap([]const u8, *Expression),
    parent_scope: ?*CallContext,
    
    pub fn deinit(self: *CallContext, allocator: std.mem.Allocator) void {
        var iterator = self.variables.iterator();
        while (iterator.next()) |entry| {
            allocator.free(entry.key_ptr.*);
            entry.value_ptr.*.deinit(allocator);
            allocator.destroy(entry.value_ptr.*);
        }
        self.variables.deinit();
    }
    
    pub fn clone(self: CallContext, allocator: std.mem.Allocator) !CallContext {
        var new_variables = std.HashMap([]const u8, *Expression).init(allocator);
        var iterator = self.variables.iterator();
        while (iterator.next()) |entry| {
            const key = try allocator.dupe(u8, entry.key_ptr.*);
            const value = try allocator.create(Expression);
            value.* = try entry.value_ptr.*.clone(allocator);
            try new_variables.put(key, value);
        }
        
        return CallContext{
            .variables = new_variables,
            .parent_scope = self.parent_scope, // Shallow copy of reference
        };
    }
};
```

**Expression enum addition:**
```zig
pub const Expression = union(enum) {
    // ... existing variants ...
    PseudoMacroDef: PseudoMacroDef,
    PseudoMacroCall: PseudoMacroCall,
    
    pub fn deinit(self: *Expression, allocator: std.mem.Allocator) void {
        switch (self.*) {
            // ... existing cases ...
            .PseudoMacroDef => |*pmacro_def| pmacro_def.deinit(allocator),
            .PseudoMacroCall => |*pmacro_call| pmacro_call.deinit(allocator),
        }
    }
    
    pub fn clone(self: Expression, allocator: std.mem.Allocator) !Expression {
        return switch (self) {
            // ... existing cases ...
            .PseudoMacroDef => |pmacro_def| .{ .PseudoMacroDef = try pmacro_def.clone(allocator) },
            .PseudoMacroCall => |pmacro_call| .{ .PseudoMacroCall = try pmacro_call.clone(allocator) },
        };
    }
};
```

### 2. HIR Level (`src/ir/hir.zig`)

Transform pseudo macros to HIR with context preservation:

```zig
pub const HIR = struct {
    pub const PseudoMacro = struct {
        name: []const u8,
        params: []FuncParam,
        body: *Expression,
        allocator: std.mem.Allocator,
        
        pub fn deinit(self: *PseudoMacro) void {
            self.allocator.free(self.name);
            for (self.params) |*param| {
                self.allocator.free(param.name);
                if (param.param_type) |param_type| {
                    self.allocator.free(param_type);
                }
            }
            self.allocator.free(self.params);
            self.body.deinit(self.allocator);
            self.allocator.destroy(self.body);
        }
    };
    
    pub const LazyCall = struct {
        macro: *Expression,
        lazy_args: []*LazyArgument,  // Unevaluated argument expressions
        lexical_context: LexicalContext, // Captured caller environment
        allocator: std.mem.Allocator,
        
        pub fn deinit(self: *LazyCall) void {
            self.macro.deinit(self.allocator);
            self.allocator.destroy(self.macro);
            for (self.lazy_args) |lazy_arg| {
                lazy_arg.deinit();
                self.allocator.destroy(lazy_arg);
            }
            self.allocator.free(self.lazy_args);
            self.lexical_context.deinit();
        }
    };
    
    pub const LazyArgument = struct {
        ast_expr: *Expression,       // Original AST expression
        context_bindings: []Binding, // Variable bindings from call site
        allocator: std.mem.Allocator,
        
        pub fn deinit(self: *LazyArgument) void {
            self.ast_expr.deinit(self.allocator);
            self.allocator.destroy(self.ast_expr);
            for (self.context_bindings) |*binding| {
                binding.deinit(self.allocator);
            }
            self.allocator.free(self.context_bindings);
        }
    };
    
    pub const LexicalContext = struct {
        bindings: []Binding,
        parent: ?*LexicalContext,
        allocator: std.mem.Allocator,
        
        pub fn deinit(self: *LexicalContext) void {
            for (self.bindings) |*binding| {
                binding.deinit(self.allocator);
            }
            self.allocator.free(self.bindings);
        }
    };
    
    pub const Binding = struct {
        name: []const u8,
        value: *Expression,
        
        pub fn deinit(self: *Binding, allocator: std.mem.Allocator) void {
            allocator.free(self.name);
            self.value.deinit(allocator);
            allocator.destroy(self.value);
        }
    };
    
    pub const Expression = union(enum) {
        // ... existing variants ...
        pseudo_macro_def: *PseudoMacro,
        lazy_call: *LazyCall,
        
        pub fn deinit(self: Expression, allocator: std.mem.Allocator) void {
            switch (self) {
                // ... existing cases ...
                .pseudo_macro_def => |pmacro| {
                    pmacro.deinit();
                    allocator.destroy(pmacro);
                },
                .lazy_call => |lazy_call| {
                    lazy_call.deinit();
                    allocator.destroy(lazy_call);
                },
            }
        }
    };
};
```

### 3. MIR Level (`src/ir/mir.zig`)

Implement lazy evaluation mechanism:

```zig
pub const MIR = struct {
    pub const LazyEvalCall = struct {
        macro_ref: BasicBlockRef,
        thunks: []ThunkRef,         // Each argument becomes a thunk
        context_frame: ContextRef,   // Preserved lexical environment
        
        pub fn deinit(self: *LazyEvalCall, allocator: std.mem.Allocator) void {
            allocator.free(self.thunks);
            // context_frame cleanup handled by MIR context management
        }
    };
    
    pub const Thunk = struct {
        expressions: []Expression,  // Unevaluated computation
        captured_vars: []Variable,  // Variables from caller's scope
        thunk_id: u32,
        
        pub fn deinit(self: *Thunk, allocator: std.mem.Allocator) void {
            for (self.expressions) |*expr| {
                expr.deinit(allocator);
            }
            allocator.free(self.expressions);
            for (self.captured_vars) |*var_ref| {
                var_ref.deinit(allocator);
            }
            allocator.free(self.captured_vars);
        }
    };
    
    pub const Expression = union(enum) {
        // ... existing variants ...
        lazy_eval_call: LazyEvalCall,
        thunk_creation: Thunk,
        thunk_evaluation: ThunkRef,
        
        pub fn deinit(self: Expression, allocator: std.mem.Allocator) void {
            switch (self) {
                // ... existing cases ...
                .lazy_eval_call => |*lazy_call| lazy_call.deinit(allocator),
                .thunk_creation => |*thunk| thunk.deinit(allocator),
                .thunk_evaluation => {}, // ThunkRef is just an ID
            }
        }
    };
};
```

### 4. Bytecode Level (`src/backend/bytecode.zig`)

New bytecode instructions for lazy evaluation:

```zig
pub const Instruction = struct {
    opcode: Opcode,
    operand: ?Value,
    
    pub const Opcode = enum {
        // ... existing opcodes ...
        CreateThunk,      // Create a lazy evaluation thunk
        EvalThunk,        // Force evaluation of a thunk
        CaptureContext,   // Capture current lexical environment
        RestoreContext,   // Restore captured environment
        PseudoMacroCall,  // Call pseudo macro with lazy args
        PushThunkArg,     // Push thunk as argument instead of value
    };
};

pub const ThunkDescriptor = struct {
    instructions: std.ArrayList(Instruction),
    captured_variables: std.HashMap([]const u8, Value),
    thunk_id: u32,
    
    pub fn init(allocator: std.mem.Allocator, thunk_id: u32) ThunkDescriptor {
        return .{
            .instructions = std.ArrayList(Instruction).init(allocator),
            .captured_variables = std.HashMap([]const u8, Value).init(allocator),
            .thunk_id = thunk_id,
        };
    }
    
    pub fn deinit(self: *ThunkDescriptor) void {
        for (self.instructions.items) |*instr| {
            if (instr.operand) |*operand| {
                operand.deinit(self.instructions.allocator);
            }
        }
        self.instructions.deinit();
        
        var iterator = self.captured_variables.iterator();
        while (iterator.next()) |entry| {
            self.captured_variables.allocator.free(entry.key_ptr.*);
            entry.value_ptr.deinit(self.captured_variables.allocator);
        }
        self.captured_variables.deinit();
    }
};
```

### 5. VM Level (`src/backend/vm.zig`)

Implement runtime support:

```zig
pub const VM = struct {
    // ... existing fields ...
    context_stack: std.ArrayList(LexicalContext),
    thunk_table: std.HashMap(u32, ThunkDescriptor),
    next_thunk_id: u32,
    
    pub fn init(allocator: std.mem.Allocator) VM {
        return .{
            // ... existing initialization ...
            .context_stack = std.ArrayList(LexicalContext).init(allocator),
            .thunk_table = std.HashMap(u32, ThunkDescriptor).init(allocator),
            .next_thunk_id = 1,
        };
    }
    
    pub fn deinit(self: *VM) void {
        // ... existing cleanup ...
        self.context_stack.deinit();
        
        var thunk_iterator = self.thunk_table.iterator();
        while (thunk_iterator.next()) |entry| {
            entry.value_ptr.deinit();
        }
        self.thunk_table.deinit();
    }
    
    fn executeCreateThunk(self: *VM, thunk_instructions: []const Instruction) !u32 {
        const thunk_id = self.next_thunk_id;
        self.next_thunk_id += 1;
        
        var thunk = ThunkDescriptor.init(self.allocator, thunk_id);
        
        // Copy instructions for the thunk
        for (thunk_instructions) |instr| {
            var new_instr = instr;
            if (instr.operand) |operand| {
                new_instr.operand = try operand.clone(self.allocator);
            }
            try thunk.instructions.append(new_instr);
        }
        
        // Capture current lexical environment
        for (self.variables.items) |variable| {
            const name_copy = try self.allocator.dupe(u8, variable.name);
            const value_copy = try variable.value.clone(self.allocator);
            try thunk.captured_variables.put(name_copy, value_copy);
        }
        
        try self.thunk_table.put(thunk_id, thunk);
        return thunk_id;
    }
    
    fn executeEvalThunk(self: *VM, thunk_id: u32) !Value {
        const thunk = self.thunk_table.get(thunk_id) orelse return error.InvalidThunk;
        
        // Save current execution state
        const saved_ip = self.ip;
        const saved_variables = try self.variables.clone();
        defer {
            self.variables.deinit();
            self.variables = saved_variables;
        }
        
        // Restore thunk's captured context
        self.variables.clearAndFree();
        var thunk_var_iterator = thunk.captured_variables.iterator();
        while (thunk_var_iterator.next()) |entry| {
            const var_name = try self.allocator.dupe(u8, entry.key_ptr.*);
            const var_value = try entry.value_ptr.clone(self.allocator);
            try self.variables.put(var_name, var_value);
        }
        
        // Execute thunk instructions
        self.ip = 0;
        const result = try self.executeInstructions(thunk.instructions.items);
        
        // Restore execution state
        self.ip = saved_ip;
        
        return result;
    }
    
    fn executePseudoMacroCall(self: *VM, macro_id: u32, thunk_ids: []const u32) !Value {
        // Push current context
        const current_context = try self.captureCurrentContext();
        try self.context_stack.append(current_context);
        defer _ = self.context_stack.pop();
        
        // Set up macro execution environment
        // Macro parameters get bound to thunk IDs, not values
        
        // Execute macro body
        // When arguments are referenced, evaluate corresponding thunks
        return self.executeMacroBody(macro_id, thunk_ids);
    }
    
    fn captureCurrentContext(self: *VM) !LexicalContext {
        var context = LexicalContext.init(self.allocator);
        for (self.variables.items) |variable| {
            const name_copy = try self.allocator.dupe(u8, variable.name);
            const value_copy = try variable.value.clone(self.allocator);
            try context.variables.put(name_copy, value_copy);
        }
        return context;
    }
    
    const LexicalContext = struct {
        variables: std.HashMap([]const u8, Value),
        
        fn init(allocator: std.mem.Allocator) LexicalContext {
            return .{
                .variables = std.HashMap([]const u8, Value).init(allocator),
            };
        }
        
        fn deinit(self: *LexicalContext) void {
            var iterator = self.variables.iterator();
            while (iterator.next()) |entry| {
                self.variables.allocator.free(entry.key_ptr.*);
                entry.value_ptr.deinit(self.variables.allocator);
            }
            self.variables.deinit();
        }
    };
};
```

## Syntax Design

### Macro Definition
```gene
;; Define a pseudo macro using 'pmacro' keyword
(pmacro when [condition body]
  (if condition body nil))

;; More complex example with multiple arguments
(pmacro unless [condition body]
  (if condition nil body))

;; Macro with multiple statements
(pmacro with-resource [resource-expr body]
  (do
    (var resource resource-expr)
    (try
      body
      (finally (cleanup resource)))))
```

### Macro Usage
```gene
;; Usage - arguments not evaluated until needed
(var x 0)
(when (> x 5)           ;; This comparison not evaluated immediately
  (print "x is large")) ;; This print not evaluated immediately

;; The macro body evaluates 'condition' and 'body' on demand
;; in the caller's context where x is visible

;; Another example
(var file-path "data.txt")
(with-resource (open-file file-path)  ;; File opening delayed
  (do
    (print "Processing file")
    (process-data resource)))         ;; 'resource' comes from macro
```

### Key Benefits
1. **Lazy Evaluation**: Expensive computations only happen if needed
2. **Conditional Execution**: Arguments might not execute at all
3. **Context Preservation**: Variables from call site remain accessible
4. **Runtime Flexibility**: Unlike compile-time macros, these work with dynamic values

## Implementation Strategy

### Phase 1: AST Support
- [ ] Add `PseudoMacroDef` and `PseudoMacroCall` AST nodes
- [ ] Implement memory management for new nodes
- [ ] Add parser support for `pmacro` syntax
- [ ] Update AST serialization for debugging

### Phase 2: HIR Transformation
- [ ] Implement AST to HIR conversion for pseudo macros
- [ ] Add context capture mechanisms
- [ ] Implement lazy argument wrapping
- [ ] Add HIR serialization support

### Phase 3: MIR Support
- [ ] Add thunk creation and management to MIR
- [ ] Implement lazy evaluation lowering
- [ ] Add MIR optimization passes for thunks
- [ ] Update MIR serialization

### Phase 4: Bytecode Generation
- [ ] Add new opcodes for lazy evaluation
- [ ] Implement thunk bytecode generation
- [ ] Add context capture/restore instructions
- [ ] Update bytecode serialization

### Phase 5: VM Runtime
- [ ] Implement thunk table management
- [ ] Add context stack for lexical scoping
- [ ] Implement lazy argument evaluation
- [ ] Add debugging support for macro execution

### Phase 6: Integration & Testing
- [ ] Create comprehensive test cases
- [ ] Add performance benchmarks
- [ ] Document usage patterns
- [ ] Add error handling for edge cases

## Integration with Current Pipeline

This design integrates seamlessly with Gene's existing `AST → HIR → MIR → Bytecode → VM` pipeline by:

1. **Reusing Infrastructure**: Memory management, error handling, and debugging systems
2. **Extending Gradually**: Each pipeline stage gets new node types without breaking existing functionality
3. **Maintaining Compatibility**: Regular functions continue to work unchanged
4. **Following Patterns**: Uses established patterns for node definitions and transformations

The pseudo macro system provides powerful metaprogramming capabilities while maintaining Gene's philosophy of gradual complexity and runtime simplicity.