# LIR (Low-level Intermediate Representation) Design

## Overview

The LIR is designed to be a low-level, register-based intermediate representation that efficiently supports the unified Gene data format while enabling optimizations. It sits between MIR and machine code/bytecode generation.

## Design Principles

1. **Register-based**: Uses virtual registers instead of a stack
2. **SSA Form**: Static Single Assignment for better optimization
3. **Type-aware**: Carries type information for optimization
4. **Gene-centric**: First-class support for Gene operations

## Register Types

```zig
pub const Register = struct {
    id: u32,
    type: TypeInfo,
    version: u32,  // For SSA
};

pub const TypeInfo = union(enum) {
    Any,                    // Unknown type
    Nil,
    Bool,
    Int,                    // Unboxed integer
    Float,                  // Unboxed float
    String,
    Symbol,
    Array: ?*TypeInfo,      // Element type if known
    Map: ?struct {
        key: *TypeInfo,
        value: *TypeInfo,
    },
    Gene: ?*ClassInfo,      // Class if known
    Class: *ClassInfo,
    Function: *FunctionType,
};
```

## LIR Instructions

### Basic Operations

```zig
pub const LIRInst = union(enum) {
    // Register operations
    Move: struct {
        dst: Register,
        src: Register,
    },
    
    LoadConst: struct {
        dst: Register,
        value: Value,
    },
    
    LoadNil: struct {
        dst: Register,
    },
    
    // Phi nodes for SSA
    Phi: struct {
        dst: Register,
        inputs: []struct {
            value: Register,
            block: BlockId,
        },
    },
```

### Gene Operations

```zig
    // Gene creation - optimized for common patterns
    CreateGene: struct {
        dst: Register,
        head: Register,
        props: []struct {
            key: Register,    // Must be string/symbol
            value: Register,
        },
        children: []Register,
    },
    
    // Gene property access with inline cache hint
    GetProp: struct {
        dst: Register,
        gene: Register,
        key: Register,
        cache_slot: ?u32,    // For inline caching
    },
    
    SetProp: struct {
        dst: Register,       // New gene (immutable) or same (mutable)
        gene: Register,
        key: Register,
        value: Register,
        cache_slot: ?u32,
    },
    
    // Gene child access
    GetChild: struct {
        dst: Register,
        gene: Register,
        index: Register,     // Must be int
    },
    
    AddChild: struct {
        dst: Register,       // New gene (immutable)
        gene: Register,
        child: Register,
    },
    
    GetHead: struct {
        dst: Register,
        gene: Register,
    },
    
    // Bulk operations for literals
    CreateGeneLiteral: struct {
        dst: Register,
        template: GeneTemplate,  // Compile-time known structure
    },
```

### Collection Operations

```zig
    // Array operations
    CreateArray: struct {
        dst: Register,
        elements: []Register,
        capacity_hint: ?u32,
    },
    
    ArrayPush: struct {
        dst: Register,        // New array (immutable) or void (mutable)
        array: Register,
        value: Register,
    },
    
    ArrayPop: struct {
        dst: Register,        // Popped value
        array: Register,      // Modified in place if mutable
    },
    
    ArrayGet: struct {
        dst: Register,
        array: Register,
        index: Register,
        bounds_check: bool,
    },
    
    ArraySet: struct {
        dst: Register,        // New array (immutable) or void (mutable)
        array: Register,
        index: Register,
        value: Register,
        bounds_check: bool,
    },
    
    ArrayLength: struct {
        dst: Register,
        array: Register,
    },
    
    // Map operations
    CreateMap: struct {
        dst: Register,
        entries: []struct {
            key: Register,
            value: Register,
        },
        capacity_hint: ?u32,
    },
    
    MapGet: struct {
        dst: Register,
        map: Register,
        key: Register,
        default: ?Register,   // Optional default value
    },
    
    MapSet: struct {
        dst: Register,        // New map (immutable) or void (mutable)
        map: Register,
        key: Register,
        value: Register,
    },
    
    MapHas: struct {
        dst: Register,
        map: Register,
        key: Register,
    },
    
    MapDelete: struct {
        dst: Register,        // New map (immutable) or void (mutable)
        map: Register,
        key: Register,
    },
```

### String Operations

```zig
    // String operations with potential optimizations
    CreateString: struct {
        dst: Register,
        parts: []Register,    // Can be chars or strings
    },
    
    StringConcat: struct {
        dst: Register,
        strings: []Register,
        total_length_hint: ?u32,  // For pre-allocation
    },
    
    StringLength: struct {
        dst: Register,
        string: Register,
    },
    
    StringSubstring: struct {
        dst: Register,
        string: Register,
        start: Register,
        end: ?Register,       // Optional end
    },
    
    StringCharAt: struct {
        dst: Register,
        string: Register,
        index: Register,
    },
```

### OOP Operations

```zig
    // Class and method operations
    GetClass: struct {
        dst: Register,
        object: Register,
    },
    
    CreateClass: struct {
        dst: Register,
        name: []const u8,
        parent: ?Register,
        fields: []FieldDef,
        methods: []MethodDef,
    },
    
    LookupMethod: struct {
        dst: Register,
        class: Register,
        method_name: []const u8,
        cache_slot: ?u32,     // For inline caching
    },
    
    // Method calls with various optimizations
    CallMethod: struct {
        dst: Register,
        method: Register,
        receiver: Register,
        args: []Register,
        known_class: ?*ClassInfo,  // For devirtualization
    },
    
    CallDirect: struct {       // Devirtualized call
        dst: Register,
        function: *Function,
        receiver: Register,
        args: []Register,
    },
    
    CallSuper: struct {
        dst: Register,
        class: Register,       // Current class for super lookup
        method_name: []const u8,
        receiver: Register,
        args: []Register,
    },
    
    // Instance operations
    CreateInstance: struct {
        dst: Register,
        class: Register,
        field_inits: []struct {
            name: []const u8,
            value: Register,
        },
    },
    
    GetField: struct {
        dst: Register,
        object: Register,
        field_name: []const u8,
        offset: ?u32,         // Known offset for optimization
    },
    
    SetField: struct {
        dst: Register,        // New object (immutable) or void (mutable)
        object: Register,
        field_name: []const u8,
        value: Register,
        offset: ?u32,
    },
```

### Arithmetic Operations

```zig
    // Specialized arithmetic for performance
    AddInt: struct {
        dst: Register,
        left: Register,
        right: Register,
        checked: bool,        // Overflow checking
    },
    
    SubInt: struct {
        dst: Register,
        left: Register,
        right: Register,
        checked: bool,
    },
    
    MulInt: struct {
        dst: Register,
        left: Register,
        right: Register,
        checked: bool,
    },
    
    DivInt: struct {
        dst: Register,
        left: Register,
        right: Register,
    },
    
    // Float operations
    AddFloat: struct {
        dst: Register,
        left: Register,
        right: Register,
    },
    
    // ... similar for other float ops
    
    // Generic arithmetic (with method dispatch)
    Add: struct {
        dst: Register,
        left: Register,
        right: Register,
    },
```

### Comparison Operations

```zig
    // Comparisons return Bool
    Equal: struct {
        dst: Register,
        left: Register,
        right: Register,
        exact: bool,          // Use identity comparison
    },
    
    NotEqual: struct {
        dst: Register,
        left: Register,
        right: Register,
    },
    
    Less: struct {
        dst: Register,
        left: Register,
        right: Register,
    },
    
    Greater: struct {
        dst: Register,
        left: Register,
        right: Register,
    },
    
    // Type checking
    IsType: struct {
        dst: Register,
        value: Register,
        type: Register,       // Class object
    },
    
    AsType: struct {
        dst: Register,
        value: Register,
        type: Register,
        safe: bool,           // Runtime check vs unchecked
    },
```

### Control Flow

```zig
    // Basic control flow
    Jump: struct {
        target: BlockId,
    },
    
    Branch: struct {
        condition: Register,
        true_target: BlockId,
        false_target: BlockId,
    },
    
    Switch: struct {
        value: Register,
        cases: []struct {
            value: Value,
            target: BlockId,
        },
        default: BlockId,
    },
    
    Return: struct {
        value: ?Register,
    },
    
    // Exception handling
    Throw: struct {
        value: Register,
    },
    
    EnterTry: struct {
        catch_target: BlockId,
        finally_target: ?BlockId,
    },
    
    ExitTry: struct {},
    
    LandingPad: struct {
        dst: Register,        // Exception value
    },
};
```

## Basic Blocks and Control Flow

```zig
pub const BasicBlock = struct {
    id: BlockId,
    instructions: []LIRInst,
    terminator: LIRInst,      // Must be control flow
    predecessors: []BlockId,
    successors: []BlockId,
    
    // For optimization passes
    dominators: ?[]BlockId,
    loop_header: ?BlockId,
};

pub const Function = struct {
    name: []const u8,
    params: []Register,
    return_type: TypeInfo,
    blocks: []BasicBlock,
    entry_block: BlockId,
    
    // For optimization
    uses: std.AutoHashMap(Register, []InstRef),
    defs: std.AutoHashMap(Register, InstRef),
};
```

## Optimization Opportunities

### 1. Inline Caching
Property and method lookups include cache slots that can be filled at runtime for monomorphic call sites.

### 2. Devirtualization
When the receiver type is known, method calls can be converted to direct calls.

### 3. Unboxing
Arithmetic operations have specialized instructions that operate on unboxed values.

### 4. Escape Analysis
Determines whether Gene objects escape and can be stack-allocated.

### 5. Common Subexpression Elimination
The SSA form makes it easy to identify and eliminate redundant computations.

## Example: Function Call Compilation

```gene
# Gene source
(add ^debug true 1 2)

# LIR
block_0:
    %0 = LoadConst "add"
    %1 = LookupMethod Any, "add", cache_slot: 42
    %2 = CreateGene Call, props: [(debug, true)], children: [1, 2]
    %3 = CallMethod %1, %2, []
    Return %3

# After optimization (if add is known)
block_0:
    %0 = LoadConst 1
    %1 = LoadConst 2
    %2 = AddInt %0, %1, checked: false
    Return %2
```

## Integration with Pipeline

```
Source -> AST -> HIR -> MIR -> LIR -> Bytecode/Machine Code
                                 ↓
                          Optimization Passes
```

The LIR is designed to be:
1. **Easy to generate from MIR**: Direct mapping for most operations
2. **Easy to optimize**: SSA form and type information
3. **Easy to lower**: Clear mapping to VM instructions or machine code

## Memory Management

The LIR includes information for precise GC:

```zig
pub const GCPoint = struct {
    instruction: InstRef,
    live_references: []Register,  // Registers containing GC'd values
};

pub const Function = struct {
    // ... other fields ...
    gc_points: []GCPoint,
    stack_maps: []StackMap,
};
```

This design provides a solid foundation for implementing the unified Gene data format efficiently while enabling advanced optimizations.