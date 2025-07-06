# Migration to Unified Gene Data Format

## Overview

This document outlines the migration plan from the current implementation to a unified Gene data format where all data types can be represented as `(type ^prop1 value1 ^prop2 value2 child1 child2)`.

## Core Concepts

### 1. The Universal Gene Type

Every value in the language will be representable as a Gene:

```gene
# Primitives as Genes
"abc" = (String ^value "abc")  # or (String 'a' 'b' 'c')
42 = (Int ^value 42)
3.14 = (Float ^value 3.14)
true = (Bool ^value true)
nil = (Nil)

# Collections as Genes
[1 2 3] = (Array 1 2 3)
{^a 1 ^b 2} = (Map ^a 1 ^b 2)

# Functions as Genes
(fn add [a b] (+ a b)) = (Function ^name "add" ^params [a b] ^body (+ a b))

# Function calls as Genes
(f ^opt true 1 2) = (Call ^target f ^opt true 1 2)
```

### 2. Everything is an Object

All operations become method calls on objects:

```gene
# Instead of special bytecode for arithmetic
(+ 1 2) = (1 .+ 2) = (Int.+ 1 2)

# String operations
("hello" .length) = (String.length "hello")
("hello" .concat " world") = (String.concat "hello" " world")

# Array operations
([1 2 3] .push 4) = (Array.push [1 2 3] 4)
```

## Architecture Changes

### 1. New Value Representation

```zig
// Core Gene type that can represent any value
pub const Gene = struct {
    head: *Value,           // The type/tag (e.g., Int, String, Call)
    props: PropertyMap,     // Properties with ^ syntax
    children: []Value,      // Positional arguments/children
    meta: ?*MetaData,       // Source location, type info, etc.
};

pub const Value = union(enum) {
    // Atomic values (cannot have properties/children)
    Nil: void,
    Bool: bool,
    Int: i64,
    Float: f64,
    Symbol: []const u8,
    
    // Complex values are always Genes
    Gene: *Gene,
    
    // Special VM values
    ReturnAddress: ReturnInfo,
    Register: u16,
};

pub const PropertyMap = union(enum) {
    Empty: void,
    Single: struct { key: []const u8, value: *Value },
    Few: [4]?struct { key: []const u8, value: *Value },  // Small map optimization
    Many: std.StringHashMap(*Value),
};
```

### 2. Class Hierarchy

```gene
# Root of the hierarchy
class Any {
  (.fn to_gene [] => Gene)  # Convert any value to Gene representation
  (.fn == [other] => Bool)
  (.fn hash [] => Int)
}

# Type classes
class Type < Any {
  (.fn new [^props... children...] => Any)
  (.fn instance? [obj] => Bool)
}

# Concrete type classes
class Int < Type {
  (.fn + [other] => Int)
  (.fn - [other] => Int)
  (.fn * [other] => Int)
  (.fn / [other] => Int)
}

class String < Type {
  (.fn length [] => Int)
  (.fn concat [other] => String)
  (.fn substr [start end] => String)
}

class Array < Type {
  (.fn push [item] => Array)
  (.fn pop [] => Any)
  (.fn get [index] => Any)
  (.fn length [] => Int)
}

class Gene < Type {
  (.fn head [] => Any)
  (.fn props [] => Map)
  (.fn children [] => Array)
  (.fn get_prop [key] => Any)
  (.fn set_prop [key value] => Gene)
  (.fn add_child [child] => Gene)
}
```

## LIR (Low-level IR) Design

The LIR will be designed to efficiently support Gene operations and method dispatch:

```
// LIR Instructions
enum LIRInstruction {
    // Gene creation and manipulation
    CreateGene { dst: Reg, head: Reg, prop_count: u16, child_count: u16 },
    SetGeneProp { gene: Reg, key: Reg, value: Reg },
    GetGeneProp { dst: Reg, gene: Reg, key: Reg },
    AddGeneChild { gene: Reg, child: Reg },
    GetGeneChild { dst: Reg, gene: Reg, index: Reg },
    GetGeneHead { dst: Reg, gene: Reg },
    
    // Optimized collection operations
    CreateArray { dst: Reg, size: u16 },
    ArrayPush { array: Reg, value: Reg },
    ArrayPop { dst: Reg, array: Reg },
    ArrayGet { dst: Reg, array: Reg, index: Reg },
    ArraySet { array: Reg, index: Reg, value: Reg },
    
    CreateMap { dst: Reg, size: u16 },
    MapSet { map: Reg, key: Reg, value: Reg },
    MapGet { dst: Reg, map: Reg, key: Reg },
    MapHas { dst: Reg, map: Reg, key: Reg },
    
    // String operations
    CreateString { dst: Reg, chars: []Reg },
    StringConcat { dst: Reg, str1: Reg, str2: Reg },
    StringLength { dst: Reg, str: Reg },
    StringSubstr { dst: Reg, str: Reg, start: Reg, end: Reg },
    
    // OOP operations
    GetClass { dst: Reg, obj: Reg },
    LookupMethod { dst: Reg, class: Reg, method_name: Const },
    CallMethod { dst: Reg, method: Reg, receiver: Reg, args: []Reg },
    CallSuper { dst: Reg, class: Reg, method_name: Const, receiver: Reg, args: []Reg },
    CreateInstance { dst: Reg, class: Reg },
    GetField { dst: Reg, obj: Reg, field_name: Const },
    SetField { obj: Reg, field_name: Const, value: Reg },
    
    // Control flow
    Jump { target: Label },
    JumpIf { cond: Reg, target: Label },
    JumpIfNot { cond: Reg, target: Label },
    Return { value: Reg },
    
    // Basic operations
    Move { dst: Reg, src: Reg },
    LoadConst { dst: Reg, value: Const },
    LoadNil { dst: Reg },
    
    // Comparison (returns Bool)
    Equal { dst: Reg, left: Reg, right: Reg },
    NotEqual { dst: Reg, left: Reg, right: Reg },
    Less { dst: Reg, left: Reg, right: Reg },
    Greater { dst: Reg, left: Reg, right: Reg },
    
    // Type checking
    IsType { dst: Reg, obj: Reg, type: Reg },
    AsType { dst: Reg, obj: Reg, type: Reg },  // Cast with runtime check
}
```

## VM Bytecode Design

The VM will have specialized instructions for common operations while maintaining the Gene abstraction:

```
enum Opcode {
    // Stack machine basics
    Push(Value),
    Pop,
    Dup,
    Swap,
    
    // Gene operations
    MakeGene,           // head props... children... -> gene
    GetProp,            // gene key -> value
    SetProp,            // gene key value -> gene'
    GetChild,           // gene index -> value
    AddChild,           // gene child -> gene'
    GetHead,            // gene -> head
    
    // Fast paths for common types
    MakeArray(u16),     // n items -> array
    MakeMap(u16),       // n pairs -> map
    MakeString(u16),    // n chars -> string
    
    ArrayPush,          // array item -> array'
    ArrayPop,           // array -> array' item
    ArrayGet,           // array index -> item
    
    MapGet,             // map key -> value
    MapSet,             // map key value -> map'
    
    // Method dispatch
    LookupMethod,       // obj method_name -> method
    CallMethod(u8),     // method receiver args... -> result
    CallSuper(u8),      // class method_name receiver args... -> result
    
    // OOP
    NewInstance,        // class -> instance
    GetField,           // instance field_name -> value
    SetField,           // instance field_name value -> instance'
    
    // Control
    Jump(i16),
    JumpIfTrue(i16),
    JumpIfFalse(i16),
    Return,
    
    // Optimized arithmetic (for performance)
    AddInt,             // int int -> int
    SubInt,             // int int -> int
    MulInt,             // int int -> int
    DivInt,             // int int -> int
    
    // Comparison
    Equal,              // a b -> bool
    Less,               // a b -> bool
    Greater,            // a b -> bool
}
```

## Migration Phases

### Phase 1: Core Infrastructure
1. Implement the Gene type and new Value representation
2. Update the parser to create Gene nodes for all expressions
3. Implement property syntax on all expressions

### Phase 2: Type System
1. Implement the class hierarchy with Any as root
2. Make all built-in types (Int, String, etc.) proper classes
3. Implement method dispatch for primitive operations

### Phase 3: Compiler Pipeline
1. Update HIR to work with Gene nodes
2. Design and implement the new LIR
3. Update MIR to LIR transformation
4. Implement new VM bytecode

### Phase 4: Runtime
1. Implement Gene manipulation instructions in VM
2. Optimize common patterns (e.g., arithmetic on unboxed integers)
3. Implement inline caching for property/method access

### Phase 5: Standard Library
1. Reimplement built-in functions as methods
2. Ensure backward compatibility with a compatibility layer
3. Update all tests

## Performance Considerations

1. **Small Gene Optimization**: Genes with ≤2 properties and ≤2 children use a specialized compact representation
2. **Inline Caching**: Property and method lookups are cached at call sites
3. **Unboxing**: The VM can operate on unboxed integers/floats for arithmetic
4. **Method Devirtualization**: Common method calls can be devirtualized when types are known

## Example Transformations

### Before (Current Implementation)
```gene
(+ 1 2)
(arr .push 42)
(if (> x 10) "big" "small")
```

### After (Unified Gene Format)
```gene
# Internally represented as:
(Call ^target + 1 2)
(Call ^target push ^receiver arr 42)
(If ^condition (Call ^target > x 10) "big" "small")

# But still written as:
(+ 1 2)
(arr .push 42)
(if (> x 10) "big" "small")
```

## Backward Compatibility

A compatibility layer will translate old-style code to the new representation:

```gene
# Old style function definition
(fn add [a b] (+ a b))

# Translates to:
(Function ^name add ^params [a b] ^body (Call ^target + a b))

# But creates the same callable function
```

## Testing Strategy

1. **Unit Tests**: Test each Gene operation in isolation
2. **Integration Tests**: Ensure all language features work with the new representation
3. **Performance Tests**: Verify no significant performance regression
4. **Compatibility Tests**: Ensure existing code continues to work

## Implementation Order

1. Start with the Gene type and basic operations
2. Implement Gene creation in the parser
3. Add LIR with Gene support
4. Update VM with new instructions
5. Migrate built-in types to the class hierarchy
6. Update the standard library
7. Optimize common patterns

This migration will fundamentally change how Gene represents and manipulates data, making it truly uniform while maintaining performance through careful optimization of common cases.