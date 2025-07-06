# Gene v2 Architecture

## Overview

Gene v2 is a ground-up implementation of the Gene programming language based on the unified data format where everything is represented as:

```
(type ^prop1 value1 ^prop2 value2 child1 child2)
```

This document describes the architecture and key design decisions.

## Core Design Principles

### 1. Everything is a Gene

All values in the language are represented using the Gene type:

- **Primitives**: `42` → `(Int ^value 42)`
- **Strings**: `"hello"` → `(String ^value "hello")`
- **Arrays**: `[1 2 3]` → `(Array 1 2 3)`
- **Function calls**: `(f x y)` → `(Call ^target f x y)`
- **Method calls**: `(x .m y)` → `(MethodCall ^receiver x ^method m y)`

### 2. Everything is an Object

```gene
1                 → (Int ^value 1)  # 1 is an instance of Int
"hello"           → (String ^value "hello") # "hello" is an instance of String
[1 2 3]           → (Array 1 2 3) # [1 2 3] is an instance of Array
(add 1 2)         → (Call ^target add 1 2) # (Call ^target add 1 2) is an instance of Call (function call or macro call)
(x .add 5)        → (MethodCall ^receiver x ^method add 5)
("hello" .length) → (MethodCall ^receiver "hello" ^method length)
```

Method calls can be translated to function calls:

```gene
("hi" .length)  → ((String .get_method "length") "hi")
([1 2] .push 3) → ((Array .get_method "push") [1 2] 3)
```

### 3. Property Access

Any gene expression can have properties attached:

```gene
(add ^checked true ^optimize false 1 2)
(loop ^parallel true ...)
```

## Architecture Layers

### 1. Value Representation

```
Value (64-bit)
├── Immediate values (NaN-boxed)
│   ├── Nil
│   ├── Bool
│   ├── Int (48-bit)
│   ├── Float
│   ├── SmallString (≤6 bytes)
│   └── SmallSymbol (≤6 bytes)
└── Heap pointers
    ├── Gene
    ├── String
    ├── Symbol
    ├── Array
    ├── Set
    ├── Map
    ├── Class
    ├── Object
    └── Function
```

### 2. Gene Type

The Gene type is optimized for common patterns:

```zig
Gene
├── header: ObjectHeader (GC info)
├── head: *Value (type/tag)
├── props: PropertyStorage
│   ├── None (0 props)
│   ├── Single (1 prop) 
│   ├── Few (2-4 props)
│   └── Many (hash map)
└── children: ChildrenStorage
    ├── None (0 children)
    ├── Single (1 child)
    ├── Few (2-4 children)
    └── Many (array)
```

### 3. Compilation Pipeline

Most compilation happens at runtime on demand due to the dynamic nature of the language. Compilation occurs when all information is available. Some code is pre-compiled, other is compiled on demand. Part of a function body can be compiled first while the rest is compiled later.

```
Source Code
    ↓
Parser (direct to Gene)
    ↓
Gene AST
    ↓
Type Inference (optional)
    ↓
LIR Generation
    ↓
Optimization Passes
    ↓
Bytecode Generation
    ↓
VM Execution
```

### 4. Virtual Machine

Register-based VM with 256 general-purpose registers:

- **Specialized instructions** for Gene operations
- **Inline caching** for property/method access
- **Fast paths** for common operations (arithmetic)
- **Precise GC** with stack maps

## Memory Management

### Object Layout

All heap objects share a common header:

```zig
ObjectHeader (8 bytes)
├── gc_bits: u8 (mark bit, generation, etc.)
├── type_tag: u8 (quick type check)
└── size: u32 (object size)
```

### Garbage Collection

Generational GC with precise stack scanning:

- **Young generation**: Nursery for new objects
- **Old generation**: Long-lived objects
- **Stack maps**: Precise root identification
- **Write barriers**: For generational correctness

## Performance Optimizations

### 1. Small Object Optimization

- **SmallString**: Strings ≤6 bytes stored inline
- **Small Gene**: Optimized storage for ≤2 props, ≤4 children
- **Integer tagging**: Small integers in Value union

### 2. Inline Caching

Property and method lookups are cached:

```zig
InlineCache
├── key: Property/method name
├── class: Last seen class
├── offset: Direct offset
└── poly_cache: For polymorphic sites
```

### 3. Method Devirtualization

When receiver type is known at compile time:

```gene
(point .distance other)  →  Point_distance(point, other)
```

### 4. Escape Analysis

Stack allocate non-escaping objects:

```gene
(let [p (Point 3 4)]     # p can be stack allocated
  (p .magnitude))        # doesn't escape
```

## Class Hierarchy

```
Any
├── Nil
├── Void
├── Bool
├── Number
│   ├── Int
│   └── Float
├── String
├── Symbol
├── Array
├── Map
├── Set
├── Gene
├── Class
├── Function
└── Object (user instances)
```

## Standard Library

Built-in methods for each type:

### Any (root class)
- `equals(other)` - Equality comparison
- `hash()` - Hash code
- `toString()` - String representation
- `class()` - Get object's class

### Number
- Arithmetic: `+`, `-`, `*`, `/`, `%`
- Comparison: `<`, `>`, `<=`, `>=`
- Conversion: `toInt()`, `toFloat()`

### String
- `length()` - String length
- `concat(other)` - Concatenation
- `substring(start, end)` - Substring
- `split(separator)` - Split into array

### Array
- `length()` - Array length
- `get(index)` - Element access
- `set(index, value)` - Element update
- `push(value)` - Add to end
- `pop()` - Remove from end

### Gene
- `head()` - Get head/type
- `props()` - Get all properties
- `children()` - Get all children
- `getProp(key)` - Get property value
- `setProp(key, value)` - Set property
- `addChild(child)` - Add child

## Example: Function Call Compilation

```gene
# Source
(add ^checked true 1 2)

# Parser output (Gene AST)
Gene {
  head: Symbol("add"),
  props: {
    "checked": Bool(true)
  },
  children: [
    Int(1),
    Int(2)
  ]
}

# LIR
%0 = LoadSymbol "add"
%1 = LookupMethod %0, "__call__"
%2 = LoadConst 1
%3 = LoadConst 2
%4 = CreateGene Call, [(checked, true)], [%1, %2, %3]
%5 = CallMethod %1, %4, []

# Optimized (if add is known)
%0 = LoadConst 1
%1 = LoadConst 2  
%2 = AddInt %0, %1

# Bytecode
LoadConst R1, 1
LoadConst R2, 2
AddInt R0, R1, R2
```

## Future Optimizations

1. **JIT Compilation**: Hot functions compiled to native code
2. **Type Specialization**: Generate specialized code for known types
3. **Partial Evaluation**: Compile-time evaluation of constant expressions
4. **Profile-Guided Optimization**: Use runtime feedback for better code