# The Unified Gene Format

## Core Philosophy

In Gene v2, **everything** is represented in a single, uniform format:

```
(type ^prop1 value1 ^prop2 value2 child1 child2 ...)
```

This isn't just syntax - it's the fundamental data structure that powers the entire language.

## Why Unified?

### 1. Simplicity
- One data structure to rule them all
- No distinction between "AST nodes" and "values"
- Code and data are truly the same

### 2. Metaprogramming Power
- Macros manipulate the same structures programs use
- Reflection is trivial - just inspect the Gene
- Code generation is just Gene construction

### 3. Optimization Opportunities
- Uniform representation enables uniform optimizations
- Property attachment allows optimization hints
- Direct manipulation without intermediate forms

## Examples

### Everything Has The Same Shape

```gene
# A number
42 → (Int ^value 42)

# A function call  
(+ 1 2) → (Call ^target + 1 2)

# A class definition
(class Point ^fields [x y]) → (Class ^name Point ^fields [x y])

# Even control flow
(if test then else) → (If test then else)
```

### Properties Are First-Class

Properties can be attached to Gene expressions and Maps. Note that primitives, arrays, and sets cannot have properties directly attached - they must be wrapped in a Gene expression if properties are needed.

```gene
# Optimization hint
(loop ^unroll 4 ...)

# Type annotation
(var x ^type Int 42)

# Documentation
(fn add [x y] ^doc "Adds two numbers"
  (+ x y))
```

### Uniform Method Dispatch

Since everything is an object:

```gene
# Primitive method call
(1 .+ 2) → (MethodCall ^receiver 1 ^method + 2)

# String method
("hello" .length) → (MethodCall ^receiver "hello" ^method length)

# Even on Gene itself
(some-gene .get-prop ^type) → (MethodCall ^receiver some-gene ^method get-prop ^type)
```

## Implementation Benefits

### 1. Parser Simplicity
- Parser only needs to build Gene objects
- No separate AST types to maintain
- Direct representation from source

### 2. Memory Efficiency
- Optimized storage (Single/Few/Many pattern)
- Small object optimization built-in
- Properties only when needed

### 3. Performance
- Inline caching on uniform structure
- Predictable memory layout
- Fast property access

## Comparison with Traditional Approaches

### Traditional AST
```
BinaryOp {
  op: "+",
  left: IntLiteral { value: 1 },
  right: IntLiteral { value: 2 }
}
```

### Gene Unified Format
```
(Call ^target + 1 2)
```

The Gene format is:
- More concise
- Self-describing
- Directly manipulable
- Naturally extensible via properties

## Design Decisions

1. **Head is always first** - Enables quick dispatch
2. **Properties before children** - Clear visual separation
3. **Properties are named** - Self-documenting
4. **Children are positional** - Natural for most uses

## Future Possibilities

The unified format enables:
- **Gradual typing**: Type properties on any expression
- **Debugging**: Source location properties
- **Optimization**: Hints and directives as properties
- **Domain-specific**: Custom properties for DSLs

The unified Gene format is not just an implementation detail - it's the core innovation that makes Gene powerful, simple, and extensible.