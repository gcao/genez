# Gene Implementation Status

This document tracks the current implementation status against the design specification.

## Summary

Gene is a functional interpreter with a working 4-stage compilation pipeline (AST → HIR → MIR → Bytecode). The core language features are implemented including functions, recursion, conditionals, macros, and a complete object-oriented programming system with methods on primitive values. The implementation takes a pragmatic approach, deferring advanced features like JIT compilation and garbage collection until the core language is more mature.

**Last Updated**: 2025-01-18 - Added incompatibilities section and updated feature status based on http_server.gene debugging.

## Compilation Pipeline

### Design Specification
- 5-stage pipeline: AST → HIR → MIR → LIR → Bytecode/Native
- Dual execution paths (interpreted and compiled)
- SSA form in MIR for optimization

### Current Implementation
- 4-stage pipeline: AST → HIR → MIR → Bytecode
- LIR stage is skipped
- MIR uses stack-based operations, not SSA
- Only interpreted execution path

### Recommendation
Keep the current 4-stage pipeline for now. The LIR stage can be added later when implementing JIT/AOT compilation. The current approach is simpler and working well.

## VM Architecture

### Design Specification
- Register-based VM with fixed-size registers
- Inline caches for property access
- Profile-guided optimization
- Three-tier execution (interpreter → baseline JIT → optimizing JIT)

### Current Implementation
- Hybrid approach: register-based bytecode with stack-based MIR
- Simple register allocation without optimization
- No inline caches or profiling
- Interpreter only

### Recommendation
The current register-based bytecode is a good foundation. Continue with this approach and add optimizations incrementally.

## Type System

### Design Specification
- Gradual typing with full type inference
- Property-based types with `^` syntax
- Union and intersection types
- Generic types with constraints
- Full OOP support

### Current Implementation
- Basic type enum in `src/core/types.zig`
- Value union type for runtime values
- Core class hierarchy implemented (Any, Number, Int, Float, String, etc.)
- Methods on primitive values working
- Basic type checking infrastructure (disabled by default)
- No property types or generics yet

### Recommendation
The type hierarchy is well-designed. Next steps should focus on:
1. Enabling and improving type checking in HIR
2. Implementing property types (`^` syntax)
3. Adding generics and type constraints

## Completed Features ✅

1. **Basic Compilation Pipeline**
   - Parser (S-expressions) ✓
   - AST construction ✓
   - HIR transformation ✓
   - MIR generation ✓
   - Bytecode generation ✓

2. **Core Language Features**
   - Functions with parameters ✓
   - Recursion ✓
   - Conditionals (if/else) ✓
   - Basic arithmetic (+, -, *, /) ✓
   - Comparison operators (<, >, ==) ✓
   - Variables (var) ✓
   - Print function ✓
   - Macros with lazy evaluation ✓
   - Unquote syntax (%) ✓
   - Basic pattern matching (literal, variable, wildcard) ✓
   - Float literals ✓

3. **VM Execution**
   - Register-based bytecode ✓
   - Function calls with proper stack frames ✓
   - Return values ✓
   - Recursive function support ✓

4. **Object-Oriented Programming**
   - Core class hierarchy (Any, Number, Int, String, etc.) ✓
   - Methods on primitive values ✓
   - Custom class definitions ✓
   - Field access and assignment ✓
   - Method dispatch ✓
   - VM instructions (DefineClass, New, GetField, SetField, CallMethod) ✓
   - Proper memory management for classes ✓
   - != operator (was incorrectly listed as not implemented) ✓

## In Progress 🚧

1. **Type System**
   - Type checking infrastructure exists but not active
   - Runtime values support all basic types
   - Need to enable type checking by default

2. **Error Handling**
   - Basic error propagation in Zig
   - No language-level try/catch yet

## Not Started ❌

1. **Advanced Compilation**
   - LIR stage
   - JIT compilation
   - AOT compilation
   - Optimization passes

2. **Memory Management**
   - Basic mark-and-sweep garbage collection ✓
   - GC API functions (gc_collect, gc_enable, gc_disable, gc_stats) ✓
   - Reference counting option
   - Memory pooling

3. **Concurrency**
   - Actors
   - Channels
   - STM (Software Transactional Memory)

4. **Language Features**
   - Advanced pattern matching (only literal, variable, wildcard patterns work)
     - Array patterns parsed but not implemented in runtime
     - Map patterns parsed but not implemented in runtime
     - Constructor patterns not implemented
     - Guard clauses not implemented
   - Package and Module System (NEW DESIGN)
     - Module loading and caching
     - Import syntax and resolution
     - Package.gene manifest parsing
     - Package path resolution
     - Ad-hoc package support
     - See docs/packages_and_modules.md for design
   - Properties (`^` shorthand syntax)
   - Automatic constructor calls with arguments
   - do blocks returning last expression value
   - Method calls on literals (must use variables)
   - String interpolation or number-to-string conversion

5. **Standard Library**
   - Collections
   - I/O
   - Network
   - File system

## Recommendations

1. **Keep Current Architecture**: The 4-stage pipeline is working well. Don't add LIR until needed for JIT.

2. **Focus on Type Checking**: Enable the basic type checking that's already implemented in HIR.

3. **Improve Error Messages**: Add better error reporting with line numbers and context.

4. **Complete Pattern Matching**: The AST support exists, implement the runtime execution.

5. **Add Module System**: This is essential for larger programs and code organization.

6. **Defer Advanced Features**: JIT, GC, and concurrency can wait until the core language is more complete.

7. **Update Design.md**: Consider updating the design to reflect the current pragmatic approach while keeping the long-term vision.

## Design vs Implementation Incompatibilities

This section documents areas where the current implementation differs from the design specification.

**Status Legend:**
- ✓ RESOLVED - Issue has been resolved
- 🚧 TO BE FIXED - Planned for implementation
- 🔧 DEFER - Lower priority, fix later

**Summary:** Most incompatibilities are planned to be fixed. Priority items include map access syntax, implicit self method calls, property syntax, if expression bugs, and function values in arrays.

### 1. **Map/Dictionary Access Syntax** 🚧 TO BE FIXED
- **Design**: Unified access pattern using `/` for objects, maps, and genes (e.g., `map/key`)
- **Implementation**: Maps require `.get` method: `(map .get "key")`
- **Plan**: Implement `map/key` syntax for map access
- **Impact**: Less uniform syntax, requires different access patterns for different data types

### 2. **Constructor Naming** ✓ RESOLVED
- **Design**: Updated to use `.ctor` for constructors (2025-01-18)
- **Implementation**: Uses `.ctor` for constructors
- **Status**: Design and implementation now aligned

### 3. **Implicit Self Method Calls** 🚧 TO BE FIXED
- **Design**: `(.method)` as shorthand for `(self .method)`
- **Implementation**: Not supported - must use explicit `(self .method)`
- **Plan**: Implement implicit self method call syntax
- **Impact**: More verbose method calls within classes

### 4. **Property Syntax** 🚧 TO BE FIXED
- **Design**: Properties with `^` prefix are a core language feature
- **Implementation**: Only supported in map/gene literals, not as general language feature
- **Plan**: Implement `^` properties as general language feature
- **Impact**: Limited use of property-based design

### 5. **Implicit Self Field Access in Function Arguments** 🔧 DEFER
- **Design**: `/field` should work anywhere within methods
- **Implementation**: Bug where `/field` doesn't work correctly as function arguments
- **Workaround**: Use explicit `self/field` or assign to variable first
- **Plan**: Fix later as lower priority

### 6. **If Expression Return Values** 🚧 TO BE FIXED
- **Design**: If expressions return their branch values
- **Implementation**: Bug with negated conditions returning boolean instead of branch value
- **Plan**: Fix if expression evaluation to return branch values correctly
- **Workaround**: Restructure conditions to avoid negation in certain cases

### 7. **Function Values in Arrays** 🚧 TO BE FIXED
- **Design**: Functions are first-class values that can be stored in arrays
- **Implementation**: Functions stored in arrays sometimes resolve as nil
- **Plan**: Fix function value resolution in arrays
- **Impact**: Limits functional programming patterns

These incompatibilities should be addressed in future updates to align the implementation with the design vision.