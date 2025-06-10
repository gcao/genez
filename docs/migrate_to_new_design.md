# Migration from Current Implementation to Design Document

This document outlines the key differences between the current Gene implementation and the comprehensive design specified in `docs/design.md`, providing a roadmap for bringing the implementation up to the design specification.

## Architecture Overview

### Current Implementation (5-stage pipeline) ✅ UPDATED
```
Source Code (.gene files)
    ↓
Parser (AST Generation)
    ↓
AST → HIR (High-level IR)
    ↓
HIR → MIR (Mid-level IR)
    ↓
MIR → LIR (Low-level IR) ✅ NEW
    ↓
LIR → Bytecode (Stack-based VM)
```

**Recent Progress:**
- ✅ Successfully implemented 5-stage compilation pipeline
- ✅ `default.gene` now executes correctly and outputs expected result (prints "3")
- ✅ Built-in functions (`print`, `+`, `-`, etc.) working properly
- ✅ Variable resolution system functioning correctly
- ✅ Fixed memory management issues in value conversion between stages
- ✅ Enhanced VM to handle built-in operators through proper dispatch system

### Design Document Target
```
Source Code (.gene files)
    ↓
Lexer (Tokenization)
    ↓
Parser (AST Generation)
    ↓
Macro Expansion
    ↓
Type Checker (Optional)
    ↓
HIR Generation
    ↓
MIR Generation & Optimization
    ↓
LIR Generation (Register-based)
    ↓
VM Execution / JIT Compilation
```

## Key Differences and Migration Tasks

### 1. IR Pipeline Differences

#### Current HIR (`src/ir/hir.zig`)
- Very basic with simple expressions
- Limited type system: void, bool, int, float, string, function
- Basic operations: literals, variables, binary ops, if expressions, function calls

#### Target HIR (Design Document)
- Rich semantic representation
- Classes, traits, inheritance
- Pattern matching
- Closures and lexical scoping
- Advanced type system with gradual typing
- Module system support

**Migration Tasks:**
- [ ] Expand HIR type system to support all design document types
- [ ] Add class/trait/inheritance representations
- [ ] Implement pattern matching IR nodes
- [ ] Add closure capture analysis
- [ ] Add module and namespace support

#### Current MIR (`src/ir/mir.zig`)
- Simple instruction set with basic blocks
- Stack-based operations
- Basic control flow (jumps, conditional jumps)
- Limited optimization

#### Target MIR (Design Document)
- Full SSA (Static Single Assignment) form
- Sophisticated optimization passes:
  - Dead code elimination
  - Constant propagation
  - Common subexpression elimination
  - Loop optimizations
  - Escape analysis
- Register allocation preparation

**Migration Tasks:**
- [ ] Convert MIR to SSA form
- [ ] Implement optimization passes
- [ ] Add register allocation hints
- [ ] Improve basic block representation

#### LIR Stage ✅ COMPLETED
**Previous:** MIR → Bytecode  
**Current:** MIR → LIR → Bytecode  
**Status:** ✅ **Implemented**

**Completed Tasks:**
- ✅ Created LIR module (`src/ir/lir.zig`) with comprehensive register-based instruction set
- ✅ Implemented register-based instruction set with 40+ instruction types
- ✅ Added basic register allocation (simple sequential allocation)
- ✅ Created MIR → LIR transform (`src/transforms/mir_to_lir.zig`)
- ✅ Created LIR → Bytecode transform (`src/transforms/lir_to_bytecode.zig`)
- ✅ Added LIR serialization for debug output (`src/ir/lir_serialize.zig`)
- ✅ Integrated LIR stage into compiler pipeline

**Remaining Tasks:**
- [ ] Implement advanced register allocation algorithms (graph coloring, linear scan)
- [ ] Add register liveness analysis
- [ ] Implement register spilling to memory

### 2. Type System Migration

#### Current Type System
```zig
pub const Type = enum {
    void,
    bool,
    int,
    float,
    string,
    function,
};
```

#### Target Type System (Design Document)
```gene
Any                    # Root type - all values are Any
├── Void              # No value (function returns)
├── Nil               # The nil value
├── Bool              # true/false
├── Number            # Abstract numeric type
│   ├── Int           # 48-bit integers (NaN-boxed)
│   └── Float         # 64-bit IEEE 754 floats
├── Char              # Unicode character
├── Str               # UTF-8 string
├── Symbol            # Interned symbol
├── ComplexSymbol     # Symbol with namespace/path
├── Map               # Key-value mapping
├── Array             # Indexed sequence
├── Set               # Unique value collection
├── Gene              # Generic container (X ^prop val ...)
├── Fn                # Function type
├── Class             # Class metaobject
├── Trait             # Trait metaobject
├── Module            # Module/namespace
├── Ref               # Mutable reference
├── Atom              # Atomic reference
├── Chan              # Channel for concurrency
├── Promise           # Future value
└── ...               # User-defined types
```

**Migration Tasks:**
- [ ] Implement comprehensive type hierarchy in `src/core/types.zig`
- [ ] Add generic type support (Array<T>, Map<K,V>, etc.)
- [ ] Implement union and intersection types
- [ ] Add type inference engine
- [ ] Support gradual typing with Any type
- [ ] Add trait/interface system

### 3. Missing Language Features

#### Object-Oriented Programming
**Current:** None  
**Target:** Full OOP with classes, inheritance, traits, mixins

**Migration Tasks:**
- [ ] Implement class definitions in AST/HIR
- [ ] Add inheritance mechanisms
- [ ] Create trait system (implements relationship)
- [ ] Add mixin system (includes relationship)
- [ ] Implement method dispatch (static, virtual, dynamic)
- [ ] Add property access with inline caching

#### Pattern Matching
**Current:** None  
**Target:** Full pattern matching on all data types

**Migration Tasks:**
- [ ] Add match expressions to AST
- [ ] Implement pattern compilation in HIR
- [ ] Add destructuring for arrays, maps, objects
- [ ] Support guard clauses
- [ ] Optimize pattern matching with dispatch trees

#### Module System
**Current:** None  
**Target:** Namespace-based modules with explicit imports/exports

**Migration Tasks:**
- [ ] Add namespace declarations
- [ ] Implement import/export system
- [ ] Create module loading infrastructure
- [ ] Add private/public visibility
- [ ] Support module properties and metadata

#### Concurrency
**Current:** None  
**Target:** Actor model, channels, STM, futures

**Migration Tasks:**
- [ ] Implement actor system
- [ ] Add channel-based communication
- [ ] Create Software Transactional Memory (STM)
- [ ] Add future/promise support
- [ ] Implement parallel collections

#### Error Handling
**Current:** Basic exceptions  
**Target:** Exceptions + Result types + error boundaries

**Migration Tasks:**
- [ ] Implement Result<T,E> type
- [ ] Add try/catch with pattern matching
- [ ] Create error propagation mechanisms
- [ ] Add assertions and contracts
- [ ] Implement error boundaries

#### Metaprogramming
**Current:** None  
**Target:** Macros, reflection, code generation

**Migration Tasks:**
- [ ] Add macro system with hygiene
- [ ] Implement reflection APIs
- [ ] Add compile-time code generation
- [ ] Support quasi-quotation
- [ ] Create AST manipulation tools

### 4. VM Architecture Migration

#### Current VM
- Stack-based bytecode execution
- Simple instruction set
- Basic value representation

#### Target VM (Design Document)
- Register-based execution
- NaN-boxing for value representation
- Inline caching for dynamic dispatch
- Tiered compilation (interpreter → baseline JIT → optimizing JIT)

**Migration Tasks:**
- [ ] Convert to register-based VM architecture
- [ ] Implement NaN-boxing value representation
- [ ] Add inline caching infrastructure
- [ ] Create call frame management
- [ ] Implement baseline JIT compiler
- [ ] Add optimizing JIT with deoptimization

### 5. Memory Management Migration

#### Current GC
- Basic garbage collection (if any)

#### Target GC (Design Document)
- Incremental mark-and-sweep
- Generational collection
- Concurrent marking
- Write barriers

**Migration Tasks:**
- [ ] Implement generational garbage collector
- [ ] Add incremental collection
- [ ] Create write barrier system
- [ ] Support weak references and finalizers
- [ ] Add memory profiling tools

### 6. Value Representation Migration

#### Current Representation
- Simple tagged unions or similar

#### Target Representation (Design Document)
```
64-bit Value Layout:
- Floats: IEEE 754 double (except NaN patterns)
- Integers: 48-bit signed integers in NaN space
- Pointers: 48-bit pointers with type tags
- Immediates: nil, true, false, small strings
```

**Migration Tasks:**
- [ ] Implement NaN-boxing in `src/core/value.zig`
- [ ] Add small string optimization
- [ ] Create efficient object header design
- [ ] Optimize for 64-bit architectures

### 7. Standard Library Development

#### Current Standard Library
- Minimal built-ins

#### Target Standard Library (Design Document)
- Comprehensive modules: core, collections, string, math, io, concurrent, time, system

**Migration Tasks:**
- [ ] Create gene/core module
- [ ] Implement gene/collections with persistent data structures
- [ ] Add gene/string with regex support
- [ ] Create gene/math with advanced numeric operations
- [ ] Implement gene/io with async I/O
- [ ] Add gene/concurrent with actor primitives
- [ ] Create gene/time with timezone support
- [ ] Add gene/system for OS interfaces

## Migration Priority

### Phase 1: Core Language Features
1. Expand type system
2. Add object-oriented programming
3. Implement pattern matching
4. Create module system

### Phase 2: Advanced Features
1. Add concurrency primitives
2. Implement error handling
3. Create metaprogramming system
4. Add comprehensive standard library

### Phase 3: Performance Optimization
1. Convert to register-based VM
2. Implement NaN-boxing
3. Add inline caching
4. Create JIT compilation

### Phase 4: Memory Management
1. Implement generational GC
2. Add concurrent collection
3. Optimize memory layout
4. Add profiling tools

## Implementation Strategy

1. **Incremental Migration**: Implement features one at a time while maintaining working system
2. **Test-Driven Development**: Add comprehensive tests for each new feature
3. **Backward Compatibility**: Ensure existing code continues to work
4. **Performance Benchmarking**: Measure performance impact of each change
5. **Documentation**: Update documentation as features are implemented

## Recent Implementation Status (December 2024)

### ✅ Files Created (LIR Stage Implementation)
- `src/ir/lir.zig` - ✅ **COMPLETED** - Low-level IR with register-based instruction set (40+ instructions)
- `src/ir/lir_serialize.zig` - ✅ **COMPLETED** - Debug serialization for LIR stage
- `src/transforms/mir_to_lir.zig` - ✅ **COMPLETED** - MIR to LIR conversion with register allocation
- `src/transforms/lir_to_bytecode.zig` - ✅ **COMPLETED** - LIR to bytecode conversion

### ✅ Files Modified (LIR Integration & Built-ins)
- `src/compiler.zig` - ✅ **UPDATED** - Added LIR stage to compilation pipeline with debug output
- `src/backend/vm.zig` - ✅ **ENHANCED** - Added built-in operator support, improved call dispatch
- `src/core/types.zig` - ✅ **EXTENDED** - Added Print to BuiltinOperatorType enum
- `src/pipeline.zig` - ✅ **MAINTAINED** - Continues to orchestrate expanded pipeline

### 🎯 Currently Working Features
- **Basic arithmetic**: Addition, subtraction with proper type handling
- **Print function**: Properly registered and functional built-in
- **Variable resolution**: Symbols resolve to actual functions/operators
- **Function calls**: Built-in operators work through enhanced call mechanism
- **Memory management**: Proper value cloning and cleanup between stages
- **Debug output**: Complete pipeline visibility with `--debug` flag

## Files to Create/Modify (Remaining)

### New Files Needed
- `src/core/value.zig` - NaN-boxing value representation
- `src/frontend/macro.zig` - Macro expansion
- `src/backend/jit.zig` - JIT compilation
- `src/gc/` - Garbage collection modules
- `src/stdlib/` - Standard library modules
- `src/concurrent/` - Concurrency primitives

### Major Modifications Needed
- `src/ir/hir.zig` - Expand to full semantic representation
- `src/ir/mir.zig` - Convert to SSA form with optimizations
- `src/backend/vm.zig` - Convert to register-based execution (currently stack-based)
- `src/core/types.zig` - Implement comprehensive type system
- `src/frontend/parser.zig` - Add support for all language constructs

This migration represents a substantial evolution from a basic language prototype to a comprehensive, production-ready programming language implementation.