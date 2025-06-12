# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Development Commands

**Build and Run:**
```bash
# Build both native and WASM targets
zig build

# Build with debug mode (enables detailed compilation pipeline output)
zig build -Ddebug=true

# Run a Gene source file
./zig-out/bin/gene run examples/fibonacci.gene

# Run with debug output (shows AST, HIR, MIR, and bytecode stages)
./zig-out/bin/gene run examples/fibonacci.gene --debug

# Compile source to bytecode (.gbc file)
./zig-out/bin/gene compile examples/default.gene

# Run compiled bytecode
./zig-out/bin/gene run examples/default.gbc
```

**Testing:**
```bash
# Run all tests
zig build test

# Run comprehensive test script
./test.sh
```

**WebAssembly:**
```bash
# Run via WASM runtime
wasmtime --dir . zig-out/bin/gene-wasi.wasm run examples/default.gene
```

## Architecture Overview

Gene is a programming language implementation featuring a 4-stage compilation pipeline:

**Compilation Pipeline:**
1. **Frontend** (`src/frontend/`) - Parsing Gene's Lisp-like syntax into AST
2. **HIR** (`src/ir/hir.zig`) - High-level IR that normalizes operators and simplifies AST
3. **MIR** (`src/ir/mir.zig`) - Mid-level IR that flattens control flow into basic blocks
4. **Backend** (`src/backend/`) - Bytecode generation and stack-based VM execution

**Key Components:**
- **Pipeline** (`src/pipeline.zig`) - Orchestrates the compilation stages, handles memory management
- **Runtime** (`src/runtime.zig`) - Entry point for file execution, integrates compilation and VM
- **Transforms** (`src/transforms/`) - Stage-to-stage conversions (AST→HIR→MIR→Bytecode)
- **Core** (`src/core/`) - Shared utilities, type system, and debug output infrastructure

**Important Design Patterns:**
- Each IR stage has its own serialization module for debugging (`*_serialize.zig`)
- Compilation uses arena allocators for AST memory management
- Debug mode shows complete pipeline transformation at each stage
- Tests are isolated in `src/tests/` and run via unified test runner (`src/all_tests.zig`)

**Type System:**
- Gradual typing with `Any` type for dynamic behavior
- `Value` union type handles runtime values across all stages
- Type checking infrastructure in `src/core/typechecker.zig` (currently minimal)

**Memory Management:**
- Arena allocators for parsing phase (cleaned up by pipeline)
- Individual stage results use provided allocators
- VM manages its own stack and execution state

When modifying the compiler, always run tests and verify examples still work. The debug mode (`--debug` flag) is essential for understanding compilation pipeline behavior.

When creating temporary files, use `tmp/` directory to avoid polluting the main project directory.

## Current Development Status

**Working Features:**
- Basic compilation pipeline (AST → HIR → MIR → Bytecode)
- Functions with parameters and recursion
- Conditionals (if/else) and arithmetic operators
- Variables and basic types (Int, Float, String, Bool)
- Register-based VM with proper call frames
- All example files now execute correctly (including fibonacci.gene)

**Recent Updates:**
- Basic type checking implemented in HIR (can be disabled via CompilerOptions.type_check)
- Runtime support for classes added (Class and Object types in Value enum)
- VM instructions for class operations (DefineClass, New, GetField, SetField, CallMethod)
- Type checking currently disabled by default in runtime.zig to allow testing

**Next Development Priorities (Phase 2):**
1. **Parser Support for Classes** - Complete class syntax parsing
2. **Pattern Matching** - Runtime implementation (AST already parses it)
3. **Module System** - Basic imports/exports and namespaces
4. **Error Handling** - try/catch/finally support
5. **Type Checker Improvements** - Handle built-in functions and more types

See `tmp/development_roadmap.md` for detailed next steps.

## Design Philosophy

Gene follows a pragmatic, phased implementation approach:
- **Phase 1** (Current): Core interpreter with 4-stage pipeline
- **Phase 2** (Next): Type system, classes, and core language features
- **Phase 3** (Future): Optimization, JIT, and advanced features
- **Phase 4** (Later): Concurrency, GC, and metaprogramming

The current implementation uses:
- Stack-based MIR (simpler than SSA form planned in design)
- 4-stage pipeline (skipping LIR until JIT is needed)
- Manual memory management (GC deferred to Phase 4)

This approach prioritizes getting a working language quickly while maintaining flexibility for future optimization.