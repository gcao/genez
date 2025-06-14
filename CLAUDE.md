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
# Run unit tests
zig build test

# Run comprehensive test script
./test.sh

# Run language test suite (REQUIRED before marking tasks as completed)
./testsuite/run_tests.sh
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

When modifying the compiler:
1. Always run unit tests (`zig build test`)
2. Run the language test suite (`./testsuite/run_tests.sh`) - ALL TESTS MUST PASS before marking any task as completed
3. Verify examples still work
4. Use debug mode (`--debug` flag) to understand compilation pipeline behavior

When creating temporary files and .gene files for testing, use `tmp/` directory to avoid polluting the main project directory.

## Language Test Suite

The `testsuite/` directory contains comprehensive language-level tests that verify Gene's features from a user perspective. These tests are CRITICAL for ensuring language correctness.

**Test Categories:**
- `basics/` - Literals, variables, basic syntax
- `control_flow/` - If/else, do blocks
- `functions/` - Function definitions and calls
- `arithmetic/` - Math operations and comparisons
- `strings/` - String operations and methods
- `arrays/` & `maps/` - Data structure operations
- `oop/` - Classes and object-oriented features
- `macros/` - Macro definitions and usage

**IMPORTANT**: Before marking ANY development task as completed, you MUST:
1. Run `./testsuite/run_tests.sh`
2. Ensure ALL tests pass (currently 19 tests should pass, 1 is skipped)
3. If your changes break existing tests, fix them before proceeding
4. Add new tests for any new features you implement

## Current Development Status

**Working Features:**
- Basic compilation pipeline (AST → HIR → MIR → Bytecode)
- Functions with parameters and recursion
- Conditionals (if/else) and arithmetic operators
- Variables and basic types (Int, String, Bool, Array, Map)
- Register-based VM with proper call frames
- Complete object-oriented programming system:
  - Core class hierarchy (Any, Number, Int, String, etc.)
  - Methods on primitive values (arithmetic, comparison, string operations)
  - Custom class definitions with fields and methods
  - Proper method dispatch and field access
- All example files execute correctly (including core_classes.gene)

**Recent Updates:**
- Core classes fully implemented with proper memory management
- Methods on primitives working (e.g., `(10 .+ 20)`, `("hello" .length)`)
- Field access uses slash notation: `obj/field`
- Parser supports operators as method names (/, %, etc.)
- Type checking infrastructure exists but disabled by default
- Comprehensive language test suite added in `testsuite/` with 19 passing tests
- Pattern matching implemented (literal, variable, wildcard patterns)
- Comparison operators > and == added
- Float literal parsing implemented
- Garbage collection implemented with mark-and-sweep algorithm
- GC API functions: gc_collect, gc_enable, gc_disable, gc_stats

**Next Development Priorities:**
1. **Module System** - Basic imports/exports and namespaces  
2. **Type Checking** - Enable and improve the existing type checker
3. **Error Handling** - try/catch/finally support
4. **String Operations** - String interpolation or number-to-string conversion
5. **Parser Improvements** - Method calls on literals, != operator

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