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

When creating temporary files and .gene files for testing, use `tmp/` directory to avoid polluting the main project directory.

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

**Next Development Priorities:**
1. **Pattern Matching** - Runtime implementation (AST already parses it)
2. **Module System** - Basic imports/exports and namespaces  
3. **Type Checking** - Enable and improve the existing type checker
4. **Error Handling** - try/catch/finally support
5. **Parser Improvements** - Float literals, method calls on literals, != operator

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