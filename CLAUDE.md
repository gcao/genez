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