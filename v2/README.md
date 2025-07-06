# Gene v2 - Unified Data Format Implementation

A ground-up reimplementation of the Gene programming language with a unified data format where everything can be represented as:

```
(type ^prop1 value1 ^prop2 value2 child1 child2)
```

## Key Design Principles

1. **Everything is a Gene**: All values, including primitives, are represented in the uniform Gene format
2. **Everything is an Object**: All operations are methods on objects
3. **Properties Everywhere**: Any expression can have properties attached
4. **Performance by Design**: Optimizations built-in from the start (small object optimization, inline caching)

## Architecture

- **Gene Type**: Core data structure with optimized storage for properties and children
- **Register-based VM**: 256 general-purpose registers with specialized Gene operations
- **LIR**: Low-level IR in SSA form for optimizations
- **Inline Caching**: Fast property and method access
- **Generational GC**: Precise garbage collection with stack maps

## Building

```bash
zig build                 # Build the interpreter
zig build test           # Run tests
zig build bench          # Run benchmarks
zig build run -- file.gene  # Run a Gene file
```

## Status

This is a complete rewrite focusing on the unified Gene format.

**Key Documents**:
- [Implementation Status](docs/IMPLEMENTATION_STATUS.md) - Current progress tracker
- [Session Log](docs/SESSION_LOG.md) - Development history
- [Format Reference](docs/GENE_FORMAT_REFERENCE.md) - Unified format examples
- [Architecture](docs/architecture.md) - Overall design

See [docs/](docs/) for all design documents.

- [x] Project structure
- [x] Design documents
- [x] Gene type implementation with optimized storage
- [x] Value representation with NaN-boxing concept
- [x] Basic test suite
- [x] Memory benchmark framework
- [ ] Parser implementation
- [ ] LIR generation
- [ ] VM execution
- [ ] Method dispatch
- [ ] Standard library

## Differences from v1

- Unified data format from the ground up
- No separate AST node types - everything is a Gene
- Register-based VM instead of stack-based
- LIR for better optimizations
- Methods on primitive types