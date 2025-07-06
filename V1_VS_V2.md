# Gene v1 vs v2

This repository contains two implementations of the Gene programming language:

## Gene v1 (Current Directory)

The original implementation with a traditional compiler architecture:
- **Status**: Feature-complete and functional
- **Architecture**: Traditional AST → HIR → MIR → Bytecode pipeline
- **Features**: 
  - Full OOP with inheritance and super calls
  - Pattern matching on arrays and maps
  - Module system with imports/exports
  - Macro system
  - Exception handling
  - String interpolation
  - Comprehensive test suite (19/20 tests passing)

V1 follows a more conventional language implementation approach with distinct AST node types and a multi-stage compilation pipeline.

## Gene v2 (v2/ Directory)

A ground-up rewrite based on the unified Gene data format:
- **Status**: Foundation complete, parser implementation next
- **Philosophy**: Everything is represented as `(type ^prop1 value1 child1 child2)`
- **Key Differences**:
  - No separate AST types - everything is a Gene
  - Properties can be attached to any expression
  - All operations are method calls (even arithmetic)
  - Designed for metaprogramming from the ground up
  - Register-based VM instead of stack-based
  - LIR (Low-level IR) for better optimizations

## Why Two Versions?

After implementing many features in v1, we realized that Gene's true potential lies in its unified data format where:
- Code and data share the same representation
- Any expression can have metadata attached
- The language is inherently reflective
- Optimizations can work on the same structure users manipulate

V2 is not just a rewrite but a fundamental rethinking of how Gene should work internally to match its external philosophy.

## Which Should I Use?

- **Use v1** if you want to experiment with a working Gene implementation today
- **Watch v2** for the future direction of the language

## Development Status

- **v1**: Maintenance mode - bug fixes only
- **v2**: Active development - implementing parser next

See [v2/docs/IMPLEMENTATION_STATUS.md](v2/docs/IMPLEMENTATION_STATUS.md) for current v2 progress.