# Gene v2 Quick Start Guide

## For Developers Resuming Work

### 1. Check Current Status
```bash
cat docs/IMPLEMENTATION_STATUS.md   # See what's done and what's next
cat docs/SESSION_LOG.md            # See what was done in last session
```

### 2. Build and Test
```bash
zig build          # Build everything
zig build test     # Run tests (should all pass)
zig build bench    # Run benchmarks
./zig-out/bin/gene test  # Run built-in tests
```

### 3. Understand the Code Structure
- `src/gene.zig` - Core Gene type with optimized storage
- `src/value.zig` - Value representation (union type)
- `src/main.zig` - CLI entry point
- `src/tests.zig` - Test suite
- `src/bench.zig` - Benchmarks

### 4. Next Implementation Steps

**Parser (Priority 1)**:
1. Create `src/parser/lexer.zig` for tokenization
2. Create `src/parser/parser.zig` for Gene AST generation
3. Integrate with REPL in `main.zig`

**Simple Interpreter (Priority 2)**:
1. Create `src/interpreter.zig` for tree-walking evaluation
2. Test with examples before building full VM

### 5. Key Design Decisions Made

- **Optimized Storage**: Properties and children use None/Single/Few/Many tiers
- **Direct AST**: Parser generates Gene objects, not intermediate AST
- **Everything is Gene**: Even primitives are internally `(Int ^value 42)`
- **Method Calls**: All operations become method calls `(+ 1 2)` → `(1 .+ 2)`

### 6. Testing Your Changes

When adding new features:
1. Add unit tests in the relevant module
2. Run `zig build test` frequently
3. Update `docs/IMPLEMENTATION_STATUS.md`
4. Add notes to `docs/SESSION_LOG.md` before finishing

### 7. Performance Targets

Current benchmarks show:
- Property access: 2ns (Single), 9ns (Few)
- Child access: 1ns
- Gene creation: ~5μs (needs pooling)

Maintain or improve these numbers!

### 8. Examples

Run the REPL:
```bash
./zig-out/bin/gene repl
> hello
You entered: hello
> exit
```

The parser will make this actually evaluate Gene expressions!

### 9. Documentation

Key references:
- `docs/GENE_FORMAT_REFERENCE.md` - Syntax examples
- `docs/architecture.md` - Overall design
- `docs/unified_gene_migration.md` - Why v2 exists

### 10. Getting Help

The codebase is designed to be self-documenting. Each major type has doc comments explaining its purpose and usage.