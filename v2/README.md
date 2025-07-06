# Gene v2

A ground-up reimplementation of the Gene programming language based on the revolutionary unified data format.

## 🚀 Quick Start

```bash
# Build and test
zig build test           # Run tests (all should pass)
zig build bench          # Run performance benchmarks

# Run Gene
./zig-out/bin/gene repl  # Start REPL
./zig-out/bin/gene test  # Run built-in tests
```

See **[QUICK_START.md](QUICK_START.md)** for development guide.

## 🎯 Core Concept

In Gene v2, **everything** is represented uniformly as:

```
(type ^prop1 value1 ^prop2 value2 child1 child2)
```

This means:
- `42` is really `(Int ^value 42)`
- `(+ 1 2)` is really `(Call ^target + 1 2)` 
- Even control flow follows this pattern

See **[docs/unified_gene_format.md](docs/unified_gene_format.md)** for the philosophy.

## 📊 Current Status

✅ **Completed**
- Core Gene type with optimized storage
- Value system with all types
- Test suite (100% passing)
- Benchmark framework
- CLI structure

🚧 **Next: Parser Implementation**
- Direct Gene AST generation
- No intermediate representation

See **[docs/IMPLEMENTATION_STATUS.md](docs/IMPLEMENTATION_STATUS.md)** for detailed progress.

## 📚 Documentation

- **[Quick Start](QUICK_START.md)** - Start developing
- **[Implementation Status](docs/IMPLEMENTATION_STATUS.md)** - Track progress  
- **[Gene Format Reference](docs/GENE_FORMAT_REFERENCE.md)** - Syntax guide
- **[Architecture](docs/architecture.md)** - System design
- **[All Docs](docs/)** - Complete documentation

## 🏗️ Architecture Highlights

- **Optimized Storage**: Properties and children use None/Single/Few/Many tiers
- **Direct AST**: Parser generates Gene objects, not intermediate forms
- **Everything is an Object**: `(1 .+ 2)` - even primitives have methods
- **Properties Everywhere**: `(add ^checked true 1 2)` - attach metadata to any expression

## 🔧 Development

When resuming work:
1. Check `docs/IMPLEMENTATION_STATUS.md` for current state
2. Read `docs/SESSION_LOG.md` for recent changes
3. Run `zig build test` to ensure everything works
4. Update status docs when making progress

## 📈 Performance

Current benchmarks show excellent performance:
- Property access: 2ns (Single), 9ns (Few)
- Child access: 1ns
- Gene creation: ~5μs (needs object pooling)

## 🤝 Contributing

This is a research project exploring the unified data format concept. The current priority is implementing the parser to enable actual Gene code execution.