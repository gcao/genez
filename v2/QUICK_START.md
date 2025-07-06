# Gene v2 Quick Start Guide

## 🎯 For Developers Starting/Resuming Work

### Step 1: Understand Current State (2 min)

```bash
# See what's implemented and what's next
cat docs/IMPLEMENTATION_STATUS.md | grep -A 10 "Current Focus"

# Check recent work
tail -30 docs/SESSION_LOG.md
```

### Step 2: Verify Everything Works (1 min)

```bash
zig build test     # All tests should pass
zig build bench    # Check performance baseline
```

### Step 3: Current Priority - Parser Implementation

The foundation is complete. **Next task: Build the parser**.

#### Create Parser Structure:

```bash
mkdir -p src/parser
```

#### Files to Create:

1. **src/parser/token.zig**
   ```zig
   pub const TokenType = enum {
       // Literals
       Int, Float, String, Symbol,
       // Delimiters  
       LeftParen, RightParen, LeftBracket, RightBracket,
       // Special
       Property, // ^name
       Method,   // .method
       // ... etc
   };
   ```

2. **src/parser/lexer.zig**
   - Token stream generation
   - Handle numbers, strings, symbols
   - Property syntax `^name`
   - Method syntax `.method`

3. **src/parser/parser.zig**
   - Recursive descent parser
   - Generate Gene objects directly
   - No intermediate AST!

### Step 4: Integration Points

Update `main.zig` runRepl function:
```zig
// Instead of just echoing:
const gene = try parser.parse(allocator, line);
const result = try interpreter.eval(allocator, gene);
try stdout.print("{}\n", .{result});
```

### Step 5: Test Your Parser

Create test files:
```gene
# test/parser/basics.gene
42
"hello"
(+ 1 2)
[1 2 3]
{^x 1 ^y 2}
```

### 📋 Today's Checklist

- [ ] Create token types enum
- [ ] Implement basic lexer (numbers, strings, symbols)
- [ ] Parse simple expressions (atoms)
- [ ] Parse lists `(a b c)`
- [ ] Update IMPLEMENTATION_STATUS.md
- [ ] Add session notes to SESSION_LOG.md

### 🏃 Quick Commands

```bash
# Run tests after changes
zig build test

# Test parser on a file
./zig-out/bin/gene run test.gene

# Quick benchmark
zig build bench
```

### 📊 Performance Guidelines

Keep these benchmarks in mind:
- Property access: < 10ns
- Method dispatch: < 20ns (future)
- Parse time: < 1ms for typical files

### 🔑 Key Reminders

1. **Parser outputs Gene objects** - not a separate AST
2. **Everything is a Gene** - even `42` becomes `(Int ^value 42)`
3. **Properties are first-class** - parse `^prop value` syntax
4. **Update docs** - Track progress in IMPLEMENTATION_STATUS.md

### 📚 Essential References

While implementing the parser, refer to:
- **[Gene Format Reference](docs/GENE_FORMAT_REFERENCE.md)** - What to parse
- **[Architecture](docs/architecture.md#compilation-pipeline)** - How parser fits in
- **[Unified Format](docs/unified_gene_format.md)** - Why we parse this way

### 💡 Pro Tips

1. Start with atoms (numbers, strings, symbols)
2. Then lists and basic calls
3. Properties can be added incrementally
4. Use the existing Gene type - it's ready!

---

**Remember**: The goal is to get Gene code running ASAP. Start simple, iterate quickly!