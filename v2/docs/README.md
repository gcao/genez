# Gene v2 Documentation

This directory contains all documentation for the Gene v2 implementation.

## 📍 Quick Navigation

### Essential Documents
1. **[Implementation Status](IMPLEMENTATION_STATUS.md)** - What's done, what's next
2. **[Gene Format Reference](GENE_FORMAT_REFERENCE.md)** - Syntax guide  
3. **[Session Log](SESSION_LOG.md)** - Development history

### Design Documents
- **[Architecture](architecture.md)** - Overall system design
- **[Unified Gene Format](unified_gene_format.md)** - Core philosophy
- **[LIR Design](lir_design.md)** - Low-level IR specification
- **[VM Design](vm_unified_design.md)** - Virtual machine architecture
- **[Migration Strategy](unified_gene_migration.md)** - Why v2 exists

## 🎯 For Different Audiences

### 👨‍💻 Developers Implementing Features
1. Read [Implementation Status](IMPLEMENTATION_STATUS.md) 
2. Check [Session Log](SESSION_LOG.md) for context
3. Use [Gene Format Reference](GENE_FORMAT_REFERENCE.md) as syntax guide

### 🏗️ Understanding the Architecture  
1. Start with [Unified Gene Format](unified_gene_format.md)
2. Read [Architecture](architecture.md) for system overview
3. Dive into [LIR](lir_design.md) and [VM](vm_unified_design.md) designs

### 📖 Learning About Gene v2
1. Read [Unified Gene Format](unified_gene_format.md) for philosophy
2. See [Migration Strategy](unified_gene_migration.md) for v1 vs v2
3. Browse [Gene Format Reference](GENE_FORMAT_REFERENCE.md) for examples

## 💡 Key Concepts

**The Unified Format**: Everything in Gene v2 is represented as:
```
(type ^prop1 value1 ^prop2 value2 child1 child2)
```

This single innovation drives the entire v2 design - from parser to VM.