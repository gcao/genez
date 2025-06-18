# Package and Module System Design Notes

## Overview

This document contains the updated design for Gene's package and module system, which should replace Chapter 8 in the main design.md document.

## Key Changes from Original Design

### 1. Package Concept Added
- Original design only had modules and namespaces
- New design introduces packages as containers for modules
- Two types: canonical (with package.gene) and ad-hoc (for scripts)

### 2. Module Path Resolution
- Three types of paths: absolute (/), relative (./), and package paths
- Clear resolution rules and search order
- Package paths searched in configured source directories

### 3. Package.gene Manifest
- Defines package metadata, dependencies, and configuration
- Similar to package.json in Node.js or Cargo.toml in Rust
- Supports semantic versioning for dependencies

### 4. Simplified Namespace Model
- Each module automatically creates a namespace
- No explicit `(ns ...)` blocks needed (though could be supported)
- Namespace matches the import path

### 5. Import Syntax Enhancements
- Added `:as` for aliasing
- Added `{}` syntax for renaming multiple imports
- Added `import?` for conditional imports
- Kept `*` for importing all public items

## Implementation Priority

1. **Phase 1: Basic Module Loading**
   - File-based modules only
   - Simple import with full qualification
   - Module caching

2. **Phase 2: Package Support**
   - Package.gene parsing
   - Package resolution algorithm
   - Source directory searching

3. **Phase 3: Advanced Features**
   - Import aliasing and renaming
   - Visibility rules (private/public)
   - Circular dependency detection

4. **Phase 4: Package Management**
   - Dependency resolution
   - Package registry integration
   - Archive package support

## Migration Path

For existing code:
- Simple imports like `(import math)` continue to work
- Namespace access with `/` remains the same
- Add package.gene to formalize existing projects

## Examples Location

See `/examples/package_demo/` for a complete example package structure.

## Design Rationale

1. **Slash notation consistency**: Using `/` for both namespace access and paths provides consistency
2. **Explicit package root**: package.gene clearly marks package boundaries
3. **Ad-hoc packages**: Enables quick scripting without boilerplate
4. **Source directories**: Allows flexible project organization
5. **Familiar concepts**: Borrows good ideas from other languages while maintaining Gene's style

## To Do

- [ ] Replace Chapter 8 in design.md with the new design
- [ ] Update parser to support import syntax
- [ ] Implement module loader
- [ ] Add package.gene parser
- [ ] Create module cache system
- [ ] Add to implementation_status.md