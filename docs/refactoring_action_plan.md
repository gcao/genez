# Gene Language Refactoring Action Plan

Based on the responses in `inconsistencies_and_design_issues.md`, here's the action plan to address the identified issues.

## High Priority Actions

### 1. Clean Up Module System Implementation
**Decision**: Keep one implementation that handles module loading, compilation, and execution.

**Actions**:
- [ ] Delete redundant module files (`module.zig`, `module_system.zig`)
- [ ] Keep and consolidate `module_registry.zig` and `module_resolver.zig`
- [ ] Ensure modules have their own root namespace
- [ ] Implement actual file loading for imports

### 2. Implement Garbage Collection Throughout
**Decision**: GC should be used in both compilation and execution.

**Actions**:
- [ ] Remove manual memory management (`deinit` calls)
- [ ] Remove arena allocators except for temporary compilation data
- [ ] Integrate GC with VM properly
- [ ] Ensure GC can traverse all object types

### 3. Fix Parser Special Cases
**Decision**: Remove invalid special cases.

**Actions**:
- [ ] Remove `(/field)` as implicit self field access - it should call `self/field`
- [ ] Remove `(/field = value)` syntax - use `(= /field value)`
- [ ] Keep method call pattern checking

### 4. Implement Property Syntax (`^prop`)
**Decision**: Property syntax is a must for Phase 1.

**Actions**:
- [ ] Implement property execution in VM
- [ ] Support properties in objects, classes, and modules
- [ ] Handle property access and modification

### 5. Complete Import System
**Decision**: Start with basic import forms only.

**Actions**:
- [ ] Implement `(import "module")` - basic form
- [ ] Implement `(import "module" [a b c])` - specific items
- [ ] Actually load and compile imported modules
- [ ] Remove other import variations for now

### 6. Implement Unified Error Handling
**Decision**: Unified error type with source locations.

**Actions**:
- [ ] Create unified error type with source location
- [ ] Preserve source locations through compilation pipeline
- [ ] Implement consistent error reporting to stderr

## Medium Priority Actions

### 7. Fix Type System for Gradual Typing
**Decision**: Support gradual typing with `Any` as default.

**Actions**:
- [ ] Rewrite type checker to assume `Any` for untyped variables
- [ ] Support variadic functions as `(... :Any => :Any)`
- [ ] Enable type checking once fixed

### 8. Complete Macro System
**Decision**: Macros are a must for Phase 1.

**Actions**:
- [ ] Implement proper macro expansion
- [ ] Support both traditional and pseudo macros
- [ ] Integrate macro expansion into compilation pipeline

### 9. Consolidate Function Types
**Decision**: Keep both `fn` and `fnx` with clear distinction.

**Actions**:
- [ ] Ensure `fn` only allows named functions
- [ ] Ensure `fnx` only allows anonymous functions
- [ ] Update parser to enforce this distinction

## Low Priority Actions

### 10. Remove LIR Stage
**Decision**: Remove LIR for now.

**Actions**:
- [ ] Delete `lir.zig`, `mir_to_lir.zig`, `lir_to_bytecode.zig`
- [ ] Update documentation to reflect 3-stage pipeline

### 11. Skip Private Functions
**Decision**: Skip private functions for now.

**Actions**:
- [ ] Remove `fn-` from documentation
- [ ] Focus on public visibility only

### 12. VM Code Duplication
**Decision**: Code duplication is acceptable for now.

**Actions**:
- [ ] No immediate action required
- [ ] Consider refactoring in Phase 2

## Key Design Clarifications

### Field vs Method Syntax
- **Fields/Properties**: Use `/` (e.g., `obj/field`)
- **Methods**: Use `.` (e.g., `obj .method args`)
- **Rationale**: Fields access properties, methods call functions

### Module vs Namespace
- **Module**: Unit of code organization, belongs to packages
- **Namespace**: Groups related functions, variables, types
- **Relationship**: Each module has its own root namespace

### Memory Ownership
- **Compilation**: Modules owned by compiler
- **Runtime**: Modules transferred to runtime
- **Lifecycle**: Modules cached permanently, freed with runtime

## Implementation Order

1. **Week 1**: Clean up module system, implement GC
2. **Week 2**: Fix parser special cases, implement properties
3. **Week 3**: Complete import system, unified errors
4. **Week 4**: Fix type system, complete macros

This plan addresses all critical inconsistencies while maintaining the core design philosophy of Gene.