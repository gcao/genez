# Unified Gene Format Implementation Plan

## Phase 1: Foundation (Week 1-2)

### 1.1 Create Core Gene Type
- [ ] Define `Gene` struct in `src/core/gene.zig`
- [ ] Implement property storage with small optimization
- [ ] Implement children storage with small optimization
- [ ] Add basic operations: create, get/set prop, get/add child

### 1.2 Update Value Type
- [ ] Add `Gene` variant to `Value` union
- [ ] Update value cloning and cleanup
- [ ] Add conversion functions: `toGene()`, `fromGene()`

### 1.3 Parser Updates
- [ ] Add property syntax to all expressions
- [ ] Parse `(expr ^prop value ...)` syntax
- [ ] Create Gene nodes for function calls
- [ ] Update AST to use Gene representation

## Phase 2: Type System (Week 3-4)

### 2.1 Class Hierarchy
- [ ] Make `Any` the root class
- [ ] Update built-in types to be classes:
  - [ ] `Int < Number < Any`
  - [ ] `Float < Number < Any`
  - [ ] `String < Any`
  - [ ] `Array < Any`
  - [ ] `Map < Any`
  - [ ] `Gene < Any`

### 2.2 Method System
- [ ] Convert operators to methods:
  - [ ] Arithmetic: `+`, `-`, `*`, `/` become methods on Number
  - [ ] Comparison: `==`, `<`, `>` become methods on Any
  - [ ] Collection operations become methods
- [ ] Update method dispatch to use class hierarchy

### 2.3 Built-in Methods
- [ ] Implement core methods for each type:
  - [ ] `Any`: `to_gene`, `==`, `hash`, `to_string`
  - [ ] `Int/Float`: arithmetic, comparison
  - [ ] `String`: `length`, `concat`, `substr`
  - [ ] `Array`: `push`, `pop`, `get`, `set`
  - [ ] `Map`: `get`, `set`, `has`, `delete`

## Phase 3: LIR Implementation (Week 5-6)

### 3.1 LIR Core
- [ ] Create `src/ir/lir.zig`
- [ ] Define LIR instruction types
- [ ] Implement basic block structure
- [ ] Add SSA form support

### 3.2 MIR to LIR Lowering
- [ ] Create `src/transforms/mir_to_lir.zig`
- [ ] Lower MIR expressions to LIR
- [ ] Handle Gene operations specially
- [ ] Implement type-based optimizations

### 3.3 LIR Optimizations
- [ ] Implement basic optimizations:
  - [ ] Dead code elimination
  - [ ] Common subexpression elimination
  - [ ] Constant folding
  - [ ] Method devirtualization

## Phase 4: VM Updates (Week 7-8)

### 4.1 New Instructions
- [ ] Add Gene manipulation instructions
- [ ] Add optimized collection operations
- [ ] Add inline cache support
- [ ] Update instruction encoding

### 4.2 Runtime Support
- [ ] Implement Gene object in runtime
- [ ] Add property access with caching
- [ ] Update method dispatch
- [ ] Implement fast paths for common operations

### 4.3 Memory Management
- [ ] Update GC to handle Gene objects
- [ ] Implement object header
- [ ] Add stack maps for precise GC
- [ ] Optimize allocation for small Genes

## Phase 5: Compiler Pipeline (Week 9-10)

### 5.1 Update HIR
- [ ] Make HIR Gene-aware
- [ ] Preserve property information
- [ ] Update HIR to MIR lowering

### 5.2 Update Bytecode Generation
- [ ] LIR to bytecode lowering
- [ ] Generate efficient code for Gene operations
- [ ] Implement inline cache initialization

### 5.3 Testing Infrastructure
- [ ] Update test framework for new representation
- [ ] Add Gene-specific tests
- [ ] Performance benchmarks

## Phase 6: Standard Library (Week 11-12)

### 6.1 Core Functions
- [ ] Reimplement as methods where appropriate
- [ ] Add Gene manipulation functions
- [ ] Update module system

### 6.2 Compatibility Layer
- [ ] Translator for old-style code
- [ ] Deprecation warnings
- [ ] Migration guide

### 6.3 Documentation
- [ ] Update language reference
- [ ] Add Gene format examples
- [ ] Performance tuning guide

## Testing Strategy

### Unit Tests
1. Gene type operations
2. Property access performance
3. Method dispatch correctness
4. Type conversions

### Integration Tests
1. Existing test suite must pass
2. New Gene-specific features
3. Performance regression tests

### Example Test Cases

```gene
# Test 1: Basic Gene creation
(test "gene creation"
  (var g = (Int ^value 42))
  (assert (== g 42))
  (assert (== (g .get-prop ^value) 42))
  (assert (== (g .head) Int)))

# Test 2: Properties on function calls
(test "call properties"
  (var result = (add ^checked true 100 200))
  (assert (== result 300))
  (assert (== (result .get-prop ^checked) true)))

# Test 3: Method dispatch
(test "method dispatch"
  (assert (== (42 .+ 8) 50))
  (assert (== ("hello" .length) 5))
  (assert (== ([1 2 3] .push 4) [1 2 3 4])))
```

## Performance Targets

1. **Property access**: < 10ns for cached access
2. **Method dispatch**: < 20ns for monomorphic calls
3. **Gene creation**: < 50ns for small Genes
4. **Arithmetic**: No regression vs current implementation

## Risk Mitigation

1. **Performance**: Keep fast paths for common operations
2. **Memory**: Small object optimizations critical
3. **Compatibility**: Extensive testing of existing code
4. **Complexity**: Incremental implementation with testing

## Milestones

1. **M1** (Week 2): Gene type working with basic operations
2. **M2** (Week 4): Type system unified, methods working
3. **M3** (Week 6): LIR complete with optimizations
4. **M4** (Week 8): VM supporting Gene operations
5. **M5** (Week 10): Full pipeline working
6. **M6** (Week 12): Standard library and docs complete

## Success Criteria

- All existing tests pass
- Performance within 10% of current implementation
- Gene format examples working correctly
- Clean, maintainable codebase
- Comprehensive documentation

## Next Steps

1. Create `gene` branch for development
2. Set up performance benchmarking
3. Begin with Gene type implementation
4. Regular progress reviews and adjustments