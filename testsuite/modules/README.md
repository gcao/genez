# Module System Test Suite

This directory contains tests for Gene's module system, including namespace declarations and import statements.

## Test Files

### Basic Tests
- `namespace_basic.gene` - Tests basic namespace declarations
- `import_basic.gene` - Tests basic import statements
- `module_combined.gene` - Tests combined namespace and import usage

### Advanced Tests
- `namespace_with_classes.gene` - Tests namespaces containing class definitions
- `namespace_syntax_variations.gene` - Tests various namespace syntax patterns
- `import_syntax_variations.gene` - Tests various import syntax patterns
- `namespace_edge_cases.gene` - Tests edge cases like empty namespaces

### Verification Tests
- `verify_ast_structure.gene` - Verifies that ns and import are parsed as special forms
- `current_limitations.gene` - Documents current limitations of the module system

## Current Status

The module system currently supports:
- ✅ Parsing `(ns name ...)` declarations
- ✅ Parsing `(import "module")` statements
- ✅ Import with aliases: `(import "module" => alias)`
- ✅ Import specific items: `(import "module" [item1 item2])`
- ✅ Import with renaming: `(import "module" [[old new]])`
- ✅ Nested namespace paths: `(ns com/example/app)`
- ✅ Namespaces containing functions, classes, variables, and macros

Not yet implemented:
- ❌ Namespace resolution and scoping
- ❌ Module loading from files
- ❌ Slash notation for accessing namespace members (e.g., `math/sin`)
- ❌ Import resolution and symbol binding
- ❌ Module exports

## Running Tests

From the testsuite directory:
```bash
./run_tests.sh
```

To run module tests only:
```bash
for test in modules/*.gene; do
  ../zig-out/bin/gene run "$test"
done
```