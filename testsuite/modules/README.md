# Module System Test Suite

This directory contains comprehensive tests for Gene's module and namespace systems.

## Test Organization

### Import Tests
- `import_basic.gene` - Basic import statements
- `import_relative.gene` - Relative imports (./module, ../module)
- `import_absolute.gene` - Absolute imports from project root
- `import_package.gene` - Package imports (searching in src directories)
- `import_syntax_variations.gene` - Various import syntax patterns
- `import_errors.gene` - Error handling for invalid imports
- `import_circular.gene` - Circular import detection
- `import_wildcards.gene` - Wildcard import patterns (future feature)
- `test_package_import.gene` - Package import with subdirectory resolution
- `test_selective_imports.gene` - Selective imports with aliasing

### Namespace Tests
- `namespace_basic.gene` - Basic namespace declarations
- `namespace_with_classes.gene` - Namespaces containing class definitions
- `namespace_syntax_variations.gene` - Various namespace syntax patterns
- `namespace_edge_cases.gene` - Edge cases (empty namespaces, do blocks)
- `namespace_with_classes.gene` - Classes defined within namespaces

### Module Definition Tests
- `module_combined.gene` - Combined namespace and module features
- `module_namespace_interaction.gene` - How modules interact with namespaces
- `module_resolution.gene` - Module path resolution
- `selective_imports.gene` - Selective import from in-memory namespace

### Feature Tests
- `basic_import_functionality.gene` - Core import functionality
- `import_new_features.gene` - New import features
- `verify_ast_structure.gene` - Verifies ns and import are special forms
- `current_limitations.gene` - Documents current limitations

### Helper Modules
These are used by various tests:
- `math.gene` - Math module with functions and constants
- `helpers.gene` - Simple helper functions
- `helper.gene` - Alternative helper module
- `utils.gene` - Utility functions
- `geometry.gene` - Geometry calculations
- `shapes.gene` - Shape definitions
- `algorithms.gene` - Algorithm implementations
- `math/basic.gene` - Basic math in subdirectory
- `math/advanced.gene` - Advanced math functions
- `utils/helpers.gene` - Helpers in subdirectory
- `data/structures.gene` - Data structure utilities
- `subdir/module1.gene`, `subdir/deep/module2.gene` - Nested modules

### Test Project
- `test_project/` - Complete project structure with package.gene
  - `package.gene` - Project configuration
  - `src/main.gene` - Main entry point
  - `lib/math.gene`, `lib/utils.gene` - Library modules
  - `src/helpers/string_utils.gene` - Nested helper module

## Current Implementation Status

### ✅ Fully Implemented
- Basic imports: `(import "module")`
- Relative imports: `(import "./module")`, `(import "../module")`
- Import with alias: `(import "module" => alias)`
- Selective imports: `(import "module" [item1 item2])`
- Selective imports with aliasing: `(import "module" [item => alias])`
- Package imports with subdirectory search
- Namespace declarations: `(ns name ...)`
- Hierarchical namespaces: `(ns com/example/app)`
- Module member access: `module/member`
- Module property syntax: `^property value`
- Circular import detection
- Module initialization and export rules
- Namespace-scoped variable resolution

### ❌ Not Yet Implemented
- Wildcard imports: `(import "module/*")`
- Explicit export control
- Module reloading
- Nested module definitions

## Running Tests

All module tests are run as part of the main test suite:
```bash
cd ../..  # Go to project root
./testsuite/run_tests.sh
```

To run specific module tests:
```bash
./zig-out/bin/gene run testsuite/modules/test_name.gene
```

## Test Conventions

1. Test files should have a corresponding `.expected` file with expected output
2. Use `println` for test output (not `print`) for consistent newlines
3. Comments in test files should explain what's being tested
4. Helper modules should have descriptive names and comments
5. Test both success cases and error cases

## Adding New Tests

1. Create a `.gene` test file with clear test cases
2. Run the test to get the output
3. Create a `.expected` file with the correct output
4. Add the test to this README in the appropriate section
5. Ensure the test passes in the full test suite