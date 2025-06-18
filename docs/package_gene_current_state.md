# Package.gene - Current State and Recommendations

## Current Parser Limitations

The Gene parser currently has these limitations regarding properties:

1. **No bare properties at top level**: `^name "value"` cannot appear outside of an expression
2. **Properties only work in maps**: The `^` syntax is only recognized inside `{}` (map literals)
3. **Properties in function calls are variables**: In `(package ^name "test")`, `^name` is parsed as a variable reference `var-ref "^name"`

## Working Format for Package.gene

Given current parser limitations, package.gene should use a **map literal**:

```gene
# package.gene - Using map literal (WORKS with current parser)
{
  ^name "my-package"
  ^version "1.0.0"
  ^description "A Gene package"
  ^author "Your Name"
  ^license "MIT"
  ^keywords ["gene" "example"]
  ^homepage "https://example.com"
  ^repository "https://github.com/user/repo"
  ^bugs "https://github.com/user/repo/issues"
  
  ^src-dirs ["src" "lib"]
  ^main "src/main"
  ^test-dir "tests"
  
  ^dependencies {
    ^gene-core "^1.0.0"
    ^http "~2.3.0"
  }
  
  ^dev-dependencies {
    ^gene-test "^1.0.0"
  }
  
  ^scripts {
    ^build "gene compile ${main}"
    ^test "gene test ${test-dir}"
    ^run "gene run ${main}"
  }
}
```

This creates a map with string keys (the `^` is stripped) and appropriate values.

## How to Load Package.gene

```zig
// In the package loader
pub fn loadPackageGene(allocator: Allocator, path: []const u8) !Package {
    const source = try std.fs.cwd().readFileAlloc(allocator, path, 1024 * 1024);
    defer allocator.free(source);
    
    // Parse as regular Gene source
    const result = try parser.parseGeneSource(allocator, source);
    
    // Should have exactly one expression - a map
    if (result.nodes.len != 1) return error.InvalidPackageFormat;
    
    const expr = result.nodes[0];
    if (expr != .Map) return error.PackageNotMap;
    
    // Extract fields from the map
    const map = expr.Map;
    const name = map.get("name") orelse return error.MissingPackageName;
    const version = map.get("version") orelse return error.MissingPackageVersion;
    
    // ... extract other fields
    
    return Package{
        .name = name.String,
        .version = version.String,
        // ...
    };
}
```

## Future Improvements

### Option 1: Extend Parser for Gene Properties
Modify the parser to recognize properties in Gene expressions:
```gene
(package
  ^name "my-package"
  ^version "1.0.0"
)
```
This would require updating `parseCall` to handle `^` prefixed identifiers specially.

### Option 2: Special Package Parser
Add a dedicated parser mode for package.gene that allows bare properties:
```gene
^name "my-package"
^version "1.0.0"
```

### Option 3: Use Gene Type
Once the Gene type is fully implemented with property support:
```gene
(package ^name "my-package" ^version "1.0.0"
  ^dependencies {
    ^gene-core "^1.0.0"
  })
```

## Recommendation

For now, use the **map literal format** as it works with the current parser and provides all needed functionality. The package loader can easily extract values from the resulting map structure.

## Test Files

- `examples/package_working.gene` - Attempts Gene expression format (doesn't work correctly)
- `testsuite/package_correct.gene` - Wrapped format attempt
- This document recommends using map literal format instead