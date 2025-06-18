# Package.gene Parsing Specification

## Overview

The `package.gene` file is a special Gene source file that defines package metadata. Unlike regular Gene files, it has special parsing rules to make package definitions more concise and readable.

## Current Parser Behavior

The current Gene parser:
1. Does **not** support bare properties at the top level
2. Properties (`^name value`) can only appear inside gene expressions (parentheses)
3. When properties appear as arguments in a function call, they are parsed as variable references (e.g., `^name` becomes `var-ref "^name"`)

## Proposed Solutions

### Option 1: Special Parser Mode (Recommended)

Add a special parsing mode for `package.gene` files that treats bare properties as properties of an implicit GeneDocument:

```gene
# package.gene - special parsing mode
^name "my-package"
^version "1.0.0"
^dependencies {
  ^foo "1.0.0"
}
```

Would be parsed as if it were:

```gene
(package
  ^name "my-package"
  ^version "1.0.0"
  ^dependencies {
    ^foo "1.0.0"
  })
```

### Option 2: Wrapped Format

Require package.gene to use standard Gene syntax with an explicit wrapper:

```gene
# package.gene - standard Gene syntax
(package
  ^name "my-package"
  ^version "1.0.0"
  ^description "A Gene package"
  ^author "Your Name"
  ^license "MIT"
  
  ^src-dirs ["src" "lib"]
  ^main "src/main"
  
  ^dependencies {
    ^gene-core "^1.0.0"
    ^http "~2.3.0"
  }
  
  ^scripts {
    ^test "gene test"
    ^build "gene build"
  })
```

### Option 3: Document-Level Properties

Extend the parser to support properties at the document level for all `.gene` files, not just package.gene:

```gene
# Any .gene file could have document properties
^module "utils/math"
^exports [add subtract multiply]

(fn add [a b] (+ a b))
(fn subtract [a b] (- a b))
```

## Implementation Details for Option 1

### Parser Changes

1. Add a `parsePackageGene` function that:
   - Accepts bare properties at the top level
   - Collects them into a GeneDocument structure
   - Validates required fields (name, version)

2. Modify `parseGeneSource` to detect when parsing `package.gene`:
   ```zig
   pub fn parseGeneSource(allocator: Allocator, source: []const u8, filename: ?[]const u8) !ParseResult {
       if (filename) |name| {
           if (std.mem.endsWith(u8, name, "package.gene")) {
               return parsePackageGene(allocator, source);
           }
       }
       // Regular parsing...
   }
   ```

### AST Representation

The package.gene content would be represented as:

```zig
pub const PackageDocument = struct {
    properties: std.StringHashMap(Value),
    location: SourceLocation,
    
    // Helper methods
    pub fn getName(self: *const PackageDocument) ?[]const u8 { ... }
    pub fn getVersion(self: *const PackageDocument) ?[]const u8 { ... }
    pub fn getDependencies(self: *const PackageDocument) ?std.StringHashMap([]const u8) { ... }
};
```

## Validation Rules

Required fields:
- `^name` - Package name (string)
- `^version` - Package version (string)

Optional fields:
- `^description` - Package description (string)
- `^author` - Author name/email (string)
- `^license` - License identifier (string)
- `^keywords` - Array of keywords (array of strings)
- `^homepage` - Project homepage (string)
- `^repository` - Source repository URL (string)
- `^bugs` - Issue tracker URL (string)
- `^src-dirs` - Source directories (array of strings, default: ["src"])
- `^main` - Main module path (string)
- `^dependencies` - Runtime dependencies (map of name to version)
- `^dev-dependencies` - Development dependencies (map of name to version)
- `^scripts` - Named scripts (map of name to command)

## Example Parser Implementation

```zig
fn parsePackageGene(allocator: Allocator, source: []const u8) !ParseResult {
    var tokens = try tokenize(allocator, source);
    var properties = std.StringHashMap(Value).init(allocator);
    var current: usize = 0;
    
    while (current < tokens.len) {
        const token = tokens[current];
        
        switch (token.kind) {
            .Property => {
                // Parse ^name value pattern
                const prop_name = token.kind.Property;
                current += 1;
                
                if (current >= tokens.len) {
                    return error.ExpectedValueAfterProperty;
                }
                
                const value = try parseExpression(allocator, tokens[current..], 0);
                try properties.put(prop_name, value.node);
                current += value.consumed;
            },
            .Comment => {
                current += 1; // Skip comments
            },
            .Newline => {
                current += 1; // Skip newlines
            },
            else => {
                return error.UnexpectedTokenInPackageGene;
            }
        }
    }
    
    // Validate required fields
    if (!properties.contains("name")) return error.MissingPackageName;
    if (!properties.contains("version")) return error.MissingPackageVersion;
    
    return ParseResult{
        .node = .{ .Package = properties },
        .consumed = tokens.len,
    };
}
```

## Migration Path

1. Initially support both wrapped and bare formats
2. Detect format by first non-comment token:
   - If `(`, use standard parser
   - If `^`, use package parser
3. Eventually deprecate wrapped format

## Benefits of Special Parsing

1. **Cleaner syntax** - No need for wrapping parentheses
2. **Familiar format** - Similar to package.json, Cargo.toml
3. **Clear intent** - package.gene files are visually distinct
4. **Better errors** - Can provide package-specific error messages

## Alternative: Just Use Data Format

Instead of making package.gene a Gene source file, it could be a pure data format:

```toml
# package.toml style
name = "my-package"
version = "1.0.0"

[dependencies]
gene-core = "^1.0.0"
```

But this would require a separate parser and lose the benefits of using Gene's own syntax.