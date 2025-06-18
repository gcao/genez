# Package.gene Parser Enhancement Implementation

## Overview

Enhance the Gene parser to support bare property syntax at the top level for package.gene files, where properties become fields of a Gene document.

## Implementation Steps

### 1. Add Property Token Type

The parser already has the infrastructure to recognize properties in maps. We need to extend this to work at the top level.

### 2. Modify Parser Entry Points

```zig
// Add filename parameter to parseGeneSource
pub fn parseGeneSource(
    parent_allocator: std.mem.Allocator, 
    source: []const u8,
    filename: ?[]const u8  // NEW: optional filename
) !ParseSourceResult {
    // Check if this is a package.gene file
    if (filename) |name| {
        if (std.mem.endsWith(u8, name, "package.gene")) {
            return parsePackageGene(parent_allocator, source);
        }
    }
    // Continue with regular parsing...
}
```

### 3. Implement parsePackageGene Function

```zig
fn parsePackageGene(parent_allocator: std.mem.Allocator, source: []const u8) !ParseSourceResult {
    // Create arena allocator
    const arena = try parent_allocator.create(std.heap.ArenaAllocator);
    arena.* = std.heap.ArenaAllocator.init(parent_allocator);
    errdefer {
        arena.deinit();
        parent_allocator.destroy(arena);
    }
    const arena_allocator = arena.allocator();

    // Tokenize
    var tokens = try tokenize(arena_allocator, source);
    
    // Create a map to hold properties
    var properties = std.ArrayList(ast.MapEntry).init(arena_allocator);
    
    var pos: usize = 0;
    while (pos < tokens.items.len) {
        const token = tokens.items[pos];
        
        switch (token.kind) {
            .Ident => |ident| {
                // Check if it's a property (starts with ^)
                if (std.mem.startsWith(u8, ident, "^")) {
                    // Parse property
                    const prop_result = try parseTopLevelProperty(
                        arena_allocator, 
                        tokens.items[pos..], 
                        0
                    );
                    try properties.append(prop_result.entry);
                    pos += prop_result.consumed;
                } else {
                    return error.UnexpectedTokenInPackageGene;
                }
            },
            .RParen => {
                // Skip stray closing parens
                pos += 1;
            },
            else => {
                return error.UnexpectedTokenInPackageGene;
            }
        }
    }
    
    // Create a map node with all properties
    const map_expr = ast.Expression{
        .Map = .{ .entries = properties.toOwnedSlice() }
    };
    
    const map_node = ast.AstNode{ .Expression = map_expr };
    
    // Return single node (the map)
    const nodes_slice = try arena_allocator.alloc(ast.AstNode, 1);
    nodes_slice[0] = map_node;
    
    return ParseSourceResult{
        .arena = arena,
        .nodes = nodes_slice,
    };
}
```

### 4. Implement parseTopLevelProperty

```zig
const PropertyParseResult = struct {
    entry: ast.MapEntry,
    consumed: usize,
};

fn parseTopLevelProperty(
    alloc: std.mem.Allocator, 
    toks: []const Token, 
    depth: usize
) !PropertyParseResult {
    if (toks.len < 2) return error.UnexpectedEOF;
    
    // First token should be property (^name)
    const prop_token = toks[0];
    if (prop_token.kind != .Ident) return error.ExpectedProperty;
    
    const ident = prop_token.kind.Ident;
    if (!std.mem.startsWith(u8, ident, "^")) {
        return error.ExpectedProperty;
    }
    
    // Extract property name (remove ^)
    const prop_name = ident[1..];
    
    // Create string key
    const key_expr = ast.Expression{
        .Literal = .{ .value = .{ .String = try alloc.dupe(u8, prop_name) } }
    };
    const key_ptr = try alloc.create(ast.Expression);
    key_ptr.* = key_expr;
    
    // Parse value
    const value_result = try parseExpression(alloc, toks[1..], depth + 1);
    const value_ptr = try alloc.create(ast.Expression);
    value_ptr.* = value_result.node.Expression;
    
    return PropertyParseResult{
        .entry = .{ .key = key_ptr, .value = value_ptr },
        .consumed = 1 + value_result.consumed,
    };
}
```

### 5. Update Pipeline

```zig
// In pipeline.zig
pub fn compileFile(allocator: std.mem.Allocator, path: []const u8, options: compiler.CompilerOptions) !CompiledResult {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    const source = try file.readToEndAlloc(allocator, std.math.maxInt(usize));
    defer allocator.free(source);

    // Pass filename to parser
    return try compileSourceWithFilename(allocator, source, path, options);
}

pub fn compileSourceWithFilename(
    allocator: std.mem.Allocator, 
    source: []const u8, 
    filename: ?[]const u8,
    options: compiler.CompilerOptions
) !CompiledResult {
    // Parse source into AST with filename
    const parse_result = try parser.parseGeneSource(allocator, source, filename);
    // ... rest of compilation
}
```

## Testing

### Test Package File
```gene
# test_package.gene
^name "test-package"
^version "1.0.0"
^description "Test package"
^dependencies {
  ^foo "1.0.0"
}
```

### Expected AST
```
(map 
  ["name" "test-package"]
  ["version" "1.0.0"]
  ["description" "Test package"]
  ["dependencies" (map ["foo" "1.0.0"])]
)
```

## Benefits

1. **Clean Syntax**: No wrapping parentheses or braces needed
2. **Familiar Format**: Similar to other package managers
3. **Backward Compatible**: Regular Gene files unchanged
4. **Easy to Parse**: Reuses existing property parsing logic

## Alternative Approach

If modifying the core parser is too invasive, we could:

1. Pre-process package.gene files to wrap them in `{}`
2. Use a separate parser just for package.gene
3. Support both formats and auto-detect

## Implementation Priority

1. ✅ Add filename parameter to parseGeneSource
2. ✅ Implement parsePackageGene function
3. ✅ Implement parseTopLevelProperty
4. ✅ Update pipeline to pass filename
5. ✅ Add tests for package.gene parsing
6. ✅ Update existing package.gene files to use new format