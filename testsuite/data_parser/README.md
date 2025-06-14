# Gene Data Parser Test Suite

This directory contains tests for Gene's data parsing functionality, which allows Gene syntax to be used as a data format similar to JSON or YAML.

## Features Tested

### Basic Data Types
- Integers: `42`, `0`, `123456`
- Floats: `3.14`, `1.0` (Note: negative floats have parsing issues)
- Booleans: `true`, `false`
- Nil: `nil`
- Strings: `"Hello"`, `"Multi\nline"`
- Symbols: `symbol`, `my-symbol`

### Collections
- Arrays: `[]`, `[1 2 3]`, `[[nested]]`
- Maps: `{}`, `{:key "value"}`, `{:nested {:map "value"}}`

### Gene Expressions
- Simple: `(atom)`, `(expr 1 2 3)`
- With properties: `(person ^name "John" ^age 30)`
- Nested: `(parent (child1) (child2))`
- Mixed: Properties and children combined

### Output Formats
- Standard Gene format (default)
- JSON output with `--json` flag

## Usage

Parse a file:
```bash
gene parse data.gene
```

Parse from stdin:
```bash
echo '(data ^key "value")' | gene parse -
```

Output as JSON:
```bash
gene parse data.gene --json
```

Show parsed data structure:
```bash
gene parse data.gene --parsed
```

### Output Formats

1. **Raw (default)**: Reproduces the original Gene syntax
2. **JSON**: Converts to JSON format for interoperability
3. **Parsed**: Shows the internal parsed data structure with type information

#### Example - Raw Output:
```
(person ^name "John" ^age 30)
```

#### Example - Parsed Output:
```
Expression [0]:
  Gene(person):
    Properties[2]:
      ^name =       String("John")
      ^age =       Int(30)
```

#### Example - JSON Output:
```json
[{
  "_type": "gene",
  "_head": "person",
  "_props": {
    "name": "John",
    "age": 30
  }
}]
```

## Known Limitations

1. **Negative Float Parsing**: Negative floats like `-0.5` are currently parsed as separate tokens (`-0`, `.`, `5`)
2. **No Schema Validation**: The parser accepts any valid Gene syntax without schema constraints
3. **No Type Conversion**: All data is preserved as-is without type coercion

## Running Tests

```bash
./run_tests.sh
```

## Test Files

- `basic_types.gene` - Tests all basic data types
- `collections.gene` - Tests arrays and maps
- `gene_expressions.gene` - Tests Gene expression syntax with properties
- `json_output.gene` - Tests JSON conversion