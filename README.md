# Gene Programming Language

Gene is a programming language implementation written in Zig. It features:

- Custom syntax parser
- Bytecode compiler
- Virtual machine
- WebAssembly target support

## Building

Requires Zig 0.13.0 or later.

```bash
# Build and run native executable
zig build && zig-out/bin/gene run examples/default.gene
zig build -Ddebug=true && zig-out/bin/gene run examples/default.gene
zig build run -- run examples/default.gene
zig build run -- eval "(print (1 + 2))"

# Run tests
zig build test

# Read code from stdin
echo '(print "Hello")' | zig-out/bin/gene run -
echo '(+ 1 2)' | zig-out/bin/gene eval -
echo '(print "hi")' | zig-out/bin/gene compile -
```

## WebAssembly Support

Gene can be compiled to WebAssembly (WASM) with WASI support, allowing you to run Gene programs through a WASM runtime.

### Requirements

- Zig 0.13.0 or later
- wasmtime (install via `brew install wasmtime` on macOS)

### Building and Running

```bash
# Build the WASI target
zig build
zig build -Ddebug=true

# Run a Gene file through WASM
wasmtime --dir . zig-out/bin/gene-wasi.wasm run examples/default.gene
```

The `--dir .` flag gives the WASM module access to the current directory, allowing it to read Gene source files.

Example output:
```
Output from default.gene
3
```

## Gene as a Data Format

Gene syntax can also be used as a data format similar to JSON or YAML. Use the `parse` command to parse Gene data files:

```bash
# Parse a Gene data file
./zig-out/bin/gene parse examples/data_example.gene

# Output as JSON
./zig-out/bin/gene parse examples/data_example.gene --json

# Show internal parsed structure
./zig-out/bin/gene parse examples/data_example.gene --parsed

# Parse from stdin
echo '(config ^version "1.0" ^debug true)' | ./zig-out/bin/gene parse -
```

`gene run -`, `gene compile -` and `gene eval -` accept code from stdin in the same way.

Gene data format supports:
- Basic types: integers, floats, booleans, strings, nil
- Symbols (identifiers)
- Arrays: `[1 2 3]`
- Maps: `{:key "value" :another 123}`
- Gene expressions with properties: `(person ^name "Alice" ^age 30)`

See `docs/design.md` Chapter 24 for more details on Gene as a data format.

## Project Structure

```
src/
├── main.zig         # Main entry point + WASM exports
├── pipeline.zig     # Top-level compilation pipeline
├── runtime.zig      # Runtime helpers for the VM
├── frontend/        # AST definitions and parser
├── ir/              # High/Mid-level IR data structures
├── transforms/      # Passes converting between IR levels
└── backend/         # Bytecode generation and virtual machine
```

## Status

Early development. Basic infrastructure for parsing, bytecode generation and VM execution is in place.

## License

Gene is released under the [MIT License](LICENSE).
