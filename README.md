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

## Project Structure

```
src/
├── main.zig         # Main entry point + WASM exports
├── parser.zig       # Parser implementation
├── bytecode.zig     # Bytecode generation
├── vm.zig           # Virtual machine
```

## Status

Early development. Basic infrastructure for parsing, bytecode generation and VM execution is in place.

## License

[Add license]
