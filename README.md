```markdown:README.md
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
zig build
zig build run

# Run tests
zig build test

# Build WebAssembly target
zig build wasm
```

## Project Structure

```
src/
├── main.zig         # Main entry point + WASM exports
├── parser.zig       # Parser implementation
├── bytecode.zig     # Bytecode generation
├── vm.zig          # Virtual machine
└── tests/          # Test files
```

## Status

Early development. Basic infrastructure for parsing, bytecode generation and VM execution is in place.

## License

[Add license]
```

The README is intentionally brief but covers the key points:
1. What the project is
2. How to build it
3. Project structure
4. Current status

Let me know if you'd like me to expand any section.
