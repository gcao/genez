# Random notes

```bash
zig build

zig build test

# Run the main program
zig build run

zig-out/bin/gene examples/default.gene

# Build gene.wasm and copy to public/
zig build copy-wasm
python3 -m http.server --directory public 8888
```