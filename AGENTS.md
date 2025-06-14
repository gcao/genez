# AGENTS Guidelines

This repository hosts the Gene compiler and virtual machine written in Zig.
Compilation proceeds through multiple stages, transforming source code to bytecode.

- **Design details:** see `docs/design.md`.
- **Implementation status:** see `docs/implementation_status.md`.

## Directory overview
- `src/frontend/` – parser and AST definitions.
- `src/ir/` – high, mid, and low level IR structures.
- `src/transforms/` – conversions between pipeline stages.
- `src/backend/` – bytecode generation and the VM.

## Building and testing
Before building, download and install Zig.

- Build: `zig build`
- Tests: `zig build test`
- Run example: `./zig-out/bin/gene run examples/default.gene`

Use `--debug` on either `zig build` or `gene run` to print each stage of the pipeline.
Example: `./zig-out/bin/gene run examples/default.gene --debug`.

WASM builds can be run with `wasmtime --dir . zig-out/bin/gene-wasi.wasm run examples/default.gene`.
Example `.gene` files live in the `examples/` directory.

After making changes, run test.sh to verify everything still works.
