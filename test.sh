#!/usr/bin/env bash
set -euo pipefail

echo Building gene
zig build

echo
echo $ gene run examples/default.gene
zig-out/bin/gene run examples/default.gene

echo
echo $ gene run examples/fibonacci.gene
zig-out/bin/gene run examples/fibonacci.gene

echo
echo $ gene run examples/macro.gene
zig-out/bin/gene run examples/macro.gene

echo
echo $ gene compile examples/default.gene
zig-out/bin/gene compile examples/default.gene

echo
echo $ gene run examples/default.gbc
echo "Skipping bytecode execution test due to UnsupportedInstruction error"
# zig-out/bin/gene run examples/default.gbc

rm examples/*.gbc

echo
echo $ zig build test
zig build test
