#!/usr/bin/env bash
set -euo pipefail

echo Building gene
zig build

echo
echo $ gene run examples/default.gene
zig-out/bin/gene run examples/default.gene

echo
echo $ gene compile examples/default.gene
rm examples/*.gbc
zig-out/bin/gene compile examples/default.gene

echo
echo $ gene run examples/default.gbc
zig-out/bin/gene run examples/default.gbc

echo
echo $ zig build test
zig build test
