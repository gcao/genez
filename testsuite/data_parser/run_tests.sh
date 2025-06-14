#!/bin/bash

# Data Parser Test Suite Runner

set -e

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Get the directory of this script
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
GENE="$SCRIPT_DIR/../../zig-out/bin/gene"

echo "=== Gene Data Parser Test Suite ==="
echo

PASSED=0
FAILED=0

# Test basic parsing
echo "--- Basic Parsing ---"
if $GENE parse "$SCRIPT_DIR/basic_types.gene" > /tmp/basic_output 2>&1; then
    # Check if float parsing issue affects the test
    if grep -q "^1$" /tmp/basic_output; then
        echo -e "${RED}FAIL${NC} basic_types (float parsing issue)"
        FAILED=$((FAILED + 1))
    else
        echo -e "${GREEN}PASS${NC} basic_types"
        PASSED=$((PASSED + 1))
    fi
else
    echo -e "${RED}FAIL${NC} basic_types"
    cat /tmp/basic_output
    FAILED=$((FAILED + 1))
fi

# Test collections
if $GENE parse "$SCRIPT_DIR/collections.gene" > /dev/null 2>&1; then
    echo -e "${GREEN}PASS${NC} collections"
    PASSED=$((PASSED + 1))
else
    echo -e "${RED}FAIL${NC} collections"
    FAILED=$((FAILED + 1))
fi

# Test Gene expressions
if $GENE parse "$SCRIPT_DIR/gene_expressions.gene" > /dev/null 2>&1; then
    echo -e "${GREEN}PASS${NC} gene_expressions"
    PASSED=$((PASSED + 1))
else
    echo -e "${RED}FAIL${NC} gene_expressions"
    FAILED=$((FAILED + 1))
fi

echo
echo "--- JSON Output ---"
# Test JSON output
if $GENE parse "$SCRIPT_DIR/json_output.gene" --json > /tmp/json_output 2>&1; then
    # Remove whitespace for comparison
    cat /tmp/json_output | tr -d '\n\r\t ' > /tmp/json_actual
    cat "$SCRIPT_DIR/json_output.expected" | tr -d '\n\r\t ' > /tmp/json_expected
    
    if diff -q /tmp/json_actual /tmp/json_expected > /dev/null; then
        echo -e "${GREEN}PASS${NC} json_output"
        PASSED=$((PASSED + 1))
    else
        echo -e "${RED}FAIL${NC} json_output"
        echo "Expected:"
        cat "$SCRIPT_DIR/json_output.expected"
        echo
        echo "Actual:"
        cat /tmp/json_output
        FAILED=$((FAILED + 1))
    fi
else
    echo -e "${RED}FAIL${NC} json_output"
    cat /tmp/json_output
    FAILED=$((FAILED + 1))
fi

echo
echo "--- Output Formats ---"
# Test parsed output format
if $GENE parse "$SCRIPT_DIR/output_formats.gene" --parsed > /tmp/parsed_output 2>&1; then
    if grep -q "Gene(config):" /tmp/parsed_output && grep -q "Properties\[1\]:" /tmp/parsed_output; then
        echo -e "${GREEN}PASS${NC} parsed_output_format"
        PASSED=$((PASSED + 1))
    else
        echo -e "${RED}FAIL${NC} parsed_output_format"
        echo "Output:"
        cat /tmp/parsed_output
        FAILED=$((FAILED + 1))
    fi
else
    echo -e "${RED}FAIL${NC} parsed_output_format"
    cat /tmp/parsed_output
    FAILED=$((FAILED + 1))
fi

echo
echo "--- Stdin Input ---"
# Test stdin parsing
if echo '(test ^prop "value")' | $GENE parse - > /dev/null 2>&1; then
    echo -e "${GREEN}PASS${NC} stdin_input"
    PASSED=$((PASSED + 1))
else
    echo -e "${RED}FAIL${NC} stdin_input"
    FAILED=$((FAILED + 1))
fi

echo
echo "=== Summary ==="
echo -e "Passed: ${GREEN}$PASSED${NC}"
echo -e "Failed: ${RED}$FAILED${NC}"
echo -e "Total: $((PASSED + FAILED))"

if [ $FAILED -gt 0 ]; then
    exit 1
fi