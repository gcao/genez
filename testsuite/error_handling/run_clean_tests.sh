#!/bin/bash

# Run error handling tests with clean output
GENE="../../zig-out/bin/gene"
PASSED=0
FAILED=0

echo "=== Error Handling Tests ==="
echo

for test in *.gene; do
    if [ -f "${test%.gene}.expected" ]; then
        test_name=$(basename "$test" .gene)
        expected="${test%.gene}.expected"
        
        # Run test and extract only the relevant output lines
        actual=$($GENE run "$test" 2>&1 | grep -E "^[A-Za-z].*[^)]$" | grep -v "error:\|leaked:\|Double free" | head -20)
        expected_content=$(cat "$expected")
        
        if [ "$actual" = "$expected_content" ]; then
            echo "✓ $test_name"
            PASSED=$((PASSED + 1))
        else
            echo "✗ $test_name"
            echo "  Expected:"
            cat "$expected" | sed 's/^/    /'
            echo "  Actual:"
            echo "$actual" | sed 's/^/    /'
            FAILED=$((FAILED + 1))
        fi
    fi
done

echo
echo "Summary: $PASSED passed, $FAILED failed"