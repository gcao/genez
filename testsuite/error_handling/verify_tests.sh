#!/bin/bash

# Simple verification script for error handling tests
GENE="../../zig-out/bin/gene"
PASSED=0
FAILED=0

echo "=== Error Handling Test Verification ==="
echo

# Function to check if output contains expected text
check_output() {
    local test_file=$1
    local expected=$2
    local test_name=$(basename "$test_file" .gene)
    
    if $GENE run "$test_file" 2>&1 | grep -q "$expected"; then
        echo "✓ $test_name"
        PASSED=$((PASSED + 1))
    else
        echo "✗ $test_name"
        FAILED=$((FAILED + 1))
    fi
}

# Test each file
check_output "try_catch_basic.gene" "Caught: Error message"
check_output "try_no_exception.gene" "No exception"
check_output "try_catch_continue.gene" "After try/catch"
check_output "try_finally_no_error.gene" "Finally executes"
check_output "try_finally_with_error.gene" "Finally executes"
check_output "nested_try_catch.gene" "Outer caught: Rethrow"
check_output "throw_in_function.gene" "Caught from function:"
check_output "multiple_catch.gene" "First catch:"
check_output "error_values.gene" "Caught array:"
check_output "finally_order.gene" "try catch finally"

echo
echo "Summary: $PASSED passed, $FAILED failed"

exit $FAILED