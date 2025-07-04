# Error Handling Test Suite

This directory contains comprehensive tests for Gene's error handling features including try/catch/finally blocks and throw expressions.

## Test Files

1. **try_catch_basic.gene** - Tests basic try/catch functionality
   - Throws an exception and catches it
   - Verifies the caught value is accessible in the catch block

2. **try_no_exception.gene** - Tests try block with no exception
   - Ensures catch block is not executed when no exception is thrown
   - Verifies normal execution flow

3. **try_catch_continue.gene** - Tests execution continues after catch
   - Shows that program execution continues normally after handling an exception
   - Verifies control flow after try/catch blocks

4. **try_finally_no_error.gene** - Tests finally block without error
   - Ensures finally block executes even when no exception occurs
   - Verifies finally semantics

5. **try_finally_with_error.gene** - Tests finally block with error
   - Ensures finally block executes after an exception is caught
   - Verifies finally always runs

6. **nested_try_catch.gene** - Tests nested try/catch blocks
   - Inner try/catch can catch and rethrow exceptions
   - Outer try/catch can catch rethrown exceptions

7. **throw_in_function.gene** - Tests throwing from within functions
   - Exceptions propagate through function calls
   - Can be caught by try/catch in caller

8. **multiple_catch.gene** - Tests multiple catch clauses
   - Currently only the first catch clause is used
   - Future enhancement: type-based catch selection

9. **error_values.gene** - Tests different value types as exceptions
   - Numbers, booleans, arrays can all be thrown
   - Not limited to string error messages

10. **finally_order.gene** - Tests execution order with try/catch/finally
    - Verifies correct order: try -> catch (on error) -> finally

## Implementation Notes

- Exceptions are implemented using a stack of exception handlers
- When an exception is thrown, control jumps to the nearest catch block
- Finally blocks always execute, regardless of whether an exception occurred
- Any Gene value can be thrown as an exception
- Memory management warnings may appear but don't affect functionality

## Running Tests

Individual tests can be run with:
```bash
../../zig-out/bin/gene run test_name.gene
```

Note: Output may include memory management diagnostics which can be ignored for functional testing.