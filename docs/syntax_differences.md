# Gene Syntax Differences

Based on analysis of the other Gene implementation (https://github.com/gcao/gene/), here are the key syntax differences from our current implementation.

*Last updated after analyzing http_server.gene*

## 1. Method Call Syntax
- **Other implementation**: Uses dot notation for method calls
  - `(future .on_success (-> (println a)))`
  - `(db .exec "select * from test_table")`
  - `((gene/today) .year)`
  - `(01:02:03 .hour)`
  - `([1 2 3] .to_json)`
  - `(svg/.to_s)`
- **Our implementation**: Uses parentheses for method calls
  - `((.+ 10 20))` or `((+ 10 20))`

## 2. Property/Field Access Syntax
- **Other implementation**: Uses slash notation `/` for field access
  - `row/0` - array indexing
  - `props/id` - map/object property access
  - `/conn` - instance variable access (with leading slash)
  - `data/.size` - property access on variables
  - `Record/.members` - static class member access
- **Our implementation**: Also uses slash notation but primarily for object fields

## 3. Instance Variable Declaration
- **Other implementation**: Uses `/` prefix for instance variables in classes
  ```gene
  (.ctor /port)  # declares and auto-initializes instance variable from parameter
  (/id = props/id)  # assigns to instance variable
  /middlewares  # access instance variable
  ```
- **Our implementation**: Not yet implemented

## 4. Class Definition Syntax
- **Other implementation**:
  ```gene
  (class Todo < Record  # inheritance with <
    (/table = "todos")  # class variable
    (/columns = ["id" "description" "status"])

    (.ctor props  # constructor
      (/id = props/id)
      (/description = props/description)
      (/status = (props/status || 0))
    )

    (.fn done _  # instance method
      (/status/.to_s == "1")
    )
  )
  ```
- **Our implementation**: Uses `class` with different method syntax

## 5. Module/Namespace Features
- **Other implementation**:
  - `(import genex/mysql:DB)` - import with alias
  - `(import genex/http/*)` - wildcard import
  - `gene/sleep_async` - namespace access
  - `genex/http/get` - nested namespace access
  - `$ns/DB_HOST` - namespace variable
- **Our implementation**: No module system yet

## 6. Special Literals and Operators
- **Other implementation**:
  - `#"string"` - string interpolation: `#"Hello #{params/name}!"`, `#"Error: #{$ex}"`
  - `01:02:03` - time literal
  - `^^stderr` - special flags/symbols with double caret
  - `||=` - or-equals operator
  - `!=` - inequality operator
  - `->` - lambda/arrow function syntax
  - `...` - spread/varargs operator in function params
  - `new` - object instantiation: `(new SimpleHttpServer 8080)`
- **Our implementation**: Limited special literals

## 7. Lambda/Function Syntax
- **Other implementation**:
  - `(-> (println a))` - lambda with arrow syntax
  - `(fnx name ...)` - another function syntax variant
- **Our implementation**: Only `fn` syntax

## 8. Async/Await Support
- **Other implementation**:
  - `(await (f "first"))` - await syntax
  - `gene/sleep_async` - async functions
  - Future objects with `.on_success` callbacks
- **Our implementation**: No async support

## 9. Control Flow and Loops
- **Other implementation**:
  - `(for [i v] in row ...)` - destructuring in for loops
  - `(for h in /handlers ...)` - for-in loops
  - Exception handling: `catch` blocks, `catch *`, `$ex` variable
  - Early return: `(return)` without value
- **Our implementation**: Basic control flow only

## 10. Special Global Variables and Functions
- **Other implementation**:
  - `$cmd_args` - command line arguments
  - `$env` - environment variables access
  - `($env "HOME")` - env var getter function
  - `$ex` - exception variable in catch blocks
  - `$tap` - macro for method chaining
  - `gene/run_forever` - built-in utility functions
  - `gene/base64_decode` - standard library functions
- **Our implementation**: No special globals

## 11. Method Definition Variations
- **Other implementation**:
  - `.fn` - instance method definition
  - `.ctor` - constructor definition
  - `.on_extended` - metaprogramming hooks
  - `.on_member_missing` - dynamic member resolution
  - Class methods vs instance methods distinction
- **Our implementation**: Single method definition syntax

## 12. Operator Overloading
- **Other implementation**:
  - Methods can be operators: `.+`, `./`, etc.
  - Allows defining custom operator behavior
- **Our implementation**: Limited operator support

## 13. String Operations
- **Other implementation**:
  - `.trim` method on strings
  - `.to_s` conversion method
  - String interpolation with `#"...#{}..."`
- **Our implementation**: Basic string support only

## 14. Collection Operations
- **Other implementation**:
  - `.add` method on arrays
  - `.contains` method for membership testing
  - `.to_json` for serialization
- **Our implementation**: Basic array/map operations

## 15. Web Programming Features
- **Other implementation**:
  - HTTP server integration: `start_server` function
  - Request/response objects with properties
  - Response helpers: `respond` function
  - Middleware pattern support
  - URL parameter parsing: `params/name`
  - Headers access: `req/.headers/authorization`
  - Authentication support with base64 decoding
- **Our implementation**: No web framework integration

## 16. Advanced Features
- **Other implementation**:
  - Duck typing: objects with `.call` method are callable
  - Chained property access: `req/.headers/authorization`
  - Array indexing with expression: `(/middlewares ./ index)`
  - Conditional variable initialization in expressions
  - Object methods returning self for chaining
- **Our implementation**: Basic type system only

## Summary
The other Gene implementation has significantly more features and a richer syntax, including:
- Consistent dot notation for method calls
- Module/import system with wildcard imports
- Async/await support
- String interpolation with `#"...#{expr}..."`
- Time literals
- More sophisticated OOP with inheritance and metaprogramming
- Lambda syntax variations (`->`, `fnx`)
- Special global variables and functions
- Exception handling with catch blocks
- Web framework integration
- Duck typing and advanced object features
- Richer standard library integration