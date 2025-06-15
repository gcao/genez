# Gene Syntax Differences - Detailed Analysis

This document compares our current Gene implementation with the reference implementation at https://github.com/gcao/gene/tree/master/examples

*Last updated after analyzing http_server.gene*

## 1. Method Call Syntax (HIGH PRIORITY)

**Reference Implementation:**
```gene
(object .method_name args)   # Method call
(array .map fn)              # Method on array
(string .trim)               # Method with no args
(object .to_json)            # Convert to JSON
("hello" .length)            # Method on literal
(10 .+ 20)                   # Operator as method
```

**Our Implementation:**
```gene
(method_name object args)    # Function call style
(.method_name object args)   # With recent fixes - working
("hello" .length)            # Working
(10 .+ 20)                   # Working
```

**Action Required:** Method syntax is already working correctly.

## 2. Property/Field Access

**Both Implementations:**
```gene
object/field_name  # Works in both
```

**Reference Implementation (Additional):**
```gene
/instance_var      # In methods, refers to instance variable
self/field         # Explicit self reference
```

**Our Implementation:**
- `/field` in methods doesn't work (must use `self/field`)

**Action Required:** Fix implicit self reference with `/field` syntax.

## 3. String Interpolation (HIGH PRIORITY)

**Reference Implementation:**
```gene
#"Hello #{name}, you are #{age} years old"
#"The result is #{(+ 1 2)}"
#"Hello #{params/name}!"  # Property access in interpolation
#"Error: #{$ex}"          # Special variables in interpolation
```

**Our Implementation:** Not supported

**Action Required:** Implement string interpolation with `#"..."` syntax. This is critical for web development.

## 4. Class Definition Syntax (HIGH PRIORITY)

**Reference Implementation:**
```gene
(class Person < Base
  (var /name)              # Instance variable declaration
  (var /age)

  (.ctor [name age]        # Constructor
    (= /name name)
    (= /age age))

  # Auto-property constructor (from http_server.gene)
  (.ctor /port)            # Automatically creates /port from parameter

  (.fn greet []
    #"Hello, I'm #{/name}")

  (.fn get_age []
    /age))
```

**Our Implementation:**
```gene
(class Person
  (fn new [name age]
    {^name name ^age age})

  (fn greet [self]
    (print self/name)))
```

**Action Required:**
- Support `.ctor` constructor syntax
- Support `.fn` for method definitions
- Support inheritance with `<`
- Support instance variable declarations with `(var /name)`
- Support direct assignment to instance variables `(= /name value)`

## 5. Module/Import System (HIGH PRIORITY)

**Reference Implementation:**
```gene
(import genex/http)
(import genex/mysql:DB)
(import ./lib/helper [helper1 helper2])
(ns myapp
  (export [func1 func2])
  ...)
```

**Our Implementation:** Basic namespace syntax only, no import/export

**Action Required:** Implement full module system with import/export.

## 6. Special Literals

**Reference Implementation:**
```gene
01:02:03           # Time literal
2019-01-01         # Date literal
/regex/i           # Regex literal
'symbol            # Symbol literal
```

**Our Implementation:** None of these are supported

**Action Required:** Add support for time, date, regex, and symbol literals.

## 7. Lambda/Function Variants

**Reference Implementation:**
```gene
(fn [x] (* x 2))        # Standard function
(fnx [req]              # Anonymous function (from http_server.gene)
  (respond req
    #"Hello!"))
(-> [x] (* x 2))        # Arrow function
```

**Our Implementation:** Only basic `fn` syntax

**Action Required:** Add `fnx` for anonymous functions and `->` for arrow functions.

## 8. Async/Await (LOW PRIORITY)

**Reference Implementation:**
```gene
(async
  (var result (await (http/get url)))
  ...)
```

**Our Implementation:** Not supported

**Action Required:** Implement async/await (future enhancement).

## 9. Global Variables and Special Functions

**Reference Implementation:**
```gene
$cmd_args           # Command line arguments
$env                # Environment variables
$ns/VAR_NAME        # Namespace-scoped variables
$ex                 # Exception in catch blocks
$tap                # Macro for method chaining

# Built-in functions
gene/run_forever    # Utility functions
gene/base64_decode  # Standard library
```

**Our Implementation:** Not supported

**Action Required:** Add special global variables and built-in functions.

## 10. Spread Operator

**Reference Implementation:**
```gene
(func args...)      # Spread arguments
[1 2 arr...]        # Spread in array literal
```

**Our Implementation:** Not supported

**Action Required:** Implement spread operator.

## 11. Macro Enhancements

**Reference Implementation:**
```gene
(macro when [cond body...]  # ^^ captures rest as array
  (if %cond %body...)) # ~@ splices array
```

**Our Implementation:** Basic macro support without splicing

**Action Required:** Add splice operator `~@` for macros.

## Implementation Priority (Updated)

### Immediate Priority (Blocking current development):
1. Fix `/field` implicit self reference in methods
2. Complete module/import system with wildcard imports
3. String interpolation `#"..."` - critical for web development

### High Priority (Core language features):
1. Class enhancements (`.ctor`, `.fn`, inheritance, auto-properties)
2. Anonymous functions (`fnx`) - needed for callbacks
3. Exception handling (catch blocks, `$ex`)
4. Special global variables (`$cmd_args`, `$env`, `$ex`)
5. Inequality operator `!=`

### Medium Priority (Important but not blocking):
1. For-in loops
2. Early return without value
3. `new` keyword for object instantiation
4. Chained property access
5. Time/date literals
6. Spread operator
7. Macro splicing

### Low Priority (Future enhancements):
1. Async/await
2. Web framework integration
3. Duck typing and runtime method checking
4. Arrow function syntax `->`
5. Symbol literals
6. Advanced array indexing expressions

## New Features from http_server.gene Analysis

### 12. Exception Handling (MEDIUM PRIORITY)

**Reference Implementation:**
```gene
# Top-level catch in functions
catch *
  #"Error: #{$ex}"

# Exception object methods
($ex .to_s)  # Convert exception to string
```

**Our Implementation:** No exception handling

**Action Required:** Implement try/catch blocks and exception handling.

### 13. Object Instantiation

**Reference Implementation:**
```gene
(new SimpleHttpServer 8080)  # Create new instance
```

**Our Implementation:** Different syntax for object creation

**Action Required:** Add `new` keyword for object instantiation.

### 14. Advanced Property Access

**Reference Implementation:**
```gene
req/.headers/authorization   # Chained property access
(/middlewares ./ index)      # Array indexing with expression
params/name                  # URL parameter access
```

**Our Implementation:** Basic slash notation only

**Action Required:** Support chained property access and advanced indexing.

### 15. Control Flow Enhancements

**Reference Implementation:**
```gene
(for h in /handlers ...)     # For-in loops
(return)                     # Early return without value
```

**Our Implementation:** Basic control flow

**Action Required:** Add for-in loops and early return.

### 16. Inequality Operator

**Reference Implementation:**
```gene
(if (index != 0) ...)  # != operator
```

**Our Implementation:** Only == operator

**Action Required:** Add != operator.

### 17. Web Framework Integration

**Reference Implementation:**
```gene
# HTTP Server
(start_server ...)           # Start web server
(respond req body)           # Send HTTP response

# Request/Response objects
req/method
req/path
req/.headers/authorization

# Middleware pattern
```

**Our Implementation:** No web framework

**Action Required:** Future enhancement - web framework integration.

### 18. Duck Typing Features

**Reference Implementation:**
```gene
# Objects with .call method are callable
(if (h .has_method "call")
  (h .call req))
```

**Our Implementation:** Static typing only

**Action Required:** Support duck typing and runtime method checking.

### 19. Auto-property Constructor

**Reference Implementation:**
```gene
(.ctor /port)  # Automatically creates /port instance variable from parameter
```

**Our Implementation:** Manual property initialization only

**Action Required:** Support auto-property syntax in constructors.