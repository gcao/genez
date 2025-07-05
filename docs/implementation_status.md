# Gene Implementation Status

This document tracks the current implementation status of Gene as a dynamic, Ruby+Lisp-like language.

## Summary

Gene is a dynamic programming language with a working 4-stage compilation pipeline (AST → HIR → MIR → Bytecode). The language combines Lisp's powerful macro system and s-expression syntax with Ruby-like object-oriented features and developer ergonomics. The implementation focuses on being a practical, dynamic language first, with type checking and optimization deferred to later phases.

**Current State**: Gene is a fully functional language with OOP, macros, modules, pattern matching, exception handling, FFI, and a growing standard library. Recent additions include string interpolation, mutable references, comprehensive string methods, and proper logical operators.

**Last Updated**: 2025-01-24 - Implemented case expressions for conditional branching, redesigned match for destructuring/binding, and added new if/then/else syntax. Gene now has clearer separation between case (for branching) and match (for destructuring). The new if syntax `(if cond (then expr) (else expr))` maps better to indented Gene where then/else are indented deeper than if.

## Language Philosophy

Gene aims to be a modern dynamic language that combines:
- **Lisp heritage**: S-expressions, powerful macros, code-as-data
- **Ruby ergonomics**: Everything is an object, methods on primitives, intuitive syntax
- **Modern features**: Pattern matching, modules, namespaces
- **Practical focus**: Fast startup, good error messages, useful standard library

## Core Features - Completed ✅

### 1. **Dynamic Language Foundation**
- **S-expression syntax** with Lisp-like structure ✓
- **Dynamic typing** with runtime type checking ✓
- **Everything is an object** (including primitives) ✓
- **Methods on all types** (numbers, strings, arrays, etc.) ✓
- **First-class functions** ✓
- **Lexical scoping** ✓

### 2. **Object-Oriented Programming**
- **Core class hierarchy** (Any, Number, Int, Float, String, Bool, etc.) ✓
- **Custom class definitions** with fields and methods ✓
- **Method dispatch** on all objects ✓
- **Constructor syntax** (`.ctor`) ✓
- **Field access** via `/` notation (e.g., `obj/field`) ✓
- **Inheritance** support ✓

### 3. **Functional Programming**
- **Functions as first-class values** ✓
- **Higher-order functions** ✓
- **Closures** (basic support) ✓
- **Tail call optimization** (in MIR) ✓
- **Pattern matching** ✓
  - Literal patterns (match exact values) ✓
  - Variable patterns (bind values) ✓
  - Wildcard patterns (_) ✓
  - Array patterns ([x y]) ✓
  - Map patterns ({^key val}) ✓
  - Nested patterns (limited) ⚠️
  - Constructor patterns ✗
  - Or patterns ✗
  - Range patterns ✗
  - Pattern guards (when clauses) ✗
- **Recursion** with proper stack frames ✓

### 4. **Macro System**
- **Hygienic macros** with lazy evaluation ✓
- **Quote/unquote** operations ✓
- **Code generation** at compile time ✓
- **AST manipulation** ✓

### 5. **Module System**
- **Import/export** functionality ✓
- **Namespaces** for code organization ✓
- **Selective imports** with aliasing ✓
- **Package configuration** (package.gene) ✓
- **Module resolution** (relative, absolute, package) ✓
- **Circular import detection** ✓

### 6. **Core Data Structures**
- **Arrays** with methods (push, pop, map, filter, etc.) ✓
- **Maps** (hash tables) with methods ✓
- **Strings** with Unicode support ✓
- **String methods** (split, trim, indexOf, contains, replace, upper/lower case, etc.) ✓
- **Numbers** (Int and Float) with full arithmetic ✓
- **Booleans** and nil ✓
- **Mutable references** (ref/deref/set!) for imperative programming ✓

### 7. **Control Flow**
- **Conditionals** (if/else expressions) ✓
  - Old syntax: `(if cond then_expr else_expr)`
  - New syntax: `(if cond (then expr) (else expr))` - better for indented Gene
- **Case expressions** for value-based branching ✓
- **Match expressions** for destructuring/binding ✓
- **Loops** (for-in, while) ✓
- **Do blocks** for sequential execution ✓
- **Early returns** ✓
- **Exception handling** (try/catch/finally, throw) ✓

### 8. **Operators**
- **Arithmetic operators** (+, -, *, /, %) ✓
- **Comparison operators** (==, !=, <, >, <=, >=) ✓
- **Logical operators** (&&, ||, !) with proper value semantics ✓
- **Bitwise operators** (&, |, ^, <<, >>) ✓
- **All operators work as methods** on objects ✓

### 9. **Developer Experience**
- **REPL** for interactive development ✓
- **Clear error messages** (basic) ✓
- **Debug mode** showing compilation stages ✓
- **Print/println** for debugging ✓

### 10. **Foreign Function Interface (FFI)**
- **C function declarations** (`c-extern`) ✓
- **C struct declarations** (`c-struct`) ✓
- **C type aliases** (`c-type`) ✓
- **Variadic function support** ✓
- **FFI callbacks** (`c-callback`) ✓
- **Native function definitions** with Gene callbacks ✓

## Ruby-like Features - Completed ✅

1. **Everything is an object**
   ```gene
   (10 .+ 20)              # => 30
   ("hello" .length)       # => 5
   ([1 2 3] .map square)   # => [1 4 9]
   ```

2. **Methods on primitives**
   ```gene
   (42 .to_string)         # => "42"
   (3.14 .round)           # => 3
   ("Gene" .downcase)      # => "gene"
   ```

3. **Intuitive OOP**
   ```gene
   (class Person
     (.ctor [name age]
       (= self/name name)
       (= self/age age))
     
     (.greet []
       (println "Hi, I'm" self/name)))
   ```

4. **String interpolation**
   ```gene
   (var name "Gene")
   (println #"Hello, #{name}!")  # => "Hello, Gene!"
   ```

5. **Mutable references for imperative code**
   ```gene
   (var counter (ref 0))
   (while (< (deref counter) 5) (do
     (println (deref counter))
     (set! counter (+ (deref counter) 1))))
   ```

6. **Logical operators return values**
   ```gene
   (|| false "default")    # => "default"
   (&& user user/name)     # => user's name if user exists, nil otherwise
   ```

## In Progress 🚧

### 1. **Standard Library**
- ✅ Basic I/O operations (file_open, file_close, file_read_all, file_write_all, file_exists)
- ✅ Basic math operations (math_sqrt)
- ✅ System operations (exit)
- ⏳ File system access (directory operations)
- ⏳ String manipulation utilities
- ⏳ Collection utilities
- ⏳ More math functions

### 2. **Error Handling**
- ✅ Try/catch/finally blocks (basic support)
- ✅ Throw expressions
- ✅ Custom exception types (Error values)
- ⏳ Stack traces with line numbers

### 3. **Advanced Pattern Matching**
- ⏳ Array patterns
- ⏳ Map patterns
- ⏳ Guard clauses
- ⏳ Destructuring in pattern matching
- ⏳ Pattern matching with extraction (e.g., `(match [^id ^status] data)`)

### 4. **Language Features - High Priority**
- ✅ **Default parameter values** - `(fn f [a b = 10])` 
- ✅ **Rest parameters/varargs** - `(fn f [a b...])` 
- ⏳ **Spread operator** - `(... array)` 
- ✅ **String interpolation** - `#"Hello #{name}"` 
- ✅ **Logical operators** - `&&` and `||` return values (not just booleans)
- ⏳ **Conditional assignment** - `||=` for defaults
- ⏳ **For loop destructuring** - `(for [k v] in map)`

### 5. **Language Features - Medium Priority**
- ⏳ **Triple-quoted strings** - `"""multi-line"""` 
- ⏳ **Named parameters** - `(fn f [^opt1 ^opt2 required])` 
- ⏳ **Dynamic selectors** - `@property` syntax for dynamic access (see test_selector.nim)
- ⏳ **Class inheritance syntax** - `(class Child < Parent)`
- ⏳ **Method chaining with tap** - `$tap` macro
- ⏳ **Advanced OOP hooks** - `.on_extended`, `.on_member_missing`
- ⏳ **Class variables** - `/table`, `/columns` notation
- ⏳ **$set macro** - Dynamic property setter
- ⏳ **Symbol literals** - `:symbol` syntax (considering `` ` `` as alternative)

### 6. **Global Variables/Built-ins**
- ⏳ **$env** - Environment variables access
- ⏳ **$cmd_args** - Command line arguments
- ⏳ **$if_main** - Conditional execution for main module

## Future Phases 🔮

### Phase 1: Language Completion (Current Focus)
- Complete standard library
- Improve error handling
- Enhanced REPL features
- Better debugging tools
- Performance improvements

### Phase 2: Type System (Deferred)
- **Gradual typing** - optional type annotations
- **Type inference** - deduce types where possible
- **Property types** with `^` syntax
- **Union and intersection types**
- **Generic types** with constraints
- **Type checking** at compile time

### Phase 3: Optimization
- **JIT compilation** - hot code optimization
- **Inline caches** - fast property access
- **Profile-guided optimization**
- **Advanced GC** - generational, concurrent

### Phase 4: Advanced Features
- **Concurrency** - actors, channels, STM
- **Native compilation** - standalone executables
- **Package manager** - dependency management

## Design Principles

1. **Developer Happiness** - The language should be joy to use
2. **Principle of Least Surprise** - Behave like Ruby where possible
3. **Power When Needed** - Lisp's power is there when you need it
4. **Performance Later** - Correctness and usability first
5. **Gradual Complexity** - Simple things simple, complex things possible

## Current Limitations

1. **Performance** - Interpreted only, no JIT yet
2. **Standard Library** - Minimal, needs expansion
3. **Tooling** - Basic REPL, no debugger or profiler
4. **Documentation** - Needs comprehensive guides
5. **Match expressions as function arguments** - Currently, match expressions cannot be used directly as function arguments due to stack tracking issues in the bytecode generator. Workaround: assign the match result to a variable first.
   ```gene
   # This doesn't work yet:
   # (print (match x (0 "zero") (_ "other")))
   
   # Use this instead:
   (var result (match x (0 "zero") (_ "other")))
   (print result)
   ```
5. **Error Messages** - Could be more helpful

## Recommendations for Contributors

1. **Focus on Dynamic Features** - Make the language more Ruby-like
2. **Expand Standard Library** - Add useful utilities
3. **Improve Developer Experience** - Better errors, REPL enhancements
4. **Write Documentation** - Tutorials, guides, examples
5. **Defer Type System** - Keep it dynamic for now

## Compatibility Notes

The implementation generally follows Ruby semantics where applicable:
- Methods on all objects
- Everything returns a value
- Nil and false are falsy, everything else is truthy
- Operators are methods
- Classes are objects

Where Gene differs from Ruby:
- S-expression syntax instead of Ruby syntax
- Immutable by default (functional heritage)
- Pattern matching is built-in
- Macros provide metaprogramming (instead of Ruby's runtime meta)
- Module system is more like Python than Ruby

## Summary

Gene is evolving as a practical dynamic language that combines the best of Lisp and Ruby. The current implementation provides a solid foundation for a developer-friendly language with powerful metaprogramming capabilities. With recent additions like string interpolation, mutable references, and comprehensive operator support, Gene is becoming increasingly suitable for real-world programming tasks. Type checking and advanced optimizations are intentionally deferred to keep the initial implementation simple and focused on being a great dynamic language.