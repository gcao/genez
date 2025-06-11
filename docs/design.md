# The Gene Programming Language

## Complete Design Document and Reference Implementation

---

# Table of Contents

**Part I: Language Design**
1. Introduction and Philosophy
2. Syntax and Grammar
3. Type System
4. Data Types and Values
5. Control Flow and Expressions
6. Functions and Closures
7. Object-Oriented Programming
8. Module System and Namespaces
9. Concurrency and Parallelism
10. Memory Management
11. Metaprogramming
12. Error Handling
13. Standard Library

**Part II: Reference Implementation**
14. Architecture Overview
15. Lexer and Parser
16. IR Pipeline (HIR, MIR, LIR)
17. Type Checker and Inference
18. Register-Based Virtual Machine
19. Garbage Collector
20. JIT Compiler
21. Runtime System
22. Tooling and Development Environment

---

# Part I: Language Design

# Chapter 1: Introduction and Philosophy

## 1.1 Overview

Gene is a dynamic, general-purpose programming language that combines the expressiveness of Lisp-like languages with modern performance optimization techniques. It aims to provide a smooth gradient from rapid prototyping to high-performance production code.

## 1.2 Core Design Principles

### 1.2.1 Simplicity Through Uniformity
Gene uses a uniform S-expression syntax with carefully chosen extensions that maintain consistency while improving readability.

### 1.2.2 Performance Without Premature Optimization
Start with a simple, correct implementation and provide clear paths to optimization without changing the fundamental code structure.

### 1.2.3 Gradual Everything
- Gradual typing: from fully dynamic to fully static
- Gradual performance: from interpreted to JIT-compiled
- Gradual concurrency: from single-threaded to massively parallel

### 1.2.4 Property-Based Design
Properties (marked with `^`) are a fundamental organizing principle, used for:
- Object properties
- Type constraints
- Metadata and annotations
- Compile-time configuration

## 1.3 Language Goals

1. **Expressiveness**: Support multiple programming paradigms naturally
2. **Performance**: Achieve near-native speed for numeric and systems code
3. **Interactivity**: Excellent REPL and live-coding experience
4. **Scalability**: From scripts to large applications
5. **Correctness**: Optional static guarantees when needed

## 1.4 Non-Goals

1. **Backward compatibility**: Gene prioritizes doing things right over compatibility
2. **Minimalism**: Gene is feature-rich by design
3. **Single paradigm**: Gene intentionally supports multiple styles

---

# Chapter 2: Syntax and Grammar

## 2.1 Basic Syntax

Gene uses S-expressions as its foundation with extensions for common patterns:

```gene
# Comments start with #
#< Block comments
   can span multiple lines >#

# Basic expressions
(+ 1 2 3)           # Function call
(var x = 10)        # Variable declaration
x                   # Variable reference
:x = (quote x)      # Evaluate :x => symbol x
%x = (unquote x)    # Evaluate :%x => value of x in most cases
```

## 2.2 Literals

```gene
# Numbers
42                 # Integer
3.14               # Float
-2.5e10            # Scientific notation

# Literals
true false nil     # Boolean and null literals

# Strings and Characters
"hello"            # String
"""multi-line
   string"""       # Multi-line string
'c'                # Character
'\n'               # Escape sequence

# Symbols and Keywords
keyword            # Keyword (self-evaluating symbol)
x                  # Evaluated to symbol x in some contextes and resolved in other contexts
x/y/z              # Evaluated to complex symbol in some contexts and resolved to "x" then
                   # nested property "y", "z" is accessed

# Collections
[1 2 3]            # Array
#[1 2 3]           # Set
{^a 1 ^b 2}        # Map with properties
(x ^prop1 value1 ^prop2 value2 child1 child2)   # Gene
```

## 2.3 Property Syntax

Properties are Gene's distinctive feature:

```gene
# Basic property
{^name "John"}             # name = "John"

# Nested property
{^address^city "NYC"}      # self/address/city = "NYC"

# Boolean properties
{^^active}                 # active = true
{^!inactive}               # inactive = nil
```

## 2.4 Path Syntax

Gene uses `/` for path navigation:

```gene
person/name                # Access property
person/address/city        # Nested access
array/0                    # Array index
```

## 2.5 Operators

Gene supports infix operators within S-expressions:

```gene
(x + y)                    # Addition
(a * b + c)                # Precedence: (* a b) + c
(x > 0 && y < 10)          # Logical operators
(a == b || c != d)         # Comparison
```

## 2.6 Special Forms

```gene
# Variable declaration
(var x :Int = 10)          # With type
(var y = 20)               # Type inferred

# Assignment
(x = 30)                   # Update variable
(/property = value)        # Update object property of self in current context

# Control flow
(if condition
  then-expr
  else-expr)

(do                        # Sequential execution
  expr1
  expr2
  expr3)

# Pattern matching
(match value
  pattern1 result1
  pattern2 result2
  _ default)
```

## 2.7 Function Syntax

```gene
# Function definition
(fn add [x y]
  (+ x y))

# With types
(fn add [x :Int y :Int => :Int]
  (+ x y))

# Generic function
(fn identity ^^T [x :T => :T]
  x)

# Anonymous function
(fnx [x] (* x 2))

# Method syntax
# It's like invoking "fn" method on current class object
(.fn method-name [args] body)
```

## 2.8 Grammar Specification

```ebnf
program     = expression*
expression  = atom | list | quoted
atom        = number | string | symbol | keyword | path
list        = '(' expression* ')'
quoted      = ':' expression

path        = atom ('/' path-segment)*
path-segment = atom | '?' atom | number

property    = '^' property-name property-value?
property-name = symbol ('^' symbol)*
```

---

# Chapter 3: Type System

## 3.1 Overview

Gene features a gradual type system that supports:
- Dynamic typing by default
- Optional type annotations
- Type inference
- Generic programming
- Type constraints and traits

## 3.2 Type Annotations

```gene
# Variable types
(var x :Int)
(var y :Str = "hello")

# Function types
(fn add [x :Int y :Int => :Int]
  (+ x y))
(fn g [x :Int y :Int]  # returns nothing/void
)

# Complex types
(var nums :(Array Int))
(var map :(Map Str Int))
(var fn-var :(Fn Int Int => Int))
```

## 3.3 Built-in Types

Gene has a well-defined type hierarchy with `Any` at the root:

```gene
# Type Hierarchy
Any                    # Root type - all values are Any
├── Void              # No value (function returns)
├── Nil               # The nil value
├── Bool              # true/false
├── Number            # Abstract numeric type
│   ├── Int           # 48-bit integers (NaN-boxed)
│   └── Float         # 64-bit IEEE 754 floats
├── Char              # Unicode character
├── Str               # UTF-8 string
├── Symbol            # Interned symbol
├── ComplexSymbol     # Symbol with namespace/path
├── Map               # Key-value mapping
├── Array             # Indexed sequence
├── Set               # Unique value collection
├── Gene              # Generic container (X ^prop val ...)
├── Fn                # Function type
├── Class             # Class metaobject
├── Trait             # Trait metaobject
├── Module            # Module/namespace
├── Ref               # Mutable reference
├── Atom              # Atomic reference
├── Chan              # Channel for concurrency
├── Promise           # Future value
└── ...               # User-defined types
```

### 3.3.1 Type Definitions

```gene
# Any - root of type hierarchy
(type Any)  # Everything is Any

# Void - no value
(fn print [s :Str => :Void]
  (io/write s))
(fn print [s :Str]    # If not specified, returns void
  (io/write s))

# Nil - singleton null value
(var x :Nil = nil)
(var x)     # default to nil
(x is Nil)  # true

# Bool
(var flag :Bool = true)
(var flag :Bool)      # Default to (new Bool)
(x is Bool)

# Number - abstract base for numeric types
(fn numeric-op [x :Number y :Number => :Number]
  (+ x y))  # Works with Int or Float

# Int - 48-bit signed integer
(var count :Int = 42)
(x is Int)
(Int/MAX)  # 140737488355327
(Int/MIN)  # -140737488355328

# Float - 64-bit IEEE 754
(var pi :Float = 3.14159)
(x is Float)
(Float/INFINITY)
(Float/NAN)

# Char - Unicode character
(var c :Char = 'A')
(var emoji :Char = '😊')
(c .code)  # 65

# Str - UTF-8 string
(var name :Str = "Gene")
# Small string optimization for ≤ 6 bytes

# Symbol - interned identifier
(var sym :Symbol = 'foo)
(new Symbol "dynamic-" "symbol")  # Create dynamically

# ComplexSymbol - namespace-qualified symbol
(var cs :ComplexSymbol = myapp/utils/helper)

# Map - key-value collection
(var m :Map = {^a 1 ^b 2})
(var m2 :(Map Str Int) = {"a" 1 "b" 2})

# Array - indexed collection
(var arr :Array = [1 2 3])
(var arr2 :(Array Int) = [1 2 3])

# Set - unique values
(var s :Set = #{1 2 3})
(var s2 :(Set Str) = #{"a" "b" "c"})

# Gene - generic container
(var g :Gene = (div ^class "main" "Hello"))
(g .head)         # 'div
(g .props)    # {^class "main"}
(g .children) # ["Hello"]

# Fn - function type
(var f :Fn = (fnx [x] (* x 2)))
(var f2 :(Fn Int Int) = (fnx [x :Int => :Int] (* x 2)))
```

### 3.3.2 Type Constructors

```gene
# Parameterized types
(Array T)           # Array of T
(Map K V)           # Map from K to V
(Set T)             # Set of T
(Gene H)            # Gene with head type H
(Fn A B)            # Function from A to B
(Fn A B C)          # Function from (A, B) to C

# Union types
(| Int Str)         # Int or Str
(| Int Nil)         # Nullable Int

# Intersection types
(& Comparable Hashable)  # Both traits

# Tuple types
(Tuple Int Str)     # Fixed 2-tuple
(Tuple A B C)       # Fixed 3-tuple

# Optional type
(Optional T)        # Same as (| T Nil)
(Option T)          # Alias
```

### 3.3.3 Type Predicates

```gene
# Type checking with specific type
(x is Int)
(x is (Array Int))
```

### 3.3.4 Type Conversions

```gene
# Numeric conversions
(x .to_int)     # Convert to Int
(x .to_float)   # Convert to Float

# String conversions
(x .to_str)     # Convert to Str (toString)
(new Str x y z) # String concatenation

# Symbol conversions
(x .to_sym)     # Convert to Symbol
("foo" .to_sym) # Str to Symbol

# Safe conversions with Optional
(x .to_int?)    # Returns (Optional Int)
(x .to_float?)  # Returns (Optional Float)
```

### 3.3.5 Built-in Type Methods

```gene
# All types have certain methods
x/.type         # Get runtime type
x/.type-name    # Get type name as string
x/.hash         # Hash code
(x/.equals y)   # Equality check

# Number methods (on Int and Float)
n/.abs          # Absolute value
n/.sign         # -1, 0, or 1
n/.to-str       # Convert to string

# Collection methods
coll/.length    # Size
coll/.empty?    # Is empty
(coll/.contains? x)  # Has element

# String methods
s/.length       # Character count
s/.bytes        # Byte count
s/.chars        # Character array
s/.upper        # Uppercase
s/.lower        # Lowercase
```

### 3.3.6 Type Metadata in Gene Type

```gene
# All Gene values can carry type information
(div ^class "main"
  "Hello") # div is a function that returns a HTMLDiv element

# Type checking on Genes
(match expr
  Int "integer gene"
  Fn  "function gene"
  _   "other")

# Runtime type creation
# TODO
```

## 3.4 Generic Types

Properties starting with capital letters in type contexts are generic type parameters:

```gene
# Generic function
(fn map ^^T ^^U [f :(Fn T => U) arr :(Array T) => :(Array U)]
  ...)

# Type constraints
(fn sort ^T :Comparable [arr :(Array T) => :(Array T)]
  ...)

# Multiple constraints
(fn process ^T :[Hashable Serializable] [x :T => :Str]
  ...)
```

## 3.5 Type Constraints and Traits

```gene
# Define a trait
(trait Comparable ^^T
  (.fn compare [other :T => :Int]))

# Implement trait
(class String implements Comparable
  (.fn compare [other :Str => :Int]
    ...))

# Use as constraint
(fn max ^T :Comparable [a :T b :T => :T]
  (if (> (a .compare b) 0) a b))
```

## 3.6 Type Inference

Gene performs bidirectional type inference:

```gene
(var x = 10)           # Inferred as Int
(var y = [1 2 3])      # Inferred as (Array Int)

# Function type inference
(fn double [x] (* x 2))  # Types inferred from usage

# Generic inference
(map (fnx [x] (* x 2)) [1 2 3])  # T=Int, U=Int inferred
```

## 3.7 Variance

TODO

## 3.8 Type Aliases

```gene
(type UserId Int)
(type Point {^x Float ^y Float})
(type Result ^^T ^^E (| [:ok T] [:err E]))
(type Matrix ^^T (Array (Array T)))
```

---

# Chapter 4: Data Types and Values

## 4.1 Value Representation

Gene uses NaN-boxing for uniform value representation:

```
64-bit Value Layout:
- Floats: IEEE 754 double (except NaN patterns)
- Integers: 48-bit signed integers in NaN space
- Pointers: 48-bit pointers with tag bits
- Immediates: nil, true, false, small strings
```

## 4.2 Primitive Values

```gene
# Nil
nil                    # The null value

# Booleans
true false            # Boolean literals

# Numbers
42                    # Integer (48-bit)
3.14                  # Float (64-bit)

# Characters
'a' '\n' '\u{1F600}'  # Unicode support
```

## 4.3 Strings

```gene
# String literals
"hello"
"""multi
   line"""

# String operations
(s .len)
(s1 .concat s2)
(s .substr start end)

# Small string optimization
# Strings ≤ 6 bytes stored inline in NaN-box
```

## 4.4 Symbols and Keywords

```gene
# Symbols - interned strings
:symbol              # Evaluated to a symbol
("dynamic" .to_sym)

# Keywords
if else do ...
```

## 4.5 Collections

### 4.5.1 Arrays

```gene
# Array literals
[]                   # Empty
[1 2 3]              # With elements
[1, 2, 3]            # Comma-separated

# Operations
(arr .push x)
(arr .pop)
(arr .get idx)
(arr .set idx val)
(arr .len)
```

### 4.5.2 Maps

```gene
# Map literals
{}                   # Empty
{^a 1 ^b 2}          # Property syntax

# Operations
(m .get key)
(m .set key val)
(m .has? key)
(m .keys)
(m .values)
```

### 4.5.3 Sets

```gene
# Set literals
#[1 2 3]             # Hash set

# Operations
(s .add x)
(s .del x)
(s .has? x)
(s .union s2)
(s .intersection s2)
```

## 4.6 Reference Types

```gene
(:symbol ^p1 "v1" ^p2 "v2" 1 2)
("string" ^p1 "v1" ^p2 "v2" 1 2)
((:x) ^p1 "v1" ^p2 "v2" 1 2)    # head is another gene
```

## 4.7 Value Semantics

```gene
# Immutable by default
(var x = [1 2 3])
(var y = x)          # Shared structure
(y .push 4)          # Creates new array

# Explicit mutation
(var x = (mutable [1 2 3]))
(x .push! 4)         # Mutates in place

# Value types
(value Point {^x :float ^y :float})
# Passed by value, not reference
```

## 4.8 The Gene Type

The Gene type is the fundamental generic container in the language, providing optimized access to both properties and children:

```gene
# Gene literal syntax
(X ^prop1 value1 ^prop2 value2 child1 child2)

# Where X is the head
# Properties are key-value pairs marked with ^
# Children are positional elements after properties

# Examples
(div ^class "container" ^id "main"
  (h1 "Hello")
  (p "World"))

(person ^name "John" ^age 30)

(+ ^checked true 1 2 3)  # Even operators can have properties
```

### 4.8.1 Gene Type Definition

```gene
# The Gene type is built into the language
(type Gene
  (var head :Any)           # The tag/head symbol
  (var props :Map)          # Property map (optimized)
  (var children :Array)     # Positional children
  (var meta :Map))          # Metadata (source location, etc.)

# Creation
(new Gene div {^class "container"} [(h1 "Hello")])
(new Gene person {^name "John" ^age 30} [])
```

### 4.8.2 Optimized Access Patterns

```gene
# Property access - O(1) with inline caching
(var expr (div ^class "container"))
expr/class                         # "container"
expr/prop                          # General property access

# Child access - O(1) array access
expr/0                             # First child
expr/1                             # Second child
expr/-1                            # Last child

# Head access
expr/.head                         # Get the head

# Reflection
expr/.props                        # Get all properties
expr/.children                     # Get all children
expr/.length                       # Number of children
expr/.size                         # Number of children
expr/.len                          # Number of children
```

### 4.8.3 Pattern Matching on Genes

```gene
(match expr
  (div ^class c children...) (process-div c children)
  (span ^id id) (process-span id)
  (X ^type "button" label) (make-button label)
  _ "unknown")

# Destructuring in function arguments
(fn render [(tag ^class c ^id i children...)]
  ...)

# Guard patterns
(match expr
  (X ^age a) when (> a 18) "adult"
  (X ^age a) "minor"
  _ "no age")
```

### 4.8.4 Gene Type Optimizations

```gene
# Small gene optimization
# Genes with ≤ 2 properties and ≤ 2 children use specialized representation

# Property inline caching
# Common property access patterns are cached at call sites

# Structural sharing
# Genes share structure when using update operations
(var g1 = (div ^class "a" child1))
(var g2 = (g1 ^class "b"))  # Shares children with g1
```

### 4.8.5 Gene Manipulation Functions

```gene
# Update properties
(g .set "prop" value)              # Add/update property
(g .del "prop")                    # Remove property

# Update children
(g .add_child x)                   # Add child
(g .add_child x i)                 # Insert child at index
(g .set_child x i)                 # Change child at index
(g .del_child i)                   # Remove child at index
(g .pop_child)                     # Remove last child
```

### 4.8.6 Gene as Universal AST Node

```gene
# All Gene code is made of Genes
(fn add [x y] (+ x y))

# Is actually:
(fn
  "add"         # name
  [x y]         # arguments
  [             # body
    (+ x y)
  ]
  )
```

### 4.8.7 Quasi-quotation with Genes

```gene
# Template syntax
:(div ^class "container"
   %title
   %items...)

# Expands based on context
(var title = (h1 "My Title"))
(var items = [(p "Item 1") (p "Item 2")])

# Hygenic macro expansion using Genes
(syntax my-when [test body...]
  (if test
     (do body...)
     nil))
```

### 4.8.8 Performance Characteristics

```gene
# Property access: O(1) with inline caching
# Child access: O(1) array indexing
# Pattern matching: Optimized dispatch trees
# Memory: Compact representation with structural sharing

# Specialized representations:
# - Empty gene: (X) - no allocation for props/children
# - Props only: (X ^a 1 ^b 2) - no children array
# - Children only: (X a b c) - no props map
# - Small gene: ≤ 2 props, ≤ 2 children - inline storage
```

---

# Chapter 5: Control Flow and Expressions

## 5.1 Everything is an Expression

In Gene, all constructs return values:

```gene
(var result = (if (> x 0) "positive" "non-positive"))

(var value = (do
  (print "calculating...")
  (+ 1 2)))
```

## 5.2 Conditionals

### 5.2.1 If Expression

```gene
# Basic if
(if condition
  then-expr
  else-expr)

# Without else (returns nil)
(if condition
  then-expr)

# If-not
(if-not condition
  then-expr
  # Else clause is not allowed
)
```

### 5.2.2 Cond Expression

```gene
(cond
  (< x 0) "negative"
  (> x 0) "positive"
  _ "zero")
```

### 5.2.3 When/Unless

```gene
# Multiple expressions, returns last
(when (> x 0)
  (print "positive")
  (* x 2))

(unless (nil? x)
  (process x))
```

## 5.3 Pattern Matching

```gene
# Basic patterns
(match value
  0 "zero"
  1 "one"
  n (new Str "number: " n))

# Destructuring
(match point
  {^x 0 ^y 0} "origin"
  {^x x ^y 0} (new Str "on x-axis at " x)
  {^x 0 ^y y} (new Str "on y-axis at " y)
  {^x x ^y y} (new Str "point at " x "," y))

# Array patterns
(match arr
  [] "empty"
  [x] (new Str "single: " x)
  [x y] (new Str "pair: " x " " y)
  [x & rest] (new Str "first: " x " rest: " rest))

# Type patterns
(match value
  x Int (* x 2)
  s Str (s .upcase)
  _ "unknown")

# Guards
(match value
  x when (> x 0) "positive"
  x when (< x 0) "negative"
  _ "zero")
```

## 5.4 Loops

### 5.4.1 For Loop

```gene
# Range iteration
(for i in (range 10)
  (print i))

# Collection iteration
(for x in arr
  (process x))

# With conditions
(for x in arr when x/.even?
  (* x 2))
```

### 5.4.2 While Loop

```gene
(while (< i 10)
  (print i)
  (i = (+ i 1)))
```

### 5.4.3 Loop/Recur

```gene
(loop
  (var i = 0)
  (var sum = 0)
  (if (>= i 10)
    sum
    (do (+ i 1) (+ sum i))))
```

## 5.5 Exception Handling

```gene
(try
  (dangerous-operation)

  catch IOError => e
    (log "IO error: " e)
    :io-failed

  catch _ => e
    (log "General error: " e)
    :error

  finally
    (cleanup))
```

## 5.6 Block Expressions

```gene
# Do block - sequential execution
(do
  (var x = 10)
  (var y = 20)
  (+ x y))  # Returns 30
```

## 5.7 Early Returns

```gene
(fn find-first [pred coll]
  (for x in coll
    (when (pred x)
      (return x)))
  nil)
```

---

# Chapter 6: Functions and Closures

## 6.1 Function Definition

Functions are first-class values in Gene:

```gene
# Basic function
(fn add [x y]
  (+ x y))

# With type annotations
(fn add [x :Int y :Int => :Int]
  (+ x y))

# Generic function
(fn identity ^^T [x :T => :T]
  x)

# Variadic function
(fn sum [args...]
  (reduce + 0 args))

# Optional parameters
(fn greet [name :Str greeting :Str = "Hello" => :Str]
  (new Str greeting " " name))
```

## 6.2 Anonymous Functions

```gene
# Lambda syntax
(fnx [x] (* x 2))

# With types
(fnx [x :Int => :Int] (* x 2))

# Shorthand syntax (future feature)
#(* % 2)              # % is implicit parameter
#(+ %1 %2)            # Multiple parameters
```

## 6.3 Closures

Functions capture their lexical environment:

```gene
(fn make-counter [initial :Int]
  (var count = initial)
  (fnx []
    (count = (+ count 1))
    count))

(var c1 = (make-counter 0))
(c1)  # 1
(c1)  # 2
```

## 6.4 Function Calls

```gene
# Regular call
(add 1 2)

# Apply
(apply add [1 2])

# Future: Partial application
(var add5 = (partial add 5))
(add5 3)  # 8

# Undecided: Pipe operations
(-> 5
    (+ 3)
    (* 2)
    (- 1))  # 15

# Undecided: Thread-last
(->> [1 2 3]
     (map (fnx [x] (* x 2)))
     (filter (fnx [x] (> x 2)))
     (reduce +))
```

## 6.5 Method Syntax

Methods are functions with special dispatch:

```gene
# Method definition in class
(class Point
  (.fn distance [other :Point => :Float]
    (math/sqrt (+ (** (- other/x /x) 2)
                  (** (- other/y /y) 2)))))

# Method call
(point .distance other-point)
(.distance other-point) = (self .distance other-point)
```

## 6.6 Multiple Dispatch (Undecided, let's leave it out)

---

# Chapter 7: Object-Oriented Programming

## 7.1 Classes

```gene
# Basic class
(class Point
  # Fields using .prop macro-method
  (.prop x Float)
  (.prop y Float)

  # Constructor
  (.new [x :Float y :Float]
    (/x = x)
    (/y = y))

  # Methods
  (.fn distance [other :Point => :Float]
    (math/sqrt (+ (** (- other/x /x) 2)
                  (** (- other/y /y) 2))))

  # Properties (getters/setters)
  (.get magnitude []
    (math/sqrt (+ (* /x /x) (* /y /y))))

  (.set magnitude [m :Float]
    (var scale = (/ m /magnitude))
    (/x = (* /x scale))
    (/y = (* /y scale))))

# Instantiation
(var p = (new Point 3.0 4.0))
(var p2 = (Point 3.0 4.0))  # new is optional
```

## 7.2 Inheritance

```gene
# Single inheritance
(class Circle extends Shape
  (.prop radius Float)

  (.new [x :Float y :Float radius :Float]
    (super x y)  # Call parent constructor
    (/radius = radius))

  # Override parent method
  (.fn area [] :Float
    (* math/pi (* /radius /radius))))

# Abstract classes
(abstract class Shape
  (.prop x Float)
  (.prop y Float)

  (.fn area [] :Float)  # Abstract method

  (.fn describe [] :Str
    (new Str "Shape with area: " (.area))))
```

## 7.3 Traits (Interfaces with Defaults)

Traits are interfaces that can provide default implementations. Classes **implement** traits, establishing an IS-A relationship that can be checked at runtime.

```gene
# Define trait - an interface contract
(trait Comparable ^^T
  # Abstract method - must be implemented
  (.fn compare [other :T => :Int])

  # Default implementations using the abstract method
  (.fn < [other :T => :Bool]
    (< (.compare other) 0))

  (.fn > [other :T => :Bool]
    (> (.compare other) 0))

  (.fn <= [other :T => :Bool]
    (not (.> other)))

  (.fn >= [other :T => :Bool]
    (not (.< other))))

# Implement trait - establishes IS-A relationship
(class String implements Comparable
  (.fn compare [other :Str => :Int]
    (cond
      (< self other) -1
      (> self other) 1
      _ 0)))

# Can check trait implementation
(str is Comparable)  # true
(str .is? Comparable)  # true

# Multiple traits
(class File implements Readable Writable Closeable
  (.fn read [] ...)
  (.fn write [data] ...)
  (.fn close [] ...))
```

## 7.4 Mixins (Copy-Paste Functionality)

Mixins are bundles of functionality that get **copied** into the including class. There's no IS-A relationship - it's purely code reuse.

```gene
# Define mixin - a bundle of functionality
(mixin Timestamped
  (.prop created-at Time = (time/now))
  (.prop updated-at Time = (time/now))

  (.fn touch []
    (/updated-at = (time/now)))

  (.fn age [] :Duration
    (time/since /created-at)))

# Include mixin - copies the code
(class Document includes Timestamped
  (.prop title Str)
  (.prop content Str)

  (.fn save []
    (.touch)  # From mixin
    (db/save self)))

# Cannot check mixin "inheritance"
(doc is Timestamped)  # ERROR: Timestamped is not a type
(doc .has-prop? :created-at)  # true - the property exists
```

## 7.5 Method Types

Gene supports multiple types of methods to provide different execution semantics:

```gene
(class Component
  (.prop state Map = {})
  (.prop listeners Array = [])

  # Regular method - standard evaluation
  (.fn update [key value]
    (/state .set key value)
    (.notify-listeners key value))

  # Pseudo macro method - lazy evaluation
  (.macro when-changed [condition body]
    (if condition
      (do
        (.update :last-change (time/now))
        body)
      nil))

  # Static method
  (.static fn create-default []
    (Component))

  # Virtual method (can be overridden)
  (.virtual fn render []
    (print "Base component"))

  # Abstract method (must be implemented by subclasses)
  (.abstract fn validate [])

  # Private method
  (.-fn internal-cleanup []
    (.clear-cache)))
```

### 7.5.1 Pseudo Macro Methods

Pseudo macro methods combine the lazy evaluation semantics of pseudo macros with object-oriented method dispatch:

```gene
(class StateMachine
  (.prop current-state Symbol = :initial)
  (.prop transition-log Array = [])

  # Macro method for state transitions with validation
  (.macro transition-to [new-state guard-expr action-expr]
    (when guard-expr
      (do
        (/transition-log .push {^from /current-state ^to new-state ^time (time/now)})
        action-expr                    # Evaluated in caller's context
        (/current-state = new-state))))

# Usage - arguments evaluated in caller's context
(var machine = (StateMachine))
(var user-authorized = true)
(var activation-data = {...})

(machine .transition-to :active user-authorized    # 'user-authorized' from caller
  (do                                              # 'activation-data' from caller
    (setup-with activation-data)
    (print "Machine activated!")))
```

### 7.5.2 Method Resolution Order

When calling methods, Gene follows this resolution order:
1. Instance methods (regular and macro)
2. Class methods
3. Parent class methods (depth-first)
4. Trait methods
5. Mixin methods

Macro methods are resolved using the same order but execute with lazy evaluation semantics.

## 7.6 Field Modifiers

```gene
(class BankAccount
  # Private field (not accessible outside)
  (.prop -balance Float)

  # Protected field (accessible in subclasses)
  (.prop #account-number Str)

  # Public by default
  (.prop name Str)

  # Static field
  (.static prop interest-rate Float = 0.05)

  # Mutable field (all fields are mutable by default)
  (.prop mut counter Int = 0)

  # Immutable field
  (.prop const id Str)

  # Optional field
  (.prop account-type (Option Str))

  # Field with default value
  (.prop status Str = "active")

  # Read-only property
  (.get balance [] :Float
    /-balance))
```

## 7.7 Static Members

```gene
(class Math
  # Static fields
  (static PI = 3.14159)
  (static E = 2.71828)

  # Static methods
  (.static fn abs [x :Number => :Number]
    (if (< x 0) (- x) x))

  # Class methods
  (.class fn create-unit-circle []
    (Circle 0 0 1)))

# Access
Math/PI
(Math .abs -5)
```

## 7.8 Key Differences Between Traits and Mixins

```gene
# Trait example - Serializable IS-A type
(trait Serializable
  (.fn serialize [] :Str)

  # Default using serialize
  (.fn save-to-file [path :Str]
    (file/write path (.serialize))))

(class Person implements Serializable
  (.prop name Str)
  (.prop age Int)

  (.fn serialize [] :Str
    (json/stringify {^name /name ^age /age})))

# Can use trait as type constraint
(fn save-all [items :(Array Serializable)]
  (for item in items
    (item .save-to-file (new Str item/.name ".json"))))

# Mixin example - Observable is NOT a type
(mixin Observable
  (.prop -observers Array = [])

  (.fn add-observer [fn]
    (/-observers .push fn))

  (.fn notify [event]
    (for obs in /-observers
      (obs event))))

(class Model includes Observable
  (.prop data Map)

  (.fn update [key value]
    (/data .set key value)
    (.notify {:changed key})))  # From mixin

# Cannot use mixin as type
(fn process [obj :Observable])  # ERROR: Observable is not a type
```

## 7.9 Method Resolution

```gene
# Method resolution order (MRO)
# 1. Instance methods
# 2. Class methods
# 3. Parent class methods (depth-first)
# 4. Trait methods
# 5. Mixin methods

# Super calls
(class Child extends Parent
  (.fn method []
    (super .method)  # Call parent version
    (super)         # Call parent with same args
    ...))
```

---

# Chapter 8: Module System and Namespaces

## 8.1 Namespace Definition

```gene
# Define namespace
(ns myapp/utils
  ^export [helper1 helper2 MyClass]
  ^import {std/io [read write]
           math [sin cos]})

# Nested namespaces
(ns myapp/utils/string
  ^export [trim split join])

# File organization mirrors namespace
# myapp/utils.gene
# myapp/utils/string.gene
```

## 8.2 Import and Export

```gene
# Export declarations
(ns mylib
  # Export specific items
  ^export [public-fn :function
           PublicClass :class
           PUBLIC-CONST :value]

  # Export with renaming
  ^export [internal-name as public-name]

  # Export all public definitions
  ^export *

  # Re-export from another module
  ^export-from other/module [item1 item2])

# Import variations
(import mylib [public-fn PublicClass])
(import mylib [[public-fn :as fn]])
(import mylib *)  # Import all
(import mylib)    # Import namespace object
```

## 8.3 Namespace Access

```gene
# Fully qualified access
myapp/utils/helper1

# After import
(import myapp/utils [helper1])
helper1  # Direct access

# Namespace alias
(alias mu myapp/utils)
mu/helper1

# Dynamic import
(require :myapp/utils)
((resolve :myapp/utils/helper1) args)
```

## 8.4 Private Definitions

```gene
(ns mylib
  # Private by default with -
  (-fn private-helper []
    ...)

  # Public function
  (fn public-fn []
    (private-helper))

  # Private variable
  (-var secret-key = "...")

  ^export [public-fn])  # Only export public items
```

## 8.5 Module Properties

```gene
(ns mylib
  ^version "1.2.3"
  ^author "John Doe"
  ^license "MIT"

  ^export [])

# Access module metadata
mylib/.version  # "1.2.3"
```

---

# Chapter 9: Concurrency and Parallelism

## 9.1 Actor Model

Gene uses the actor model for safe concurrent programming:

```gene
# Define an actor
(actor Counter
  (var count :Int = 0)

  (.receive msg
    (match msg
      [:inc n] (/count = (+ /count n))
      [:get reply-ch] (reply-ch .send /count)
      [:reset] (/count = 0))))

# Create and use actor
(var counter = (spawn Counter))
(counter .send [:inc 5])
(counter .send [:inc 3])

# Get value with channel
(var ch = (chan Int))
(counter .send [:get ch])
(var value = (ch .receive))  # 8
```

## 9.2 Channels

Channels provide synchronized communication between threads:

```gene
# Create channels
(var ch = (chan))           # Unbuffered
(var ch = (chan Int 10))    # Buffered, typed

# Send and receive
(ch .send value)
(var value = (ch .receive))

# Non-blocking operations
(match (ch .try-receive)
  [:ok value] (process value)
  [:err :empty] (default-action))

# Select on multiple channels
(select
  (ch1 .receive) => value
    (handle-ch1 value)
  (ch2 .send data) =>
    (print "sent to ch2")
  (timeout 1000) =>
    (print "timeout!"))
```

## 9.3 Parallel Collections

```gene
# Parallel map
(pmap (fnx [x] (* x x)) large-array)

# Parallel reduce
(preduce + 0 large-array)

# Parallel for
(pfor x in large-array when (> x 100)
  (expensive-computation x))

# Control parallelism
(with-parallelism 4
  (pmap process-item items))
```

## 9.4 Futures and Promises

```gene
# Future - computation in background
(var fut = (future
  (expensive-calculation)))

# Get result (blocks if not ready)
(var result = (fut .get))

# Check if ready
(if (fut .ready?)
  (fut .get)
  "still computing...")

# Promise - single assignment
(var p = (promise))

# In another thread
(p .deliver result)

# Wait for promise
(var value = (p .get))
```

## 9.5 Software Transactional Memory (STM)

```gene
# Refs for coordinated state
(var account1 = (ref 1000))
(var account2 = (ref 500))

# Atomic transaction
(dosync
  (account1 .alter - 100)
  (account2 .alter + 100))

# Ensure consistency
(fn transfer [from to amount]
  (dosync
    (when (< (from .get) amount)
      (throw :insufficient-funds))
    (from .alter - amount)
    (to .alter + amount)))
```

## 9.6 Atoms

For independent state updates:

```gene
# Create atom
(var counter = (atom 0))

# Update operations
(counter .swap! inc)
(counter .swap! + 5)
(counter .reset! 0)

# Compare and swap
(counter .compare-and-set! old-val new-val)

# Watch for changes
(counter .add-watch :logger
  (fnx [key ref old new]
    (print "Changed from" old "to" new)))
```

## 9.7 Thread Management

```gene
# Create threads
(var t = (thread
  (fnx []
    (print "Running in thread" (thread/current)))))

# Thread pool
(var pool = (thread-pool 4))
(pool .submit task)
(pool .shutdown)

# Thread-local variables
(var ^:dynamic *context* = nil)

(binding [*context* "worker-1"]
  (process-with-context))
```

---

# Chapter 10: Memory Management

## 10.1 Garbage Collection Strategy

Gene uses a hybrid garbage collection approach:

1. **Incremental Mark-and-Sweep**: For initial implementation
2. **Generational GC**: For mature implementation
3. **Reference Counting**: For deterministic cleanup

## 10.2 Memory Layout

```gene
# Value representation (64-bit NaN-boxing)
- Immediate values: nil, true, false, small ints, chars
- Small strings (≤ 6 bytes): Inline in NaN-box
- Pointers: 48-bit addresses with type tags

# Object header (2 words)
- Type/class pointer
- GC marks and flags
- Hash code cache
- Reference count (optional)
```

## 10.3 Allocation Strategies

```gene
# Stack allocation hints
(var ^:stack point = (Point 3 4))

# Region-based allocation
(with-region r
  (var data = (r .alloc BigStructure))
  (process data))  # All region memory freed here

# Object pools
(var pool = (object-pool Point 1000))
(var p = (pool .acquire))
(use p)
(pool .release p)
```

## 10.4 Weak References

```gene
# Weak references
(var weak = (weak-ref object))
(match (weak .get)
  [:ok obj] (use obj)
  [:err :collected] (recreate))

# Weak collections
(var cache = (weak-map))
(cache .set key large-object)

# Finalizers
(object .add-finalizer
  (fnx []
    (cleanup-resources)))
```

## 10.5 Memory Profiling

```gene
# Runtime memory info
(gc/heap-size)
(gc/used-memory)
(gc/collect)

# Allocation tracking
(with-allocation-tracking
  (run-code))
(gc/allocation-report)

# Memory pressure handling
(gc/add-pressure-handler
  (fnx [level]
    (when (> level 0.8)
      (clear-caches))))
```

---

# Chapter 11: Metaprogramming

## 11.1 Pseudo Macros

Pseudo macros are a unique metaprogramming feature that provides lazy evaluation semantics without the complexity of compile-time macros. Unlike traditional Lisp macros that operate at compile-time, pseudo macros work at runtime with delayed evaluation.

### 11.1.1 Core Concept

**Key Properties:**
1. **Lazy Evaluation**: Arguments are not evaluated at call time
2. **Lexical Scoping**: Arguments are evaluated in the caller's context when needed
3. **On-Demand Resolution**: Inside the macro body, argument references trigger evaluation in the original context

```gene
# Define a pseudo macro using 'macro' keyword
(macro when [condition body]
  (if condition body nil))

# Usage - arguments not evaluated until needed
(var x 0)
(when (> x 5)           ;; This comparison not evaluated immediately
  (print "x is large")) ;; This print not evaluated immediately

# The macro body evaluates 'condition' and 'body' on demand
# in the caller's context where x is visible
```

### 11.1.2 Advanced Examples

```gene
# Conditional execution with multiple statements
(macro unless [condition body]
  (if condition nil body))

# Resource management with cleanup
(macro with-resource [resource-expr body]
  (do
    (var resource resource-expr)
    (try
      body
      (finally (cleanup resource)))))

# Usage examples
(var file-path "data.txt")
(with-resource (open-file file-path)  ;; File opening delayed
  (do
    (print "Processing file")
    (process-data resource)))         ;; 'resource' comes from macro
```

### 11.1.3 Pseudo Macro Methods

Pseudo macros can also be defined as methods within classes, combining lazy evaluation with object-oriented programming:

```gene
(class Component
  (.prop state Map = {})
  (.prop dirty Bool = false)

  # Regular method
  (.fn update [key value]
    (/state .set key value)
    (/dirty = true))

  # Pseudo macro method - lazy evaluation with object context
  (.macro when-dirty [condition cleanup-expr]
    (when (and /dirty condition)
      (do
        cleanup-expr               # Evaluated in caller's context
        (/dirty = false))))

  # Macro method for conditional updates
  (.macro conditional-update [guard-expr key-expr value-expr]
    (when guard-expr
      (.update key-expr value-expr))))

# Usage - arguments evaluated in caller's context
(var comp = (Component))
(var should-cleanup = true)
(var user-data = {...})

(comp .when-dirty should-cleanup
  (save-to-disk user-data))        # 'user-data' from caller's scope

(comp .conditional-update (user .has-permission?)
  :user-id (user .id)
  :data user-data)                 # All expressions from caller
```

### 11.1.4 Macro Method Inheritance

Pseudo macro methods participate in inheritance like regular methods:

```gene
(class BaseComponent
  (.macro with-timing [operation-expr]
    (do
      (var start = (time/now))
      operation-expr
      (var end = (time/now))
      (.log "Operation took" (- end start) "ms"))))

(class Button extends BaseComponent
  (.fn click []
    (.with-timing                  # Inherited macro method
      (do
        (.handle-click)
        (.update-ui)))))
```

### 11.1.5 Implementation Architecture

Pseudo macros integrate into Gene's compilation pipeline through:

1. **AST Level**: `PseudoMacroDef` and `PseudoMacroCall` nodes
2. **HIR Level**: Lazy argument wrapping with context capture
3. **MIR Level**: Thunk-based lazy evaluation mechanism
4. **Bytecode Level**: Runtime context switching and thunk evaluation
5. **VM Level**: Execution with preserved lexical environments

See `docs/design_of_macros.md` for complete implementation details.

## 11.2 Traditional Macros

Gene also supports traditional compile-time macros for cases where compile-time code generation is needed:

```gene
# Define compile-time macro
(defmacro when [test body...]
  :(if %test
     (do %body...)
     nil))

# Use macro
(when (> x 0)
  (print "positive")
  (* x 2))

# Macro with pattern matching
(defmacro defn
  [[name args... => ret] body...]
    :(fn %name %args... => %ret %body...)
  [[name args...] body...]
    :(fn %name %args... %body...))

# Hygiene
(defmacro swap! [a b]
  (gensym tmp
    :(let %tmp %a
       (%a = %b)
       (%b = %tmp))))
```

## 11.3 Quotation

```gene
# Quote prevents evaluation
:x                    # Symbol x
:(+ 1 2)             # Gene (+ 1 2) not evaluated

# Unquote in quasiquote context
(var x = 5)
:(list 1 %x 3)       # Evaluates to (list 1 5 3)

# Unquote-splice
(var items = [2 3 4])
:(list 1 %items... 5) # Evaluates to (list 1 2 3 4 5)
```

## 11.4 Code Generation

```gene
# Generate code at compile time
(meta fn generate-accessors [fields]
  (for f in fields
    :(fn %(symbol "get-" f) [] /%f)))

(class Point
  (.prop x Float)
  (.prop y Float)
  %(generate-accessors [:x :y])...)

# Conditional compilation
(meta if debug?
  (fn log [msg] (print "[DEBUG]" msg))
  (fn log [msg]))  # No-op in release
```

## 11.5 Reflection

```gene
# Type introspection
(obj .type)
(obj .type-name)
(obj .is? Type)

# Method introspection
(obj .methods)
(obj .has-method? :name)
(obj .invoke :method-name args...)

# Field access
(obj .fields)
(obj .get-field :field-name)
(obj .set-field! :field-name value)

# Module introspection
(module .exports)
(module .imports)
(module .defined-names)
```

## 11.6 AST Manipulation

```gene
# Parse code
(var ast = (parse "(+ 1 2)"))

# Walk AST
(walk ast
  (fnx [node]
    (match node
      (Symbol name) (transform-symbol name)
      (Gene head props children)
        (Gene head props (map walk children))
      _ node)))

# Compile-time AST transformation
(meta transform some-code
  (optimize-ast some-code))
```

---

# Chapter 12: Error Handling

## 12.1 Exception System

```gene
# Throw exceptions
(throw :key)
(throw {:type :error :message "Failed"})
(throw (Exception "message"))

# Try-catch
(try
  (risky-operation)

  catch :network-error => e
    (handle-network e)

  catch Exception => e
    (log-error e)
    (re-throw)

  finally
    (cleanup))
```

## 12.2 Result Types

```gene
# Result type for explicit error handling
(type Result ^^T ^^E (| [:ok T] [:err E]))

(fn safe-divide [a :Float b :Float => :(Result Float Str)]
  (if (= b 0)
    [:err "Division by zero"]
    [:ok (/ a b)]))

# Chain operations
(var result =
  (safe-divide 10 2)
  .and-then (fnx [x] (safe-divide x 2))
  .map (fnx [x] (* x 10))
  .unwrap-or 0)
```

## 12.3 Error Propagation

```gene
# Early return on error
(fn process-file [path :Str => :(Result Data Error)]
  (var file = (open-file? path))  # Returns Result
  (var data = (read-data? file))  # Returns Result
  (var parsed = (parse? data))    # Returns Result
  [:ok parsed])

# Error boundaries
(with-error-boundary
  ^on-error (fnx [e] (log e) default-value)
  (dangerous-computation))
```

## 12.4 Assertions and Contracts

```gene
# Assertions
(assert (> x 0))
(assert (> x 0) "x must be positive")

# Pre/post conditions
(fn sqrt [x :Float => :Float]
  ^pre [(>= x 0)]
  ^post [(>= result 0)]
  (math/sqrt x))

# Invariants
(class Account
  ^invariant [(>= /balance 0)]

  (.fn withdraw [amount :Float]
    ^pre [(<= amount /balance)]
    (/balance = (- /balance amount))))
```

---

# Chapter 13: Standard Library

## 13.1 Core Functions

```gene
# Identity and composition
(identity x)
(compose f g)
(partial f arg1 arg2)

# Predicates
(nil? x)
(empty? coll)
(even? n)
(odd? n)

# Comparison
(= a b)
(< a b)
(> a b)
(compare a b)

# Type checking
(x is Type)
```

## 13.2 Collections

```gene
# Sequence operations
(map f coll)
(filter pred coll)
(reduce f init coll)
(take n coll)
(drop n coll)
(concat coll1 coll2)

# Array specific
(arr .push x)
(arr .pop)
(arr .slice start end)
(arr .sort)
(arr .reverse)

# Map operations
(m .get k)
(m .set k v)
(m .merge m2)
(m .select-keys [:k1 :k2])

# Set operations
(s1 .union s2)
(s1 .intersection s2)
(s1 .difference s2)
```

## 13.3 String Operations

```gene
(str .len)
(str .upper)
(str .lower)
(str .trim)
(str .split pattern)
(str .replace old new)
(str .starts-with? prefix)
(str .ends-with? suffix)
(str .contains? substr)
```

## 13.4 Math

```gene
# Constants
math/pi
math/e

# Functions
(math/abs x)
(math/sqrt x)
(math/pow x y)
(math/sin x)
(math/cos x)
(math/log x)
(math/exp x)
(math/round x)
(math/floor x)
(math/ceil x)
```

## 13.5 I/O

```gene
# Files
(file/read path)
(file/write path content)
(file/exists? path)
(file/delete path)

# Streams
(with-open f (file/open path)
  (f .read-line))

# Network
(http/get url)
(http/post url data)

# JSON
(json/parse str)
(json/stringify data)
```

## 13.6 Time and Date

```gene
(time/now)
(time/parse "2023-01-01")
(time/format date pattern)
(time/add date amount unit)
(time/between date1 date2)
```

---

# Part II: Reference Implementation

# Chapter 14: Architecture Overview

## 14.1 High-Level Architecture

Gene uses a unified runtime architecture supporting seamless interoperability between interpreted and compiled code at function granularity:

```
                    Source Code (.gene files)
                            ↓
                    Lexer (Tokenization)
                            ↓
                    Parser (AST Generation)
                            ↓
                    Macro Expansion
                            ↓
                    Type Checker (Optional)
                            ↓
                    HIR Generation
                            ↓
                    MIR Generation & Optimization
                            ↓
              ┌─────────────┴─────────────┐
              │                           │
      ═══ Fast Path ═══           ═══ Optimized Path ═══
              │                           │
      MIR → Bytecode                MIR → LIR → Native
              │                           │
              ↓                           ↓
    ┌─────────────────────────────────────────────────────────┐
    │                 Unified Runtime                         │
    ├─────────────────────────────────────────────────────────┤
    │  ┌─────────────┐  ┌─────────────┐  ┌─────────────────┐  │
    │  │Interpreted  │  │    JIT      │  │  AOT Compiled   │  │
    │  │ Functions   │  │ Functions   │  │   Functions     │  │
    │  │             │  │             │  │                 │  │
    │  │Quick Start  │  │ Hot Paths   │  │Core Libraries   │  │
    │  │Development  │  │ Optimized   │  │System Functions │  │
    │  └─────────────┘  └─────────────┘  └─────────────────┘  │
    │         │                │                    │         │
    │         └────────────────┼────────────────────┘         │
    │                          │                              │
    │            ┌─────────────────────────────┐              │
    │            │ Unified Function Dispatch   │              │
    │            │    (Transparent Calls)      │              │
    │            └─────────────────────────────┘              │
    └─────────────────────────────────────────────────────────┘
```

## 14.2 Execution Modes

Gene supports three execution modes that can coexist seamlessly within the same program:

### 14.2.1 Interpreted Execution (Fast Compilation)
- **Use Case**: Development, REPL, cold code, rapid iteration
- **Compilation Time**: ~1-5ms per function
- **Runtime Performance**: Good for development, adequate for most code
- **Memory Usage**: Moderate (bytecode + VM state)

### 14.2.2 JIT Compilation (Balanced)
- **Use Case**: Hot paths identified at runtime
- **Compilation Time**: ~10-50ms per function
- **Runtime Performance**: Near-native speed
- **Trigger**: Functions called > 1000 times

### 14.2.3 AOT Compilation (Maximum Performance)
- **Use Case**: Core libraries, system functions, deployment
- **Compilation Time**: ~100-500ms per function
- **Runtime Performance**: Native C-level speed
- **Usage**: Standard library, critical paths

## 14.3 Core Components

1. **Unified Frontend**
   - Lexer: Token generation
   - Parser: AST construction
   - Macro expander: Pseudo macro and traditional macro support
   - Shared HIR/MIR: Common intermediate representation

2. **Dual Backend Paths**
   - **Fast Path**: MIR → Bytecode (simple register allocation)
   - **Optimized Path**: MIR → LIR → Native (advanced optimization)

3. **Unified Runtime System**
   - **Function Table**: Tracks execution mode per function
   - **Transparent Dispatch**: Calls work regardless of execution mode
   - **Tiered Compilation**: Automatic promotion from interpreted → JIT → AOT
   - **Profile-Guided Optimization**: Runtime statistics drive compilation decisions

4. **Memory Management**
   - **Unified GC**: Works across all execution modes
   - **Value Representation**: NaN-boxing for uniform handling
   - **Cross-Mode Calls**: Zero-cost calls between execution modes

## 14.4 Mixed Execution Examples

### Development Workflow
```gene
# User function starts interpreted (fast compilation)
(fn fibonacci [n]
  (if (< n 2) n
    (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

# Built-in functions are AOT compiled
(print "Computing...")          # Native speed

# Library functions may be JIT compiled
(map fibonacci [1 2 3 4 5])     # 'map' gets JIT compiled if hot
```

### Production Deployment
```zig
// Production binary contains mixed execution modes
Gene Binary {
    // AOT compiled - maximum performance
    core_functions: [print, +, -, *, map, filter, reduce, ...],
    stdlib: [io/*, math/*, string/*, ...],

    // Interpreted initially - fast startup
    user_code: [main, business_logic, ...],

    // JIT compiled at runtime - optimized hot paths
    hot_functions: [], // Populated during execution

    // Unified runtime manages all modes transparently
    runtime: FunctionTable + VM + JIT + Profiler,
}
```

## 14.5 Key Design Decisions

- **Function-Level Granularity**: Compilation decisions made per function, not per module
- **Transparent Interoperability**: Calls work seamlessly across execution modes
- **Progressive Optimization**: Functions automatically upgrade from interpreted → JIT → AOT
- **Unified Value Representation**: NaN-boxing enables zero-cost cross-mode calls
- **Profile-Guided Decisions**: Runtime statistics drive optimization choices
- **Fast Development Cycle**: Always start interpreted for immediate feedback
- **Production Performance**: Hot paths automatically reach native speeds

---

# Chapter 15: Lexer and Parser

## 15.1 Lexer Design

The lexer converts source text into tokens:

```zig
// src/frontend/token.zig
pub const Token = union(enum) {
    // Literals
    integer: i64,
    float: f64,
    string: []const u8,
    char: u21,  // Unicode codepoint
    symbol: []const u8,
    keyword: []const u8,

    // Delimiters
    left_paren,
    right_paren,
    left_bracket,
    right_bracket,
    left_brace,
    right_brace,

    // Special
    quote,      // :
    unquote,    // %
    property,   // ^
    dot,        // .
    slash,      // /

    // Operators
    plus, minus, star, slash_op,
    eq, ne, lt, gt, le, ge,
    and_op, or_op, not_op,

    // Keywords
    if_kw, else_kw, do_kw, fn_kw, var_kw, class_kw,
    macro_kw, defmacro_kw,
    // ... etc

    pub fn deinit(self: *Token, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .string, .symbol, .keyword => |str| allocator.free(str),
            else => {},
        }
    }
};
```

## 15.2 Parser Design

The parser builds an AST from tokens:

```zig
// src/frontend/ast.zig - Expression types (already implemented)
pub const Expression = union(enum) {
    // Atoms
    Literal: Literal,
    Variable: Variable,

    // Compound expressions
    If: If,
    FuncCall: FuncCall,
    FuncDef: FuncDef,
    VarDecl: VarDecl,
    ArrayLiteral: ArrayLiteral,
    MapLiteral: MapLiteral,
    DoBlock: DoBlock,

    // Object-oriented
    ClassDef: ClassDef,
    MatchExpr: MatchExpr,

    // Module system
    ModuleDef: ModuleDef,
    ImportStmt: ImportStmt,
    ExportStmt: ExportStmt,

    // Memory management
    pub fn deinit(self: *Expression, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .Literal => |*lit| lit.deinit(allocator),
            .Variable => |*var_expr| var_expr.deinit(allocator),
            .If => |*if_expr| if_expr.deinit(allocator),
            .FuncCall => |*func_call| func_call.deinit(allocator),
            // ... other cases
        }
    }

    pub fn clone(self: Expression, allocator: std.mem.Allocator) !Expression {
        return switch (self) {
            .Literal => |lit| .{ .Literal = try lit.clone(allocator) },
            .Variable => |var_expr| .{ .Variable = try var_expr.clone(allocator) },
            // ... other cases
        };
    }
};
```

## 15.3 Parsing Algorithm

```zig
// src/frontend/parser.zig
pub const Parser = struct {
    allocator: std.mem.Allocator,
    tokens: TokenStream,
    current: usize,

    pub fn parseExpression(self: *Parser) !Expression {
        const token = self.peek();
        return switch (token) {
            .left_paren => self.parseGene(),
            .left_bracket => self.parseArrayLiteral(),
            .left_brace => self.parseMapLiteral(),
            .quote => self.parseQuotedExpr(),
            .integer => |n| Expression{ .Literal = .{ .value = .{ .Int = n } } },
            .symbol => |s| self.parseSymbolOrPath(s),
            else => error.UnexpectedToken,
        };
    }

    fn parseGene(self: *Parser) !Expression {
        try self.expect(.left_paren);

        // Parse head expression
        const head = try self.allocator.create(Expression);
        errdefer self.allocator.destroy(head);
        head.* = try self.parseExpression();

        // Parse arguments
        var args = std.ArrayList(*Expression).init(self.allocator);
        errdefer {
            for (args.items) |arg| {
                arg.deinit(self.allocator);
                self.allocator.destroy(arg);
            }
            args.deinit();
        }

        while (!self.checkToken(.right_paren)) {
            const arg = try self.allocator.create(Expression);
            errdefer self.allocator.destroy(arg);
            arg.* = try self.parseExpression();
            try args.append(arg);
        }

        try self.expect(.right_paren);

        return Expression{
            .FuncCall = .{
                .func = head,
                .args = args,
            }
        };
    }

    fn peek(self: *Parser) Token {
        return self.tokens.items[self.current];
    }

    fn expect(self: *Parser, expected: Token) !void {
        const token = self.advance();
        if (!std.meta.eql(token, expected)) {
            return error.UnexpectedToken;
        }
    }
};
```

---

# Chapter 16: IR Pipeline (HIR, MIR, LIR)

## 16.1 Overview

Gene uses a three-tier IR pipeline for progressive lowering and optimization:

1. **HIR (High-level IR)**: Close to source, preserves semantic information
2. **MIR (Mid-level IR)**: SSA form, suitable for optimization
3. **LIR (Low-level IR)**: Register-based bytecode for VM execution

## 16.2 HIR (High-level Intermediate Representation)

HIR preserves high-level constructs while desugaring syntax:

```zig
pub const HIR = union(enum) {
    // Values
    Const: Value,
    Var: VarId,

    // Bindings
    Let: struct {
        var_id: VarId,
        type_hint: ?Type,
        init: *HIR,
        body: *HIR,
    },

    // Functions
    Function: struct {
        id: FnId,
        params: []FnParam,
        body: *HIR,
        captures: []VarId,
    },

    Call: struct {
        func: *HIR,
        args: []*HIR,
    },

    // Control flow
    If: struct {
        cond: *HIR,
        then_branch: *HIR,
        else_branch: *HIR,
    },

    Match: struct {
        expr: *HIR,
        arms: []MatchArm,
    },

    // Object-oriented
    Class: struct {
        id: ClassId,
        parent: ?ClassId,
        traits: []TraitId,
        fields: []Field,
        methods: []Method,
    },

    New: struct {
        class: ClassId,
        args: []*HIR,
    },

    FieldAccess: struct {
        obj: *HIR,
        field: FieldId,
    },

    MethodCall: struct {
        obj: *HIR,
        method: MethodId,
        args: []*HIR,
    },

    // Blocks
    Block: []*HIR,

    // Loops
    Loop: struct {
        body: *HIR,
    },

    Break: ?*HIR,
    Continue,

    pub const FnParam = struct {
        var_id: VarId,
        param_type: Type,
    };

    pub const MatchArm = struct {
        pattern: Pattern,
        body: *HIR,
    };
};
```

### HIR Generation Example

```gene
# Source
(fn add [x :Int y :Int => :Int]
  (+ x y))

# HIR
Function {
    id: FnId(1),
    params: [(VarId(1), Int), (VarId(2), Int)],
    body: Call {
        func: Var(BuiltinId(Add)),
        args: [Var(VarId(1)), Var(VarId(2))],
    },
    captures: [],
}
```

## 16.3 MIR (Mid-level Intermediate Representation)

MIR uses SSA (Static Single Assignment) form for optimization:

```zig
pub const MIRFunction = struct {
    id: FnId,
    params: []VarId,
    locals: []LocalVar,
    blocks: []BasicBlock,

    pub const LocalVar = struct {
        var_id: VarId,
        var_type: Type,
    };
};

pub const BasicBlock = struct {
    id: BlockId,
    instructions: []MIR,
    terminator: Terminator,
};

pub const MIR = union(enum) {
    // SSA operations
    Assign: struct {
        dest: VarId,
        value: Operand,
    },

    // Arithmetic
    BinOp: struct {
        dest: VarId,
        op: BinaryOperation,
        left: Operand,
        right: Operand,
    },

    UnOp: struct {
        dest: VarId,
        op: UnaryOperation,
        operand: Operand,
    },

    // Memory
    Load: struct {
        dest: VarId,
        addr: Operand,
    },

    Store: struct {
        addr: Operand,
        value: Operand,
    },

    // Objects
    AllocObject: struct {
        dest: VarId,
        class: ClassId,
    },

    GetField: struct {
        dest: VarId,
        obj: Operand,
        field: FieldId,
    },

    SetField: struct {
        obj: Operand,
        field: FieldId,
        value: Operand,
    },

    // Function calls
    Call: struct {
        dest: ?VarId,
        func: Operand,
        args: []Operand,
    },

    // Type operations
    Cast: struct {
        dest: VarId,
        value: Operand,
        to_type: Type,
    },

    TypeCheck: struct {
        dest: VarId,
        value: Operand,
        target_type: Type,
    },
};

pub const Terminator = union(enum) {
    Return: ?Operand,
    Branch: BlockId,
    CondBranch: struct {
        cond: Operand,
        then_block: BlockId,
        else_block: BlockId,
    },
    Switch: struct {
        value: Operand,
        targets: []SwitchTarget,
        default: BlockId,
    },

    pub const SwitchTarget = struct {
        value: Value,
        target: BlockId,
    };
};

pub const Operand = union(enum) {
    Const: Value,
    Var: VarId,
};
```

### MIR Optimization Passes

```zig
fn optimizeMir(allocator: std.mem.Allocator, func: *MIRFunction) !void {
    // Dead code elimination
    try deadCodeElimination(allocator, func);

    // Constant propagation
    try constantPropagation(allocator, func);

    // Common subexpression elimination
    try commonSubexpressionElimination(allocator, func);

    // Inline small functions
    try inlineFunctions(allocator, func);

    // Loop optimizations
    try loopInvariantCodeMotion(allocator, func);

    // Escape analysis for stack allocation
    try escapeAnalysis(allocator, func);
}
```

## 16.4 LIR (Low-level Intermediate Representation)

LIR is register-based bytecode for the VM:

```zig
pub const LIR = union(enum) {
    // Constants
    LoadConst: struct { dest: Reg, val: Value },
    LoadNil: struct { dest: Reg },

    // Arithmetic
    Add: struct { dest: Reg, left: Reg, right: Reg },
    Sub: struct { dest: Reg, left: Reg, right: Reg },
    Mul: struct { dest: Reg, left: Reg, right: Reg },
    Div: struct { dest: Reg, left: Reg, right: Reg },

    // Comparison
    Eq: struct { dest: Reg, left: Reg, right: Reg },
    Lt: struct { dest: Reg, left: Reg, right: Reg },
    Le: struct { dest: Reg, left: Reg, right: Reg },

    // Logical
    And: struct { dest: Reg, left: Reg, right: Reg },
    Or: struct { dest: Reg, left: Reg, right: Reg },
    Not: struct { dest: Reg, src: Reg },

    // Control flow
    Jump: struct { target: Label },
    JumpIf: struct { cond: Reg, target: Label },
    JumpIfNot: struct { cond: Reg, target: Label },

    // Function calls
    Call: struct { dest: Reg, func: Reg, args: []Reg },
    TailCall: struct { func: Reg, args: []Reg },
    Return: struct { value: ?Reg },

    // Object operations
    NewObject: struct { dest: Reg, class: ClassId },
    GetField: struct { dest: Reg, obj: Reg, field: FieldId },
    SetField: struct { obj: Reg, field: FieldId, value: Reg },

    // Method dispatch
    GetMethod: struct { dest: Reg, obj: Reg, method: MethodId },
    CallMethod: struct { dest: Reg, obj: Reg, method: MethodId, args: []Reg },

    // Array operations
    NewArray: struct { dest: Reg, size: Reg },
    GetElement: struct { dest: Reg, arr: Reg, idx: Reg },
    SetElement: struct { arr: Reg, idx: Reg, value: Reg },
    ArrayLen: struct { dest: Reg, arr: Reg },

    // Type operations
    TypeCheck: struct { dest: Reg, value: Reg, type_id: TypeId },
    Cast: struct { dest: Reg, value: Reg, type_id: TypeId },

    // Memory
    Move: struct { dest: Reg, src: Reg },

    // Inline cache support
    LoadIC: struct { dest: Reg, cache_id: u32 },
    StoreIC: struct { cache_id: u32, value: Reg },
};

pub const Reg = u16;  // Virtual register
pub const Label = u32; // Jump target
```

### Register Allocation

```zig
pub const RegisterAllocator = struct {
    // Infinite virtual registers during LIR generation
    next_vreg: u16,

    // Physical register assignment
    phys_regs: []PhysReg,

    // Spill slots for registers that don't fit
    spill_slots: []SpillSlot,

    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) RegisterAllocator {
        return .{
            .next_vreg = 0,
            .phys_regs = &.{},
            .spill_slots = &.{},
            .allocator = allocator,
        };
    }
};

fn allocateRegisters(allocator: std.mem.Allocator, func: *LIRFunction) !void {
    // Build interference graph
    const graph = try buildInterferenceGraph(allocator, func);
    defer graph.deinit();

    // Graph coloring algorithm
    const coloring = try graphColoring(allocator, graph, NUM_PHYSICAL_REGS);
    defer coloring.deinit();

    // Rewrite instructions with physical registers
    try rewriteWithAllocation(allocator, func, coloring);
}
```

---

# Chapter 17: Type Checker and Inference

## 17.1 Type System Architecture

```zig
pub const TypeChecker = struct {
    // Global type environment
    globals: TypeEnv,

    // Type variable generator
    next_tvar: u32,

    // Constraints for inference
    constraints: std.ArrayList(Constraint),

    // Type cache for expressions
    expr_types: std.HashMap(ExprId, Type),

    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) TypeChecker {
        return .{
            .globals = TypeEnv.init(allocator),
            .next_tvar = 0,
            .constraints = std.ArrayList(Constraint).init(allocator),
            .expr_types = std.HashMap(ExprId, Type).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *TypeChecker) void {
        self.globals.deinit();
        self.constraints.deinit();
        self.expr_types.deinit();
    }
};

pub const InferredType = union(enum) {
    // Primitives
    Any,
    Void,
    Nil,
    Bool,
    Int,
    Float,
    Char,
    Str,
    Symbol,

    // Compounds
    Array: *InferredType,
    Map: struct { key: *InferredType, value: *InferredType },
    Set: *InferredType,
    Tuple: []InferredType,

    // Functions
    Fn: struct {
        params: []InferredType,
        ret: *InferredType,
    },

    // Objects
    Class: ClassId,
    Trait: TraitId,

    // Type variables
    Var: TypeVar,

    // Generic types
    Generic: struct {
        base: *InferredType,
        args: []InferredType,
    },

    // Union types
    Union: []InferredType,

    // Intersection types
    Intersection: []InferredType,
};

pub const Constraint = union(enum) {
    Equal: struct { t1: InferredType, t2: InferredType },
    Subtype: struct { sub: InferredType, sup: InferredType },
    HasField: struct { type: InferredType, field: []const u8, field_type: InferredType },
    HasMethod: struct { type: InferredType, method: []const u8, method_type: InferredType },
    Implements: struct { type: InferredType, trait_id: TraitId },
};
```

## 17.2 Type Inference Algorithm

```zig
const TypeInferenceError = error{
    UndefinedVariable,
    TypeMismatch,
    OccursCheck,
    ArityMismatch,
    OutOfMemory,
};

const TypeChecker = struct {
    // ... previous fields ...

    fn inferExpr(self: *TypeChecker, expr: *const HIR) TypeInferenceError!InferredType {
        return switch (expr.*) {
            .Const => |val| self.typeOfValue(val),

            .Var => |id| self.lookupVar(id),

            .Let => |let_expr| {
                const init_type = try self.inferExpr(let_expr.init);
                try self.bindVar(let_expr.var_id, init_type);
                return self.inferExpr(let_expr.body);
            },

            .Function => |func| {
                // Create type variables for parameters
                var param_types = try self.allocator.alloc(InferredType, func.params.len);
                for (func.params, 0..) |param, i| {
                    param_types[i] = param.param_type orelse try self.freshTypeVar();
                }

                // Infer body type
                const ret_type_ptr = try self.allocator.create(InferredType);
                ret_type_ptr.* = try self.inferExpr(func.body);

                return InferredType{ .Fn = .{
                    .params = param_types,
                    .ret = ret_type_ptr,
                } };
            },

            .Call => |call| {
                const func_type = try self.inferExpr(call.func);
                var arg_types = try self.allocator.alloc(InferredType, call.args.len);
                for (call.args, 0..) |arg, i| {
                    arg_types[i] = try self.inferExpr(arg);
                }

                // Generate constraints
                const ret_type = try self.freshTypeVar();
                const ret_type_ptr = try self.allocator.create(InferredType);
                ret_type_ptr.* = ret_type;

                try self.addConstraint(Constraint{ .Equal = .{
                    .t1 = func_type,
                    .t2 = InferredType{ .Fn = .{
                        .params = arg_types,
                        .ret = ret_type_ptr,
                    } },
                } });

                return ret_type;
            },

            .If => |if_expr| {
                const cond_type = try self.inferExpr(if_expr.cond);
                try self.addConstraint(Constraint{ .Equal = .{
                    .t1 = cond_type,
                    .t2 = InferredType.Bool,
                } });

                const then_type = try self.inferExpr(if_expr.then_branch);
                const else_type = try self.inferExpr(if_expr.else_branch);

                // Unify branches
                _ = try self.unify(then_type, else_type);
                return then_type;
            },

            // ... other cases
            else => return TypeInferenceError.TypeMismatch,
        };
    }

    fn solveConstraints(self: *TypeChecker) TypeInferenceError!Substitution {
        var subst = Substitution.init(self.allocator);

        while (self.constraints.popOrNull()) |constraint| {
            switch (constraint) {
                .Equal => |eq| {
                    const s = try self.unify(eq.t1, eq.t2);
                    subst = try subst.compose(s);
                },

                .Subtype => |sub| {
                    try self.checkSubtype(sub.sub, sub.sup);
                },

                // ... other constraints
                else => {},
            }
        }

        return subst;
    }

    fn unify(self: *TypeChecker, t1: InferredType, t2: InferredType) TypeInferenceError!Substitution {
        return switch (t1) {
            .Var => |v1| switch (t2) {
                .Var => |v2| if (v1 == v2)
                    Substitution.init(self.allocator)
                else
                    Substitution.singleton(self.allocator, v1, t2),
                else => {
                    if (try t2.containsVar(v1)) {
                        return TypeInferenceError.OccursCheck;
                    }
                    return Substitution.singleton(self.allocator, v1, t2);
                },
            },
            .Array => |arr_t1| switch (t2) {
                .Array => |arr_t2| self.unify(arr_t1.*, arr_t2.*),
                else => TypeInferenceError.TypeMismatch,
            },
            .Fn => |fn1| switch (t2) {
                .Fn => |fn2| {
                    if (fn1.params.len != fn2.params.len) {
                        return TypeInferenceError.ArityMismatch;
                    }

                    var subst = Substitution.init(self.allocator);
                    for (fn1.params, fn2.params) |p1, p2| {
                        const s = try self.unify(p1, p2);
                        subst = try subst.compose(s);
                    }

                    const s = try self.unify(fn1.ret.*, fn2.ret.*);
                    return subst.compose(s);
                },
                else => TypeInferenceError.TypeMismatch,
            },
            else => if (std.meta.eql(t1, t2))
                Substitution.init(self.allocator)
            else
                TypeInferenceError.TypeMismatch,
        };
    }
};
```

## 17.3 Generic Type Instantiation

```zig
pub const GenericContext = struct {
    // Type parameters in scope
    type_params: std.HashMap([]const u8, TypeParam),

    // Instantiation cache
    cache: std.HashMap(InstantiationKey, InferredType),

    allocator: std.mem.Allocator,

    pub const InstantiationKey = struct {
        type_id: TypeId,
        args: []InferredType,
    };

    pub fn init(allocator: std.mem.Allocator) GenericContext {
        return .{
            .type_params = std.HashMap([]const u8, TypeParam).init(allocator),
            .cache = std.HashMap(InstantiationKey, InferredType).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *GenericContext) void {
        self.type_params.deinit();
        self.cache.deinit();
    }
};

const TypeChecker = struct {
    // ... previous fields ...

    fn instantiateGeneric(self: *TypeChecker,
                         base: InferredType,
                         args: []InferredType) TypeInferenceError!InferredType {
        return switch (base) {
            .Generic => |generic| {
                const params_len = generic.args.len;
                if (params_len != args.len) {
                    return TypeInferenceError.ArityMismatch;
                }

                // Build substitution
                var subst = Substitution.init(self.allocator);
                for (generic.args, args) |param, arg| {
                    // In a real implementation, params would be type variables
                    // subst = subst.extend(param, arg);
                }

                // Apply substitution
                return try generic.base.applySubst(&subst);
            },
            else => TypeInferenceError.TypeMismatch,
        };
    }

    fn checkTraitImpl(self: *TypeChecker,
                     class: ClassId,
                     trait_id: TraitId) TypeInferenceError!void {
        const trait_def = try self.getTrait(trait_id);
        const class_def = try self.getClass(class);

        // Check all required methods
        for (trait_def.methods) |method| {
            const impl_method = class_def.findMethod(method.name) orelse {
                return TypeInferenceError.TypeMismatch; // Missing method
            };

            // Check method signature compatibility
            try self.checkMethodCompat(method, impl_method);
        }
    }
};
```

## 17.4 Gradual Typing

```zig
const TypeChecker = struct {
    // ... previous fields ...

    fn checkGradual(self: *TypeChecker, static_type: InferredType, dynamic_type: InferredType) TypeInferenceError!void {
        switch (static_type) {
            .Any => return, // Any is compatible with everything
            else => switch (dynamic_type) {
                .Any => return, // Any is compatible with everything
                else => {},
            },
        }

        // Same types are compatible
        if (std.meta.eql(static_type, dynamic_type)) {
            return;
        }

        switch (static_type) {
            .Float => switch (dynamic_type) {
                .Int => return, // Numeric coercion
                else => {},
            },
            .Array => |arr1| switch (dynamic_type) {
                .Array => |arr2| return self.checkGradual(arr1.*, arr2.*), // Array covariance
                else => {},
            },
            .Fn => |fn1| switch (dynamic_type) {
                .Fn => |fn2| {
                    // Contravariant in parameters
                    if (fn1.params.len != fn2.params.len) {
                        return TypeInferenceError.ArityMismatch;
                    }
                    for (fn1.params, fn2.params) |p1, p2| {
                        try self.checkGradual(p2, p1); // Note: reversed order for contravariance
                    }
                    // Covariant in return
                    return self.checkGradual(fn1.ret.*, fn2.ret.*);
                },
                else => {},
            },
            else => {},
        }

        return TypeInferenceError.TypeMismatch;
    }

    fn insertRuntimeChecks(self: *TypeChecker, allocator: std.mem.Allocator, expr: HIR) !HIR {
        return switch (expr) {
            .Call => |call| {
                if (try self.needsCheck(call.func)) {
                    // Insert runtime type check
                    const checked_func = try allocator.create(HIR);
                    checked_func.* = HIR{ .RuntimeCheck = .{
                        .expr = call.func,
                        .expected_type = try self.getExpectedType(),
                    } };

                    return HIR{ .Call = .{
                        .func = checked_func,
                        .args = call.args,
                    } };
                } else {
                    return expr;
                }
            },
            else => expr,
        };
    }
};
```

---

# Chapter 18: Register-Based Virtual Machine

## 18.1 VM Architecture

```zig
pub const VM = struct {
    // Registers
    registers: std.ArrayList(Value),

    // Call stack
    frames: std.ArrayList(CallFrame),

    // Operand stack (for complex operations)
    stack: std.ArrayList(Value),

    // Heap
    heap: Heap,

    // Global variables
    globals: std.HashMap(GlobalId, Value),

    // Loaded modules
    modules: std.HashMap(ModuleId, Module),

    // Inline caches
    inline_caches: std.ArrayList(InlineCache),

    // Runtime type info
    types: TypeRegistry,

    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) VM {
        return .{
            .registers = std.ArrayList(Value).init(allocator),
            .frames = std.ArrayList(CallFrame).init(allocator),
            .stack = std.ArrayList(Value).init(allocator),
            .heap = Heap.init(allocator),
            .globals = std.HashMap(GlobalId, Value).init(allocator),
            .modules = std.HashMap(ModuleId, Module).init(allocator),
            .inline_caches = std.ArrayList(InlineCache).init(allocator),
            .types = TypeRegistry.init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *VM) void {
        self.registers.deinit();
        self.frames.deinit();
        self.stack.deinit();
        self.heap.deinit();
        self.globals.deinit();
        self.modules.deinit();
        self.inline_caches.deinit();
        self.types.deinit();
    }
};

pub const CallFrame = struct {
    // Function being executed
    function: FunctionId,

    // Instruction pointer
    ip: usize,

    // Base pointer for register window
    bp: usize,

    // Return address
    return_ip: usize,

    // Number of registers
    register_count: usize,
};

pub const InlineCache = struct {
    // Cached type
    cached_type: ?TypeId,

    // Cached method/field offset
    cached_offset: ?usize,

    // Hit count for JIT decisions
    hit_count: u32,
};
```

## 18.2 Instruction Execution

```zig
const RuntimeError = error{
    NotCallable,
    TypeError,
    OutOfMemory,
    StackOverflow,
    UndefinedFunction,
};

const VM = struct {
    // ... previous fields ...

    fn execute(self: *VM) RuntimeError!Value {
        while (true) {
            const frame = try self.currentFrame();
            const instruction = try self.fetchInstruction();

            switch (instruction) {
                .LoadConst => |load| {
                    try self.setRegister(load.dest, load.val);
                },

                .Add => |add| {
                    const l = try self.getRegister(add.left);
                    const r = try self.getRegister(add.right);
                    const result = try self.addValues(l, r);
                    try self.setRegister(add.dest, result);
                },

                .Call => |call| {
                    const func_val = try self.getRegister(call.func);
                    var arg_vals = try self.allocator.alloc(Value, call.args.len);
                    defer self.allocator.free(arg_vals);

                    for (call.args, 0..) |reg, i| {
                        arg_vals[i] = try self.getRegister(reg);
                    }

                    const result = try self.callFunction(func_val, arg_vals);
                    try self.setRegister(call.dest, result);
                },

                .JumpIf => |jump| {
                    const cond_val = try self.getRegister(jump.cond);
                    if (self.isTruthy(cond_val)) {
                        var current_frame = &self.frames.items[self.frames.items.len - 1];
                        current_frame.ip = jump.target;
                        continue;
                    }
                },

                .GetField => |get_field| {
                    const obj_val = try self.getRegister(get_field.obj);
                    const result = try self.getFieldCached(obj_val, get_field.field);
                    try self.setRegister(get_field.dest, result);
                },

                .Return => |ret| {
                    const ret_val = if (ret.value) |reg|
                        try self.getRegister(reg)
                    else
                        Value.Nil;

                    if (self.frames.items.len == 1) {
                        return ret_val;
                    }

                    try self.popFrame();
                    // Set return value in caller's register
                },

                // ... other instructions
                else => {},
            }

            var current_frame = &self.frames.items[self.frames.items.len - 1];
            current_frame.ip += 1;
        }
    }

    fn callFunction(self: *VM, func: Value, args: []Value) RuntimeError!Value {
        return switch (func) {
            .Function => |id| {
                const func_def = try self.getFunction(id);

                // Create new frame
                const frame = CallFrame{
                    .function = id,
                    .ip = 0,
                    .bp = self.registers.items.len,
                    .return_ip = self.currentFrame().ip,
                    .register_count = func_def.register_count,
                };

                // Allocate registers for new frame
                try self.registers.resize(
                    self.registers.items.len + func_def.register_count
                );

                // Initialize new registers to Nil
                for (self.registers.items[frame.bp..]) |*reg| {
                    reg.* = Value.Nil;
                }

                // Copy arguments to registers
                for (args, 0..) |arg, i| {
                    self.registers.items[frame.bp + i] = arg;
                }

                try self.frames.append(frame);
                return Value.Nil; // Execution continues
            },

            .NativeFunction => |native_fn| {
                // Call native function directly
                return try native_fn.call(self, args);
            },

            else => RuntimeError.NotCallable,
        };
    }
};
```

## 18.3 Inline Caching

```zig
const JIT_THRESHOLD: u32 = 100;

const VM = struct {
    // ... previous fields ...

    fn getFieldCached(self: *VM, obj: Value, field: FieldId) RuntimeError!Value {
        const cache_id = try self.currentCacheId();
        var cache = &self.inline_caches.items[cache_id];

        const obj_type = obj.getTypeId();

        // Check cache hit
        if (cache.cached_type) |cached_type| {
            if (cached_type == obj_type) {
                if (cache.cached_offset) |offset| {
                    cache.hit_count += 1;
                    return self.getFieldAtOffset(obj, offset);
                }
            }
        }

        // Cache miss - look up field
        const offset = try self.lookupFieldOffset(obj_type, field);

        // Update cache
        cache.cached_type = obj_type;
        cache.cached_offset = offset;
        cache.hit_count = 1;

        // Check if hot enough for JIT
        if (cache.hit_count > JIT_THRESHOLD) {
            const current_frame = try self.currentFrame();
            try self.markForJit(current_frame.function);
        }

        return self.getFieldAtOffset(obj, offset);
    }

    fn getFieldAtOffset(self: *VM, obj: Value, offset: usize) Value {
        return switch (obj) {
            .Object => |ptr| {
                const obj_data = self.heap.get(ptr);
                return obj_data.fields[offset];
            },
            else => Value.Nil,
        };
    }
};
```

## 18.4 Value Representation

```zig
pub const ValueData = extern union {
    float: f64,
    integer: i64,
    pointer: *HeapObject,
    immediate: u64,
};

pub const Value = struct {
    data: ValueData,

    // NaN-boxing tags
    const NIL_TAG: u64 = 0xFFF8_0000_0000_0000;
    const TRUE_TAG: u64 = 0xFFF8_0000_0000_0001;
    const FALSE_TAG: u64 = 0xFFF8_0000_0000_0002;
    const INT_TAG: u64 = 0xFFF9_0000_0000_0000;
    const CHAR_TAG: u64 = 0xFFFA_0000_0000_0000;
    const SMALL_STR_TAG: u64 = 0xFFFB_0000_0000_0000;
    const PTR_TAG: u64 = 0xFFFC_0000_0000_0000;

    pub const Nil = Value{ .data = .{ .immediate = NIL_TAG } };

    pub fn newBool(b: bool) Value {
        return Value{
            .data = .{ .immediate = if (b) TRUE_TAG else FALSE_TAG },
        };
    }

    pub fn newInt(i: i64) Value {
        if (i >= -(1 << 47) and i < (1 << 47)) {
            // Fits in 48 bits
            return Value{
                .data = .{ .immediate = INT_TAG | (@as(u64, @bitCast(i)) & 0x0000_FFFF_FFFF_FFFF) },
            };
        } else {
            // Allocate BigInt
            return newObject(HeapObject{ .BigInt = i });
        }
    }

    pub fn newFloat(f: f64) Value {
        return Value{
            .data = .{ .float = f },
        };
    }

    pub fn newObject(allocator: std.mem.Allocator, obj: HeapObject) !Value {
        const ptr = try allocator.create(HeapObject);
        ptr.* = obj;
        return Value{
            .data = .{ .immediate = PTR_TAG | (@intFromPtr(ptr) & 0x0000_FFFF_FFFF_FFFF) },
        };
    }

    pub fn getType(self: Value) ValueType {
        const bits = self.data.immediate;

        // Check for float (NaN boxing)
        if ((bits & 0xFFF8_0000_0000_0000) != 0xFFF8_0000_0000_0000) {
            return ValueType.Float;
        }

        return switch (bits & 0xFFFF_0000_0000_0000) {
            NIL_TAG => ValueType.Nil,
            TRUE_TAG, FALSE_TAG => ValueType.Bool,
            INT_TAG => ValueType.Int,
            CHAR_TAG => ValueType.Char,
            SMALL_STR_TAG => ValueType.SmallStr,
            PTR_TAG => {
                const ptr: *HeapObject = @ptrFromInt(bits & 0x0000_FFFF_FFFF_FFFF);
                return ptr.getType();
            },
            else => unreachable,
        };
    }

    pub fn deinit(self: Value, allocator: std.mem.Allocator) void {
        if (self.getType() == .Object) {
            const bits = self.data.immediate;
            if ((bits & 0xFFFF_0000_0000_0000) == PTR_TAG) {
                const ptr: *HeapObject = @ptrFromInt(bits & 0x0000_FFFF_FFFF_FFFF);
                ptr.deinit(allocator);
                allocator.destroy(ptr);
            }
        }
    }
};

pub const ValueType = enum {
    Nil,
    Bool,
    Int,
    Float,
    Char,
    SmallStr,
    Object,
};

pub const HeapObject = union(enum) {
    String: []const u8,
    Array: []Value,
    Map: std.HashMap(Value, Value),
    BigInt: i64,
    Class: ClassObject,
    Instance: InstanceObject,

    pub fn getType(self: HeapObject) ValueType {
        return switch (self) {
            .String => ValueType.Object,
            .Array => ValueType.Object,
            .Map => ValueType.Object,
            .BigInt => ValueType.Int,
            .Class => ValueType.Object,
            .Instance => ValueType.Object,
        };
    }

    pub fn deinit(self: *HeapObject, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .String => |str| allocator.free(str),
            .Array => |arr| allocator.free(arr),
            .Map => |*map| map.deinit(),
            .BigInt => {},
            .Class => |*class| class.deinit(allocator),
            .Instance => |*instance| instance.deinit(allocator),
        }
    }
};
```

---

# Chapter 19: Garbage Collector

## 19.1 GC Design Overview

Gene's garbage collector uses a hybrid approach optimized for both throughput and latency:

### Core Components:
1. **Incremental Mark-and-Sweep**: Spreads GC work across multiple small pauses
2. **Generational Collection**: Separates young (nursery) and old objects
3. **Write Barriers**: Track cross-generational references
4. **Concurrent Marking**: Mark phase can run concurrently with mutator threads

### Memory Layout:
- **Young Generation**: Small (2-8MB), collected frequently
- **Old Generation**: Large, collected less frequently
- **Large Object Space**: Direct allocation for objects > 8KB
- **Permanent Generation**: For type metadata, compiled code

## 19.2 Collection Algorithms

### Minor Collection (Young Generation):
1. Stop-the-world pause
2. Evacuate live objects to old generation
3. Reset young generation allocation pointer
4. Update remembered sets

### Major Collection (Full GC):
1. **Concurrent Mark Phase**:
   - Mark from roots (stack, globals, registers)
   - Process write barrier buffer
   - Mark concurrently while mutator runs

2. **Stop-the-world Remark**:
   - Brief pause to finalize marking
   - Process any remaining gray objects

3. **Concurrent Sweep**:
   - Sweep dead objects back to free lists
   - Compact if fragmentation exceeds threshold

## 19.3 Object Header Design

Each heap object needs:
- Type ID (16 bits)
- GC flags (8 bits): marked, forwarded, has_finalizer, pinned
- Age (8 bits): for generational promotion
- Size (32 bits): object size in bytes
- Hash code (32 bits): cached identity hash

## 19.4 Write Barriers

Gene uses a snapshot-at-the-beginning (SATB) write barrier:
- Logs all pointer updates during concurrent marking
- Ensures no live objects are missed
- Implemented as inline code in mutator

## 19.5 Finalization and Weak References

- **Finalizers**: Objects with finalizers are tracked separately
- **Weak References**: Cleared during GC if referent is unreachable
- **Phantom References**: For post-mortem cleanup
- **Soft References**: Cleared based on memory pressure

---

# Chapter 20: JIT Compiler

## 20.1 JIT Architecture

Gene uses a tiered compilation strategy:

### Execution Tiers:
1. **Interpreter**: Initial execution, profiling
2. **Baseline JIT**: Quick compilation, minimal optimization
3. **Optimizing JIT**: Full optimization for hot code

### Profiling Information:
- Call counts per function
- Type feedback at call sites
- Branch frequencies
- Inline cache hit rates

## 20.2 Compilation Pipeline

### Baseline JIT:
1. Direct translation of LIR to machine code
2. Generic inline caches
3. Minimal register allocation
4. Quick compilation (< 1ms per function)

### Optimizing JIT:
1. Build SSA form from LIR
2. Type specialization based on profiling
3. Inlining decisions
4. Advanced optimizations:
   - Loop unrolling
   - Escape analysis
   - Vectorization
   - Dead code elimination
5. Register allocation with graph coloring
6. Machine code generation

## 20.3 Deoptimization

When assumptions fail:
1. Guard failures trigger deoptimization
2. Stack frames are reconstructed for interpreter
3. Execution continues in interpreter
4. Recompilation with updated profile

### Deoptimization Points:
- Type guards
- Class hierarchy changes
- Inline cache misses
- Arithmetic overflow
- Null checks

## 20.4 Inline Caching Strategy

### Monomorphic Inline Caches:
- Single type cached
- Direct call/field access
- Fall back on miss

### Polymorphic Inline Caches:
- Up to 4 types cached
- Linear search dispatch
- Reorder by frequency

### Megamorphic Sites:
- Too many types seen
- Use vtable dispatch
- May prevent inlining

---

# Chapter 21: Runtime System

## 21.1 Runtime Architecture

### Core Components:
- **Object Model**: Manages classes, traits, methods
- **Module Loader**: Loads and links Gene modules
- **Native Interface**: FFI for C/Zig interop
- **Scheduler**: Manages actors and green threads
- **Memory Manager**: Coordinates with GC
- **Exception Handler**: Stack unwinding, error propagation

## 21.2 Object Model

### Class Representation:
- Class metadata object
- Method table (vtable)
- Field layout descriptor
- Trait implementations
- Static fields storage

### Method Dispatch:
1. **Static Dispatch**: Known at compile time
2. **Virtual Dispatch**: Through vtable
3. **Interface Dispatch**: Trait method lookup
4. **Dynamic Dispatch**: Runtime method resolution

### Field Access:
- Fixed offsets for instance fields
- Hash table for expando properties
- Inline caching for fast access

## 21.3 Module System

### Module Loading Process:
1. Parse module file
2. Verify bytecode
3. Link dependencies
4. Initialize static fields
5. Execute module initializer

### Module Isolation:
- Each module has its own namespace
- Explicit exports/imports
- Lazy loading supported
- Hot reloading capability

## 21.4 Native Interface (FFI)

### Calling C/Zig Functions:
- Type marshalling
- Calling convention adaptation
- GC safe points
- Error handling

### Embedding Gene:
- C API for host applications
- Callback registration
- Memory management coordination
- Thread safety

## 21.5 Actor Runtime

### Actor Scheduling:
- Work-stealing scheduler
- CPU core affinity
- Priority queues
- Fair scheduling

### Message Passing:
- Lock-free queues
- Batched processing
- Flow control
- Dead letter handling

---

# Chapter 22: Tooling and Development Environment

## 22.1 REPL (Read-Eval-Print Loop)

### Features:
- Multi-line editing
- Syntax highlighting
- Auto-completion
- History search
- Pretty printing
- Time travel debugging

### Implementation:
- Incremental parsing
- Preservation of REPL state
- Hot code reload
- Inspection commands

## 22.2 Language Server Protocol (LSP)

### Capabilities:
- Syntax checking
- Type checking
- Auto-completion
- Go to definition
- Find references
- Rename refactoring
- Code formatting
- Documentation hover

### Architecture:
- Incremental analysis
- Persistent index
- Background compilation
- Workspace symbols

## 22.3 Debugger

### Features:
- Breakpoints
- Step execution
- Variable inspection
- Expression evaluation
- Stack traces
- Thread debugging
- Time travel debugging

### Implementation:
- Debug information in bytecode
- Safe points for debugging
- Remote debugging protocol
- Integration with IDEs

## 22.4 Profiler

### Profiling Modes:
- CPU profiling
- Memory profiling
- Allocation tracking
- Lock contention
- Actor message flow

### Output Formats:
- Flame graphs
- Call trees
- Hot spot analysis
- Memory heap dumps
- Timeline views

## 22.5 Package Manager

### Features:
- Dependency resolution
- Version constraints
- Local/global packages
- Build system integration
- Documentation generation

### Package Format:
- Manifest file (gene.toml)
- Source distribution
- Binary distribution
- Cross-compilation support

## 22.6 Build System

### Compilation Pipeline:
1. Dependency analysis
2. Parallel compilation
3. Incremental builds
4. Cross-compilation
5. Optimization levels

### Output Targets:
- Standalone executables
- Shared libraries
- WASM modules
- Embedded bytecode

---

# Appendix A: Standard Library Overview

## Core Modules:

### gene/core:
- Basic types and operations
- Type conversion functions
- Comparison and equality
- Basic I/O

### gene/collections:
- Array, Map, Set implementations
- Persistent data structures
- Concurrent collections
- Specialized collections (PriorityQueue, etc.)

### gene/string:
- String manipulation
- Regular expressions
- Text encoding/decoding
- String formatting

### gene/math:
- Numeric operations
- Trigonometry
- Random numbers
- Big integers/decimals

### gene/io:
- File I/O
- Network I/O
- Streams and buffers
- Serialization

### gene/concurrent:
- Actors
- Channels
- Futures/Promises
- STM primitives

### gene/time:
- Date/time handling
- Timers
- Time zones
- Duration arithmetic

### gene/system:
- Process management
- Environment variables
- File system operations
- OS interfaces

---

# Appendix B: Language Grammar (EBNF)

```ebnf
program = toplevel*

toplevel = namespace
         | import
         | definition
         | expression

namespace = '(' 'ns' symbol namespace-opts ')'

import = '(' 'import' symbol import-spec? ')'

definition = function
           | variable
           | class
           | trait
           | macro
           | type-alias

expression = atom
           | gene
           | array
           | map
           | set
           | quote
           | unquote

atom = nil | boolean | number | character | string | symbol | keyword

gene = '(' expression property* expression* ')'

property = '^' property-path expression?

array = '[' expression* ']'

map = '{' property* '}'

set = '#' '[' expression* ']'

quote = ':' expression

unquote = '%' expression

# ... rest of grammar
```

