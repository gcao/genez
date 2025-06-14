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
13. Foreign Function Interface (FFI)
14. Standard Library

**Part II: Reference Implementation**
15. Architecture Overview
16. Lexer and Parser
17. IR Pipeline (HIR, MIR, LIR)
18. Type Checker and Inference
19. Register-Based Virtual Machine
20. Garbage Collector
21. JIT Compiler
22. Runtime System
23. Tooling and Development Environment

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

In Gene's unified design, **functions and methods are the same thing**. Methods are simply functions where the first parameter is conventionally named `self`. This unification provides consistency and flexibility.

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

# Method syntax - just sugar for defining a function in a class
# These are equivalent:
(.fn distance [other :Point => :Float] ...)  # Method syntax
(fn distance [self :Point other :Point => :Float] ...)  # Function syntax

# Macro syntax - for lazy evaluation
(macro unless [condition body] ...)  # Standalone macro
(.macro validate [rule] ...)         # Method macro with implicit self

# The dot prefix is syntactic sugar that:
# 1. Adds implicit 'self' as first parameter
# 2. Binds the function/macro to the class namespace
# 3. Works for both .fn (functions) and .macro (macros)
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

Gene features a unified, class-based type system where **everything is an object** and **every type is a class**. This provides:

- **Uniform object model**: All values are instances of classes
- **Metaclass hierarchy**: Classes themselves are objects (instances of `Class`)
- **Dynamic typing by default**: Type checking at runtime
- **Optional type annotations**: Static type checking when desired
- **Type inference**: Automatic type deduction
- **Generic programming**: Parameterized classes and methods
- **Type constraints**: Interface-based constraints via traits

Key principles:
1. Every value has a class (accessible via `.class`)
2. Every class inherits from `Any` (except `Any` itself)
3. `Class` is the metaclass - the class of all classes
4. Type annotations use class names (`:Int`, `:Str`, etc.)
5. Type checking uses `is` operator or `.is?` method

## 3.2 Type Annotations

Type annotations in Gene use class names. Since all types are classes, type annotations are simply references to class objects:

```gene
# Variable types - referring to classes
(var x :Int)                # x must be instance of Int class
(var y :Str = "hello")      # y must be instance of Str class

# Function types - class constraints on parameters
(fn add [x :Int y :Int => :Int]
  (+ x y))
(fn print [s :Str]          # Returns Void by default
  (io/write s))

# Complex types - parameterized classes
(var nums :(Array Int))     # Array class parameterized with Int
(var map :(Map Str Int))    # Map class with Str keys, Int values
(var fn-var :(Fn Int Int => Int))  # Function class

# Type annotations are first-class
(var type-var = Int)        # Store class in variable
(var x :type-var = 42)      # Use variable as type annotation
```

## 3.3 Built-in Types as Classes

In Gene, **everything is an object** and **every type is a class**. The type system is fully unified under a class hierarchy where even primitive types like `Int` and `Float` are classes. This provides consistency and allows all values to share common behavior.

```gene
# Unified Class Hierarchy
Any                    # Root class - all classes inherit from Any
├── Class             # The class of all classes (including itself)
├── Void              # Class for no value (function returns)
├── Nil               # Class for the nil value
├── Bool              # Class for boolean values
├── Number            # Abstract numeric class
│   ├── Int           # Class for 48-bit integers (NaN-boxed)
│   ├── Float         # Class for 64-bit IEEE 754 floats
│   ├── Int8          # Class for 8-bit signed integers
│   ├── UInt8         # Class for 8-bit unsigned integers
│   ├── Int16         # Class for 16-bit signed integers
│   ├── UInt16        # Class for 16-bit unsigned integers
│   ├── Int32         # Class for 32-bit signed integers
│   ├── UInt32        # Class for 32-bit unsigned integers
│   ├── Int64         # Class for 64-bit signed integers
│   ├── UInt64        # Class for 64-bit unsigned integers
│   └── Float32       # Class for 32-bit floats
├── Char              # Class for Unicode characters
├── Str               # Class for UTF-8 strings
├── Symbol            # Class for interned symbols
├── ComplexSymbol     # Class for namespace-qualified symbols
├── Collection        # Abstract collection class
│   ├── Array         # Class for indexed sequences
│   ├── Map           # Class for key-value mappings
│   └── Set           # Class for unique value collections
├── Gene              # Class for generic containers
├── Fn                # Class for functions
├── Trait             # Class for traits/interfaces
├── Module            # Class for modules/namespaces
├── Ref               # Class for mutable references
├── Atom              # Class for atomic references
├── Chan              # Class for channels (concurrency)
├── Promise           # Class for future values
├── Pointer           # Abstract pointer class (FFI)
│   ├── CPtr          # Class for C pointers
│   ├── CArray        # Class for C arrays
│   ├── CStruct       # Class for C structs
│   ├── CUnion        # Class for C unions
│   └── CFn           # Class for C function pointers
└── ...               # User-defined classes
```

### Key Principles:

1. **Everything is an Object**: Every value in Gene is an instance of a class
2. **Every Type is a Class**: `Int`, `Float`, `Str`, etc. are all classes
3. **Class is a Class**: The `Class` type is itself a class (instance of `Class`)
4. **Uniform Method Dispatch**: All values support method calls
5. **Shared Behavior**: Common operations defined in parent classes

### 3.3.1 Class-Based Type System

All types in Gene are classes. Even primitive values like integers and booleans are instances of their respective classes. This unified approach means:

```gene
# The Class class itself
Class                  # The metaclass - class of all classes
(Class .class)         # Returns Class (it's its own class)
(Class .superclass)    # Returns Any

# Any - root of the class hierarchy
Any                    # Base class for everything
(Any .class)           # Returns Class
(Any .superclass)      # Returns nil (no parent)

# Every value has a class
(42 .class)            # Returns Int
(3.14 .class)          # Returns Float
("hello" .class)       # Returns Str
(Int .class)           # Returns Class (Int is a class)

# Type checking with classes
(42 is Int)            # true - 42 is instance of Int
(42 is Number)         # true - Int inherits from Number
(42 is Any)            # true - everything is Any
(Int is Class)         # true - Int is a class
(Int is Any)           # true - Class inherits from Any

# Class relationships
(Int .superclass)      # Returns Number
(Number .superclass)   # Returns Any
(Int .ancestors)       # Returns [Int Number Any]

# Void - class for no value
(class Void extends Any)  # Built-in class
(fn print [s :Str => :Void]
  (io/write s))

# Nil - class for null value
(class Nil extends Any)    # Built-in singleton class
(nil .class)               # Returns Nil
(nil is Nil)               # true
(nil is Any)               # true

# Bool - class for booleans
(class Bool extends Any)   # Built-in class
(true .class)              # Returns Bool
(false .class)             # Returns Bool
(true is Bool)             # true

# Number - abstract numeric class
(abstract class Number extends Any
  (.fn + [other :Number => :Number])  # Abstract
  (.fn - [other :Number => :Number])  # Abstract
  (.fn * [other :Number => :Number])  # Abstract
  (.fn / [other :Number => :Number])  # Abstract
  (.fn abs [] => :Number)              # Concrete
  (.fn to_str [] => :Str))             # Concrete

# Int - integer class
(class Int extends Number
  # Class constants
  (.const MAX 140737488355327)   # 48-bit max
  (.const MIN -140737488355328)  # 48-bit min

  # Instance methods (inherited + specific)
  (.fn + [other :Number => :Number] ...)
  (.fn mod [other :Int => :Int] ...)
  (.fn bit_and [other :Int => :Int] ...))

# Float - floating point class
(class Float extends Number
  (.const INFINITY ...)
  (.const NAN ...)
  (.const EPSILON ...)

  (.fn + [other :Number => :Number] ...)
  (.fn sqrt [] => :Float] ...)
  (.fn floor [] => :Int] ...))

# Char - character class
(class Char extends Any
  (.fn code [] => :Int)        # Get Unicode codepoint
  (.fn to_str [] => :Str)      # Convert to string
  (.fn is_digit? [] => :Bool)  # Check if digit
  (.fn is_alpha? [] => :Bool)) # Check if alphabetic

('A' .class)           # Returns Char
('A' .code)            # Returns 65
('😊' is Char)         # true

# Str - string class
(class Str extends Any implements Comparable
  (.fn length [] => :Int)
  (.fn chars [] => :(Array Char))
  (.fn + [other :Str => :Str])       # Concatenation
  (.fn substr [start :Int len :Int => :Str])
  (.fn compare [other :Str => :Int]))  # For Comparable

("hello" .class)       # Returns Str
("hello" .length)      # Returns 5
("hello" is Comparable) # true (Str implements Comparable)

# Symbol - symbol class
(class Symbol extends Any
  (.fn to_str [] => :Str)
  (.fn namespace [] => :(Optional Str)))

('foo .class)          # Returns Symbol
(foo is Symbol)        # true (when foo is unquoted symbol)

# Collection - abstract collection class
(abstract class Collection extends Any
  (.fn length [] => :Int)
  (.fn empty? [] => :Bool]
    (== (.length) 0))
  (.fn contains? [item :Any => :Bool]))

# Array - array class
(class Array extends Collection
  (.fn get [index :Int => :Any])
  (.fn set [index :Int value :Any => :Void])
  (.fn push [item :Any => :Void])
  (.fn map [f :Fn => :Array]))

([1 2 3] .class)       # Returns Array
([1 2 3] is Collection) # true
([1 2 3] .length)      # Returns 3

# Map - map class
(class Map extends Collection
  (.fn get [key :Any => :Any])
  (.fn set [key :Any value :Any => :Void])
  (.fn keys [] => :Array)
  (.fn values [] => :Array))

({^a 1} .class)        # Returns Map
({^a 1} is Collection) # true

# Gene - the Gene expression class
(class Gene extends Any
  (.fn head [] => :Any)        # Get head symbol
  (.fn props [] => :Map)       # Get properties
  (.fn children [] => :Array)) # Get children

# Fn - function class (unified functions and methods)
(class Fn extends Any
  (.fn arity [] => :Int)       # Number of parameters
  (.fn call [&args => :Any])   # Invoke function
  (.fn partial [&args => :Fn]) # Partial application
  (.fn bind [obj :Any => :Fn]) # Bind first arg (for method conversion)
  (.fn name [] => :Str)        # Function name
  (.fn params [] => :Array))   # Parameter metadata

# All functions are Fn instances
((fnx [x] x) .class)   # Returns Fn
(+ .class)             # Returns Fn (+ is a function)
(Point/distance .class) # Returns Fn (methods are functions)

# Functions and methods are distinct but related
(var f = (fnx [x y] (+ x y)))
(f 2 3)                # Function call: 5
# (2 .f 3)             # ERROR: f is not a method of Int

# But methods can be used as functions
(var plus = Int/+)     # Extract + method from Int
(plus 2 3)             # 5 - method used as function

# FFI Types - for C interoperability
(var ptr :CPtr = (c-malloc 100))     # Raw C pointer
(var arr :(CArray Int) = (c-array :Int 10))  # C array
(var fn-ptr :CFn = (c-dlsym "sqrt")) # C function pointer

# Additional numeric types for FFI
(var i8 :Int8 = 127)         # 8-bit signed
(var u8 :UInt8 = 255)        # 8-bit unsigned
(var i16 :Int16 = 32767)     # 16-bit signed
(var u16 :UInt16 = 65535)    # 16-bit unsigned
(var i32 :Int32)             # 32-bit signed
(var u32 :UInt32)            # 32-bit unsigned
(var i64 :Int64)             # 64-bit signed
(var u64 :UInt64)            # 64-bit unsigned
(var f32 :Float32)           # 32-bit float
```

### 3.3.2 Metaclasses and Class Methods

Since classes are objects (instances of `Class`), they can have their own methods (class methods) and properties:

```gene
# Class methods on built-in classes
(Int .parse "42")              # Returns 42
(Float .parse "3.14")          # Returns 3.14
(Str .from_chars ['h' 'i'])    # Returns "hi"
(Array .new 10)                # Creates array with capacity 10

# The Class class has methods too
(Class .new "DynamicClass" Any)  # Create class at runtime
(Int .methods)                    # List all methods
(Int .instance_methods)           # List instance methods
(Int .class_methods)              # List class methods

# Metaclass hierarchy
(42 .class)                    # Int
(Int .class)                   # Class
(Class .class)                 # Class (it's its own class)

# Every class is an instance of Class
(Int is Class)                 # true
(Float is Class)               # true
(MyCustomClass is Class)       # true
(Class is Class)               # true

# But Class is also a subclass of Any
(Class .superclass)            # Any
(Class is Any)                 # true
```

### 3.3.3 Common Methods from Any

All objects inherit these methods from the `Any` class:

```gene
(class Any
  # Identity and type
  (.fn class [] => :Class)           # Get the object's class
  (.fn is? [type :Class => :Bool])   # Type check
  (.fn hash [] => :Int)              # Hash code
  (.fn equals [other :Any => :Bool]) # Equality

  # String representation
  (.fn to_str [] => :Str)            # Convert to string
  (.fn inspect [] => :Str)           # Debug representation

  # Method dispatch
  (.fn send [method :Symbol &args => :Any])  # Dynamic dispatch
  (.fn responds_to? [method :Symbol => :Bool])

  # Cloning
  (.fn clone [] => :Any)             # Shallow copy
  (.fn deep_clone [] => :Any))       # Deep copy

# Examples
(42 .to_str)                   # "42"
(42 .class)                    # Int
(42 .is? Number)               # true
(42 .send '+ 8)                # 50
(42 .responds_to? 'sqrt)       # false
```

### 3.3.4 Primitive Values as Objects

Even though primitive types like integers and floats are classes, Gene optimizes their representation for performance:

```gene
# Primitive values are still objects
(42 .class)                    # Int
(42 .to_str)                   # "42"
(42 .+ 8)                      # 50 (method call syntax)
(42 + 8)                       # 50 (operator syntax - same thing)

# But they're optimized internally
# - Integers use NaN-boxing (48-bit values)
# - Floats use IEEE 754 representation
# - Small strings use inline storage
# - Method calls on primitives are optimized/inlined

# Boxing happens transparently when needed
(var obj :Any = 42)            # 42 is treated as object
(obj .class)                   # Int
(obj .+ 8)                     # 50

# Literals create instances
42                             # Creates instance of Int
3.14                           # Creates instance of Float
"hello"                        # Creates instance of Str
true                           # Creates instance of Bool
nil                            # The singleton instance of Nil
```

### 3.3.5 Type Constructors

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

## 3.6 Gradual Typing Philosophy

Gene embraces gradual typing, allowing developers to start with dynamic code and incrementally add types for safety and performance:

### 3.6.1 The Gradual Spectrum

```gene
# Fully dynamic - no type annotations
(fn add [x y] (+ x y))

# Partially typed - annotate what matters
(fn add [x :Number y] (+ x y))  # Only x is typed

# Fully typed - maximum safety
(fn add [x :Int y :Int => :Int] (+ x y))

# Gradual migration in practice
(fn process-user [user]           # Start dynamic
  (do
    (validate user)
    (save user)))

(fn process-user [user :User]     # Add types later
  (do
    (validate user)               # validate inferred to take User
    (save user)))                 # save inferred to take User
```

### 3.6.2 Type Checking Modes

```gene
# File-level type checking pragmas
(pragma ^type-check :strict)   # All functions must be typed
(pragma ^type-check :gradual)  # Default - mix typed/untyped
(pragma ^type-check :none)     # No type checking

# Function-level overrides
(fn ^:no-check hacky-function [x]  # Skip type checking
  (unsafe-operations x))

(fn ^:strict critical-function [x :Data => :Result]
  (validated-operations x))
```

### 3.6.3 Runtime Type Safety

```gene
# Types are checked at runtime when needed
(fn process [x]
  (x .validate!)  # Runtime error if x doesn't have validate!
  (save x))

# Gradual contracts - runtime checks for untyped code
(fn process [x] ^:contract {:validate! true}
  (x .validate!)  # Contract ensures method exists
  (save x))
```

## 3.7 Type Inference

Gene performs bidirectional type inference with gradual typing support:

```gene
# Local type inference
(var x = 10)           # Inferred as Int
(var y = [1 2 3])      # Inferred as (Array Int)

# Function type inference
(fn double [x] (* x 2))  # x:Number inferred from + usage

# Gradual inference - mixing typed and untyped
(fn process [data]
  (var typed :Int = (data .count))  # data must have .count returning Int
  (print typed))                    # print accepts Any

# Generic inference
(map (fnx [x] (* x 2)) [1 2 3])  # T=Int, U=Int inferred
```

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

## 6.1 Unified Functions and Methods

In Gene, **functions and methods are unified at the type level** - both are instances of the `Fn` class. However, they have distinct calling conventions:

- All functions and methods are instances of the `Fn` class
- Methods are functions with an implicit `self` parameter
- Method calls `(obj .method)` only resolve to methods in the class hierarchy
- Functions must be explicitly added to a class to be used as methods
- Methods can be extracted from classes and used as standalone functions

### Core Principles:

1. **Functions are objects**: Every function is an instance of `Fn`
2. **Methods are functions**: Methods are functions with an implicit `self` parameter
3. **Clear separation**: Method calls only resolve to methods, not standalone functions
4. **Extraction allowed**: Methods can be extracted and used as functions

```gene
# Basic function
(fn add [x y]
  (+ x y))

# Function call only:
(add 2 3)                   # Function call: 5
# (2 .add 3)                # ERROR: add is not a method of Int

# Built-in arithmetic methods work differently:
(2 .+ 3)                    # 5 - because + is a method of Number class
(2 + 3)                     # 5 - operator syntax (sugar for method call)

# Method definition in a class
(class Point
  (.fn distance [other :Point => :Float]
    (math/sqrt (+ (** (- other/x /x) 2)
                  (** (- other/y /y) 2)))))

# Is equivalent to:
(class Point
  # Function with explicit self
  (fn distance [self :Point other :Point => :Float]
    (math/sqrt (+ (** (- other/x self/x) 2)
                  (** (- other/y self/y) 2)))))

# Both can be called as:
(p1 .distance p2)           # Method syntax
(distance p1 p2)            # Function syntax (if in scope)
```

### Function Definition Syntax:

```gene
# Standalone function
(fn add [x :Int y :Int => :Int]
  (+ x y))

# Method using .fn (adds implicit self)
(.fn add [y :Int => :Int]
  (+ self y))

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

## 6.2 Unified Calling Conventions

The unification of functions and methods enables flexible calling patterns:

```gene
# Define a function
(fn multiply [x :Number y :Number => :Number]
  (* x y))

# Call as function
(multiply 3 4)              # 12

# Cannot call function as method - this would error!
# (3 .multiply 4)          # ERROR: multiply not found in Int class

# Must use function syntax
(multiply 3 4)             # 12 - correct way

# Methods can be extracted as functions
(var dist-fn = Point/distance)  # Get method as function
(dist-fn p1 p2)                 # Call it as function

# To use a function as a method, you must add it to the class
(class-extension Int
  # Add a function as a method
  (.fn double [] (* self 2)))

(5 .double)                 # 10 - now it works!

# Partial application creates a new function
(var add5 = (add .partial 5))   # Partial application
(add5 3)                        # 8 - function call
# (3 .add5)                      # ERROR: add5 not a method of Int
```

### Method Resolution:

When you write `(obj .method args)`, Gene:
1. Looks for `method` in `obj`'s class hierarchy
2. If found, calls it with `obj` as the implicit `self`
3. If not found, raises a "method not found" error

**Important**: Method calls `(obj .method)` only search for methods defined in the class hierarchy. They do NOT search for standalone functions. To call a function with an object as the first argument, use the function call syntax: `(function obj args)`.

This clear separation ensures:
- Predictable method resolution
- No unexpected function shadowing
- Clear distinction between OO and functional styles

## 6.3 Anonymous Functions

Anonymous functions work the same way and can be used as methods:

```gene
# Lambda syntax
(fnx [x] (* x 2))

# With types
(fnx [x :Int => :Int] (* x 2))

# Anonymous functions are still just functions
(var doubler = (fnx [x] (* x 2)))
(doubler 5)                 # 10 - function call
# (5 .doubler)              # ERROR: doubler is not a method

# Shorthand syntax (future feature)
#(* % 2)              # % is implicit parameter
#(+ %1 %2)            # Multiple parameters
```

## 6.4 Closures

Functions capture their lexical environment, and this works uniformly whether called as functions or methods:

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

Gene's object system is built on the unified class hierarchy described in Chapter 3, where everything is an object and every type is a class. This chapter covers how to define your own classes and work with the object system.

## 7.1 Classes

In Gene's unified type system, user-defined classes integrate seamlessly with built-in classes:

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

  # Methods - using .fn adds implicit self parameter
  (.fn distance [other :Point => :Float]
    (math/sqrt (+ (** (- other/x /x) 2)
                  (** (- other/y /y) 2))))

  # Alternative: explicit self (same effect)
  (fn distance-explicit [self :Point other :Point => :Float]
    (math/sqrt (+ (** (- other/x self/x) 2)
                  (** (- other/y self/y) 2))))

  # Properties (getters/setters)
  (.get magnitude []
    (math/sqrt (+ (* /x /x) (* /y /y))))

  (.set magnitude [m :Float]
    (var scale = (/ m /magnitude))
    (/x = (* /x scale))
    (/y = (* /y scale)))
  
  # Macro method for DSL-like syntax
  (.macro move-by [dx dy]
    (/x = (+ /x dx))     # dx/dy evaluated in caller's context
    (/y = (+ /y dy))
    self))

# Instantiation
(var p = (new Point 3.0 4.0))
(var p2 = (Point 3.0 4.0))  # new is optional

# User classes are part of the hierarchy
(p .class)                  # Point
(Point .class)              # Class
(Point .superclass)         # Any (default parent)
(p is Point)                # true
(p is Any)                  # true
(Point is Class)            # true

# Using macro methods - lazy evaluation
(var step 5)
(p .move-by step (* step 2))  # step evaluated when macro runs
# Equivalent to: (/x = (+ /x 5)), (/y = (+ /y 10))
```

### Class Definition in the Unified System

```gene
# All classes inherit from Any by default
(class Person)              # Implicitly: extends Any

# Check the class hierarchy
(Person .superclass)        # Any
(Person .ancestors)         # [Person Any]
(Person is Class)           # true
(Person is Any)             # true

# Create instances
(var john = (Person))
(john .class)               # Person
(john is Person)            # true
(john is Any)               # true

# Built-in classes work the same way
(42 .class)                 # Int
(Int .superclass)           # Number
(Int is Class)              # true
```

### Unified Functions and Methods in Classes

In Gene's unified system, methods are just functions associated with a class. This provides flexibility in how you define and use them:

```gene
(class Calculator
  # Method using .fn (implicit self)
  (.fn add [x :Number => :Number]
    (+ self x))

  # Function with explicit self (same as above)
  (fn multiply [self :Number x :Number => :Number]
    (* self x))

  # Static method (no self parameter)
  (fn pi [] => :Float
    3.14159))

# Usage - all these work:
(var calc = (Calculator))
(calc .add 5)               # Method call
(add calc 5)                # Function call (if in scope)

# Extract method as function
(var add-fn = Calculator/add)
(add-fn calc 5)             # Call as function

# Standalone functions are NOT automatically methods
(fn double [x :Number => :Number] (* x 2))
(double 5)                  # 10 - function call
# (5 .double)               # ERROR: double is not a method of Int

# To make it a method, add it to the class:
(class-extension Int
  (.fn double [] (* self 2)))
(5 .double)                 # Now it works!
```

## 7.2 Inheritance

Since all types are classes in Gene's unified system, user-defined classes can even inherit from built-in classes (with some restrictions):

```gene
# Inherit from built-in collection class
(class Stack extends Array
  (.fn push [item :Any]
    (super .push item))

  (.fn pop []
    (if (.empty?)
      (error "Stack underflow")
      (super .pop))))

# Single inheritance from user class
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

Gene supports three core method types with visibility modifiers:

### Core Method Types:
1. **Regular methods** (.fn) - Standard evaluation, instance methods
2. **Macro methods** (.macro) - Lazy evaluation with pseudo macro semantics
3. **Static methods** (.static fn) - Class-level methods without instance

### Visibility Modifiers:
- **Public** (default) - Accessible everywhere
- **Private** (.-fn) - Only accessible within the class
- **Protected** (.#fn) - Accessible in class and subclasses

```gene
(class Component
  (.prop state Map = {})

  # Regular public method
  (.fn update [key value]
    (/state .set key value))

  # Private helper method
  (.-fn validate-key [key]
    (key is Symbol))

  # Protected method for subclasses
  (.#fn internal-update [data]
    (/state = data))

  # Macro method - lazy evaluation
  (.macro when-changed [condition body]
    (if condition body nil))

  # Static factory method
  (.static fn create []
    (Component)))
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

## 8.1 Core Concepts

Gene's module system is based on these principles:
- Each file or source string corresponds to a module
- Each module corresponds to a namespace
- There is a global namespace available everywhere
- Namespaces can be created explicitly within a module
- Slash notation (`/`) is used for namespace member access

## 8.2 Module and File Correspondence

```gene
# File: math/utils.gene
# This file automatically creates the namespace 'math/utils'

(fn square [x]
  (* x x))

(fn cube [x]
  (* x x x))

# Another file can access these as:
# math/utils/square
# math/utils/cube
```

## 8.3 Explicit Namespace Creation

```gene
# Create a namespace explicitly within a module
(ns geometry
  (class Point
    (x Int)
    (y Int))
  
  (class Circle
    (center Point)
    (radius Float))
  
  (fn area [c]
    (* 3.14159 c/radius c/radius)))

# Access from outside:
(var p (new geometry/Point))
(p/x = 10)
(p/y = 20)

(var c (new geometry/Circle))
(c/center = p)
(c/radius = 5.0)
(print (geometry/area c))
```

## 8.4 Import System

```gene
# Import specific members from a module
(import math/utils [square cube])
# Now can use square and cube directly
(print (square 5))
(print (cube 3))

# Import with alias
(import math/utils [square as sq])
(print (sq 4))

# Import entire module under a namespace
(import math/utils as mu)
(print (mu/square 5))
(print (mu/cube 3))

# Import from explicit namespace
(import geometry [Point Circle area])
(var p (new Point))  # Can use Point directly
```

## 8.5 Global Namespace

```gene
# Items defined at the top level without ns are in the global namespace
(fn global-helper [x]
  (+ x 1))

# Available everywhere without import
(print (global-helper 5))

# Built-in functions are in the global namespace
(print "Hello")  # print is global
(+ 1 2)          # + is global
```

## 8.6 Namespace Access Patterns

```gene
# Slash notation for accessing namespace members
math/utils/square      # Fully qualified access
geometry/Point         # Access class from namespace
obj/field             # Object field access (same syntax)

# After import
(import math/utils [square])
square                 # Direct access after import

# Namespace nesting
(ns company
  (ns department
    (class Employee
      (name String)
      (id Int))))

# Access: company/department/Employee
(var emp (new company/department/Employee))
```

## 8.7 Module Loading

```gene
# Modules are loaded lazily on first access or import
# File: config.gene
(var database-url "localhost:5432")
(var api-key "secret")

# File: main.gene
(import config [database-url])  # Loads config.gene
(print database-url)

# Or direct access (also loads the module)
(print config/api-key)
```

## 8.8 Visibility and Exports

```gene
# By default, all top-level definitions in a module are accessible
# Future enhancement: private definitions with - prefix

# File: lib.gene
(fn public-fn []      # Accessible from other modules
  (helper))

(fn -private-fn []    # Future: Not accessible from other modules
  ...)

# Future: explicit export lists
# ^export [public-fn]
```

## 8.9 Name Collision Resolution

Gene handles name collisions with these rules:

### 8.9.1 Import Precedence

```gene
# Local definitions always win over imports
(import math [max])    # Imports math/max

(fn max [a b]         # Local max shadows imported max
  (if (> a b) a b))

(print (max 5 3))     # Uses local max, not math/max
(print (math/max 5 3)) # Can still use fully qualified name
```

### 8.9.2 Multiple Import Conflicts

```gene
# File: lib1.gene
(fn process [x] (+ x 1))

# File: lib2.gene  
(fn process [x] (* x 2))

# File: main.gene
(import lib1 [process])
(import lib2 [process])  # Error: 'process' already imported from lib1

# Solutions:

# Option 1: Use aliases
(import lib1 [process as process1])
(import lib2 [process as process2])
(print (process1 5))  # 6
(print (process2 5))  # 10

# Option 2: Use qualified names
(import lib1)
(import lib2)
(print (lib1/process 5))  # 6
(print (lib2/process 5))  # 10

# Option 3: Selective import
(import lib1 [process])
(import lib2 [other-fn])  # Import different members
```

### 8.9.3 Namespace Collisions

```gene
# Explicit namespaces can shadow module namespaces
# File: geometry.gene exists

(ns geometry          # Local namespace shadows module
  (class Square
    (side Float)))

# Access patterns:
geometry/Square       # Refers to local namespace
./geometry/Square     # Future: Refers to module (file)

# Or use import to be explicit
(import geometry as geom-module)
geom-module/Point     # From module
geometry/Square       # From local namespace
```

### 8.9.4 Global vs Local Precedence

```gene
# Local definitions shadow global ones
(fn print [x]         # Shadows global print
  (global/print "Custom: " x))  # Can access global via global/

(print "Hello")       # Uses local print

# Imported names shadow globals but not locals
(import custom [+])   # Custom + operator
(+ 1 2)              # Uses imported +, not global
(global/+ 1 2)       # Access global + explicitly
```

### 8.9.5 Resolution Order

The name resolution order in Gene is:
1. Local variables and parameters
2. Local function/class definitions  
3. Imported names
4. Module-level definitions in current module
5. Namespace members (via / notation)
6. Global namespace

```gene
(var x 10)           # Local variable

(import math [x])    # Error: local 'x' already defined

(fn test []
  (var x 20)         # Function-local x
  (print x)          # Prints 20, not 10
  (print module/x))  # Future: Access module-level x
```

### 8.9.6 Best Practices

```gene
# 1. Use qualified names when clarity is needed
(import utils)
(import helpers)
(utils/process data)
(helpers/process data)

# 2. Use aliases for common conflicts
(import threading [lock as thread-lock])
(import database [lock as db-lock])

# 3. Group related imports
(import geometry [Point Circle Rectangle])
(import math [sin cos tan])

# 4. Avoid shadowing built-ins unless intentional
# Bad: (fn print [...])
# Good: (fn custom-print [...])
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

# Class-based macro with .macro (implicit self)
(class QueryBuilder
  (.macro where [condition]
    (.add-condition self condition)  # self is implicit
    self)  # Return self for chaining
  
  (.macro select [fields...]
    (/fields = fields)  # Access self's properties
    self))

# Usage - arguments not evaluated until needed
(var x 0)
(when (> x 5)           ;; This comparison not evaluated immediately
  (print "x is large")) ;; This print not evaluated immediately

# Using class macros
(var query (QueryBuilder))
(query .where (> age 18)     # Condition not evaluated yet
       .select name email)    # Fields captured as symbols

# The macro body evaluates arguments on demand
# in the caller's context where variables are visible
```

### 11.1.2 Method Macros with .macro

Just like `.fn` adds an implicit `self` parameter to functions, `.macro` adds an implicit `self` parameter to macros:

```gene
# Standalone macro
(macro log [level message]
  (when (>= level *log-level*)
    (print (timestamp) level message)))

# Method macro with implicit self
(class DSL
  (.macro rule [name body]
    # self is implicitly available
    (.add-rule self name body)
    self)
  
  (.macro when [condition action]
    # Can access self's properties
    (/conditions = (cons [condition action] /conditions))
    self))

# Usage shows the difference
(log :info "Starting")              # Standalone macro

(var dsl (DSL))
(dsl .rule 'validate-age           # Method macro - self is dsl
     (when (< age 18)
           (error "Too young"))
     .rule 'validate-email          # Chaining works
     (when (not (valid-email? email))
           (error "Invalid email")))

# .macro vs macro:
# - macro: defines standalone macro
# - .macro: defines method macro with implicit self
# - Both have lazy evaluation of arguments
```

### 11.1.3 Advanced Examples

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
# Question mark operator for Result types
(fn process-file [path :Str => :(Result Data Error)]
  (var file = (open-file? path))   # ? unwraps Ok or returns Err
  (var data = (read-data? file))
  (var parsed = (parse? data))
  [:ok parsed])

# Bang operator for exceptions
(fn process-file-unsafe [path :Str => Data]
  (var file = (open-file! path))   # ! unwraps Ok or throws
  (var data = (read-data! file))
  (parse! data))

# Error context and stack traces
(fn process-user [id :Int => :(Result User Error)]
  (with-context {:user-id id :operation "process"}
    (var user = (db/find-user? id))
    (validate-user? user)))

# Async error propagation
(async fn fetch-and-process [url :Str => :(Result Data Error)]
  (var response = (await (http/get? url)))
  (var data = (await (response.json?)))
  (process-data? data))

# Error boundaries for fault isolation
(with-error-boundary
  ^on-error (fnx [e]
    (log/error "Operation failed" e)
    (metric/increment :errors)
    default-value)
  ^retry 3
  ^timeout 5000
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

# Chapter 13: Foreign Function Interface (FFI)

## 13.1 Overview

Gene provides a fast, zero-copy Foreign Function Interface (FFI) for seamless interoperability with C and other languages that expose C-compatible ABIs. The FFI system is designed to:

- Minimize overhead when calling native functions
- Provide automatic type marshalling between Gene and C types
- Support both calling C from Gene and embedding Gene in C applications
- Enable direct memory sharing where safe
- Maintain memory safety through careful API design

## 13.2 C Type Mapping

Gene provides direct mappings between its types and C types:

```gene
# Primitive type mappings
Gene Type    C Type         Notes
---------    ------         -----
Int          int64_t        48-bit in Gene, promoted to 64-bit for C
Float        double         Direct mapping
Bool         bool           0/1 in C, true/false in Gene
Char         uint32_t       Unicode codepoint
Nil          void*          NULL pointer
Str          const char*    UTF-8 null-terminated (with caveats)

# Pointer types
(CPtr T)     T*             Raw C pointer
(CArray T)   T[]            C array with known size
(CStruct)    struct         C struct (see below)
(CUnion)     union          C union
(CFn ...)    T(*)(...)      C function pointer
```

### 13.2.1 C Type Declarations

```gene
# Declare C types
(c-type size_t :UInt64)
(c-type FILE :CPtr)

# C struct declaration
(c-struct Point
  (^x :Float)
  (^y :Float))

# C union declaration
(c-union Value
  (^i :Int)
  (^f :Float)
  (^p :CPtr))

# Opaque pointer type
(c-type SDL_Window :CPtr)  # Opaque struct pointer
```

## 13.3 Declaring External Functions

### 13.3.1 Basic Function Declaration

```gene
# Simple C function
(c-extern printf [format :CStr &rest => :Int]
  ^lib "libc")

# Function with specific calling convention
(c-extern MessageBoxA [hwnd :CPtr title :CStr text :CStr type :UInt32 => :Int]
  ^lib "user32.dll"
  ^calling-conv :stdcall)

# Function that may not exist (optional binding)
(c-extern? experimental_feature [x :Int => :Int]
  ^lib "mylib"
  ^symbol "exp_feat_v2")  # Actual symbol name
```

### 13.3.2 Complex Function Signatures

```gene
# Function taking/returning structs
(c-extern SDL_CreateWindow [
  title :CStr
  x :Int y :Int
  w :Int h :Int
  flags :UInt32
  => :SDL_Window]
  ^lib "SDL2")

# Function with callbacks
(c-type Comparator :CFn [:CPtr :CPtr => :Int])

(c-extern qsort [
  base :CPtr
  n :size_t
  size :size_t
  cmp :Comparator
  => :Void]
  ^lib "libc")

# Variadic functions
(c-extern open [path :CStr flags :Int &rest => :Int]
  ^lib "libc"
  ^variadic true)
```

## 13.4 Memory Management

### 13.4.1 Automatic Memory Handling

```gene
# String conversion (automatic)
(printf "Hello %s\n" "World")  # Gene string auto-converted to C string

# Explicit string management
(let [c-str (str-to-cstr "Hello")]
  (some-c-function c-str)
  (free-cstr c-str))  # Manual cleanup

# Pinned memory for C access
(with-pinned [buffer (make-array :UInt8 1024)]
  (read-file fd buffer 1024))  # Buffer memory won't move during GC
```

### 13.4.2 Manual Memory Management

```gene
# Allocate C memory
(let [ptr (c-malloc (* 100 (c-sizeof :Float)))]
  (when ptr
    # Use the memory
    (c-memset ptr 0 (* 100 (c-sizeof :Float)))
    # Must free manually
    (c-free ptr)))

# Stack allocation for temporary C memory
(with-c-stack [buffer :Float 100]  # 100 floats on stack
  (process-floats buffer 100))

# Managed C memory (auto-freed)
(let [ptr (c-alloc :Float 100)]  # Freed when out of scope
  (process-floats ptr 100))
```

## 13.5 Struct and Union Access

### 13.5.1 Struct Manipulation

```gene
# Define struct
(c-struct Rectangle
  (^x :Float)
  (^y :Float)
  (^width :Float)
  (^height :Float))

# Create and use struct
(let [rect (Rectangle ^x 10 ^y 20 ^width 100 ^height 50)]
  (rect .x)           # Get field: 10
  (rect .x = 15)      # Set field
  (rect .width))      # Get field: 100

# Pass to C function
(c-extern draw_rect [r :(CPtr Rectangle) => :Void] ^lib "graphics")
(draw_rect (c-ref rect))  # Pass pointer to struct

# Struct arrays
(let [rects (c-array Rectangle 10)]
  ((rects . 0) .x = 5)      # First element's x
  ((rects . 1) .y = 10))    # Second element's y
```

### 13.5.2 Nested Structures

```gene
(c-struct Color
  (^r :UInt8)
  (^g :UInt8)
  (^b :UInt8)
  (^a :UInt8))

(c-struct Vertex
  (^position :Point)      # Nested struct
  (^color :Color)
  (^texcoord :(CArray Float 2)))  # Fixed array

# Access nested fields
(let [v (Vertex)]
  (v .position .x = 10.0)
  (v .color .r = 255)
  (v .texcoord . 0 = 0.5))
```

## 13.6 Callbacks and Function Pointers

### 13.6.1 Gene Functions as C Callbacks

```gene
# Define callback signature
(c-type EventHandler :CFn [:(CPtr Event) => :Void])

# Gene function as callback
(fn handle-event [event :(CPtr Event)]
  (match (event .type)
    EVENT_CLICK (println "Click!")
    EVENT_KEY   (println "Key press!")
    _           nil))

# Register callback (automatic conversion)
(c-extern register_handler [handler :EventHandler => :Void] ^lib "mylib")
(register_handler handle-event)  # Gene fn auto-converted to C fn pointer

# Callback with closure (requires wrapper)
(let [counter (ref 0)]
  (register_handler
    (c-callback [event :(CPtr Event) => :Void]
      (counter = (+ @counter 1))
      (println "Event #" @counter))))
```

### 13.6.2 Calling C Function Pointers

```gene
# Get function pointer from C
(c-extern get_handler [] => :EventHandler] ^lib "mylib")

(let [handler (get-handler)]
  (when handler
    (handler event-ptr)))  # Call like normal function

# Function pointer in struct
(c-struct VTable
  (^init :CFn [:(CPtr Self) => :Void])
  (^update :CFn [:(CPtr Self) :Float => :Void])
  (^render :CFn [:(CPtr Self) => :Void]))

(let [obj (get-object)]
  ((obj .vtable .init) obj)
  ((obj .vtable .update) obj 0.016)
  ((obj .vtable .render) obj))
```

## 13.7 Library Management

### 13.7.1 Loading Libraries

```gene
# Load library explicitly
(c-load-lib "mylib"
  ^search-paths ["/usr/local/lib" "~/lib"]
  ^version "2.0")

# Lazy loading (on first use)
(c-extern some_func [] ^lib "biglib" ^lazy true)

# Platform-specific loading
(c-extern windows_func []
  ^lib (if (windows?) "mylib.dll"
        (if (macos?) "libmylib.dylib"
            "libmylib.so")))

# Load with dependencies
(c-load-lib "complex_lib"
  ^deps ["dependency1" "dependency2"])
```

### 13.7.2 Dynamic Symbol Resolution

```gene
# Runtime symbol lookup
(when-let [sym (c-dlsym "experimental_function" ^lib "mylib")]
  (let [func (c-cast sym :CFn [:Int => :Int])]
    (func 42)))

# Versioned symbols
(c-extern foo []
  ^lib "mylib"
  ^symbol "foo@@VERS_2.0")  # Specific version
```

## 13.8 Safety and Error Handling

### 13.8.1 Null Pointer Handling

```gene
# Safe pointer dereferencing
(when-valid ptr
  (ptr .field))  # Only executes if ptr is non-null

# Null checks in function calls
(c-extern risky_func [p :(CPtr Data) => :Int]
  ^lib "lib"
  ^null-check true)  # Auto-check args for null

# Manual null checking
(if (c-null? ptr)
  (error "Null pointer!")
  (process ptr))
```

### 13.8.2 Bounds Checking

```gene
# Array with bounds checking
(let [arr (c-array :Int 10 ^checked true)]
  (arr . 5 = 42)    # OK
  (arr . 20 = 99))  # Runtime error

# Unsafe array access (faster)
(let [arr (c-array :Int 10)]
  (unsafe
    (arr . 5 = 42)))  # No bounds check
```

### 13.8.3 Resource Management

```gene
# RAII-style resource management
(c-resource SDL_Window
  ^create SDL_CreateWindow
  ^destroy SDL_DestroyWindow)

(with-c-resource [window (SDL_CreateWindow "Test" 0 0 640 480 0)]
  # Window automatically destroyed on scope exit
  (render-loop window))

# Manual resource tracking
(c-track ptr ^finalizer (fnx [] (cleanup-func ptr)))
```

## 13.9 Embedding Gene in C

### 13.9.1 C API for Gene

```c
// Initialize Gene runtime
gene_runtime* rt = gene_init();

// Create Gene values
gene_value* num = gene_make_int(rt, 42);
gene_value* str = gene_make_string(rt, "Hello from C");

// Call Gene functions
gene_value* result = gene_call(rt, "my-gene-func", 2, num, str);

// Extract C values
if (gene_is_int(result)) {
    int64_t n = gene_get_int(result);
}

// Cleanup
gene_shutdown(rt);
```

### 13.9.2 Exposing C Functions to Embedded Gene

```c
// C function to expose
gene_value* c_add(gene_runtime* rt, int argc, gene_value** argv) {
    if (argc != 2) return gene_make_error(rt, "Expected 2 arguments");

    int64_t a = gene_get_int(argv[0]);
    int64_t b = gene_get_int(argv[1]);

    return gene_make_int(rt, a + b);
}

// Register with Gene
gene_register_function(rt, "c-add", c_add, 2);
```

## 13.10 Performance Optimization

### 13.10.1 Zero-Copy Operations

```gene
# Share memory between Gene and C
(let [buffer (make-direct-buffer :Float 1000)]
  # Buffer memory is directly accessible from C
  (c-process-floats (buffer .ptr) (buffer .size))
  # Changes visible in Gene immediately
  (buffer . 0))  # See C's modifications

# String views (no copy)
(let [s "Large string"]
  (with-string-view [ptr len] s
    (c-func-expecting-string ptr len)))
```

### 13.10.2 Inline FFI Calls

```gene
# Inline C code (JIT compilation)
(c-inline [x :Int y :Int => :Int]
  "return x + y;")

# Inline with Gene value access
(c-inline [arr :(Array Int) => :Int]
  "int sum = 0;"
  "for (int i = 0; i < GENE_ARRAY_LENGTH(arr); i++) {"
  "  sum += GENE_ARRAY_GET_INT(arr, i);"
  "}"
  "return sum;")
```

## 13.11 Platform-Specific Features

### 13.11.1 Windows COM Interop

```gene
(when (windows?)
  (c-com-interface IUnknown
    (^QueryInterface [:REFIID :(CPtr CPtr) => :HRESULT])
    (^AddRef [=> :ULONG])
    (^Release [=> :ULONG]))

  (with-com-object [obj (create-com-object CLSID_Something)]
    (obj .SomeMethod)))
```

### 13.11.2 Objective-C Interop (macOS)

```gene
(when (macos?)
  (objc-class NSString)

  (let [s (NSString stringWithUTF8String: "Hello")]
    (s length)))
```

## 13.12 Best Practices

1. **Prefer High-Level Wrappers**: Wrap low-level FFI in Gene functions
2. **Validate at Boundaries**: Check types and nulls at FFI boundaries
3. **Use Resource Management**: Leverage with-* forms for cleanup
4. **Minimize Allocations**: Reuse buffers when calling C repeatedly
5. **Document Memory Ownership**: Be clear about who frees what
6. **Test Extensively**: FFI code needs extra testing
7. **Profile Performance**: Ensure FFI overhead is acceptable

---

# Chapter 14: Package Management and Tooling

## 13.1 Package Manager

Gene includes a built-in package manager for dependency management:

```gene
# gene.toml
[package]
name = "my-app"
version = "0.1.0"
authors = ["dev@example.com"]

[dependencies]
http-client = "2.1.0"
json = "1.0.0"
async-std = { version = "1.5.0", features = ["unstable"] }

[dev-dependencies]
test-framework = "3.0.0"
```

**Features:**
- Semantic versioning
- Lock files for reproducible builds
- Private registries support
- Git dependencies
- Local path dependencies

## 13.2 Development Tools

**Language Server Protocol (LSP):**
- Auto-completion
- Go-to-definition
- Find references
- Rename refactoring
- Real-time error checking

**Debugger Support:**
- Breakpoints in all execution modes
- Variable inspection
- Stack trace navigation
- Hot code reload
- Time-travel debugging (future)

**Profiling Tools:**
- CPU profiling
- Memory profiling
- Allocation tracking
- GC statistics
- Flame graphs

## 13.3 Documentation

Gene supports inline documentation with markdown:

```gene
#doc """
# HTTP Client

A simple HTTP client for making web requests.

## Examples

```gene
(def client (http/client))
(def response (client.get "https://api.example.com/data"))
```
"""
(module http-client
  ...)
```

# Chapter 15: Standard Library

## 14.1 Core Functions

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

# Chapter 15: Architecture Overview

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

## 14.2 Implementation Phases

The Gene implementation follows a pragmatic, phased approach:

### Phase 1: Core Interpreter (Current Status ✅)
- 4-stage pipeline: Source → AST → HIR → MIR → Bytecode
- Stack-based MIR for simplicity
- Register-based bytecode VM
- Basic type system without active checking
- Functions, recursion, conditionals, arithmetic

### Phase 2: Type System & Classes (Next)
- Active type checking in HIR
- Runtime support for classes and objects
- Pattern matching runtime
- Module system basics

### Phase 3: Optimization & JIT
- Add LIR stage for optimization
- SSA form in MIR
- Basic JIT compilation
- Inline caches for property access

### Phase 4: Advanced Features
- Full concurrent runtime (actors, channels)
- Garbage collection
- Metaprogramming (macros)
- AOT compilation support

## 14.3 Execution Modes

Gene will eventually support three execution modes that can coexist seamlessly within the same program:

### 14.3.1 Interpreted Execution (Fast Compilation)
- **Use Case**: Development, REPL, cold code, rapid iteration
- **Compilation Time**: ~1-5ms per function
- **Runtime Performance**: Good for development, adequate for most code
- **Memory Usage**: Moderate (bytecode + VM state)

### 14.3.2 JIT Compilation (Balanced)
- **Use Case**: Hot paths identified at runtime
- **Compilation Time**: ~10-50ms per function
- **Runtime Performance**: Near-native speed
- **Trigger**: Functions called > 1000 times or marked with `^jit`

### 14.3.3 AOT Compilation (Maximum Performance)
- **Use Case**: Core libraries, system functions, deployment
- **Compilation Time**: ~100-500ms per function
- **Runtime Performance**: Native C-level speed
- **Usage**: Standard library, critical paths, functions marked with `^compiled`

### Function Compilation Hints

```gene
# Default: interpreted, may be JIT compiled if hot
(fn calculate [x y]
  (+ x y))

# Force JIT compilation on first call
(fn process-batch [data]
  ^jit
  (map complex-transform data))

# Pre-compile to native code
(fn matrix-multiply [a b]
  ^compiled
  ^inline
  (heavy-computation a b))

# Never compile (always interpret)
(fn debug-helper [x]
  ^interpreted
  (inspect x))
```

## 14.4 Core Components

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

# Chapter 16: Lexer and Parser

## 15.1 Lexer Design

The lexer converts source text into tokens, recognizing:

**Literal Values:**
- Integers (64-bit signed)
- Floating-point numbers (64-bit IEEE 754)
- Strings with escape sequences
- Unicode characters
- Symbols (interned strings starting with `:`)
- Keywords (language-reserved identifiers)

**Delimiters:**
- Parentheses `()` for S-expressions
- Brackets `[]` for arrays and parameter lists
- Braces `{}` for maps and destructuring

**Special Characters:**
- Quote `:` for symbols
- Unquote `%` for template expansion
- Property `^` for metadata
- Dot `.` for method calls and paths
- Slash `/` for namespaces

**Operators:**
- Arithmetic: `+`, `-`, `*`, `/`
- Comparison: `=`, `!=`, `<`, `>`, `<=`, `>=`
- Logical: `and`, `or`, `not`

**Keywords:**
- Control flow: `if`, `else`, `do`
- Definitions: `fn`, `var`, `class`
- Macros: `macro`, `defmacro`

The lexer maintains source location information for each token to enable precise error reporting.

## 15.2 Parser Design

The parser builds an Abstract Syntax Tree (AST) from tokens using recursive descent parsing.

**AST Node Types:**

**Atomic Expressions:**
- Literals (numbers, strings, booleans, nil)
- Variables (identifiers)
- Symbols (quoted identifiers)

**Compound Expressions:**
- Function calls (S-expressions)
- Function definitions
- Variable declarations
- Conditionals (if/else)
- Array and map literals
- Do blocks (sequential evaluation)

**Advanced Constructs:**
- Class definitions with inheritance
- Pattern matching expressions
- Module definitions and imports

**Parsing Strategy:**

The parser uses a simple recursive descent algorithm:
1. Peek at the current token to determine expression type
2. Dispatch to appropriate parsing method
3. Recursively parse sub-expressions
4. Build AST nodes with proper parent-child relationships

**Key Design Decisions:**
- S-expressions make parsing trivial - no precedence rules needed
- Every form starts with an identifying token or keyword
- Uniform syntax reduces parser complexity
- Memory management uses arena allocation for efficiency

## 15.3 Error Recovery

The parser implements error recovery strategies:
- Skip to next balanced delimiter on syntax errors
- Provide context-aware error messages
- Track source locations for all AST nodes
- Support partial parsing for IDE features

---

# Chapter 17: IR Pipeline (HIR, MIR, LIR)

## 16.1 Overview

Gene's IR pipeline is designed for progressive lowering and optimization:

### Current Implementation (Phase 1)
- **AST**: Direct representation of S-expressions
- **HIR**: High-level IR preserving semantic information
- **MIR**: Stack-based intermediate representation
- **Bytecode**: Register-based VM instructions (no LIR stage)

### Full Design (Phase 3+)
1. **HIR (High-level IR)**: Close to source, preserves semantic information
2. **MIR (Mid-level IR)**: SSA form, suitable for optimization
3. **LIR (Low-level IR)**: Register-based bytecode for VM execution

The current 4-stage pipeline (AST→HIR→MIR→Bytecode) provides a working interpreter with good performance. The LIR stage will be added when implementing advanced optimizations and JIT compilation.

## 16.2 HIR (High-level Intermediate Representation)

HIR preserves high-level language constructs while normalizing syntax variations.

**Key Characteristics:**
- Close to source semantics
- Type annotations preserved
- Desugars syntax extensions
- Maintains lexical scope information

**HIR Elements:**

**Values and Variables:**
- Constants with known types
- Variable references with unique IDs
- Captured variables tracked explicitly

**Bindings and Scope:**
- Let bindings with optional type hints
- Lexical scope tracking
- Closure capture analysis

**Functions:**
- Named and anonymous functions
- Parameter type annotations
- Captured variable lists
- Return type inference points

**Control Flow:**
- If-then-else with typed branches
- Pattern matching with exhaustiveness
- Loops with break/continue

**Object-Oriented Features:**
- Class definitions with inheritance chains
- Trait implementations
- Method dispatch information
- Field access with type tracking

**HIR Transformations:**
- Desugar syntax extensions
- Resolve lexical scopes
- Assign unique IDs to all bindings
- Prepare for type checking

The HIR serves as the input to both the type checker and the MIR generator, maintaining enough information for accurate error reporting while simplifying subsequent analysis.

## 16.3 MIR (Mid-level Intermediate Representation)

### Current Implementation (Phase 1)
The current MIR uses a stack-based approach for simplicity:

**Design Principles:**
- Virtual stack machine model
- Simple instruction set
- Direct mapping from HIR
- Minimal optimization

**Instruction Categories:**
- **Load Operations**: Push constants, variables, and parameters onto stack
- **Arithmetic**: Pop operands, compute, push result (Add, Sub, Mul, Div)
- **Comparison**: Pop operands, compare, push boolean (LT, GT, EQ)
- **Control Flow**: Conditional and unconditional jumps
- **Function Calls**: Manage arguments and return values on stack
- **I/O**: Print instruction for basic output

**Benefits of Stack-Based MIR:**
- Trivial code generation from tree-structured HIR
- No register allocation complexity
- Simple to implement and debug
- Sufficient performance for interpreter

This approach prioritizes correctness and simplicity over optimization, which is appropriate for the initial implementation phase.

### Future Design (Phase 3)
MIR will eventually use SSA (Static Single Assignment) form for optimization:

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

### Current Status
In Phase 1, bytecode is generated directly from MIR without an intermediate LIR stage. This simplifies the implementation while still producing efficient register-based bytecode.

### Future Design (Phase 3)
LIR will be introduced as an optimization layer for register-based bytecode:

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

Future LIR implementation will include sophisticated register allocation:

**Allocation Strategy:**
- Start with unlimited virtual registers
- Build interference graph from liveness analysis
- Apply graph coloring algorithm
- Spill to memory when necessary

**Key Components:**
- **Virtual Registers**: Unlimited during code generation
- **Physical Registers**: Target architecture constraints
- **Spill Slots**: Memory locations for excess values
- **Live Ranges**: Track variable lifetimes

**Allocation Process:**
1. **Liveness Analysis**: Determine when values are needed
2. **Interference Graph**: Find which values conflict
3. **Graph Coloring**: Assign colors (registers) to nodes
4. **Spilling**: Move excess values to memory
5. **Code Rewriting**: Replace virtual with physical registers

**Optimizations:**
- Coalescing to eliminate moves
- Splitting live ranges
- Rematerialization instead of spilling
- Callee-saved register usage

---

# Chapter 18: Type Checker and Inference

## 17.1 Type System Architecture

Gene implements gradual typing with Hindley-Milner style type inference.

**Core Components:**
- **Type Environment**: Maps variables to types in each scope
- **Type Variables**: Placeholders for unknown types during inference
- **Constraint System**: Equations between types to be solved
- **Type Cache**: Memoization of inferred expression types

**Type Representation:**

**Primitive Types:**
- Any (top type for gradual typing)
- Void, Nil, Bool
- Numeric: Int, Float
- Text: Char, Str, Symbol

**Compound Types:**
- Arrays with element type
- Maps with key and value types
- Sets with element type
- Tuples with fixed element types

**Function Types:**
- Parameter types (possibly inferred)
- Return type (possibly inferred)
- Effect annotations (future)

**Advanced Types:**
- Type variables for inference
- Generic types with parameters
- Union types for alternatives
- Intersection types for constraints
- Class and trait types

**Constraint Types:**
- Equality constraints (type unification)
- Subtype constraints (inheritance)
- Structural constraints (fields/methods)
- Trait implementation constraints

## 17.2 Type Inference Algorithm

Gene uses Hindley-Milner type inference extended with gradual typing.

**Inference Process:**

**1. Type Variable Generation:**
- Fresh variables for unknown types
- Unique identifiers prevent conflicts
- Scope tracking for generalization

**2. Constraint Generation:**
- Walk AST/HIR collecting constraints
- Function calls generate equality constraints
- Operators impose type requirements
- Control flow unifies branches

**3. Constraint Solving:**
- Unification algorithm finds solutions
- Occurs check prevents infinite types
- Substitution builds type mappings

**4. Type Reconstruction:**
- Apply substitutions to get final types
- Generalize free variables
- Insert runtime checks for gradual types

**Key Algorithms:**

**Expression Type Inference:**
- **Literals**: Direct type from value
- **Variables**: Lookup in environment
- **Functions**: Infer parameter and return types
- **Applications**: Match function type with arguments
- **Conditionals**: Unify branch types

**Unification Process:**
1. Compare type structures
2. Bind type variables consistently
3. Check recursive constraints
4. Build substitution mapping

**Gradual Typing Integration:**
- `Any` type unifies with everything
- Runtime checks at boundaries
- Blame tracking for errors
- Optimization opportunities

**Type Inference Features:**
- Local type inference (no annotations needed)
- Polymorphic functions
- Row polymorphism for records
- Principal types guarantee

**Error Recovery:**
- Partial type information preserved
- Error nodes in type tree
- Continue inference after errors
- Useful error messages with context

## 17.3 Generic Type Instantiation

Gene supports parametric polymorphism with type parameters.

**Generic Type System:**

**Type Parameters:**
- Named type variables in function/class definitions
- Constraints on type parameters
- Variance annotations (co/contra/invariant)
- Higher-kinded types (future)

**Instantiation Process:**
1. **Type Application**: Apply concrete types to parameters
2. **Constraint Checking**: Verify type arguments satisfy bounds
3. **Monomorphization**: Generate specialized code per instantiation
4. **Caching**: Reuse previously instantiated types

**Generic Contexts:**
- Type parameter scope management
- Nested generic contexts
- Type parameter capture in closures
- Associated type projections

**Instantiation Cache:**
- Memoize instantiated types
- Share common instantiations
- Reduce compilation time
- Enable separate compilation

**Trait Implementation:**
- Check method signatures match
- Verify all required methods present
- Handle default implementations
- Support extension methods

**Type Parameter Inference:**
- Deduce type arguments from usage
- Bidirectional type flow
- Constraint propagation
- Ambiguity resolution

## 17.4 Gradual Typing

Gene seamlessly integrates static and dynamic typing.

**Core Principles:**
- **Any Type**: Universal dynamic type
- **Consistent Subtyping**: Any relates to all types
- **Runtime Checks**: Inserted at static/dynamic boundaries
- **Blame Tracking**: Precise error attribution

**Type Compatibility Rules:**

**Static to Dynamic:**
- Implicit conversion to Any
- No runtime checks needed
- Preserves type information

**Dynamic to Static:**
- Runtime type check required
- Cast may fail at runtime
- Blame assigned to cast location

**Function Types:**
- Contravariant parameters
- Covariant returns
- Higher-order gradual types

**Optimization Opportunities:**
- Eliminate redundant checks
- Type flow analysis
- JIT specialization on types
- Profile-guided optimization

**Runtime Check Insertion:**
- Minimal check placement
- Flow-sensitive analysis
- Check coalescing
- Fast path generation

**Error Handling:**
- Type errors as exceptions
- Detailed error messages
- Stack trace preservation
- Recovery strategies

---

# Chapter 19: Register-Based Virtual Machine

## Current Implementation Status

The VM is currently implemented with:
- Register-based bytecode (not stack-based)
- Basic instruction set for arithmetic, control flow, and function calls
- Function call frames with proper register allocation
- Support for recursive functions

Missing features (planned for later phases):
- Object-oriented instructions (GetField, SetField, etc.)
- Type checking instructions
- Inline caches for optimization
- JIT compilation support

## 18.1 VM Architecture

The Gene VM is a register-based virtual machine optimized for dynamic language execution.

**Core Components:**

**Register File:**
- Unlimited virtual registers per function
- Register windows for function calls
- Efficient parameter passing via register mapping
- No register spilling in interpreted mode

**Call Stack Management:**
- Function activation frames
- Instruction pointer tracking
- Base pointer for register windows
- Return address handling
- Local variable storage

**Memory Subsystems:**
- **Heap**: Dynamic object allocation
- **Stack**: Auxiliary operand stack for complex expressions
- **Globals**: Module-level variable storage
- **Modules**: Namespace isolation and management

**Optimization Infrastructure:**
- **Inline Caches**: Fast property/method lookup
- **Type Feedback**: Runtime type profiling
- **Hot Path Detection**: JIT compilation triggers
- **Profile Data**: Optimization decisions

**Runtime Type System:**
- Type registry for dynamic dispatch
- Method lookup tables
- Trait implementation tracking
- Generic instantiation cache

**Call Frame Structure:**
- Function reference
- Instruction pointer (current position)
- Base pointer (register window start)
- Return address (caller's IP)
- Register count (frame size)

**Inline Cache Design:**
- Polymorphic inline caches (PIC)
- Type-specialized fast paths
- Hit counting for optimization
- Automatic invalidation on type changes

## 18.2 Instruction Execution

The VM executes bytecode instructions in a tight loop with efficient dispatch.

**Execution Model:**
- Fetch-decode-execute cycle
- Direct threaded dispatch (future optimization)
- Register-based operands
- Minimal memory allocation

**Core Execution Loop:**
1. Fetch current instruction from bytecode
2. Decode instruction type and operands
3. Execute operation on registers
4. Update instruction pointer
5. Handle control flow changes

**Instruction Categories:**

**Data Movement:**
- **LoadConst**: Load constant into register
- **Move**: Copy between registers
- **LoadVar/StoreVar**: Variable access

**Arithmetic Operations:**
- Read operands from source registers
- Perform type-appropriate operation
- Store result in destination register
- Handle type coercion and overflow

**Control Flow:**
- **Jump**: Unconditional branch
- **JumpIf/JumpIfNot**: Conditional branches
- **Call**: Function invocation
- **Return**: Function exit

**Function Calls:**
1. **Setup Phase:**
   - Validate callable value
   - Allocate new register window
   - Create activation frame

2. **Parameter Passing:**
   - Copy arguments to parameter registers
   - Handle default parameters
   - Support variadic functions

3. **Execution Transfer:**
   - Push call frame
   - Set instruction pointer to function start
   - Continue execution in new context

4. **Return Handling:**
   - Pop call frame
   - Restore caller's context
   - Place return value in destination register

**Error Handling:**
- Type mismatches
- Stack overflow detection
- Undefined variable access
- Invalid operations

**Optimization Opportunities:**
- Instruction fusion
- Superinstructions for common patterns
- Inline caching for property access
- Call site optimization

## 18.3 Inline Caching

Inline caching accelerates property access and method dispatch in dynamic languages.

**Cache Architecture:**
- Per-callsite caches
- Type-specialized fast paths
- Polymorphic inline caches (PIC)
- Megamorphic handling

**Cache Entry Structure:**
- Cached type ID
- Property/method offset
- Hit counter
- Guard conditions

**Cache Operations:**

**Monomorphic Cache (Single Type):**
1. Check if object type matches cached type
2. If hit: use cached offset directly
3. If miss: look up property, update cache
4. Track hit rate for optimization

**Polymorphic Cache (Multiple Types):**
- Chain of type checks
- Up to 4-8 entries typically
- Fall back to generic lookup
- Reorder by frequency

**Cache Invalidation:**
- Type shape changes
- Method redefinition
- Class hierarchy modifications
- Module reloading

**JIT Integration:**
- Hot cache sites trigger compilation
- Type feedback guides optimization
- Speculative optimization with guards
- Deoptimization on assumption violation

**Performance Benefits:**
- Property access: 10-100x speedup
- Method dispatch: 5-50x speedup
- Enables type specialization
- Reduces hash table lookups

**Implementation Strategies:**
- Direct offset caching
- Hidden class transitions
- Method table caching
- Inline cache trees for complex patterns

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

# Chapter 20: Garbage Collector

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

# Chapter 21: JIT Compiler

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

# Chapter 22: Runtime System

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

## 22.4 Native Interface (FFI)

The FFI system is a critical component of the Gene runtime, providing seamless bidirectional interoperability with C and other native code. See Chapter 13 for the complete FFI design and usage.

### Calling C/Zig Functions:

**Type Marshalling:**
- Automatic conversion between Gene and C types
- NaN-boxing aware marshalling for numeric types
- String conversion with UTF-8 validation
- Struct packing/unpacking with alignment handling
- Array and pointer translation

**Calling Convention Adaptation:**
- Support for multiple calling conventions (cdecl, stdcall, fastcall)
- Register vs stack parameter passing
- Variadic function support
- Platform-specific ABI compliance

**GC Safe Points:**
- Automatic GC suspension during C calls
- Pinned memory regions for C access
- Safe point insertion for long-running C functions
- Stack scanning for conservative GC

**Error Handling:**
- errno preservation and translation
- Exception to error code mapping
- Signal handling during FFI calls
- Longjmp safety

### Embedding Gene:

**C API for Host Applications:**
```c
// Core runtime management
gene_runtime* gene_init(gene_config* config);
void gene_shutdown(gene_runtime* rt);

// Value creation and manipulation
gene_value* gene_make_int(gene_runtime* rt, int64_t n);
gene_value* gene_make_float(gene_runtime* rt, double f);
gene_value* gene_make_string(gene_runtime* rt, const char* s);
gene_value* gene_make_array(gene_runtime* rt, size_t capacity);

// Type checking and conversion
bool gene_is_int(gene_value* v);
int64_t gene_get_int(gene_value* v);
const char* gene_to_string(gene_runtime* rt, gene_value* v);

// Function calls
gene_value* gene_call(gene_runtime* rt, const char* fn_name,
                      int argc, gene_value** argv);
gene_value* gene_call_method(gene_runtime* rt, gene_value* obj,
                            const char* method, int argc, gene_value** argv);
```

**Callback Registration:**
- Function pointer wrapping with closure support
- Automatic parameter marshalling for callbacks
- Callback lifetime management
- Thread-safe callback invocation

**Memory Management Coordination:**
- Reference counting for shared objects
- GC root registration for C-held references
- Memory pressure notifications
- Custom allocator support

**Thread Safety:**
- Per-thread runtime contexts
- Lock-free message passing to Gene threads
- Safe concurrent FFI calls
- Thread-local storage integration

### FFI Performance Optimizations:

**Fast Path for Common Types:**
- Inline type checks and conversions
- Specialized paths for integers and floats
- String interning for repeated conversions
- Struct layout caching

**Zero-Copy Operations:**
- Direct memory access for arrays
- Shared memory buffers
- Memory-mapped file support
- DMA-friendly memory allocation

**JIT Integration:**
- Inline C function calls in JIT code
- Constant folding for known C functions
- Type-specialized call stubs
- Profile-guided optimization

## 22.5 Actor Runtime

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

# Chapter 23: Tooling and Development Environment

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

# Chapter 24: Gene as a Data Format

Gene's S-expression syntax makes it an excellent data interchange format, similar to JSON, YAML, or EDN. This chapter describes how Gene can be used purely as a data format, separate from its role as a programming language.

## 24.1 Data Format Overview

Gene data files use the same S-expression syntax as Gene code but are interpreted as pure data structures rather than executable code.

### Basic Data Types:
- **Integers**: `42`, `-17`, `0`
- **Floats**: `3.14`, `-0.5`, `1.0e10`
- **Strings**: `"hello"`, `"multi\nline"`
- **Symbols**: `name`, `user-id`, `+`
- **Booleans**: `true`, `false`
- **Nil**: `nil`
- **Arrays**: `[1 2 3]`, `["a" "b" "c"]`
- **Maps**: `{:name "John" :age 30}`
- **Genes**: `(person "John" 30)`, `(div ^class "container" (p "Hello"))`

### Gene Document Structure:
A Gene data file consists of one or more top-level expressions. Each expression represents a complete data structure.

```gene
# config.gene - A configuration file
(config
  (server
    ^host "localhost"
    ^port 8080
    ^ssl true)
  
  (database
    ^type "postgresql"
    ^host "db.example.com"
    ^port 5432
    ^name "myapp")
  
  (features
    ["auth" "api" "websocket"]))
```

## 24.2 Gene Data Parser

The Gene data parser is a specialized parser that reads Gene syntax and produces data structures without evaluating code.

### Key Differences from Code Parser:
1. **No Evaluation**: Expressions are not evaluated as function calls
2. **Symbol Preservation**: Symbols remain as symbols, not variable references
3. **Property Support**: Full support for property syntax (^prop)
4. **Document Mode**: Can parse multiple top-level expressions

### Parser API:
```gene
# Parse a single expression
(parse-data "(person ^name \"John\" ^age 30)")
# => (gene 'person {:name "John" :age 30} [])

# Parse a document (multiple expressions)
(parse-document "(config ...)\n(users ...)")
# => [(gene 'config ...) (gene 'users ...)]

# Parse from file
(parse-data-file "config.gene")
# => (gene 'config ...)
```

## 24.3 Data Format Use Cases

### Configuration Files:
```gene
# app.config.gene
(application
  ^name "MyApp"
  ^version "1.0.0"
  
  (dependencies
    (gene/core ^version "0.1.0")
    (gene/web ^version "0.2.0" ^features ["ssl" "websocket"]))
  
  (build
    ^output "dist/"
    ^optimize true
    ^target ["native" "wasm"]))
```

### Data Serialization:
```gene
# users.gene
(users
  (user ^id 1 ^name "Alice" ^email "alice@example.com"
    (preferences
      ^theme "dark"
      ^notifications true))
  
  (user ^id 2 ^name "Bob" ^email "bob@example.com"
    (preferences
      ^theme "light"
      ^notifications false)))
```

### DSL Data:
```gene
# ui-layout.gene
(layout ^type "vertical"
  (header ^height 60
    (logo ^align "left")
    (nav ^align "right"
      (link ^href "/home" "Home")
      (link ^href "/about" "About")))
  
  (main ^flex 1
    (content ^padding 20))
  
  (footer ^height 40
    (copyright "© 2024")))
```

## 24.4 Data Access API

Once parsed, Gene data structures can be accessed using a simple API:

```gene
# Given: data = (person ^name "John" ^age 30 (address ^city "NYC"))

# Access properties
data.name          # "John"
data.^name         # "John" (alternative syntax)
data[^name]        # "John" (map-style access)

# Access children
data.0             # First child: (address ^city "NYC")
data.-1            # Last child
data.children      # All children as array

# Access head/tag
data.head          # 'person
data.tag           # 'person (alias)

# Check structure
data.props         # {:name "John" :age 30}
data.prop-count    # 2
data.child-count   # 1

# Pattern matching
(match data
  (person ^name n ^age a) (print "Person:" n "age" a)
  _ "Not a person")
```

## 24.5 Comparison with Other Formats

### Advantages over JSON:
- **More concise**: No quotes for keys, less punctuation
- **Comments**: Native support for comments
- **Symbols**: Distinct from strings
- **Richer types**: Direct support for symbols, keywords
- **Properties**: Metadata without nesting

### Advantages over XML:
- **Much more concise**: No closing tags
- **Simpler**: No namespaces, schemas
- **More readable**: Less visual noise
- **Native data types**: Not everything is a string

### Similar to EDN:
- S-expression based
- Rich data types
- Extensible
- Human-readable

### Example Comparison:
```json
// JSON
{
  "type": "user",
  "name": "John",
  "age": 30,
  "tags": ["admin", "active"]
}
```

```xml
<!-- XML -->
<user name="John" age="30">
  <tag>admin</tag>
  <tag>active</tag>
</user>
```

```gene
# Gene
(user ^name "John" ^age 30
  tags ["admin" "active"])
```

## 24.6 Document Type

Gene data files are parsed into a special `GeneDocument` structure that represents a collection of top-level expressions. This provides a consistent way to handle Gene data files as a single entity.

```gene
# Input file: data.gene
(config ^name "myapp")
(user ^id 1 ^name "Alice")

# Parsed result:
(GeneDocument
  (config ^name "myapp")
  (user ^id 1 ^name "Alice"))
```

The GeneDocument type:
- Has "GeneDocument" as its head
- Contains all top-level expressions as children
- Can be serialized to JSON, printed, or manipulated like any other Gene data
- Provides a unified interface for working with Gene data files

## 24.7 Implementation Details

### Parser Modes:
1. **Code Mode**: Default mode, parses for execution
2. **Data Mode**: Parses as pure data structures
3. **Document Mode**: Parses multiple top-level forms

### Command Line Usage:
```bash
# Parse and pretty-print a data file
gene parse data.gene

# Parse and output as JSON
gene parse --json data.gene

# Parse and validate against schema
gene parse --schema schema.gene data.gene

# Parse from stdin
echo '(config ^port 8080)' | gene parse -
```

### Integration with Gene Code:
```gene
# Load data from file
(var config (parse-data-file "config.gene"))

# Convert data to code
(var code (data->code config))

# Evaluate data as code (careful!)
(var result (eval (data->code config)))

# Serialize data back to Gene format
(write-data-file "output.gene" config)
```

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

