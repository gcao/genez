# Gene Unified Format Quick Reference

## Core Format

Everything in Gene is represented as:
```
(type ^prop1 value1 ^prop2 value2 child1 child2 ...)
```

## Primitive Representations

### Literals
```gene
42                  →  (Int ^value 42)
3.14               →  (Float ^value 3.14)
"hello"            →  (String ^value "hello")
true               →  (Bool ^value true)
nil                →  (Nil)
:symbol            →  (Symbol ^name "symbol")
```

### Collections
```gene
[1 2 3]            →  (Array 1 2 3)
{^x 1 ^y 2}        →  (Map ^x 1 ^y 2)
#{1 2 3}           →  (Set 1 2 3)
```

## Expressions

### Function Calls
```gene
(f x y)            →  (Call ^target f x y)
(+ 1 2)            →  (Call ^target + 1 2)
```

### Method Calls
```gene
(obj .method arg)  →  (MethodCall ^receiver obj ^method method arg)
(x .+ y)           →  (MethodCall ^receiver x ^method + y)
("hello" .length)  →  (MethodCall ^receiver "hello" ^method length)
```

### Property Access
```gene
obj/prop           →  (PropertyAccess ^object obj ^property prop)
                   →  (obj .get_member "prop")  # Compiles to method call
```

### Assignment
```gene
(= x 42)           →  (Assign ^target x 42)
(= obj/prop val)   →  (obj .set_member "prop" val)  # Compiles to method call
```

## Definitions

### Variables
```gene
(var x 42)         →  (Var ^name x 42)
(let x 42)         →  (Let ^name x 42)
```

### Functions
```gene
(fn add [x y]      →  (Function ^name add ^params [x y]
  (+ x y))              (+ x y))

(fn [x] (* x x))   →  (Function ^params [x] (* x x))  # Anonymous
```

### Classes
```gene
(class Point       →  (Class ^name Point
  ^fields [x y]         ^fields [x y]
  (.fn distance [other]  (Method ^name distance ^params [other]
    ...))                  ...))
```

### Methods
```gene
(.fn method [x]    →  (Method ^name method ^params [x]
  body)                 body)
```

## Control Flow

### Conditionals
```gene
(if test           →  (If test then else)
  then
  else)
```

### Pattern Matching
```gene
(match value       →  (Match value
  pattern1 expr1        (Case pattern1 expr1)
  pattern2 expr2)       (Case pattern2 expr2))
```

### Loops
```gene
(for [x coll]      →  (For ^binding x ^collection coll
  body)                 body)

(while test        →  (While test body)
  body)
```

## Properties on Expressions

Any expression can have properties:
```gene
(add ^checked true 1 2)       # Call with property
(loop ^parallel true ...)     # Loop with property
[1 2 3] ^immutable true      # Array with property
```

## Macros

```gene
($ macro-name args)   →  (Macro ^name macro-name args)
(defmacro when [test body]
  ~(if ,test ,body nil))
```

## Quote Forms

```gene
'expr              →  (Quote expr)
~expr              →  (Quasiquote expr)
,expr              →  (Unquote expr)
,@expr             →  (UnquoteSplice expr)
`symbol            →  (Symbol ^name "symbol")  # Backtick quote
```

## Module System

```gene
(import foo)       →  (Import ^module foo)
(import foo.bar)   →  (Import ^module foo.bar)
(import {x y} from foo)  →  (Import ^names [x y] ^from foo)
(export x)         →  (Export x)
```

## Special Forms

```gene
(do expr1 expr2)   →  (Do expr1 expr2)
(throw exception)  →  (Throw exception)
(try ... catch ... finally ...)
```

## Implementation Notes

1. The parser generates Gene objects directly - no separate AST
2. All operations become method calls on objects
3. Properties are first-class and can be attached to any expression
4. The unified format simplifies metaprogramming
5. Pattern matching works on the Gene structure itself