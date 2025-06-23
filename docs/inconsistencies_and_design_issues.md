# Gene Language: Inconsistencies and Design Issues

This document lists implementation inconsistencies and design issues found in the Gene language implementation as of 2025-01-19. Each issue is numbered for easy reference and response.

## 1. Module System vs Namespace System Confusion

### 1.1 Current State
- **4 different module-related files**: `module.zig`, `module_system.zig`, `module_registry.zig`, `module_resolver.zig`
- Modules have namespaces as members, but namespaces can also be declared independently via `(ns name body)`
- The VM references `module.namespace.members` but the parser creates standalone namespace declarations
- Module resolution uses slash notation (`module/member`) which conflicts with field access notation (`obj/field`)

### 1.2 Questions
- Should modules and namespaces be the same thing?
- If different, what is the exact relationship between them?
- Should we keep both concepts or merge them?

**Your response:**
* Module is the unit of code organization. modules belong to packages. packages can have depencies.
* Namespace is a way to group related functions, variables, types etc.
* Each module has its own root namespace.
* Namespaces can be nested within other namespaces.
* Module and namespace are different things.

---

## 2. Memory Management Strategy

### 2.1 Current State
Three different approaches coexist:
- Arena allocators for parsing (cleaned up after compilation)
- Manual memory management with explicit `deinit()` calls everywhere
- A garbage collector implementation that exists but isn't integrated with VM

### 2.2 Specific Issues
- Some values are cloned, others aren't, with no clear ownership model
- The GC exists (`gc.zig`) but the VM doesn't use it properly
- Mix of arena-allocated and individually-allocated memory makes cleanup complex
- Comments like "Note: registry itself is allocated in the arena, so don't destroy it" indicate confusion

### 2.3 Questions
- Should Gene be garbage collected, use arenas, or manual memory management?
- How should ownership work between compilation and runtime?
- Should the GC be removed until Phase 4 as originally planned?

**Your response:**
* Gene developers should not need to worry about memory management in general. GC is the way to go.
* Compilation and execution are interleaved. GC probably needs to be used in both compilation and execution.
* Efficient GC is necessary for ease of use and performance.

---

## 3. Field Access Syntax Using Slash (`/`)

### 3.1 Current State
```gene
obj/field      # Field access
math/pi        # Module member access
10/2           # Division (ambiguous in some contexts)
(/field)       # Implicit self field access
```

### 3.2 Issues
- Slash is overloaded for both field access and division
- Parser has to use context to disambiguate
- Different from most languages that use `.` for field access

### 3.3 Questions
- Why was `/` chosen for field access instead of `.`?
- Should we change to `.` for both methods and fields?
- How should module member access be distinguished from field access?

**Your response:**
* / is for accessing properties, members. `.` is for method calls.
* member and field access are similar concepts.

---

## 4. Method Call vs Field Access Syntax

### 4.1 Current State
- Methods: `(obj .method args)` or `(.method args)` for implicit self
- Fields: `obj/field` or `(/field)` for implicit self
- Inconsistent syntax between the two

### 4.2 Questions
- Why different syntax for methods (`.`) vs fields (`/`)?
- Should they be unified?

**Your response:**
* fields are like properties of an object/instance.
* methods are like functions that are associated with an object/instance.
* `.` is for methods because it is like calling a function.
* `/` is for fields because it is like accessing a property.

---

## 5. Type System Design Mismatch

### 5.1 Current State
- Type checker implemented but disabled: "Type checking disabled - too strict for current language features"
- Type checker assumes static typing but Gene is meant to be gradually typed
- `print` registered as taking 1 argument but used variadically
- No support for `Any` type in type checking

### 5.2 Questions
- Should the current type checker be scrapped and rewritten for gradual typing?
- How should variadic functions be handled in the type system?
- What is the path to enable type checking?

**Your response:**
* We want to support gradual typing. Variables without type annotations are of type `Any`.
* variadic functions are of type `(... :Any => :Any)`.

---

## 6. Two Function Types (fn vs fnx)

### 6.1 Current State
- `fn` - supposedly for named functions
- `fnx` - supposedly for anonymous functions
- But both can actually be anonymous

### 6.2 Questions
- What is the real distinction between `fn` and `fnx`?
- Should we keep both or merge them?

**Your response:**
* `fn` is for named functions.
* `fnx` is for anonymous functions. It is a very common pattern to pass a function as an argument to another function.

---

## 7. Import Syntax Variations

### 7.1 Current State
```gene
(import "module")                    # Basic
(import "module" => alias)           # Alias
(import "module" [a b c])            # Specific items
(import "module" [a => a2 b => b2])  # Renamed items
(import "module" *)                  # All items
```

### 7.2 Questions
- Are all these variations necessary?
- Should we simplify to fewer forms?

**Your response:**
Let's start with these two:
(import "module")                    # Basic
(import "module" [a b c])            # Specific items

---

## 8. Parser Special Cases

### 8.1 Current State
The parser is full of special cases:
- "Check for special _ parameter (without brackets)"
- "Check if this is a method call pattern"
- "Check if this is a simple field access pattern"
- "Check if this is an assignment pattern"

### 8.2 Questions
- Should the grammar be made more uniform to reduce special cases?
- Which special cases are essential vs accidental complexity?

**Your response:**
These are must:
- "Check if this is a method call pattern"
These are not valid logic:
- "Check if this is a simple field access pattern"
  (/field)  should be interpreted as calling self/field
- "Check if this is an assignment pattern"
  (/field = value) should be implemented later. For now it should be written as (= /field value)

---

## 9. VM Implementation Patterns

### 9.1 Current State
- Giant switch statement with repeated code for each operator
- No abstraction for common patterns like binary operations
- Magic numbers for register allocation
- Each arithmetic operator has ~30 lines of nearly identical code

### 9.2 Questions
- Should we refactor to use a more table-driven approach?
- How can we reduce code duplication in operator handling?

**Your response:**
Let's use the best practices in an interpreting VM.
Code duplication is ok for now.

---

## 10. Error Handling Strategy

### 10.1 Current State
- Different error enums for parser, VM, type checker
- No unified error reporting mechanism
- Some errors print to debug log, others are returned
- No source location tracking for runtime errors

### 10.2 Questions
- Should we have a unified error type?
- How should source locations be preserved through compilation?
- What's the intended error reporting strategy?

**Your response:**
* Yes let's have a unified error type.
* Source locations should be preserved through compilation.
* The error reporting strategy should be:
  * Print the error message to stderr.
  * Include the source location in the error message.
  * Include the error type in the error message.
  * Include the error message in the error message.

---

## 11. Half-Implemented Features

### 11.1 Current State
- LIR stage exists but unused (pipeline skips from MIR to bytecode)
- Pattern matching partially implemented
- Macro system minimal
- Import system doesn't actually load module files
- Property syntax (`^prop`) parsed but not executed

### 11.2 Questions
- Should incomplete features be removed until properly implemented?
- Which features are priority for Phase 1 completion?

**Your response:**
* We can remove LIR for now.
* Macro is a must for phase 1.
* Import is a must for phase 1.
* Property syntax is a must for phase 1.

---

## 12. Multiple Module System Implementations

### 12.1 Current State
Four files suggest multiple attempts:
- `module.zig` - Complex module with imports, namespace, AST nodes
- `module_system.zig` - Different module system
- `module_registry.zig` - Currently used registry
- `module_resolver.zig` - Module resolution logic

### 12.2 Questions
- Which module system should be kept?
- Should the others be deleted?

**Your response:**
* Keep one implementation that handles the module loading, compilation, and execution.
* Delete the other implementations.

---

## 13. Private Function Syntax (`fn-`)

### 13.1 Current State
- Design shows `fn-` for private functions
- Parser doesn't recognize this syntax
- No visibility/export system implemented

### 13.2 Questions
- Is `fn-` the final syntax for private functions?
- When should visibility be implemented?

**Your response:**
Let's skip private functions for now.

---

## 14. Unclear Ownership in Module Loading

### 14.1 Current State
```zig
// Example of ownership confusion:
// Who owns the module registry? The compiler? The runtime? The VM?
// When should modules be freed?
```

### 14.2 Questions
- Who owns loaded modules?
- When should module memory be freed?
- Should modules be cached permanently?

**Your response:**
* Modules are owned by the compiler until they are executed.
* Modules are then owned by the runtime.
* Modules are freed when the runtime is freed.
* Modules are cached permanently.

---

## Summary

Please provide your thoughts on each numbered issue. Your responses will help clarify the design intent and guide refactoring efforts to make the implementation more consistent and well-rounded.