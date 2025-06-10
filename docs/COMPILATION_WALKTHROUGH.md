# Gene Compilation Pipeline: Step-by-Step Walkthrough

This document explains how `examples/default.gene` is parsed, compiled, and executed through Gene's 4-stage compilation pipeline.

## Source Code: default.gene

```gene
#!/usr/bin/env gene run

(print "Output from default.gene")
(print (1 + 2))
```

This simple Gene program demonstrates:
- Function calls (`print`)
- String literals (`"Output from default.gene"`)
- Arithmetic expressions (`1 + 2`)
- Lisp-like syntax with parentheses

## Pipeline Overview

Gene uses a 4-stage compilation pipeline:
1. **Parsing** → AST (Abstract Syntax Tree)
2. **AST → HIR** (High-level Intermediate Representation)
3. **HIR → MIR** (Mid-level Intermediate Representation)  
4. **MIR → Bytecode** (Stack-based bytecode for VM execution)

## Stage 1: Parsing (Source → AST)

**Entry Point:** `src/frontend/parser.zig:parseGeneSource()`

### Tokenization
The parser first tokenizes the source into tokens:
```
Token { kind: LParen }
Token { kind: Ident("print") }
Token { kind: String("Output from default.gene") }
Token { kind: RParen }
Token { kind: LParen }
Token { kind: Ident("print") }
Token { kind: LParen }
Token { kind: Int(1) }
Token { kind: Ident("+") }
Token { kind: Int(2) }
Token { kind: RParen }
Token { kind: RParen }
```

### AST Generation
The tokens are parsed into an Abstract Syntax Tree:
```
AstNode.Expression {
  .Call = {
    .function = "print",
    .args = [String("Output from default.gene")]
  }
}
AstNode.Expression {
  .Call = {
    .function = "print", 
    .args = [
      Call {
        .function = "+",
        .args = [Int(1), Int(2)]
      }
    ]
  }
}
```

**Memory Management:** Uses arena allocator for AST nodes, cleaned up by pipeline.

## Stage 2: AST → HIR (High-level IR)

**Entry Point:** `src/transforms/ast_to_hir.zig:convert()`

HIR normalizes the AST by:
- Converting all operators to function calls
- Creating explicit main function
- Simplifying expression nesting

### HIR Output:
```
HIR.Program {
  functions: [
    HIR.Function {
      name: "main",
      params: [],
      body: [
        HIR.Statement.Expression {
          .call = {
            .function = "print",
            .args = [HIR.Expression.literal.string("Output from default.gene")]
          }
        },
        HIR.Statement.Expression {
          .call = {
            .function = "print",
            .args = [
              HIR.Expression.call {
                .function = "+",
                .args = [
                  HIR.Expression.literal.int(1),
                  HIR.Expression.literal.int(2)
                ]
              }
            ]
          }
        }
      ]
    }
  ]
}
```

**Key Transformations:**
- `(1 + 2)` becomes explicit function call `+(1, 2)`
- Top-level expressions wrapped in main function
- String literals duplicated for memory safety

## Stage 3: HIR → MIR (Mid-level IR)

**Entry Point:** `src/transforms/hir_to_mir.zig:convert()`

MIR flattens control flow into basic blocks and introduces temporary variables:

### MIR Output:
```
MIR.Program {
  functions: [
    MIR.Function {
      name: "main",
      basic_blocks: [
        MIR.BasicBlock {
          instructions: [
            // First print statement
            MIR.Instruction.LoadConstant { 
              dest: %0, 
              value: "Output from default.gene" 
            },
            MIR.Instruction.Call { 
              dest: %1, 
              function: "print", 
              args: [%0] 
            },
            
            // Second print statement with arithmetic
            MIR.Instruction.LoadConstant { 
              dest: %2, 
              value: 1 
            },
            MIR.Instruction.LoadConstant { 
              dest: %3, 
              value: 2 
            },
            MIR.Instruction.Call { 
              dest: %4, 
              function: "+", 
              args: [%2, %3] 
            },
            MIR.Instruction.Call { 
              dest: %5, 
              function: "print", 
              args: [%4] 
            }
          ]
        }
      ]
    }
  ]
}
```

**Key Transformations:**
- Expressions flattened to sequence of instructions
- Temporary variables (%0, %1, etc.) introduced
- Control flow organized into basic blocks
- Function calls become explicit instructions

## Stage 4: MIR → Bytecode

**Entry Point:** `src/transforms/mir_to_bytecode.zig:convert()`

MIR is converted to stack-based bytecode for the virtual machine:

### Bytecode Output:
```
Function "main" {
  Instructions: [
    LoadConst "Output from default.gene"  // Push string to stack
    Call "print" 1                        // Call print with 1 argument
    Pop                                    // Clean up result
    
    LoadConst 1                           // Push 1 to stack  
    LoadConst 2                           // Push 2 to stack
    Call "+" 2                            // Call + with 2 arguments
    Call "print" 1                        // Call print with 1 argument  
    Pop                                    // Clean up result
  ]
}
```

**Key Transformations:**
- Temporary variables eliminated (stack-based)
- Load constants become `LoadConst` instructions
- Function calls become `Call` instructions with argument count
- Stack management with `Pop` instructions

## Stage 5: Execution

**Entry Point:** `src/runtime.zig:execute()` → `src/backend/vm.zig:execute()`

The virtual machine executes the bytecode:

### VM Execution Trace:
```
Stack: []
LoadConst "Output from default.gene"
Stack: ["Output from default.gene"]

Call "print" 1
  -> Built-in print function called with "Output from default.gene"
  -> Output: "Output from default.gene"
Stack: [nil]  // print returns nil

Pop
Stack: []

LoadConst 1
Stack: [1]

LoadConst 2  
Stack: [1, 2]

Call "+" 2
  -> Built-in + function called with (1, 2)
  -> Returns: 3
Stack: [3]

Call "print" 1
  -> Built-in print function called with 3
  -> Output: "3"
Stack: [nil]

Pop
Stack: []
```

**VM Features:**
- Stack-based execution model
- Built-in functions (print, +, -, *, etc.)
- Automatic memory management
- Error handling and stack traces

## Memory Management

### During Compilation:
- **AST Phase:** Arena allocator (freed by pipeline)
- **HIR/MIR:** Individual allocators per stage
- **Bytecode:** Persistent until execution complete

### During Execution:
- **VM Stack:** Managed by VM, grows/shrinks as needed
- **String literals:** Stored in bytecode, referenced by stack
- **Function results:** Temporary on stack, cleaned by Pop instructions

## Debug Output

When running with `--debug` flag, Gene shows each stage:

```bash
./zig-out/bin/gene run examples/default.gene --debug
```

Output includes:
- **AST:** Tree structure of parsed code
- **HIR:** Normalized high-level representation  
- **MIR:** Flattened instruction sequence
- **Bytecode:** Final VM instructions
- **Execution:** Step-by-step VM trace

This pipeline design allows for:
- **Modularity:** Each stage has clear responsibilities
- **Optimization:** Multiple levels for different optimizations
- **Debugging:** Visibility into each transformation
- **Extensibility:** Easy to add new language features