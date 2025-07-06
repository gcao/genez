# Unified Gene VM Design

## Overview

This document describes the VM instruction set and architecture designed specifically for the unified Gene data format. The VM is a register-based machine with specialized support for Gene operations, method dispatch, and common optimization patterns.

## VM Architecture

### Registers and Stack

```zig
pub const VM = struct {
    // General purpose registers
    registers: [256]Value,
    
    // Special registers
    ip: usize,              // Instruction pointer
    sp: usize,              // Stack pointer
    fp: usize,              // Frame pointer
    
    // Call stack
    call_stack: []CallFrame,
    
    // Operand stack for complex operations
    stack: []Value,
    
    // Global data
    constants: []Value,
    globals: []Value,
    modules: ModuleRegistry,
    
    // Inline cache
    inline_caches: []InlineCache,
    
    // GC data
    gc: GarbageCollector,
};

pub const CallFrame = struct {
    return_ip: usize,
    return_fp: usize,
    function: *Function,
    locals: []Value,
    
    // For super calls
    current_class: ?*Class,
};
```

### Value Representation

```zig
pub const Value = union(enum) {
    // Immediate values (fit in 64 bits)
    Nil,
    Bool: bool,
    Int: i48,           // 48-bit integers
    Float: f64,
    SmallString: [6]u8, // Strings up to 6 bytes
    
    // Heap-allocated values
    String: *String,
    Symbol: *Symbol,
    Array: *Array,
    Map: *Map,
    Gene: *Gene,
    Class: *Class,
    Object: *Object,
    Function: *Function,
    
    // VM internal
    Register: u8,
    ReturnAddress: usize,
};

pub const Gene = struct {
    header: ObjectHeader,
    head: Value,
    props: PropertyStorage,
    children: ChildrenStorage,
};

// Optimized storage for properties
pub const PropertyStorage = union(enum) {
    None,
    Single: struct { key: Value, value: Value },
    Few: [4]?struct { key: Value, value: Value },
    Many: *std.StringHashMap(Value),
};

// Optimized storage for children
pub const ChildrenStorage = union(enum) {
    None,
    Single: Value,
    Few: [4]Value,
    Many: []Value,
};
```

## Instruction Set

### Instruction Format

```zig
pub const Instruction = packed struct {
    opcode: Opcode,      // 8 bits
    dst: u8,            // 8 bits - destination register
    src1: u8,           // 8 bits - source register 1
    src2: u8,           // 8 bits - source register 2
    extra: u32,         // 32 bits - immediate or additional data
};
```

### Basic Operations

```zig
pub const Opcode = enum(u8) {
    // Constants and register operations
    LoadConst,          // dst = constants[extra]
    LoadGlobal,         // dst = globals[extra]
    StoreGlobal,        // globals[extra] = src1
    Move,               // dst = src1
    
    // Stack operations (for varargs, etc.)
    Push,               // stack.push(src1)
    Pop,                // dst = stack.pop()
    
    // Gene creation
    MakeGene,           // dst = Gene(src1=head, props, children)
                        // extra = (prop_count << 16) | child_count
                        // followed by prop pairs and children
    
    MakeGeneConst,      // dst = Gene from constant template[extra]
    
    // Gene access
    GetHead,            // dst = src1.head
    GetProp,            // dst = src1.props[src2]
                        // extra = inline cache slot
    SetProp,            // dst = src1.with_prop(src2, src3)
                        // extra = inline cache slot
    GetChild,           // dst = src1.children[src2]
    SetChild,           // dst = src1.with_child(src2, src3)
    AddChild,           // dst = src1.with_new_child(src2)
    
    // Optimized Gene operations for common patterns
    GetPropConst,       // dst = src1.props[constants[extra]]
    SetPropConst,       // dst = src1.with_prop(constants[extra], src2)
    
    // Collections
    MakeArray,          // dst = Array(size=extra)
                        // followed by elements
    MakeMap,            // dst = Map(size=extra)
                        // followed by key-value pairs
    
    ArrayGet,           // dst = src1[src2]
    ArraySet,           // src1[src2] = src3 (mutable)
    ArrayPush,          // src1.push(src2) (mutable)
    ArrayPop,           // dst = src1.pop() (mutable)
    ArrayLength,        // dst = src1.length
    
    MapGet,             // dst = src1[src2]
    MapSet,             // src1[src2] = src3 (mutable)
    MapHas,             // dst = src1.has(src2)
    MapDelete,          // src1.delete(src2) (mutable)
    
    // Strings
    MakeString,         // dst = String(parts...)
                        // extra = part count
    StringConcat,       // dst = src1 + src2
    StringLength,       // dst = src1.length
    StringSlice,        // dst = src1[src2:src3]
    
    // OOP - Class operations
    DefineClass,        // dst = Class(name, parent, fields, methods)
                        // extra = constant index for class data
    GetClass,           // dst = src1.__class__
    
    // OOP - Method dispatch
    LookupMethod,       // dst = src1.__class__.methods[src2]
                        // extra = inline cache slot
    CallMethod,         // dst = method(receiver, args...)
                        // src1 = method, src2 = receiver
                        // extra = (arg_count << 16) | cache_slot
    CallDirect,         // dst = function(args...)
                        // src1 = function constant index
                        // extra = arg_count
    CallSuper,          // dst = super.method(receiver, args...)
                        // extra = (method_name_idx << 16) | arg_count
    
    // OOP - Instance operations  
    NewInstance,        // dst = Object(src1=class)
    GetField,           // dst = src1.fields[src2]
                        // extra = field offset (if known)
    SetField,           // src1.fields[src2] = src3
                        // extra = field offset (if known)
    GetFieldConst,      // dst = src1.fields[constants[extra]]
    SetFieldConst,      // src1.fields[constants[extra]] = src2
    
    // Arithmetic (specialized for performance)
    AddInt,             // dst = src1 + src2 (integers)
    SubInt,             // dst = src1 - src2
    MulInt,             // dst = src1 * src2
    DivInt,             // dst = src1 / src2
    ModInt,             // dst = src1 % src2
    NegInt,             // dst = -src1
    
    AddFloat,           // dst = src1 + src2 (floats)
    SubFloat,           // dst = src1 - src2
    MulFloat,           // dst = src1 * src2
    DivFloat,           // dst = src1 / src2
    NegFloat,           // dst = -src1
    
    // Generic arithmetic (with method dispatch)
    Add,                // dst = src1.+(src2)
    Sub,                // dst = src1.-(src2)
    Mul,                // dst = src1.*(src2)
    Div,                // dst = src1./(src2)
    
    // Comparison
    Equal,              // dst = src1 == src2
    NotEqual,           // dst = src1 != src2
    Less,               // dst = src1 < src2
    Greater,            // dst = src1 > src2
    LessEqual,          // dst = src1 <= src2
    GreaterEqual,       // dst = src1 >= src2
    
    // Type operations
    IsType,             // dst = src1 is src2
    AsType,             // dst = src1 as src2 (with check)
    
    // Control flow
    Jump,               // ip = extra
    JumpIf,             // if src1 then ip = extra
    JumpIfNot,          // if !src1 then ip = extra
    JumpEqual,          // if src1 == src2 then ip = extra
    JumpNotEqual,       // if src1 != src2 then ip = extra
    JumpLess,           // if src1 < src2 then ip = extra
    JumpGreater,        // if src1 > src2 then ip = extra
    
    // Function calls
    Call,               // dst = function(args...)
                        // src1 = function
                        // extra = arg_count
    Return,             // return src1
    
    // Exception handling
    Throw,              // throw src1
    EnterTry,           // enter try block, catch at extra
    ExitTry,            // exit try block
    LandingPad,         // dst = caught exception
    
    // Debugging
    Print,              // print(src1)
    Breakpoint,         // debugger breakpoint
};
```

## Inline Caching

The VM uses inline caching to optimize property access and method dispatch:

```zig
pub const InlineCache = struct {
    key: Value,              // Property name or method name
    class: ?*Class,          // Last seen class
    offset: ?u32,            // Property/method offset
    
    // For polymorphic sites
    poly_cache: ?*PolyCache,
};

pub const PolyCache = struct {
    entries: [4]struct {
        class: *Class,
        offset: u32,
    },
    count: u8,
};
```

## Example: Method Call Compilation

```gene
# Gene source
(point .distance other)

# Bytecode
LoadLocal    R1, 0        // R1 = point
LoadLocal    R2, 1        // R2 = other
LoadConst    R3, "distance"
LookupMethod R4, R1, R3, cache_slot=10
CallMethod   R0, R4, R1, arg_count=1, cache_slot=11

# After inline cache hits
LoadLocal    R1, 0
LoadLocal    R2, 1
CallDirect   R0, Point_distance, arg_count=2  // Devirtualized
```

## Memory Layout

### Object Header

```zig
pub const ObjectHeader = packed struct {
    gc_bits: u8,            // GC mark bits, generation, etc.
    type_tag: TypeTag,      // Quick type identification
    class: *Class,          // Class pointer
    size: u32,              // Object size for GC
};
```

### Optimizations

1. **Small String Optimization**: Strings ≤ 6 bytes are stored inline
2. **Small Gene Optimization**: Genes with ≤ 2 props and ≤ 2 children use compact representation
3. **Integer Tagging**: Small integers are stored inline in Value
4. **Inline Caching**: Monomorphic property/method access is optimized
5. **Method Devirtualization**: Known receiver types bypass method lookup

## Garbage Collection

The VM uses a generational garbage collector with precise stack scanning:

```zig
pub const GCInfo = struct {
    stack_map: StackMap,         // Which stack slots contain references
    register_map: RegisterMap,   // Which registers contain references
    safe_points: []usize,        // Instruction offsets where GC can run
};
```

## Performance Considerations

1. **Register Allocation**: The first 16 registers are fast-path
2. **Instruction Fusion**: Common patterns can be fused (e.g., Load + Call)
3. **Inline Threading**: Computed goto for instruction dispatch
4. **Specialization**: Hot functions can be specialized for known types
5. **Escape Analysis**: Stack allocate non-escaping Genes

## Integration with JIT

The VM is designed to work with a future JIT compiler:

```zig
pub const ProfileData = struct {
    call_counts: std.AutoHashMap(usize, u64),
    type_feedback: std.AutoHashMap(usize, TypeSet),
    branch_taken: std.AutoHashMap(usize, bool),
};
```

Hot functions can be compiled to native code while maintaining the same semantics as the VM.