# LLVM Code Generation

## Overview

The Hybrid compiler generates LLVM Intermediate Representation (IR) which is then optimized and compiled to machine code. This document describes how Hybrid constructs map to LLVM IR and the code generation process.

## LLVM Integration

### Build Configuration

The compiler automatically detects and links with LLVM:

```bash
# Automatic detection order:
1. llvm-config in PATH
2. Homebrew LLVM on macOS (/opt/homebrew/opt/llvm/bin/llvm-config)
3. System LLVM installation
```

### Required LLVM Components

- LLVM Core libraries (version 20+)
- Support for IR generation
- Target machine support for native code generation

## Type Mapping

Hybrid types map directly to LLVM types:

| Hybrid Type | LLVM Type | Description |
|-------------|-----------|-------------|
| `int` | `i32` | 32-bit signed integer |
| `float` | `float` | 32-bit IEEE floating point |
| `double` | `double` | 64-bit IEEE floating point |
| `char` | `i8` | 8-bit integer |
| `bool` | `i8` | 8-bit integer |
| `void` | `void` | No value |
| `string` | `ptr` | Pointer to i8 |
| `T[]` | `{ ptr, i32 }` | Struct with pointer and size |

## Memory Management

### Stack Allocation

All local variables use stack allocation via `alloca`:

```llvm
; int x = 42
%x = alloca i32, align 4
store i32 42, ptr %x, align 4
```

### Global Variables

Global variables are allocated as LLVM globals:

```llvm
; int globalCount = 0
@globalCount = global i32 0
```

### Arrays

Arrays are represented as structs containing a pointer and size:

```llvm
; int[] arr = [1, 2, 3]
%arraytmp = alloca i32, i32 3, align 4
; Initialize elements
%elem0 = getelementptr i32, ptr %arraytmp, i32 0
store i32 1, ptr %elem0, align 4
; ... more elements ...

; Create array struct
%arrayStruct = alloca { ptr, i32 }, align 8
%ptrField = getelementptr inbounds { ptr, i32 }, ptr %arrayStruct, i32 0, i32 0
store ptr %arraytmp, ptr %ptrField, align 8
%sizeField = getelementptr inbounds { ptr, i32 }, ptr %arrayStruct, i32 0, i32 1
store i32 3, ptr %sizeField, align 4
```

## Function Generation

### Function Definition

```c
int add(int x, int y) {
    return x + y
}
```

Generates:

```llvm
define i32 @add(i32 %a, i32 %b) {
entry:
  %a1 = alloca i32, align 4
  store i32 %a, ptr %a1, align 4
  %b2 = alloca i32, align 4
  store i32 %b, ptr %b2, align 4
  %a3 = load i32, ptr %a1, align 4
  %b4 = load i32, ptr %b2, align 4
  %addtmp = add i32 %a3, %b4
  ret i32 %addtmp
}
```

### Calling Convention

- Parameters passed by value
- Return values in registers
- Stack cleanup handled automatically

## Expression Code Generation

### Arithmetic Operations

Integer operations:
```llvm
%sum = add i32 %x, %y      ; Addition
%diff = sub i32 %x, %y     ; Subtraction
%prod = mul i32 %x, %y     ; Multiplication
%quot = sdiv i32 %x, %y    ; Signed division
```

Floating-point operations:
```llvm
%sum = fadd double %x, %y   ; Addition
%diff = fsub double %x, %y  ; Subtraction
%prod = fmul double %x, %y  ; Multiplication
%quot = fdiv double %x, %y  ; Division
```

### Comparison Operations

Integer comparisons:
```llvm
%eq = icmp eq i32 %x, %y    ; Equal
%ne = icmp ne i32 %x, %y    ; Not equal
%lt = icmp slt i32 %x, %y   ; Less than (signed)
%gt = icmp sgt i32 %x, %y   ; Greater than (signed)
%le = icmp sle i32 %x, %y   ; Less or equal (signed)
%ge = icmp sge i32 %x, %y   ; Greater or equal (signed)
```

Floating-point comparisons:
```llvm
%eq = fcmp oeq double %x, %y  ; Ordered equal
%lt = fcmp olt double %x, %y  ; Ordered less than
```

### Type Casting

Automatic type promotion:
```llvm
; int to double
%promoted = sitofp i32 %int_val to double

; double to int
%truncated = fptosi double %float_val to i32
```

## Control Flow Generation

### If-Else Statements

```c
if x > 0 {
    return 1
} else {
    return -1
}
```

Generates basic blocks with conditional branches:

```llvm
entry:
  %cmp = icmp sgt i32 %x, 0
  br i1 %cmp, label %then, label %else

then:
  ret i32 1

else:
  ret i32 -1
```

### While Loops

```c
while i < 10 {
    i = i + 1
}
```

Generates loop structure with phi nodes:

```llvm
entry:
  br label %loop

loop:
  %i = phi i32 [ %i_start, %entry ], [ %i_next, %loop_body ]
  %cmp = icmp slt i32 %i, 10
  br i1 %cmp, label %loop_body, label %exit

loop_body:
  %i_next = add i32 %i, 1
  br label %loop

exit:
  ; Continue after loop
```

### Foreach Loops

```c
for int num in numbers {
    sum = sum + num
}
```

Generates indexed iteration over array elements:

```llvm
entry:
  ; Extract array pointer and size
  %arrayPtr = extractvalue { ptr, i32 } %numbers, 0
  %arraySize = extractvalue { ptr, i32 } %numbers, 1
  
  ; Initialize loop counter
  %loopcounter = alloca i32, align 4
  store i32 0, ptr %loopcounter, align 4
  
  ; Create loop variable
  %num = alloca i32, align 4
  br label %forcond

forcond:
  %counter = load i32, ptr %loopcounter, align 4
  %loopcond = icmp slt i32 %counter, %arraySize
  br i1 %loopcond, label %forbody, label %forcont

forbody:
  ; Load current element
  %elemptr = getelementptr i32, ptr %arrayPtr, i32 %counter
  %elemval = load i32, ptr %elemptr, align 4
  store i32 %elemval, ptr %num, align 4
  
  ; Loop body code here
  ; ...
  
  br label %forinc

forinc:
  ; Increment counter
  %nextcounter = add i32 %counter, 1
  store i32 %nextcounter, ptr %loopcounter, align 4
  br label %forcond

forcont:
  ; Continue after loop
```

### Break and Skip Statements

#### Break Statement

```c
while condition {
    if done { break }
    // more code
}
```

Uses unconditional branch to exit block:

```llvm
whilebody:
  ; Check break condition
  %done = load i1, ptr %done, align 1
  br i1 %done, label %break_to_exit, label %continue_body

break_to_exit:
  br label %whilecont  ; Jump to loop exit

continue_body:
  ; Rest of loop body
```

#### Skip Statement

```c
for int n in nums {
    if n % 2 == 0 { skip }
    // process odd numbers
}
```

Uses unconditional branch to loop increment:

```llvm
forbody:
  ; Check skip condition
  %mod = srem i32 %n, 2
  %is_even = icmp eq i32 %mod, 0
  br i1 %is_even, label %skip_to_inc, label %continue_body

skip_to_inc:
  br label %forinc  ; Jump to increment

continue_body:
  ; Rest of loop body
```

### Loop Control Flow Stack

The compiler maintains stacks for break and skip targets:

- `LoopExitBlocks`: Stack of exit blocks for break statements
- `LoopContinueBlocks`: Stack of continue blocks for skip statements

Each loop pushes its blocks on entry and pops on exit, enabling proper nested loop behavior.

## Optimization

### Automatic Optimizations

The LLVM backend performs several optimizations:

1. **Constant folding**: `2 + 3` -> `5`
2. **Dead code elimination**: Removes unreachable code
3. **Common subexpression elimination**: Reuses computed values
4. **Peephole optimizations**: Local instruction improvements

### Optimization Levels

Future support for optimization flags:
- `-O0`: No optimization (debug builds)
- `-O1`: Basic optimizations
- `-O2`: Standard optimizations
- `-O3`: Aggressive optimizations

## String Handling

### String Constants

String literals become global constants:

```llvm
@.str = private unnamed_addr constant [6 x i8] c"Hello\00"
```

### Null Strings

`null` initialization for strings:

```llvm
; string s = null
%s = alloca ptr, align 8
store ptr null, ptr %s, align 8
```

## Array Operations

### Array Indexing

```c
arr[i]
```

Extracts pointer from array struct and uses `getelementptr`:

```llvm
; Extract array pointer from struct
%arr_struct = load { ptr, i32 }, ptr %arr, align 8
%arr_ptr = extractvalue { ptr, i32 } %arr_struct, 0

; Calculate element address
%elem_ptr = getelementptr i32, ptr %arr_ptr, i32 %i
%value = load i32, ptr %elem_ptr
```

### Array Assignment

```c
arr[i] = value
```

Extracts pointer and stores at calculated address:

```llvm
; Extract array pointer from struct
%arr_struct = load { ptr, i32 }, ptr %arr, align 8
%arr_ptr = extractvalue { ptr, i32 } %arr_struct, 0

; Store at element address
%elem_ptr = getelementptr i32, ptr %arr_ptr, i32 %i
store i32 %value, ptr %elem_ptr
```

## Debug Information

### Source Location Tracking

Future enhancement to include debug metadata:

```llvm
!dbg !{line: 10, column: 5, file: "test.hy"}
```

### Variable Names

Currently, LLVM uses numbered registers. Future support for named values in debug builds.

## Error Handling

### Code Generation Errors

The compiler reports errors for:
- Type mismatches
- Undefined variables
- Undefined functions
- Invalid operations

### Runtime Errors

The generated IR includes runtime guards for array bounds. Each indexed access branches to an error block that calls `abort()` when the index is negative or exceeds the recorded array size.

Future enhancements:
- Null pointer checks
- Division by zero checks
- Safer handling for raw pointer arithmetic

## Examples

### Complete Function Example

Input:
```c
int factorial(int n) {
    if n <= 1 {
        return 1
    }
    return n * factorial(n - 1)
}
```

Generated LLVM IR:
```llvm
define i32 @factorial(i32 %n) {
entry:
  %n1 = alloca i32, align 4
  store i32 %n, ptr %n1, align 4
  %n2 = load i32, ptr %n1, align 4
  %cmptmp = icmp sle i32 %n2, 1
  br i1 %cmptmp, label %then, label %else

then:
  ret i32 1

else:
  %n3 = load i32, ptr %n1, align 4
  %n4 = load i32, ptr %n1, align 4
  %subtmp = sub i32 %n4, 1
  %calltmp = call i32 @factorial(i32 %subtmp)
  %multmp = mul i32 %n3, %calltmp
  ret i32 %multmp
}
```

### Top-level Expression

Input:
```c
2 + 3 * 4
```

Generated wrapper function:
```llvm
define void @__anon_expr() {
entry:
  ret i32 14  ; Constant folded
}
```
