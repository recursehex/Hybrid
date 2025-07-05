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
| `bool` | `i1` | 1-bit integer |
| `void` | `void` | No value |
| `string` | `ptr` | Pointer to i8 |
| `T[]` | `ptr` | Pointer to element type T |

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

Arrays are allocated on the stack with explicit size:

```llvm
; int[] arr = [1, 2, 3]
%arr = alloca [3 x i32], align 4
; Initialize elements using getelementptr
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

## Optimization

### Automatic Optimizations

The LLVM backend performs several optimizations:

1. **Constant folding**: `2 + 3` → `5`
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

Uses `getelementptr` for address calculation:

```llvm
%idx_ext = sext i32 %i to i64
%elem_ptr = getelementptr i32, ptr %arr, i64 %idx_ext
%value = load i32, ptr %elem_ptr
```

### Array Assignment

```c
arr[i] = value
```

Generates:

```llvm
%idx_ext = sext i32 %i to i64
%elem_ptr = getelementptr i32, ptr %arr, i64 %idx_ext
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

Currently no runtime error checking. Future enhancements:
- Array bounds checking
- Null pointer checks
- Division by zero checks

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