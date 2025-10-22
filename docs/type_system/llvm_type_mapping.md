# LLVM Type Mapping

Hybrid types map directly to LLVM types:

| Hybrid Type | LLVM IR Type | Notes |
|-------------|--------------|-------|
| `int` | `i32` | 32-bit signed integer |
| `uint` | `i32` | 32-bit unsigned integer |
| `short` | `i16` | 16-bit signed integer |
| `ushort` | `i16` | 16-bit unsigned integer |
| `long` | `i64` | 64-bit signed integer |
| `ulong` | `i64` | 64-bit unsigned integer |
| `byte` | `i8` | 8-bit unsigned integer |
| `sbyte` | `i8` | 8-bit signed integer |
| `float` | `float` | 32-bit IEEE float |
| `double` | `double` | 64-bit IEEE float |
| `char` | `i16` | 16-bit Unicode character |
| `schar` | `i8` | 8-bit signed character |
| `lchar` | `i32` | 32-bit Unicode character |
| `bool` | `i8` | 8-bit integer |
| `void` | `void` | No value |
| `string` | `ptr` | Pointer to UTF-16 data (opaque pointer in IR) |
| `T[]` | `struct {ptr, i32}` | Array struct with pointer and size |