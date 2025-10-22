# Type System

Hybrid uses a static type system with explicit type declarations. All variables must be declared with a type and initialized at the point of declaration. The compiler performs type checking at compile time and generates appropriate LLVM IR for each type.

## Type Safety

The type system enforces several safety rules:

1. **No implicit narrowing conversions**: Assignments that reduce width or change signedness (e.g. `int` → `short`, `char` → `schar`, `long` → `int`, `int` ↔ `uint`, or `double`/`float` → integer types) must use explicit casts such as `short value = short: large`, `uint u = uint: signed`.
2. **Strict integer size checking**: Cannot mix different sized integers (e.g. `short` and `int`)
3. **Array bounds**: Array indices must be integers
4. **Function calls**: Arguments must match parameter types exactly (except for int-to-float promotion)
5. **Bool isolation**: Boolean values cannot be converted to or from other types
6. **Range checking**: Integer literals must fit in the target type's range
7. **Null safety**: All types are non-nullable by default; append `?` to opt into storing `null`. Nullable types must be accessed with null-safe operators `?.` and `?[]`.