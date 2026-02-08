# Heap Allocation and Release

Hybrid supports explicit heap allocation and release using the `new` expression and the `free` statement. They are layered on top of the ARC runtime to integrate with constructors, destructors, and automatic releases.

## Syntax

```cs
// Objects
Rectangle box = new Rectangle(10, 20)   // useful for polymorphism
Rectangle box2 = new()                  // inherits the variable's type
Shape polymorphic = new Rectangle(5, 6)

// Arrays
int[] numbers = new int[8]
int[] inferred = new[4]         // type inferred from the target
int[,] grid = new[2, 3]

// Manual release (optional)
free box
```

### Forms
- `new Type(args...)` allocates a heap instance and invoke the matching constructor.
- `new(args...)` is the shorthand for target-typed construction when the surrounding context provides the type.
- `new Type[size]` / `new[size]` allocates a 1-D array of the given length. Elements are zero-initialized.
- `new Type[size1, size2]` / `new[size1, size2]` allocates a rectangular array with one bound per dimension.
- `free expr` explicitly releases an ARC-managed reference. ARC will still release automatically when the last strong reference dies; `free` simply schedules an explicit release at that point in code.

## Behaviour and ARC rules
- `new` returns a strong ARC-managed reference. Assigning it to a variable participates in the normal retain/release flow.
- Constructors run before the allocated storage is stored. Polymorphic construction (e.g. `Shape s = new Rectangle()`) uses the derived constructor and preserves the vtable/descriptor metadata.
- Arrays allocated with `new` carry their length in the `{ ptr, len, dims }` struct Hybrid uses for array values. Bounds checks continue to apply.
- Smart pointer targets (`unique<T>`, `shared<T>`, `weak<T>`) are supported. `new(...)` uses the wrapper’s payload type to build the value and wraps it in the smart pointer control block; `weak<T>` still expects a `shared<T>` payload.
- `free` lowers to `hybrid_release`, triggering destructors once the refcount reaches zero. It is rarely used as ARC releases automatically at scope exit or on reassignment, but it is available for deterministic teardown sites.

> [!TIP]
> Only use `free` when you need a deterministic release point (for example, to flush a buffer or close a handle before leaving a long-lived scope).

## Toggling ARC
- The driver flag `--arc-enabled={true,false}` controls whether ARC retain/release calls are emitted (default: `true`).
- When ARC is disabled the compiler still accepts ARC-capable types, but retain/release/autorelease insertion and ARC diagnostics are skipped. Smart pointer helpers stay available and compile to type-correct stubs (`arcUseCount()` returns `0`, `weak.lock()` yields an empty handle).
- The test runner forwards the flag via `./run_tests.sh -a on|off`, or individual tests can add `// RUN_OPTS: --arc-enabled=false` to opt out of ARC for a single fixture.

### ARC-on/off performance comparisons
- Use `./scripts/arc_bench.sh` to benchmark ARC workloads under both `--arc-enabled=true` and `--arc-enabled=false`.
- The script writes CSV/JSON artifacts under `build/arc-bench/` and reports median timing deltas with configurable warn/fail thresholds.
- ARC-off runs are for comparative profiling only; ARC-off can intentionally skip ownership enforcement and may leak by design in stress fixtures.

## Valid targets
- Classes and structs (ARC-managed reference types).
- Smart pointer wrappers when the target type is `unique<T>`, `shared<T>`, or `weak<T>`; arguments are forwarded to the payload constructor and then wrapped.
- Arrays created via `new` (or any other ARC-managed array reference).
- Raw pointers to ARC-managed classes/structs inside `unsafe` blocks (`Foo@`), though regular references are preferred.

## Diagnostics and unsupported cases
- Smart pointers (`unique<T>`, `shared<T>`, `weak<T>`) reject `free` because their control blocks own the payload. Use the smart pointer APIs instead.
- Stack values are not heap-managed; `free` reports an error when applied to locals allocated on the stack, even if the type is ARC-capable.
- `free` is compile-time checked, so double frees and any subsequent use of a manually released value both emit diagnostics to prevent use-after-free bugs.
- Array allocations expect one bound per dimension; negative lengths are rejected.

> [!CAUTION]
> Attempting to use `free` on stack locals or smart pointers is rejected at compile time, so use it only on objects created with `new`.

## Examples

```cs
class Widget
{
    int weight

    Widget()
    {
        this.weight = 1
    }

    Widget(int w)
    {
        this.weight = w
    }

    ~Widget()
    {
        print(this.weight)
    }
}

void main()
{
    Widget cached = new Widget(10)
    Widget quick = new()

    int[] samples = new[3]
    samples[0] = 1
    samples[1] = 2
    samples[2] = 3

    free cached               // optional manual release point
    //` samples` and `quick` are released automatically at scope exit
}
```
