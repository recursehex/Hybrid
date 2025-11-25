# Heap Allocation and Release

Hybrid supports explicit heap allocation and release using the `new` expression and the `free` statement. They are layered on top of the ARC runtime to integrate with constructors, destructors, and automatic releases.

## Syntax

```cs
// Objects
Rectangle box = new Rectangle(10, 20)
Rectangle box2 = new()          // target-typed; inherits the variable's type
Shape polymorphic = new Rectangle(5, 6)

// Arrays
int[] numbers = new int[8]
int[] inferred = new[4]         // type inferred from the target

// Manual release (optional)
free box
```

### Forms
- `new Type(args...)` allocates a heap instance and invoke the matching constructor.
- `new(args...)` is the shorthand for target-typed construction when the surrounding context provides the type.
- `new Type[size]` / `new[size]` allocates a 1-D array of the given length. Elements are zero-initialized.
- `free expr` explicitly releases an ARC-managed reference. ARC will still release automatically when the last strong reference dies; `free` simply schedules an explicit release at that point in code.

## Behaviour and ARC rules
- `new` returns a strong ARC-managed reference. Assigning it to a variable participates in the normal retain/release flow.
- Constructors run before the allocated storage is stored. Polymorphic construction (e.g. `Shape s = new Rectangle()`) uses the derived constructor and preserves the vtable/descriptor metadata.
- Arrays allocated with `new` carry their length in the `{ ptr, len, dims }` struct Hybrid uses for array values. Bounds checks continue to apply.
- `free` lowers to `hybrid_release`, triggering destructors once the refcount reaches zero. It is rarely used as ARC releases automatically at scope exit or on reassignment, but it is available for deterministic teardown sites.

## Valid targets
- Classes and structs (ARC-managed reference types).
- Arrays created via `new` (or any other ARC-managed array reference).
- Raw pointers to ARC-managed classes/structs inside `unsafe` blocks (`Foo@`), though regular references are preferred.

## Diagnostics and unsupported cases
- Smart pointers (`unique<T>`, `shared<T>`, `weak<T>`) reject `free` because their control blocks own the payload. Use the smart pointer APIs instead.
- Stack values are not heap-managed; `free` reports an error when applied to locals allocated on the stack, even if the type is ARC-capable.
- `new` requires an ARC-managed reference type. Attempting `new` on smart-pointer wrappers emits a diagnostic.
- Array allocations expect a single length expression; negative lengths are rejected.

## Examples

```cs
class Widget
{
    int weight

    Widget(int w)
    {
        this.weight = w
    }

    ~Widget()
    {
        print(this.weight)
    }
}

int main()
{
    Widget cached = new Widget(10)
    Widget quick = new()      // type comes from the variable

    int[] samples = new[3]
    samples[0] = 1
    samples[1] = 2
    samples[2] = 3

    free cached               // optional manual release point
    return samples[0] + samples[1] + samples[2]
}
```