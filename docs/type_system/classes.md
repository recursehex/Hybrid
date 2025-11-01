# Classes

Hybrid supports classes that extend the struct model with member functions (methods), access modifiers, and formatter support for full object-oriented programming. Unlike structs, which are restricted to only member variables and constructors, classes allow you to define instance methods, static methods, and properties to create rich, encapsulated types. Classes continue to use value semantics by default, but add rules around initialization and visibility that mirror familiar behaviour while enforcing safer guarantees.

**Key distinction**: Use `struct` for simple data containers with fields and a constructor. Use `class` when you need methods, access control, static members, and full OOP capabilities.

## Quick Example

```cs
class Door
{
    private int knocks
    public int opens
    static int created = 0
    const int maxOpens

    Door(int maxOpens)
    {
        this.maxOpens = maxOpens
        this.knocks = 0
        this.opens = 0
        Door.created++  // allowed: static field was initialized at declaration
    }

    void Knock()
    {
        this.knocks++
        if (this.opens < this.maxOpens)
        {
            this.opens++
        }
    }
}
```

## Value Semantics and Construction

- Class instances follow the same value semantics as structs. `Door d = Door(2)` allocates storage in the surrounding scope.
- Classes may omit an empty constructor, in which case `Door()` is a compile-time error.
- All non-`static` members must be assigned in *every* constructor before they are read or mutated. The compiler tracks constructor assignments so it can emit diagnostics when a member is skipped.

## Member Initialization Rules

Hybrid now tracks whether a member has been initialized before allowing mutations.

| Scenario | Requirement | Diagnostic |
| --- | --- | --- |
| Static field | Must be initialized inline (`static int count = 0`) or assigned before any mutation | `Cannot increment or otherwise modify uninitialized member 'count' of class 'Foo'` |
| Instance field | Must be assigned in each constructor before mutation | Same diagnostic, referencing the instance member |

Examples:

```cs
class Counter
{
    static int created

    Counter()
    {
        Counter.created++  // error: static field lacks initializer
    }
}
```

```cs
class Pad
{
    int slots

    Pad(int slots)
    {
        this.slots = slots      // OK – initialized in constructor
    }

    Pad()
    {
        // slots is never assigned → reading or mutating it later errors
    }

    void Use()
    {
        this.slots--            // error if called on an instance built via Pad()
    }
}
```

Assignments inside constructors or other methods mark the member as initialized, enabling later mutations:

```cs
class Ticker
{
    static int count = 0        // inline initializer enables mutation

    Ticker()
    {
        Ticker.count++
    }
}
```

## Access Modifiers and Mutability

- Members are **read-only outside the class** unless explicitly marked `public`. This behaviour is identical to structs and is documented in detail in [`docs/access_modifiers.md`](access_modifiers.md).
- `public`, `private`, and `protected` control read/write visibility. For example, `public int opens` allows external code both to read and assign.
- `const` members may only be assigned in constructors (or at the declaration site for statics). Any later mutation produces `Cannot write to const member ...`.

## Methods, Static Members, and Formatters

- Methods are public by default. Applying `private` / `protected` restricts call sites according to the access rules above.
- Static methods and fields live in shared storage. Reads and writes via any instance observe the same underlying value. The initialization rules described earlier apply before the first mutation.
- Defining `string this()` provides a formatter used by the REPL or `print` calls:

  ```cs
  class Circle
  {
      int radius
      Point location

      Circle(int radius, Point location)
      {
          this.radius = radius
          this.location = location
      }

      string this()
      {
          return $"radius = {this.radius}, location = {this.location}"
      }
  }
  ```

## Diagnostics Summary

- **Uninitialized member mutation** – triggered when incrementing/assigning a member that has not been initialized yet. Generated for both static and instance fields.
- **Const mutation** – writes outside constructors (or inline initializers) for `const` fields continue to emit `Cannot write to const member ...`.
- The existing access-control diagnostics (`read-only outside definition`, `Cannot call private member`, etc.) remain unchanged.

These rules keep classes aligned with the modern expectations of type and memory safety. Static fields require initialization before use, and instance fields must be handled in every constructor, but Hybrid strengthens them with explicit compiler tracking so that mistakes surface immediately rather than resulting in default-initialized values at runtime.