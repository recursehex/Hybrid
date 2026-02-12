# Classes

Hybrid supports classes that extend the struct model with member functions (methods), access modifiers, and formatter support for full object-oriented programming. Unlike structs, which are restricted to only member variables and constructors, classes allow you to define instance methods, static methods, and properties to create complex, encapsulated types.

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
        Door.created++
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

## Properties and Indexers

Properties expose field access through `get` and `set` accessors. The setter
receives an implicit `value` parameter that is only valid inside the setter
body. Accessing `obj.prop` calls the getter, and assigning `obj.prop = rhs`
invokes the setter (compound assignments call getter + setter).
Properties and indexers that declare a setter default to public read/write
access, so `public` is optional. Declaring a property as `private` or
`protected` is rejected because it would block the accessors.

Indexers provide array-like access via `this[...]` and are always instance
members. `obj[index]` uses the indexer when present; otherwise it falls back to
native array indexing.
Indexers may omit `set` for read-only access; indexers that declare a setter
must also declare a getter.

```cs
class IntList
{
    private int[] items

    int count
    {
        get items.size
    }

    int this[int index]
    {
        get items[index]
        set items[index] = value
    }

    IntList(int size)
    {
        this.items = new int[size]
    }
}
```

## Value Semantics and Construction

- Class instances follow the same value semantics as structs. `Door d = Door(2)` allocates storage in the surrounding scope.
- Classes must declare at least one constructor. If you want default construction, explicitly declare an empty constructor; omitting constructors entirely is a compile-time error because members have no default values.
- All non-`static` members must be assigned in *every* constructor before the constructor returns. The compiler tracks constructor assignments so it can emit diagnostics when a member is skipped.
- You may omit the type name when calling a constructor in a variable initializer: writing `Door front = (2)` is equivalent to `Door front = Door(2)` and uses the matching class constructor.
- This shorthand also works for other target-typed contexts:
  - Arrays: `Widget[] items = [(5), (6)]` calls the `Widget(int)` constructor for each element.
  - Strings: ``string label = $"Value: `(rect)`"`` or `print(rect)` will call `string this()` when present.
  - Smart pointers: `shared<Widget> primary = #5` builds the payload with `Widget(5)` and wraps it in a `shared<Widget>`; the same applies to `unique<T>`. For `weak<T>`, the shorthand expects a `shared<T>` value, e.g. `weak<Widget> watcher = #sharedOwner`.

> [!IMPORTANT]
> The compiler never synthesizes constructors for classes. Declare at least one and initialize every instance member inside each overload, or construction fails with "cannot assign uninitialized member" diagnostics.

```cs
Door front = (2)            // shorthand for Door front = Door(2)
Door copy = (front)         // invokes the copy constructor when available
shared<Door> refDoor = #2   // payload constructed via Door(int)
```

### Default Parameters and Named Arguments

- Constructors and methods may declare trailing default parameter values. Defaults must be compile-time constants, cannot appear on `ref` parameters, and once one parameter has a default all following parameters must as well.
- Overrides and redeclarations must keep those default values identical to the original signature; changing or omitting a default is rejected.
- Calls can supply arguments by name; positional arguments must come first and the remaining arguments must be named. Named arguments let you override a later default without providing earlier ones.

```cs
class Logger
{
    bool appendNewline

    Logger(bool appendNewline = true)
    {
        this.appendNewline = appendNewline
    }
}

Logger loud = Logger()                     // uses default
Logger quiet = Logger(appendNewline=false) // named argument skips earlier defaults
```

## Smart Pointers and ARC Helpers

- ARC already frees class and struct instances when they leave scope; the smart pointer wrappers are optional conveniences for sharing, exclusivity, and weak observation.
- Helper builders: `make_unique<T>(args...)`, `make_shared<T>(args...)`, `weak_from_shared<T>(shared<T>)`, and `shared<T>.weak()` construct the payload (or reuse an existing shared handle) and wire the appropriate retain/release or weak control block plumbing.
- Accessors: `@ptr` yields the payload value for smart pointers (e.g. `@owner == 5` for `shared<int>`). `ptr->member` works only for smart pointers in safe code; raw pointers still require `unsafe` and explicit dereference.
- Construction shorthand: use `#payload` to build smart pointers from a target type. It accepts payload constructors (`shared<Foo> handle = #5` calls `Foo(int)`), and `weak<T>` expects a `shared<T>` payload (`weak<Foo> watcher = #handle`).
- Smart pointers and raw references point to the same ARC-managed allocations, so you can pass either to APIs without special conversion functions; pick the wrapper only when you need its ownership semantics.
- Target-typed `new(...)` also works with smart pointer destinations, constructing the payload from the wrapper’s generic type before wiring the control block; `weak<T>` still expects a `shared<T>` payload.
- Disabling ARC lowering (`--arc-enabled=false`) keeps these helpers available but they degrade to type-correct stubs: `arcUseCount()` reports `0` and `weak.lock()` returns an empty handle so ARC-on vs ARC-off comparisons do not require source changes.

### Construction syntax

- `#payload` wraps an existing value or builds one with constructor arguments: `shared<int> a = #42`, `unique<Gadget> g = #(1, 2)`.
- Target-typed payload construction also works without `#`: `shared<WatchTarget> owner = (30)` or `unique<Resource> r = (1)`.
- `new(...)` can target the wrapper type to build and wrap in one step: `shared<int> n = new(11)` or `shared<int> explicitN = new shared<int>(3)`.
- Weak handles come from a `shared<T>` owner (`weak<Foo> w = #owner`, `owner.weak()`, or `weak_from_shared(owner)`) and can be cleared with `()` / `#()`; locking an empty weak yields a zero-count `shared<T>`.

### Avoiding reference cycles

- Mark back references `weak<T>` whenever the forward edge holds a strong owner (parent/child trees, UI hierarchies, cache entries) so ARC can reclaim both sides.
- Observer lists should keep `weak<T>` handles and lock them when dispatching to avoid resurrecting already-destroyed listeners.

```cs
class Widget
{
    shared<Widget>[] children
    weak<Widget> parent

    Widget(shared<Widget> parent)
    {
        this.parent = parent
        this.children = []
    }
}
```

## Heap Allocation with `new` and `free`

- `new ClassName(args)` allocates a heap instance, runs the class constructor, and returns a strong ARC-managed reference. The type can be inferred from the assignment target using `new(args)`.
- `new StructName(args)` works the same way for structs when you want a heap-backed instance instead of stack storage.
- Polymorphic construction is supported: `Shape s = new Rectangle(2, 4)` calls the `Rectangle` constructor while producing a base-typed reference with the correct vtable/descriptor metadata.
- Manual release is optional. `free instance` schedules an explicit ARC release; destructors still run automatically when the last strong reference is lost.
- `free` is rejected for stack values, smart pointers, and other non-ARC-managed types.

## Member Initialization Rules

Hybrid tracks whether a member has been initialized before allowing mutations.

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
        this.slots = slots      // OK - initialized in constructor
    }

    Pad()
    {
        // slots is never assigned, reading or mutating it later errors
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

- Members are **read-only outside the class** unless explicitly marked `public`.
- `public`, `private`, and `protected` control read/write visibility. For example, `public int opens` allows external code both to read and assign.
- `const` members may only be assigned in constructors (or at the declaration site for statics). Any later mutation produces `Cannot write to const member ...`.

### `public`
- **Scope**: Full read and write access from any code
- **Use case**: When external code needs to modify member directly
- **Syntax**:
  ```cs
  public int count = 10  // Both readable and writable externally
  ```

### `private`
- **Scope**: Read and write only within the defining struct/class
- **Use case**: Complete encapsulation, hidden implementation details
- **Syntax**:
  ```cs
  private int UUID = 123456  // Hidden from external code entirely
  private void ClearData() { ... }
  ```

### `protected`
- **Scope**: Readable and modifiable by the defining struct/class and its subclasses
- **Use case**: Inheritance hierarchies
- **Syntax**:
  ```cs
  protected int size = 0
  protected void Save() { ... }
  ```

### `static`
- **Scope**: Shared among all instances, callable without instance
- **Use case**: Utility functions, shared state
- **Syntax**:
  ```cs
  static int max = 16
  static void QuickSort(ref List<int> list) { ... }
  ```

### `const`
- **Scope**: Immutable after initialization
- **Use case**: Constants, configuration values
- **Syntax**:
  ```cs
  const decimal pi = 3.1415926535897932384626433
  ```

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
          return $"radius = `this.radius`, location = `this.location`"
      }
  }
  ```

### Operator overloads

Classes can declare operator overloads as instance methods using the same allowlist as structs (`=`, `+=`, `-=`, `*=`, `/=`, `%=`, `+`, `-`, `*`, `/`, `%`, `==`, `!=`, `<`, `>`, `<=`, `>=`, `@`, `#`, `[]`).

```cs
class Measure
{
    int value

    Measure +(const ref Measure rhs)
    {
        return Measure(this.value + rhs.value)
    }

    bool >=(const ref Measure rhs)
    {
        return this.value >= rhs.value
    }
}
```

`@` and `#` overload calls still require `unsafe`. For `[]`, return `ref` to support indexed assignment.

## Destructors and `free`

- Declare a single destructor per type with `~TypeName() { ... }`; parameters, generics, return types, `static`, and `abstract` are rejected.
- ARC runs destructors automatically when the last strong reference is released, including when a `shared<T>` control block drops to zero and when scope teardown releases locals.
- Class destructors are always virtual: dispatch goes through the vtable slot stored in the runtime type descriptor, so `Base b = new Derived(); free b` (or any base reference going out of scope) invokes the derived destructor. This is intentional parity with best practices in C++ while removing the "forgot to mark it virtual" footgun. Struct destructors remain non-virtual as structs do not support inheritance.
- Manual calls (`value.~TypeName()`) retain and release around the call; they require a strong, non-smart-pointer receiver and must not be repeated on the same value.
- `free expr` lowers to the same ARC release path that triggers destructors; freeing smart pointer handles or non-ARC/stack values is diagnosed, and mixes of `free` plus manual destructor calls surface as double releases.

## Inheritance

Classes support single inheritance and multi-interface implementation. The language enforces the following rules:

- A class may inherit from at most one base class and implement zero or more interfaces using the `inherits` clause: `class Player inherits Entity, Drawable`.
- Interfaces are pure contracts. They cannot declare fields or constructors and every member is implicitly abstract.
- Classes that declare an interface must implement all of its members. Implementations may be inherited from the base class; otherwise the derived class must provide an override.
- Use `virtual` on base class members that may be customised and `override` in derived classes. Attempting to override a non-virtual member produces a diagnostic.
- Mark a class `abstract` to allow abstract members. Non-abstract classes cannot declare abstract methods and must provide concrete implementations for any abstract member inherited from the base hierarchy.
- `protected` members are visible inside the declaring class and any subclass. Access from unrelated types is rejected.
- `base` expressions are only valid inside instance methods of a class that declares a base class. They allow you to invoke the base implementation or access protected helpers: `base.UpdatePosition()`.

Example:

```cs
abstract class Entity
{
    protected int health

    Entity()
    {
        this.health = 100
    }

    virtual void TakeDamage(int amount)
    {
        this.health -= amount
    }

    abstract void Draw()
}

interface Drawable
{
    void Draw()
}

class Player inherits Entity, Drawable
{
    Player(int hp)
    {
        this.health = hp
    }

    override void TakeDamage(int amount)
    {
        base.TakeDamage(amount / 2)
    }

    override void Draw()
    {
        // render the player
    }
}
```

Attempting to omit an interface member or forget to override an abstract base method results in a compile-time error, keeping derived classes honest.

## Generics

Classes, interfaces, and functions can declare generic type parameters. You must provide explicit type arguments because type inference is not supported.

```cs
class Box<T>
{
    T value

    Box(T value)
    {
        this.value = value
    }

    void SwapWith<U>(ref U target, U replacement)
    {
        target = replacement
    }
}

void Assign<T>(ref T target, T value)
{
    target = value
}

void main()
{
    Box<int> ints = Box<int>(7)
    int number = 0
    Assign<int>(ref number, 42)
    ints.SwapWith<int>(ref number, 99)
}
```

- Use `class Name<T, U>` to declare generic classes or interfaces.
- Invoke `Name<int, string>` with explicit `<>` arguments. Omitting them (e.g., `Assign(ref number, 7)`) produces a diagnostic reminding you to add `<T>`.
- Generic classes can inherit from other generic types: `class List<T> inherits Sequence<T>`.
- There is no variance or constraint syntax yet-every binding is invariant, so `Box<Derived>` is not assignable to `Box<Base>` unless your own hierarchy makes that conversion legal.

Nested instantiations are preserved verbatim, so you can freely compose templates:

```cs
Pair<Pair<int, int>, Pair<int, int>> corners = (Pair<int, int>(1, 2), Pair<int, int>(3, 4))
int diagonal = corners.first.second + corners.second.first
```

### Runtime representation

Hybrid specializes each instantiation. `Box<int>` and `Box<string>` each receive their own LLVM IR bodies and runtime descriptors. The final mangled name encodes both class-level and method-level bindings (e.g. `Box.SwapWith$RV_void_P2R_int,R_int<T=int,U=int>`), so even methods whose generic parameters only appear inside the body are unique per instantiation. See [Generics Runtime Strategy](generics_runtime.md) for the detailed trade-offs and the caching rules that keep specialization bloat in check.

You can introspect the instantiated metadata at runtime via the new `describeType()` intrinsic:

```cs
Box<int> ints = Box<int>(3)
string summary = describeType("Box<int>")
// summary == "type:Box<int>|args:T=int|base:none|interfaces:none"
```

The returned string lists the concrete type arguments, base class, implemented interfaces, and any generic method instantiations emitted during the current session. Tests under `test/generics/basic/metadata_dump.hy` pin this behaviour so regressions are caught automatically.

### Generic Diagnostics & Limits

- Hybrid warns when a declaration introduces more than eight generic parameters or when a type literal nests more than four `<>` pairs. These heuristics keep APIs readable without banning legitimate high-arity cases.
- Pass `--diagnostics generics` (or set `HYBRID_SHOW_GENERIC_METRICS=1` when using `run_tests.sh`) to print aggregate metrics: total type/function specialisations emitted, cache hit rates, peak binding depth, and the exact byte-size of the generated LLVM module.
- `--dump-generic-stack` prints the active binding frames whenever the compiler aborts due to excessive instantiation recursion, making it easier to spot cyclical constraints.
- The following hard limits are configurable per build:
  - `--max-generic-depth <n>`: cap the recursive binding stack (defaults to 128).
  - `--max-generic-instantiations <n>`: stop emitting new specialisations once the total unique type/function bindings exceed `<n>`.
  - `--max-nested-generics <n>`: reject individual type spellings whose nesting depth exceeds `<n>`.

Together these switches provide early feedback for pathological templates without penalising everyday generic code.

### Keyword reference

| Keyword | Applies to | Meaning |
| --- | --- | --- |
| `inherits` | class/interface declarations | Lists a single base class followed by any number of interfaces. |
| `abstract` | class or method | Marks a class as non-instantiable and signals members without implementations. Abstract methods are implicitly virtual. |
| `interface` | type declaration | Declares a contract with only abstract, non-field members. Interfaces cannot declare fields or constructors. |
| `virtual` | methods | Allows overriding in derived classes. Only meaningful on instance methods. |
| `override` | methods | Replaces a virtual or abstract member from the base class hierarchy. Signature must match the overridden method. |
| `protected` | fields/methods | Grants read/write/call access inside the declaring class and any subclasses. |
| `base` | expressions | Evaluates to the current instance viewed as the base class. Valid only inside instance methods on classes with a base. |

### Declaring a base class

Write the base class first in the `inherits` list:

```cs
class Button inherits Widget, Clickable, Accessible
{
    // ...
}
```

- Only classes may specify a base class. Interfaces may extend other interfaces but cannot derive from classes.
- If the first type resolves to an interface, the compiler treats the class as having no base class and adds that type to the interface list.
- Inheritance cycles (direct or indirect) produce a diagnostic.
- Derived constructors automatically receive `this` pointing at the derived storage. If the base class exposes a parameterless constructor it runs automatically; otherwise call it explicitly inside the constructor body with `base(...)`.

### Interfaces as contracts

Interfaces collect the members a type must provide:

```cs
interface Clickable
{
    void Click()
    void DrawHighlight()
}

class Checkbox inherits Widget, Clickable
{
    override void DrawHighlight() { /* ... */ }
    override void Click() { /* ... */ }
}
```

- Interface members are implicitly abstract; adding a body emits `Interfaces cannot declare fields` / `Abstract methods cannot have bodies`.
- A class may inherit implementations from its base class. If the base class already implements the interface method, the derived class does not have to restate it.
- Missing implementations trigger `Class 'Foo' does not implement interface member 'Interface.Member'`.

### Virtual and override semantics

- Mark base class members with `virtual` (or `abstract`) to allow specialisation. Leaving a method non-virtual forbids overrides in derived classes.
- Derived classes must use `override` to replace virtual members. Omitting the keyword produces `Method 'Bar' does not override a base class method`.
- Attempting to override a non-virtual member emits `Method 'Bar' cannot override non-virtual member 'Baz'`.
- Overrides must match the signature (parameter types, ref modifiers, and return type). The compiler relies on the stored metadata to verify slot compatibility.

### Abstract classes and methods

- `abstract class` declarations cannot be instantiated directly.
- Abstract classes may contain a mix of abstract and concrete members. Every abstract method must be implemented by the first concrete descendant; otherwise `Class 'Foo' must override abstract member 'Bar' defined in 'Base'`.
- Abstract members imply `virtual`; you do not need to add both keywords.
- Abstract methods cannot be static. Static abstract members produce `Abstract method 'Foo' of class 'Bar' cannot be static`.

### Protected members

`protected` strikes a balance between `private` and `public`:

```cs
class Shape
{
    protected double area
}

class Circle inherits Shape
{
    void Resize(double r)
    {
        this.area = r * r * Math.PI // ok: within subclass
    }
}

void Example()
{
    Shape s = /* ... */
    double x = s.area   // error: Cannot read protected member 'area' without inheriting from it
}
```

- Protected visibility propagates through the inheritance chain. Derived classes can read and write these members; other code cannot.
- Attempts to access protected members from unrelated types produce `Cannot read protected member 'Member' of class 'Owner' without inheriting from it`.

### Using `base`

`base` reuses existing logic from the superclass:

```cs
class LoggingButton inherits Button
{
    override void Click()
    {
        Logger.Info("click")
        base.Click()  // reuses Button.Click
    }

    static void Helper()
    {
        base.Click() // error: 'base' may only be used inside instance methods
    }
}
```

- Only valid in instance methods and constructors.
- If the class lacks a base class, `Type 'Foo' does not have a base class` is emitted.
- When the derived and base types have the same layout (Hybrid classes always use value semantics), `base` performs a bitcast to the base view and then reuses the base implementation.

### Diagnostics for inheritance

The compiler surfaces precise errors to keep hierarchies correct. Examples include:

- `Class 'Foo' can only inherit from another class` - triggered when a non-class type appears first in the `inherits` list.
- `Interface 'Foo' can only inherit from other interfaces` - ensures interfaces do not derive from classes.
- `Inheritance cycle detected for type 'Foo'` - prevents recursive hierarchies.
- `Class 'Foo' does not implement interface member 'Interface.Member'` - missing interface implementation.
- `Class 'Foo' must override abstract member 'Base.Member' defined in 'Base'` - concrete class failed to supply a definition.
- `Static method 'Foo' cannot be marked override` / `Abstract method 'Foo' cannot be static` - invalid modifier combinations.
- `Method 'Foo' does not override a base class method` - `override` used without a matching virtual base member.
- `Protected member 'Bar' ... without inheriting from it` - invalid access from outside the hierarchy.
- `'base' may only be used inside instance methods` / `Type 'Foo' does not have a base class` - misuse of the `base` expression.

## Runtime Polymorphism

Hybrid represents every class instance as a value that begins with a runtime header. Constructors initialise the header automatically; it links the instance to a static type descriptor that stores:

- the class name and a pointer to its base type descriptor (if any)
- a vtable pointer plus slot count for `virtual`/`override` dispatch
- interface entries containing cached method tables for each implemented interface

Because instances continue to use value semantics, assigning `Derived` into a `Base` variable copies the full value, including its descriptor link. Subsequent method calls dispatched through the base type still observe the derived overrides, and copying or passing the value by value preserves the descriptor without extra work.

Virtual calls use the vtable slot recorded during semantic analysis. `base.Method()` and static members bypass the vtable and call the resolved implementation directly. Interface variables obtain their method table by calling a runtime helper (`hybrid_lookup_interface_table`) that walks the descriptor chain; if a class claims an interface but the table is missing, the helper traps so the program does not continue with undefined behaviour.

These runtime mechanics make polymorphic calls predictable while keeping Hybrid's value semantics and explicit construction guarantees intact.
