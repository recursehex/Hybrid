# Structs

## Overview

Structs in Hybrid are user-defined composite types that group related data together. Structs cannot define methods, unlike classes. This design keeps structs as simple data containers, similar to C structs but with constructor support. Because fields do not receive implicit default values, every struct must declare at least one constructor so instances can initialize their storage. Structs may declare multiple constructors, provided each one initializes all instance fields before returning. If you need methods, access modifiers, or full object-oriented programming capabilities, use classes instead.

Structs can still declare properties and indexers to control access to their
fields; the syntax is the same as for classes.

> [!IMPORTANT]
> Every constructor overload must assign every instance field before exiting, as field have no default values. Omitting a field assignment in any constructor results in a compile-time error.

## Struct Declaration

### Basic Syntax

```cs
struct StructName
{
    type1 field1
    type2 field2
    // ... more fields
    
    // Constructor
    StructName(parameters)
    {
        // initialization code
    }
}
```

### Example Declarations

```cs
// Simple struct with constructor overloads
struct Point
{
    int x
    int y

    Point()
    {
        this.x = 0
        this.y = 0
    }

    Point(int x, int y)
    {
        this.x = x
        this.y = y
    }
}

// Struct with multiple types
struct Person
{
    string name
    int age
    float height
    
    Person(string n, int a, float h)
    {
        this.name = n
        this.age = a
        this.height = h
    }
}
```

## Fields

### Field Declaration

Fields are declared with their type followed by the field name:

```cs
struct Student
{
    string name      // String field
    int id           // Integer field
    float gpa        // Float field
    bool active      // Boolean field

    Student(string name, int id, float gpa, bool active)
    {
        this.name = name
        this.id = id
        this.gpa = gpa
        this.active = active
    }
}
```

### Field Types

Fields can be of any supported type:

| Type Category | Examples | Usage |
|--------------|----------|-------|
| Primitive types | `int`, `float`, `double`, `char`, `bool` | Basic data storage |
| String type | `string` | Text data |
| Array types | `int[]`, `float[]`, `char[]` | Collections |
| Struct types | `Point`, `Rectangle` | Nested structures |
| Sized integers | `byte`, `short`, `long` | Specific integer sizes |

### Nullable Fields

Append `?` to a field's type when the value is optional:

```cs
struct User
{
    string name
    Address? address
    string? alias

    User(string name, Address? address, string? alias)
    {
        this.name = name
        this.address = address
        this.alias = alias
    }
}
```

- Nullable fields must be accessed with the `?.` operator (for example, `user.address?.city`).
- Assigning a nullable expression to a non-nullable field triggers a compile-time error.
- Constructors can initialize nullable fields with `null` to signal the absence of a value.

### Nested Structs

Structs can contain fields of other struct types:

```cs
struct Point
{
    int x
    int y

    Point(int x, int y)
    {
        this.x = x
        this.y = y
    }
}

struct Circle
{
    Point center    // Nested struct field
    int radius
    
    Circle(Point p, int r)
    {
        this.center = p
        this.radius = r
    }
}
```

## Constructors

### Constructor Methods

Constructors are special methods with the same name as the struct:

```cs
struct Vector3
{
    float x
    float y
    float z
    
    // Constructor with parameters
    Vector3(float x, float y, float z)
    {
        this.x = x
        this.y = y
        this.z = z
    }
}
```

### The `this` Keyword

Inside constructor methods, `this` refers to the current struct instance:

```cs
struct Counter
{
    int value
    
    Counter(int value)
    {
        this.value = value  // 'this.value' refers to the member variable
    }
}
```

You may provide more than one constructor when you need different parameter sets. Regardless of the overload, ensure each constructor assigns every instance field before it exits.

### Constructor Overloading

Hybrid supports multiple constructors per struct, and at least one constructor must be present. Overloads let you provide alternative initialization logic while still guaranteeing that all fields become initialized.

## Struct Instantiation

### With Assignment

Create a struct instance and assign it to a variable:

```cs
// Using constructor
Point p1 = Point(10, 20)

// Accessing fields after creation
int xCoord = p1.x  // 10
int yCoord = p1.y  // 20
```

For longer argument lists, break the constructor call across multiple lines. The call follows the same multiline rules as function invocations:

```cs
Rectangle positioned = Rectangle(
    originX,
    originY + offset
)
```

When the declared type is already known, you can omit the type name before the argument list:

```cs
Point origin = (0, 0)       // calls Point(int, int)
Rectangle banner = (3, 7)   // calls Rectangle(int, int)
```

### Without Assignment

Structs can be instantiated without assignment (standalone constructor calls):

```c
// Standalone instantiation
Point(30, 40)  // Creates a temporary Point instance

// Useful for side effects or passing directly to functions
processPoint(Point(50, 60))
```

## Member Access

### Dot Operator

Access struct fields using the dot (`.`) operator:

```cs
struct Book
{
    string title
    string author
    int year
}

Book b = Book("1984", "Orwell", 1949)
string bookTitle = b.title     // "1984"
string bookAuthor = b.author   // "Orwell"
int pubYear = b.year           // 1949
```

### Field Modification

Struct fields can be modified after creation:

```cs
Point p = Point(0, 0)
p.x = 100  // Modify x field
p.y = 200  // Modify y field
```

### Nested Member Access

Access nested struct fields by chaining dot operators:

```cs
struct Point
{
    int x
    int y
}

struct Rectangle
{
    Point topLeft
    Point bottomRight
}

Rectangle rect = Rectangle(Point(0, 0), Point(100, 100))
int leftX = rect.topLeft.x        // 0
int bottomY = rect.bottomRight.y  // 100

// Modify nested fields
rect.topLeft.x = 10
rect.bottomRight.y = 200
```

## Structs in Functions

### Struct Parameters

Structs can be passed as function parameters:

```cs
struct Point
{
    int x
    int y
}

int distance(Point p1, Point p2)
{
    int dx = p2.x - p1.x
    int dy = p2.y - p1.y
    return dx * dx + dy * dy  // Squared distance
}

// Usage
Point a = Point(0, 0)
Point b = Point(3, 4)
int dist = distance(a, b)  // Returns 25
```

### Returning Structs

Functions can return struct instances:

```cs
Point createOrigin()
{
    return Point(0, 0)
}

Point translatePoint(Point p, int dx, int dy)
{
    return Point(p.x + dx, p.y + dy)
}
```

## Memory Management

### Stack Allocation

Structs are currently allocated on the stack:

```cs
Point p = Point(10, 20)  // Stack allocated
// Memory automatically freed when out of scope
```

### Heap Allocation

Structs can be heap allocated with `new`:

```cs
Point@ p = new Point(10, 20)    // Heap allocated
Point@ p = new(10, 20)          // Compact syntax
```

- Structs are value types with stack-based storage by default. `Point p = Point(1, 2)` allocates storage in the surrounding scope.
- `new Point(args)` allocates a heap instance and runs the constructor, returning an ARC-managed reference. Manual release is optional via `free`.
- `free p` is rejected on stack-allocated struct values; use it only on heap references produced by `new` (or pointers to them inside `unsafe` blocks).

## Type System Integration

### Type Checking

Structs are strongly typed and checked at compile time:

```cs
struct Cat
{
    string name
}

struct Dog
{
    string name
}

Cat c = Cat("Whiskers")
Dog d = Dog("Buddy")

// Type error: Cannot assign Cat to Dog
// Dog d2 = c  // Compile error
```

### Type Propagation

Field types are properly propagated through member access:

```cs
struct Data
{
    int count
    float average
}

Data d = Data(10, 3.14)
int c = d.count      // Type: int
float a = d.average  // Type: float
// int wrong = d.average  // Type error
```

## Implementation Details

### LLVM Representation

Structs are represented as LLVM structure types:

```llvm
; struct Point { int x; int y; }
%struct.Point = type { i32, i32 }

; Point p = Point(10, 20)
%p = alloca %struct.Point
%1 = getelementptr %struct.Point, ptr %p, i32 0, i32 0
store i32 10, ptr %1
%2 = getelementptr %struct.Point, ptr %p, i32 0, i32 1
store i32 20, ptr %2
```

### Constructor Implementation

Constructors are implemented as regular functions that initialize struct fields:

```llvm
define void @Point_constructor(ptr %this, i32 %x, i32 %y) {
entry:
  %x_ptr = getelementptr %struct.Point, ptr %this, i32 0, i32 0
  store i32 %x, ptr %x_ptr
  %y_ptr = getelementptr %struct.Point, ptr %this, i32 0, i32 1
  store i32 %y, ptr %y_ptr
  ret void
}
```

## Best Practices

### Naming Conventions

1. **Struct Names**: Use PascalCase for struct names
   ```cs
   struct CustomerOrder { }  // Good
   struct customer_order { }  // Avoid
   ```

2. **Field Names**: Use camelCase for field names
   ```cs
   struct Person
   {
       string firstName  // Good
       int ageInYears    // Good
   }
   ```

### Constructor Design

1. **Initialize All Fields**: Always initialize all fields in constructors
   ```cs
   struct Account
   {
       string owner
       int balance
       
       Account(string o, int b)
       {
           this.owner = o
           this.balance = b
       }
   }
   ```

2. **Parameter Naming**: Use clear parameter names
   ```cs
   struct Rectangle
   {
       int width
       int height
       
       Rectangle(int width, int height)
       {  // Clear names
           this.width = width
           this.height = height
       }
   }
   ```

## Examples

### Complex Number

```cs
struct Complex
{
    float real
    float imag
    
    Complex(float r, float i)
    {
        this.real = r
        this.imag = i
    }
}

Complex addComplex(Complex a, Complex b)
{
    return Complex(a.real + b.real, a.imag + b.imag)
}

// Usage
Complex c1 = Complex(3.0, 4.0)
Complex c2 = Complex(1.0, 2.0)
Complex sum = addComplex(c1, c2)  // (4.0, 6.0)
```

### Linked List Node

```cs
struct Node
{
    int value
    int next_index  // Index to next node (simplified)
    
    Node(int val, int next)
    {
        this.value = val
        this.next_index = next
    }
}

// Create a simple linked structure
Node n1 = Node(10, 1)
Node n2 = Node(20, 2)
Node n3 = Node(30, -1)  // -1 indicates end
```

### Game Entity

```cs
struct Position
{
    float x
    float y
}

struct Entity
{
    string name
    Position pos
    int health
    int damage
    
    Entity(string n, Position p, int h, int d)
    {
        this.name = n
        this.pos = p
        this.health = h
        this.damage = d
    }
}

// Create game entities
Position spawn = Position(100.0, 200.0)
Entity player = Entity("Hero", spawn, 100, 25)
Entity enemy = Entity("Goblin", Position(150.0, 200.0), 50, 10)

// Access nested data
float playerX = player.pos.x  // 100.0
int enemyHP = enemy.health     // 50
```

## Testing

Test files demonstrating struct features:

- `test/structs/struct_basic.hy` - Basic struct declaration and instantiation
- `test/structs/struct_advanced.hy` - Nested structs and complex scenarios
