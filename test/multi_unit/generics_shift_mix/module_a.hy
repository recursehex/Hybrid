// Shared generic helpers used by module_b.

class Pair<T, U>
{
    T first
    U second

    Pair(T first, U second)
    {
        this.first = first
        this.second = second
    }
}

class List<T>
{
    T head

    List(T head)
    {
        this.head = head
    }
}

class Matrix<T>
{
    T entry

    Matrix(T entry)
    {
        this.entry = entry
    }

    T Echo()
    {
        return this.entry
    }
}

class BitBox<T>
{
    T payload

    BitBox(T payload)
    {
        this.payload = payload
    }

    T Value()
    {
        return this.payload
    }
}