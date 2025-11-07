// Runtime smoke test for generic class instantiation and substitution.

class Box<T>
{
    T value

    Box(T value)
    {
        this.value = value
    }

    void Set(T value)
    {
        this.value = value
    }
}

void main()
{
    Box<int> numbers = (42)     // shorthand
    assert numbers.value == 42

    numbers.Set(7)
    assert numbers.value == 7

    Box<string> words = Box<string>("hello")    // full construction
    assert words.value == "hello"
}