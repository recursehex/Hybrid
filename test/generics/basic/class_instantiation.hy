// Runtime smoke test for generic class instantiation and substitution.

class Box<T>
{
    T value

    Box(T value)
    {
        this.value = value
    }

    T Get()
    {
        return this.value
    }

    void Set(T value)
    {
        this.value = value
    }
}

void main()
{
    Box<int> numbers = (42)
    assert numbers.Get() == 42

    numbers.Set(7)
    assert numbers.Get() == 7

    Box<string> words = Box<string>("hello")
    assert words.Get() == "hello"
}