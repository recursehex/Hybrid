// Consumes Foo defined in module_a and provides an entry point
int readFoo(Foo data)
{
    assert data.value >= 0
    return data.value
}

int main()
{
    Foo temp = Foo(42)
    int result = readFoo(temp)
    assert result == 42
    return 0
}