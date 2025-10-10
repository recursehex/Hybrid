// Provides the struct definition consumed by module_b
struct Foo
{
    int value

    Foo(int v)
    {
        this.value = v
    }
}

int makeFooValue(int seed)
{
    Foo local = Foo(seed)
    assert local.value == seed
    return local.value
}