// RUN_OPTS: --max-generic-depth 4

class Wrapper<T>
{
    T value

    Wrapper(T value)
    {
        this.value = value
    }
}

class TooDeep
{
    Wrapper<Wrapper<Wrapper<Wrapper<Wrapper<Wrapper<int>>>>>>> chain

    TooDeep(Wrapper<Wrapper<Wrapper<Wrapper<Wrapper<Wrapper<int>>>>>>> chain)
    {
        this.chain = chain
    }
}