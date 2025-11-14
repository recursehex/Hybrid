// RUN_OPTS: --diagnostics generics

class Identity<T>
{
    T value

    Identity(T value)
    {
        this.value = value
    }
}

int main()
{
    Identity<int> a = (1)
    Identity<int> b = (2)
    return a.value + b.value
}