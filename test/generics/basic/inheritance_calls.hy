// Runtime smoke test for generic inheritance member access

class Box<T>
{
    protected T value

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

class TrackingBox<T> inherits Box<T>
{
    TrackingBox(T initial)
    {
        base(initial)
    }

    T Snapshot()
    {
        return this.Get()
    }
}

void main()
{
    TrackingBox<int> tracked = (5)
    tracked.Set(10)

    assert tracked.Get() == 10
    assert tracked.Snapshot() == 10
    tracked.Set(5)
    assert tracked.Get() == 5

    Box<int> asBase = tracked
    assert asBase.Get() == 5
}