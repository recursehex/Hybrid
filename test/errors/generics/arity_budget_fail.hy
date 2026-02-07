// EXPECT_DIAGNOSTIC: Generic instantiation budget exceeded (limit 1)
// EXPECT_DIAGNOSTIC: Unknown field type 'Box<string>' in struct 'NeedsString'
// RUN_OPTS: --max-generic-instantiations 1

class Box<T>
{
    T value

    Box(T value)
    {
        this.value = value
    }
}

class NeedsInt
{
    Box<int> keeper

    NeedsInt()
    {
        this.keeper = Box<int>(0)
    }
}

class NeedsString
{
    Box<string> keeper

    NeedsString()
    {
        this.keeper = Box<string>("s")
    }
}
