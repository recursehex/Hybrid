// EXPECT_OUTPUT: Indexer parameter name 'value' is reserved
class BadIndexer
{
    int this[int value]
    {
        get value
        set value
    }

    BadIndexer()
    {
    }
}
