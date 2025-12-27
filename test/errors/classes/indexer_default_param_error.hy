// EXPECT_OUTPUT: Indexer parameters cannot declare default values
class DefaultIndexer
{
    int this[int index = 0]
    {
        get 0
        set value
    }

    DefaultIndexer()
    {
    }
}
