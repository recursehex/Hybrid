// EXPECT_DIAGNOSTIC: Indexer parameters cannot declare default values
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// EXPECT_DIAGNOSTIC: Unknown variable name: get
// EXPECT_DIAGNOSTIC: Unknown variable name: set
// EXPECT_DIAGNOSTIC: Unknown variable name: value
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// EXPECT_DIAGNOSTIC: Unknown function referenced: DefaultIndexer
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
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
