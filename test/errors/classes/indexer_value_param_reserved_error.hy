// EXPECT_DIAGNOSTIC: Indexer parameter name 'value' is reserved
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// EXPECT_DIAGNOSTIC: Unknown variable name: get
// EXPECT_DIAGNOSTIC: Unknown variable name: value
// EXPECT_DIAGNOSTIC: Unknown variable name: set
// EXPECT_DIAGNOSTIC: Unknown variable name: value
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// EXPECT_DIAGNOSTIC: Unknown function referenced: BadIndexer
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
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
