// EXPECT_DIAGNOSTIC: Expected type argument in generic list
// EXPECT_DIAGNOSTIC: Failed to parse member type
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// EXPECT_DIAGNOSTIC: Unknown variable name: invalidField
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// Expect unknown type argument diagnostic
class List<T>
{
    List()
    {
    }
}

class Uses<T>
{
    List<U> invalidField
}

void main()
{
}
