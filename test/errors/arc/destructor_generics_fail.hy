// EXPECT_DIAGNOSTIC: Destructors cannot declare generic parameters
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// destructors cannot declare generic parameters

class GenericDestructor<T>
{
    GenericDestructor() {}
    ~GenericDestructor<T>() {}
}

int main()
{
    GenericDestructor<int> value = GenericDestructor<int>()
    return 0
}
