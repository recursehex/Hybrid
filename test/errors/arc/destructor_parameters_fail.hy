// EXPECT_DIAGNOSTIC: Destructors cannot declare parameters
// EXPECT_DIAGNOSTIC: Unknown variable name: code
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// destructors must not declare parameters

class DestructorWithParams
{
    DestructorWithParams() {}
    ~DestructorWithParams(int code) {}
}

int main()
{
    DestructorWithParams value = DestructorWithParams()
    return 0
}
