// EXPECT_DIAGNOSTIC: Type 'DuplicateDestructor' already declares a destructor
// EXPECT_DIAGNOSTIC: Tuple literals require at least two elements
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// EXPECT_DIAGNOSTIC: Unexpected token while parsing an expression
// duplicate destructor definitions should be rejected

class DuplicateDestructor
{
    DuplicateDestructor() {}
    ~DuplicateDestructor() {}
    ~DuplicateDestructor() {}
}

int main()
{
    DuplicateDestructor value = DuplicateDestructor()
    return 0
}
