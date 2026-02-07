// EXPECT_DIAGNOSTIC: Cannot return local variable 'local' by reference
// EXPECT_DIAGNOSTIC: Failed to generate IR for function
ref int getRefToLocal()
{
    int local = 42
    return ref local
}
