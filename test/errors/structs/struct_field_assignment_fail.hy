// EXPECT_DIAGNOSTIC: Cannot implicitly convert 'long' to 'int' in assignment to field 'value'; explicit cast required
// EXPECT_DIAGNOSTIC: Failed to generate IR for function
struct Holder
{
    int value

    Holder(int value)
    {
        this.value = value
    }
}

void assignWrongType()
{
    Holder h = Holder(0)
    long wide = 42
    h.value = wide   // expect: Cannot implicitly convert 'long' to 'int' in assignment to field 'value'; explicit cast required
}
