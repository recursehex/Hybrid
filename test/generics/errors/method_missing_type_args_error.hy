// EXPECT_DIAGNOSTIC: Method 'Holder.Fill' requires explicit type arguments
// EXPECT_DIAGNOSTIC: Failed to generate IR for function
// Expect error when invoking generic method without explicit type arguments

class Holder<T>
{
    Holder(T value)
    {
        this.value = value
    }

    void Fill<U>(ref U slot, U input)
    {
        slot = input
    }

    T value
}

void main()
{
    Holder<int> holder = Holder<int>(0)
    int result = 0
    holder.Fill(ref result, 5)
}
