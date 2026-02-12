// EXPECT_DIAGNOSTIC: Manual destructor calls are not allowed on smart pointers
// EXPECT_DIAGNOSTIC: Failed to generate IR for function
// manual destructor invocation on smart pointer should be rejected

class Manual
{
    Manual() {}
    ~Manual() {}
}

int main()
{
    shared<Manual> handle = shared<Manual>()
    handle.~Manual()
    return 0
}
