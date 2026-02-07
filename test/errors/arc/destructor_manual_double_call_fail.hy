// EXPECT_DIAGNOSTIC: Value 'value' was manually released after manual destructor and cannot be used afterwards
// EXPECT_DIAGNOSTIC: Destructor for 'value' is invoked multiple times in the same scope
// manual destructor should not be invoked more than once

int drops = 0

class ManualTwice
{
    ManualTwice() {}
    ~ManualTwice()
    {
        drops = drops + 1
    }
}

int main()
{
    ManualTwice value = ManualTwice()
    value.~ManualTwice()
    value.~ManualTwice()
    return 0
}
