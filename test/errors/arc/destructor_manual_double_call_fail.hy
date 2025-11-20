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