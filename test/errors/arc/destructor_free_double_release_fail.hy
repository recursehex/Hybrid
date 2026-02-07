// EXPECT_DIAGNOSTIC: Value 'value' was manually released after manual destructor and cannot be used afterwards
// EXPECT_DIAGNOSTIC: Value 'value' is released multiple times (previously via manual destructor) (again via free)
// manual destructor followed by free should be diagnosed as a double release

class Payload
{
    Payload() {}
    ~Payload() {}
}

int main()
{
    Payload value = Payload()
    value.~Payload()
    free value
    return 0
}
