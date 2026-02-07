// EXPECT_DIAGNOSTIC: Value 'value' was manually released after free and cannot be used afterwards
// EXPECT_DIAGNOSTIC: Value 'value' is released multiple times (previously via free) (again via free)
// double free should be diagnosed at compile time

class Payload
{
    Payload() {}
    ~Payload() {}
}

int main()
{
    Payload value = new Payload()
    free value
    free value
    return 0
}
