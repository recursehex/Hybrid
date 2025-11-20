// free on smart pointer handle should be rejected

class Thing
{
    Thing() {}
}

int main()
{
    shared<Thing> handle
    free handle
    return 0
}