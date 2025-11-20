// manual destructor invocation on smart pointer should be rejected

class Manual
{
    Manual() {}
    ~Manual() {}
}

int main()
{
    shared<Manual> handle
    handle.~Manual()
    return 0
}