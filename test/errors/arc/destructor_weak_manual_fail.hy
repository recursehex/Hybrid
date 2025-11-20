// manual destructor on weak reference should be rejected

class Manual
{
    Manual() {}
    ~Manual() {}
}

int main()
{
    weak Manual ref
    ref.~Manual()
    return 0
}