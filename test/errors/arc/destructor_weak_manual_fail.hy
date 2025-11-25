// manual destructor on weak smart pointer should be rejected

class Manual
{
    Manual() {}
    ~Manual() {}
}

int main()
{
    weak<Manual> ref = weak<Manual>()
    ref.~Manual()
    return 0
}