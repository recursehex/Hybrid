// free on stack value should be rejected

class Foo
{
    Foo() {}
}

int main()
{
    Foo local = Foo()
    free local
    return 0
}