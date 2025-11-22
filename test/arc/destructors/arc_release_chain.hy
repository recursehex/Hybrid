int baseDrops = 0
int derivedDrops = 0

class Base
{
    Base() {}

    ~Base()
    {
        baseDrops++
    }
}

class Derived : Base
{
    Derived() : Base() {}

    ~Derived()
    {
        derivedDrops++
    }
}

void build()
{
    Base handle = Derived()
}

int main()
{
    build()
    assert baseDrops == 1
    assert derivedDrops == 1
    return 0
}