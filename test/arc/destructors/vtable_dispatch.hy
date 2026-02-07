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

class Derived inherits Base
{
    Derived() {}
    ~Derived()
    {
        derivedDrops++
    }
}

void build()
{
    Base value = Derived()
    value.~Base()
}

int main()
{
    build()
    build()
    assert baseDrops == 2
    assert derivedDrops == 2
    return 0
}