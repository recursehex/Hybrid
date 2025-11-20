int baseDrops = 0
int derivedDrops = 0

class Base
{
    Base() {}
    ~Base()
    {
        baseDrops = baseDrops + 1
    }
}

class Derived : Base
{
    Derived() : Base() {}
    ~Derived()
    {
        derivedDrops = derivedDrops + 1
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