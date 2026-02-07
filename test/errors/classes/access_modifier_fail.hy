// EXPECT_DIAGNOSTIC: Cannot increment or otherwise modify uninitialized member 'visitors' of class 'Widget'
// EXPECT_DIAGNOSTIC: Constructor for class 'Widget' must initialize members 'exposure', 'privateData'
// EXPECT_DIAGNOSTIC: Constructor for class 'Machine' must initialize member 'calls'
// EXPECT_DIAGNOSTIC: Constructor for class 'ProtectedExample' must initialize member 'counter'
class Widget
{
    int privateData
    public int exposure
    static int visitors
    const int limit

    Widget(int limit)
    {
        this.limit = limit
        this.visitors++
    }

    void Touch()
    {
        this.privateData++
        this.exposure++
        this.visitors++
    }
}

void assignPrivate()
{
    Widget w = Widget(3)
    w.privateData = 1  // expect: Member 'privateData' of class 'Widget' is read-only outside its definition
}

void assignConst()
{
    Widget w = Widget(5)
    w.limit = 4  // expect: Cannot write to const member 'limit' of class 'Widget'
}

void assignStatic()
{
    Widget.visitors = 10  // expect: Member 'visitors' of class 'Widget' is read-only outside its definition
}
class Machine
{
    private void Service()
    {
        this.calls++
    }

    public int calls

    Machine()
    {
        this.calls = 0
    }

    public void Run()
    {
        this.Service()
    }
}

void callPrivateMethod()
{
    Machine m = Machine()
    m.Service()  // expect: Cannot call private member 'Service' of class 'Machine'
}

class ProtectedExample
{
    protected int counter

    ProtectedExample()
    {
        this.counter = 1
    }
}

void readProtected()
{
    ProtectedExample example = ProtectedExample()
    int value = example.counter     // expect: Cannot read protected member 'counter' of class 'ProtectedExample' without inheriting from it
}
