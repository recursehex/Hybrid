// Reflection smoke test for describeType() output on generic classes.

class Box<T>
{
    T value

    Box(T value)
    {
        this.value = value
    }

    void SwapWith<U>(ref U slot, U replacement)
    {
        slot = replacement
    }

    void CopyValue<U>(ref U slot, U value)
    {
        slot = value
    }
}

void verify_box_instantiations()
{
    Box<int> numbers = Box<int>(3)
    int observed = 0
    numbers.SwapWith<int>(ref observed, 42)
    string snapshot = ""
    numbers.CopyValue<string>(ref snapshot, "ok")

    string intSummary = describeType("Box<int>")
    string expectedInt = "type:Box<int>|kind:class|baseClass:none|interfaces:none|genericParameters:T|typeArgumentBindings:T=int|genericMethodInstantiations:Box.CopyValue=1,Box.SwapWith=1"
    assert intSummary == expectedInt
}

void verify_other_bindings()
{
    Box<string> words = Box<string>("hello")
    string wordSummary = describeType("Box<string>")
    string expectedWord = "type:Box<string>|kind:class|baseClass:none|interfaces:none|genericParameters:T|typeArgumentBindings:T=string|genericMethodInstantiations:none"
    assert wordSummary == expectedWord

    string openSummary = describeType("Box")
    string expectedOpen = "type:Box|kind:class|baseClass:none|interfaces:none|genericParameters:T|typeArgumentBindings:none|genericMethodInstantiations:none"
    assert openSummary == expectedOpen
}

int main()
{
    verify_box_instantiations()
    verify_other_bindings()
    return 0
}