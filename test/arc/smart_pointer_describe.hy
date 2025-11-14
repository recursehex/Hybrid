// ARC smart pointer describeType() smoke test.

int main()
{
    string uniqueOwned = describeType("unique<int>")
    assert uniqueOwned == "type:unique<int>|kind:class|baseClass:none|interfaces:none|genericParameters:T|typeArgumentBindings:T=int|genericMethodInstantiations:none"

    string sharedOwned = describeType("shared<int>")
    assert sharedOwned == "type:shared<int>|kind:class|baseClass:none|interfaces:none|genericParameters:T|typeArgumentBindings:T=int|genericMethodInstantiations:none"

    string weakObserved = describeType("weak<int>")
    assert weakObserved == "type:weak<int>|kind:class|baseClass:none|interfaces:none|genericParameters:T|typeArgumentBindings:T=int|genericMethodInstantiations:none"

    string uniqueOpen = describeType("unique")
    assert uniqueOpen == "type:unique|kind:class|baseClass:none|interfaces:none|genericParameters:T|typeArgumentBindings:none|genericMethodInstantiations:none"
    return 0
}