// ARC smart pointer describeType() smoke test.

int main()
{
    string uniqueOwned = describeType("unique<int>")
    assert uniqueOwned == "type:unique<int>|kind:struct|baseClass:none|interfaces:none|genericParameters:T|typeArgumentBindings:T=int|genericMethodInstantiations:unique.get=1"

    string sharedOwned = describeType("shared<int>")
    assert sharedOwned == "type:shared<int>|kind:struct|baseClass:none|interfaces:none|genericParameters:T|typeArgumentBindings:T=int|genericMethodInstantiations:shared.arcUseCount=1,shared.get=1,shared.weak=1"

    string weakObserved = describeType("weak<int>")
    assert weakObserved == "type:weak<int>|kind:struct|baseClass:none|interfaces:none|genericParameters:T|typeArgumentBindings:T=int|genericMethodInstantiations:weak.get=1,weak.lock=1"

    string uniqueOpen = describeType("unique")
    assert uniqueOpen == "type:unique|kind:class|baseClass:none|interfaces:none|genericParameters:T|typeArgumentBindings:none|genericMethodInstantiations:none"
    return 0
}