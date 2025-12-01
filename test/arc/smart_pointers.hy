// Comprehensive smart pointer compile-time test
// Tests construction, copy/move semantics, and destruction for unique, shared, and weak

class TestObject
{
    int value

    TestObject(int v)
    {
        this.value = v
    }

    int getValue()
    {
        return this.value
    }
}

void testUniquePointers()
{
    unique<int> uniqueInt = (42)
    unique<TestObject> uniqueObj = (10)
}

void testUniqueReassignment()
{
    unique<TestObject> donor = (11)
    unique<TestObject> slot = (12)

    slot = donor
    donor = (13)

    int i = 0
    while i < 15
    {
        slot = donor
        donor = (20 + i)
        i++
    }
}

void testSharedPointers()
{
    shared<int> sharedInt = (99)
    shared<TestObject> sharedObj = (20)
    shared<TestObject> sharedCopy = sharedObj
    shared<TestObject> sharedNext = (25)
    sharedCopy = sharedNext
}

void testWeakPointers()
{
    shared<int> sharedInt = (55)
    weak<int> weakInt = (sharedInt)
}

void testSmartPointerAssignments()
{
    shared<TestObject> sharedA = (30)
    shared<TestObject> sharedB = (31)
    sharedA = sharedB
    shared<TestObject> sharedC = (32)
    sharedA = sharedC

    shared<TestObject> owner = (40)
    weak<TestObject> watcher = (owner)
    weak<TestObject> watcher2 = (owner)
    watcher = watcher2
}

void testTypeDescriptions()
{
    string uniqueDesc = describeType("unique<int>")
    assert uniqueDesc == "type:unique<int>|kind:class|baseClass:none|interfaces:none|genericParameters:T|typeArgumentBindings:T=int|genericMethodInstantiations:none"

    string sharedDesc = describeType("shared<TestObject>")
    assert sharedDesc == "type:shared<TestObject>|kind:class|baseClass:none|interfaces:none|genericParameters:T|typeArgumentBindings:T=TestObject|genericMethodInstantiations:shared.use_count=1"

    string weakDesc = describeType("weak<int>")
    assert weakDesc == "type:weak<int>|kind:class|baseClass:none|interfaces:none|genericParameters:T|typeArgumentBindings:T=int|genericMethodInstantiations:weak.lock=1"
}

int main()
{
    testUniquePointers()
    testUniqueReassignment()
    testSharedPointers()
    testWeakPointers()
    testSmartPointerAssignments()
    testTypeDescriptions()
    return 0
}