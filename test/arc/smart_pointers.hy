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
    unique<int> uniqueInt = unique<int>(42)
    unique<TestObject> uniqueObj = unique<TestObject>(TestObject(10))
}

void testSharedPointers()
{
    shared<int> sharedInt = shared<int>(99)
    shared<TestObject> sharedObj = shared<TestObject>(TestObject(20))
    shared<TestObject> sharedCopy = sharedObj
}

void testWeakPointers()
{
    shared<int> sharedInt = shared<int>(55)
    weak<int> weakInt = weak<int>(sharedInt)
}

void testTypeDescriptions()
{
    string uniqueDesc = describeType("unique<int>")
    assert uniqueDesc == "type:unique<int>|kind:class|baseClass:none|interfaces:none|genericParameters:T|typeArgumentBindings:T=int|genericMethodInstantiations:none"

    string sharedDesc = describeType("shared<TestObject>")
    assert sharedDesc == "type:shared<TestObject>|kind:class|baseClass:none|interfaces:none|genericParameters:T|typeArgumentBindings:T=TestObject|genericMethodInstantiations:none"

    string weakDesc = describeType("weak<int>")
    assert weakDesc == "type:weak<int>|kind:class|baseClass:none|interfaces:none|genericParameters:T|typeArgumentBindings:T=int|genericMethodInstantiations:none"
}

int main()
{
    testUniquePointers()
    testSharedPointers()
    testWeakPointers()
    testTypeDescriptions()
    return 0
}