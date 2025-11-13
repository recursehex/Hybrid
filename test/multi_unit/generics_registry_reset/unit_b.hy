// Second unit: exercises Ledger<T> and reuses Cache<T> from unit_a.

class Ledger<T>
{
    T seed

    Ledger(T initial)
    {
        this.seed = initial
    }

    T Transfer<U>(T payload, U annotation)
    {
        return payload
    }
}

int main()
{
    int cached = verifyCache()
    assert cached == 73

    Ledger<string> audit = ("alpha")
    string current = audit.Transfer<int>("alpha", 10)
    assert current == "alpha"

    string summary = describeType("Ledger<string>")
    string expected = "type:Ledger<string>|kind:class|baseClass:none|interfaces:none|genericParameters:T|typeArgumentBindings:T=string|genericMethodInstantiations:Ledger.Transfer=1"
    assert summary == expected
    return 0
}