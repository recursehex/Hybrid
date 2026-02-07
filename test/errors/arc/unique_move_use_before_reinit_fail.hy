// EXPECT_DIAGNOSTIC: unique pointer 'donor' was moved and cannot be used until reassigned
// Using a moved unique<T> before it is reassigned should still fail.

struct Widget
{
    int value

    Widget(int v)
    {
        this.value = v
    }
}

int consume(unique<Widget> widget)
{
    return 0
}

int main()
{
    unique<Widget> donor = unique<Widget>(Widget(1))
    unique<Widget> slot = unique<Widget>(Widget(2))

    slot = donor
    int leaked = consume(donor) // should fail: donor was moved already

    donor = unique<Widget>(Widget(leaked))
    return leaked
}
