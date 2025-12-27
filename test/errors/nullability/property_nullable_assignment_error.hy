// EXPECT_OUTPUT: Cannot assign nullable value to non-nullable property 'value'
class Holder
{
    int value
    {
        get
        set
    }

    Holder()
    {
        this.value = 1
    }
}

int main()
{
    Holder h = Holder()
    int? maybe = null
    h.value = maybe
    return 0
}
