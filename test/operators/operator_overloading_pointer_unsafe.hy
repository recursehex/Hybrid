struct PointerLike
{
    int value

    PointerLike()
    {
        this.value = 0
    }

    PointerLike(int value)
    {
        this.value = value
    }

    int @()
    {
        return this.value
    }

    int #()
    {
        return this.value + 1000
    }
}

PointerLike p = PointerLike(23)
int main()
{
    unsafe {
        int derefValue = @p
        int addressValue = #p
        assert derefValue == 23
        assert addressValue == 1023
    }
    return 0
}
