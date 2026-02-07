extern unsafe int __hybrid_debug_strong_count(byte@ object)

class Piece
{
    int id

    Piece(int id)
    {
        this.id = id
    }
}

unsafe int strongCount(byte@ object)
{
    return __hybrid_debug_strong_count(object)
}

class BaseOwner
{
    private Piece primary
    protected Piece helper

    BaseOwner(Piece primary, Piece helper)
    {
        this.primary = primary
        this.helper = helper
    }

    Piece exposeHelper()
    {
        return this.helper
    }
}

class DerivedOwner inherits BaseOwner
{
    private Piece child

    DerivedOwner(Piece primary, Piece helper, Piece child)
    {
        base(primary, helper)
        this.child = child
    }

    Piece readChild()
    {
        return this.child
    }
}

int main()
{
    Piece first = (1)
    Piece second = (2)
    Piece third = (3)

    DerivedOwner derived = (first, second, third)
    BaseOwner baseView = derived

    unsafe
    {
        byte@ firstPtr1 = first
        byte@ secondPtr1 = second
        byte@ thirdPtr1 = third
        assert strongCount(firstPtr1) == 2
        assert strongCount(secondPtr1) == 2
        assert strongCount(thirdPtr1) == 2
    }

    Piece helper = baseView.exposeHelper()
    unsafe
    {
        byte@ secondPtr2 = second
        assert strongCount(secondPtr2) == 3
    }

    {
        Piece child = derived.readChild()
        unsafe
        {
            byte@ thirdPtr2 = third
            assert strongCount(thirdPtr2) == 3
        }
    }

    unsafe
    {
        byte@ thirdPtr3 = third
        assert strongCount(thirdPtr3) == 2
    }

    return 0
}
