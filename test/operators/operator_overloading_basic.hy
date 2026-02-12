struct IntBox
{
    int value

    IntBox()
    {
        this.value = 0
    }

    IntBox(int value)
    {
        this.value = value
    }

    ref IntBox =(const ref IntBox rhs)
    {
        this.value = rhs.value
        return this
    }

    IntBox +=(const ref IntBox rhs)
    {
        this.value += rhs.value
        return this
    }

    IntBox -=(const ref IntBox rhs)
    {
        this.value -= rhs.value
        return this
    }

    IntBox *=(const ref IntBox rhs)
    {
        this.value *= rhs.value
        return this
    }

    IntBox /=(const ref IntBox rhs)
    {
        this.value /= rhs.value
        return this
    }

    IntBox %=(const ref IntBox rhs)
    {
        this.value %= rhs.value
        return this
    }

    IntBox +(const ref IntBox rhs)
    {
        return IntBox(this.value + rhs.value)
    }

    IntBox -(const ref IntBox rhs)
    {
        return IntBox(this.value - rhs.value)
    }

    IntBox *(const ref IntBox rhs)
    {
        return IntBox(this.value * rhs.value)
    }

    IntBox /(const ref IntBox rhs)
    {
        return IntBox(this.value / rhs.value)
    }

    IntBox %(const ref IntBox rhs)
    {
        return IntBox(this.value % rhs.value)
    }

    bool ==(const ref IntBox rhs)
    {
        return this.value == rhs.value
    }

    bool !=(const ref IntBox rhs)
    {
        return this.value != rhs.value
    }

    bool <(const ref IntBox rhs)
    {
        return this.value < rhs.value
    }

    bool >(const ref IntBox rhs)
    {
        return this.value > rhs.value
    }

    bool <=(const ref IntBox rhs)
    {
        return this.value <= rhs.value
    }

    bool >=(const ref IntBox rhs)
    {
        return this.value >= rhs.value
    }
}

IntBox a = IntBox(10)
IntBox b = IntBox(4)
assert a.value == 10
assert b.value == 4

IntBox sum = a + b
assert sum.value == 14

IntBox diff = a - b
assert diff.value == 6

IntBox prod = a * b
assert prod.value == 40

IntBox quot = a / b
assert quot.value == 2

IntBox rem = a % b
assert rem.value == 2

a += b
assert a.value == 14

a -= IntBox(3)
assert a.value == 11

a *= IntBox(2)
assert a.value == 22

a /= IntBox(2)
assert a.value == 11

a %= IntBox(6)
assert a.value == 5

IntBox assigned = IntBox(0)
assigned = a
assert assigned.value == 5

assert assigned == a
assert assigned != b
assert b < assigned
assert assigned > b
assert b <= assigned
assert assigned >= b
