class Vector3
{
    int x
    int y
    int z

    Vector3()
    {
        this.x = 0
        this.y = 0
        this.z = 0
    }

    Vector3(int x, int y, int z)
    {
        this.x = x
        this.y = y
        this.z = z
    }

    int LengthSq()
    {
        return this.x * this.x + this.y * this.y + this.z * this.z
    }

    ref Vector3 =(const ref Vector3 rhs)
    {
        this.x = rhs.x
        this.y = rhs.y
        this.z = rhs.z
        return this
    }

    Vector3 +=(const ref Vector3 rhs)
    {
        this.x += rhs.x
        this.y += rhs.y
        this.z += rhs.z
        return this
    }

    Vector3 -=(const ref Vector3 rhs)
    {
        this.x -= rhs.x
        this.y -= rhs.y
        this.z -= rhs.z
        return this
    }

    Vector3 *=(const ref Vector3 rhs)
    {
        this.x *= rhs.x
        this.y *= rhs.y
        this.z *= rhs.z
        return this
    }

    Vector3 /=(const ref Vector3 rhs)
    {
        this.x /= rhs.x
        this.y /= rhs.y
        this.z /= rhs.z
        return this
    }

    Vector3 %=(const ref Vector3 rhs)
    {
        this.x %= rhs.x
        this.y %= rhs.y
        this.z %= rhs.z
        return this
    }

    Vector3 +(const ref Vector3 rhs)
    {
        return Vector3(this.x + rhs.x, this.y + rhs.y, this.z + rhs.z)
    }

    Vector3 -(const ref Vector3 rhs)
    {
        return Vector3(this.x - rhs.x, this.y - rhs.y, this.z - rhs.z)
    }

    Vector3 *(const ref Vector3 rhs)
    {
        return Vector3(this.x * rhs.x, this.y * rhs.y, this.z * rhs.z)
    }

    Vector3 /(const ref Vector3 rhs)
    {
        return Vector3(this.x / rhs.x, this.y / rhs.y, this.z / rhs.z)
    }

    Vector3 %(const ref Vector3 rhs)
    {
        return Vector3(this.x % rhs.x, this.y % rhs.y, this.z % rhs.z)
    }

    bool ==(const ref Vector3 rhs)
    {
        return this.x == rhs.x && this.y == rhs.y && this.z == rhs.z
    }

    bool !=(const ref Vector3 rhs)
    {
        return this.x != rhs.x || this.y != rhs.y || this.z != rhs.z
    }

    bool <(const ref Vector3 rhs)
    {
        return this.LengthSq() < rhs.LengthSq()
    }

    bool >(const ref Vector3 rhs)
    {
        return this.LengthSq() > rhs.LengthSq()
    }

    bool <=(const ref Vector3 rhs)
    {
        return this.LengthSq() <= rhs.LengthSq()
    }

    bool >=(const ref Vector3 rhs)
    {
        return this.LengthSq() >= rhs.LengthSq()
    }

    ref int [](const ref int index)
    {
        if index == 0 {
            return ref this.x
        }
        if index == 1 {
            return ref this.y
        }
        return ref this.z
    }
}

Vector3 a = Vector3(12, 18, 24)
Vector3 b = Vector3(3, 6, 9)

assert a[0] == 12
assert a[1] == 18
assert a[2] == 24

a[2] = a[2] + 6
assert a.z == 30

Vector3 sum = a + b
assert sum.x == 15
assert sum.y == 24
assert sum.z == 39

Vector3 diff = a - b
assert diff.x == 9
assert diff.y == 12
assert diff.z == 21

Vector3 prod = Vector3(3, 6, 9) * Vector3(2, 3, 4)
assert prod.x == 6
assert prod.y == 18
assert prod.z == 36

Vector3 quot = a / Vector3(3, 3, 6)
assert quot.x == 4
assert quot.y == 6
assert quot.z == 5

Vector3 rem = a % Vector3(5, 7, 8)
assert rem.x == 2
assert rem.y == 4
assert rem.z == 6

a += b
assert a.x == 15
assert a.y == 24
assert a.z == 39

a -= Vector3(5, 4, 3)
assert a.x == 10
assert a.y == 20
assert a.z == 36

a *= Vector3(2, 2, 1)
assert a.x == 20
assert a.y == 40
assert a.z == 36

a /= Vector3(5, 4, 6)
assert a.x == 4
assert a.y == 10
assert a.z == 6

a %= Vector3(3, 7, 4)
assert a.x == 1
assert a.y == 3
assert a.z == 2

Vector3 assigned = Vector3()
assigned = a
assert assigned.x == 1
assert assigned.y == 3
assert assigned.z == 2

assert assigned == Vector3(1, 3, 2)
assert assigned != b
assert assigned < b
assert assigned <= b
assert assigned > Vector3(1, 1, 1)
assert assigned >= Vector3(1, 3, 2)

assigned[1] = assigned[1] + 9
assert assigned[1] == 12
assert assigned > Vector3(1, 3, 2)
assert assigned >= Vector3(1, 3, 2)
assert assigned < Vector3(20, 20, 20)
assert assigned <= Vector3(20, 20, 20)

assert assigned.LengthSq() == 149
