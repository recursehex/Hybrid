// Ensures repeated instantiations exercise generics cache hits.

class Holder<T>
{
    T value

    Holder(T value)
    {
        this.value = value
    }

    T Fetch()
    {
        return this.value
    }
}

void AssignIdentity<T>(ref T target, T value)
{
    target = value
}

int sumTriplet(int a, int b, int c)
{
    int firstValue = 0
    int secondValue = 0
    int thirdValue = 0
    AssignIdentity<int>(ref firstValue, a)
    AssignIdentity<int>(ref secondValue, b)
    AssignIdentity<int>(ref thirdValue, c)

    Holder<int> first = (firstValue)
    Holder<int> second = (secondValue)
    Holder<int> third = (thirdValue)

    return first.Fetch() + second.Fetch() + third.Fetch()
}

int sumPair(int a, int b)
{
    int leftValue = 0
    int rightValue = 0
    AssignIdentity<int>(ref leftValue, a)
    AssignIdentity<int>(ref rightValue, b)

    Holder<int> left = (leftValue)
    Holder<int> right = (rightValue)

    return left.Fetch() + right.Fetch()
}

int main()
{
    int total = sumTriplet(1, 2, 3) + sumPair(4, 5) + sumPair(6, 7)
    assert total == 28
    return 0
}