class Bucket
{
    int[] slots

    Bucket(int size)
    {
        this.slots = new[size]
        this.slots[0] = 7
    }

    int Count()
    {
        return this.slots.size
    }

    int First()
    {
        return this.slots[0]
    }
}

int main()
{
    Bucket bucket = Bucket(4)
    assert bucket.Count() == 4
    assert bucket.First() == 7
    return 0
}
