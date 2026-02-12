struct CounterBuffer
{
    int storage

    CounterBuffer()
    {
        this.storage = 0
    }

    ref int [](const ref int index)
    {
        return ref this.storage
    }
}

CounterBuffer buffer = CounterBuffer()
buffer[0] = 7
assert buffer[1] == 7

buffer[2] = buffer[2] + 5
assert buffer[3] == 12
