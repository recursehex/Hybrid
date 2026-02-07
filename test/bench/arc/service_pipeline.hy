// ARC benchmark: macro-style service loop using constructor-backed queue storage.

class Job
{
    int id
    int weight

    Job(int id, int weight)
    {
        this.id = id
        this.weight = weight
    }
}

class Stage
{
    int bias

    Stage(int bias)
    {
        this.bias = bias
    }
}

class JobQueue
{
    Job[] slots

    JobQueue(int size)
    {
        this.slots = new[size]
    }

    void Store(int index, Job value)
    {
        this.slots[index] = value
    }

    Job Read(int index)
    {
        return this.slots[index]
    }
}

int runServicePipeline(int rounds)
{
    Stage stage = (3)
    Job active = (0, 0)
    JobQueue queue = (16)

    int seed = 0
    while seed < 16
    {
        queue.Store(seed, (seed, seed + stage.bias))
        seed++
    }

    int step = 0
    int total = 0
    while step < rounds
    {
        int slot = step % 16
        Job incoming = (step, step + stage.bias)
        queue.Store(slot, incoming)

        int readSlot = (slot + 5) % 16
        active = queue.Read(readSlot)
        total += active.id + active.weight + stage.bias
        step++
    }

    return total + active.id + active.weight
}

int main()
{
    int sink = runServicePipeline(6000)
    if sink < 0
    {
        return 1
    }
    return 0
}
