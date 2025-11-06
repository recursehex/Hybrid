class Door
{
    private int knocks
    public int opens
    static int created = 0
    const int maxOpens

    Door(int max)
    {
        this.maxOpens = max
        this.knocks = 0
        this.opens = 0
        Door.created++
    }

    void Reset()
    {
        this.knocks = 0
        this.opens = 0
    }

    void Knock()
    {
        this.knocks++
        if this.opens < this.maxOpens
        {
            this.opens++
        }
    }

    int Remaining()
    {
        return this.maxOpens - this.opens
    }
}

Door front = Door(2)
Door back = Door(3)

front.Knock()
front.Knock()
back.Knock()

assert front.opens == 2
assert front.Remaining() == 0
assert back.opens == 1
assert back.Remaining() == 2

// Public field writable outside
back.opens++
assert back.opens == 2

back.Reset()
assert back.opens == 0

back.Knock()
assert back.opens == 1

// Static value observed across instances
assert Door.created == 2