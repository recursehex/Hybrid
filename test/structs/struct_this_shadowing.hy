// Ensure member assignments can disambiguate parameter and local shadowing via `this`

struct Shadow
{
    int value
    int total

    Shadow(int value)
    {
        this.value = value  // parameter shadows member

        int total = value * 2  // local shadows member
        this.total = total
    }
}

Shadow s = Shadow(3)

assert s.value == 3
assert s.total == 6
