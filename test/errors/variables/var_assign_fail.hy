int seed = 10

void varAssign()
{
    int total = seed
    total = total + 5

    int[] samples = [1, 2, 3]
    int first = samples[0]

    bool ready = false
    if (total > 0)
    {
        ready = true
    }

    ref int alias = ref total
    alias = alias + first

    bool ready = true   // should fail: same-scope redeclaration
    int first = 99      // should also fail: duplicate local binding
}

int seed = 20          // should fail: duplicate global binding
