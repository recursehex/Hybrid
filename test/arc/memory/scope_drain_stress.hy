int drops = 0

class Token
{
    int id

    Token(int value)
    {
        this.id = value
    }

    ~Token()
    {
        drops += this.id
    }
}

void churnScopes()
{
    int i = 1
    int expected = 0
    while i < 21
    {
        {
            Token root = (i)
            {
                Token alias = root
                Token copy = alias
            }
            assert drops == expected
        }

        expected += i
        assert drops == expected
        i++
    }
}

int main()
{
    churnScopes()
    assert drops == 210
    return 0
}
