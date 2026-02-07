// Ensure functions can return arrays by value

int[] make_numbers()
{
    return [1, 2, 3]
}

string[] make_names()
{
    string[] local = ["alpha", "beta", "gamma"]
    return local
}

int main()
{
    int[] numbers = make_numbers()
    assert numbers[0] == 1
    assert numbers[1] == 2
    assert numbers[2] == 3

    // Returned array can be consumed directly
    assert make_numbers()[0] + make_numbers()[1] == 3

    string[] names = make_names()
    assert names[0] == "alpha"
    assert names[2] == "gamma"

    return 0
}