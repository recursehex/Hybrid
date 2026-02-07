/* Leading block comment should be ignored */

int multiply(int a, int b)
{
    return a * b
}

/* Multi-line block comment
   that spans several lines */
int main()
{
    int x = 2
    int y = /* inline block comment */ 3
    int z = multiply(x, y /* block comment inside call */)

    /* Comment before an assertion */
    assert z == 6
    return 0
}
