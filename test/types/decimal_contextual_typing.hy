decimal fromLiteral = 12.5
assert int: (fromLiteral * 10) == 125

decimal assignTarget = 0
assignTarget = 3.75
assert int: (assignTarget * 100) == 375

decimal addOne(decimal value)
{
    return value + 1
}

decimal returnLiteral()
{
    return 2.25
}

decimal fromCall = addOne(4.5)
decimal fromReturn = returnLiteral()

assert int: (fromCall * 10) == 55
assert int: (fromReturn * 100) == 225
