decimal whole = 42
decimal fractional = 12.5
decimal exponent = 1.25e2
decimal leadingDot = .5
decimal wholeNumber = 2

assert int: whole == 42
assert int: (fractional * 10) == 125
assert int: exponent == 125
assert int: (leadingDot * 10) == 5
assert int: wholeNumber == 2

decimal implicitFromInt = 77
assert int: implicitFromInt == 77
