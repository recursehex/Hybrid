int sourceInt = 42
decimal fromInt = decimal: sourceInt
assert int: fromInt == 42

ulong sourceUlong = 99
decimal fromUlong = decimal: sourceUlong
assert int: fromUlong == 99

double sourceDouble = 3.5
decimal fromDouble = decimal: sourceDouble
double toDouble = double: fromDouble
assert int: (toDouble * 10) == 35

float sourceFloat = 1.25
decimal fromFloat = decimal: sourceFloat
assert int: (double: fromFloat * 100) == 125

decimal value = 123.9
int toInt = int: value
long toLong = long: value
assert toInt == 123
assert toLong == 123
