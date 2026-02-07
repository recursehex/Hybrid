decimal left = 10
decimal right = 3

assert int: (left + right) == 13
assert int: (left - right) == 7
assert int: (left * right) == 30
assert int: (left / right) == 3
assert int: (left % right) == 1

assert left > right
assert left >= right
assert right < left
assert right <= left
assert left != right
assert left == 10

decimal mixed = 5
mixed += 2
mixed *= 3
mixed -= 1
mixed /= 2
assert int: mixed == 10

decimal counter = 1
counter++
++counter
counter--
assert int: counter == 2
