int[] numbers = [1, 2, 3]

void foreachRefTypeMismatch()
{
    for ref float value in numbers
    {
        value += 1.0
    }
}