// EXPECT_DIAGNOSTIC: No overload of function 'square' matches delegate 'Transform'
// EXPECT_DIAGNOSTIC: Failed to generate IR for function
// EXPECT_OUTPUT: No overload of function 'square' matches delegate 'Transform'
delegate int Transform(int value)

float square(float x)
{
    return x * x
}

int main()
{
    Transform t = square
    return 0
}
