// EXPECT_DIAGNOSTIC: Class 'Uses' must declare at least one constructor
// Expect generic arity mismatch diagnostic
class Box<T>
{
    Box() {}
}

class Uses
{
    Box<int, string> invalidField
}

void main() {}
