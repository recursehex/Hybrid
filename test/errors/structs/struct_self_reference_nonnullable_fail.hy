// EXPECT_DIAGNOSTIC: Struct 'Loop' must declare at least one constructor
// Non-nullable self references should be rejected with a clear diagnostic.

struct Loop
{
    Loop next
}
