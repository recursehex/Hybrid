// EXPECT_DIAGNOSTIC: Duplicate generic parameter 'T' in type 'Bad'
// Expect duplicate generic parameter diagnostic
class Bad<T, T>
{
    Bad() {}
}

void main() {}
