// EXPECT_DIAGNOSTIC: '@autoreleasepool' is reserved for upcoming ARC support
int main()
{
    @autoreleasepool {
        int value = 1
    }

    return 0
}
