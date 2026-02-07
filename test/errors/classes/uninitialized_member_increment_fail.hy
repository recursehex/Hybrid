// EXPECT_DIAGNOSTIC: Cannot increment or otherwise modify uninitialized member 'created' of class 'Counter'
class Counter
{
    static int created

    Counter()
    {
        Counter.created++  // expect: Cannot increment or otherwise modify uninitialized member 'created' of class 'Counter'
    }
}
