class LibraryCard
{
    static int nextNumber = 100
    static int cardsInUse = 0
    int number
    string holder

    LibraryCard(string holder)
    {
        this.holder = holder
        this.number = LibraryCard.nextNumber
        LibraryCard.nextNumber++
        LibraryCard.cardsInUse++
    }

    void Cancel()
    {
        LibraryCard.cardsInUse--
    }

    int CardNumber()
    {
        return this.number
    }
}

LibraryCard alice = ("Alice")
LibraryCard bob = ("Bob")

assert alice.CardNumber() == 100
assert bob.CardNumber() == 101
assert LibraryCard.cardsInUse == 2

bob.Cancel()
assert LibraryCard.cardsInUse == 1

LibraryCard carol = ("Carol")
assert carol.CardNumber() == 102
assert LibraryCard.cardsInUse == 2