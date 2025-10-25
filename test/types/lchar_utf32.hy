int main()
{
    lchar smile = '😀'
    lchar clef = '\U0001D11E'
    lchar beta = 'β'

    assert int: smile == 0x1F600
    assert int: clef == 0x1D11E
    assert int: beta == 0x03B2

    lchar[] runes = ['😀', 'β', '\U0001D11E']
    assert int: runes[0] == 0x1F600
    assert int: runes[1] == 0x03B2
    assert int: runes[2] == 0x1D11E

    return 0
}