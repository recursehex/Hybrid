// Verify support for binary, octal, hex, and scientific notation literals
int main() {
    int binary = 0b11010110
    assert binary == 214

    byte nibble = 0b00001111
    assert nibble == 15

    ulong allBits = 0b1111111111111111111111111111111111111111111111111111111111111111
    assert allBits == 0xFFFFFFFFFFFFFFFF

    int permissions = 0o755
    assert permissions == 493

    uint octMask = 0o7777
    assert octMask == 4095

    uint hexColor = 0x00FFAA33
    assert hexColor == 0x00FFAA33

    ulong fullMask = 0xFFFFFFFFFFFFFFFF
    assert fullMask == allBits

    double fractional = .5
    assert fractional == 0.5

    double uppercaseExp = 1.2E+3
    assert uppercaseExp == 1200

    double ratio = 1e3 / 1e1
    assert ratio == 100

    double avogadro = 6.022e23
    double planck = 6.626e-34
    double product = avogadro * planck
    assert product > 3.9e-10 && product < 4.1e-10

    float small = 1.25e2
    assert small == 125

    double mixed = 0xF + 0b1010 + 0o12
    assert mixed == 35

    return 0
}
