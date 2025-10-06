// Deliberately missing the Foo definition; this compile should fail
int readFoo(Foo data) {
    return data.value
}

int main() {
    Foo temp = Foo(5)
    return readFoo(temp)
}