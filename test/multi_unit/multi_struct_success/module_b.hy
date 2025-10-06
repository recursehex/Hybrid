// Consumes Foo defined in module_a and provides an entry point
int readFoo(Foo data) {
    return data.value
}

int main() {
    Foo temp = Foo(42)
    return readFoo(temp)
}
