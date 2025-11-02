// Verify that structs support multiple constructors and that each initializes all fields

struct Box
{
    int width
    int height

    Box(int size)
    {
        this.width = size
        this.height = size
    }

    Box(int width, int height)
    {
        this.width = width
        this.height = height
    }
}

Box square = Box(5)
Box rectangle = Box(3, 7)

assert square.width == 5
assert square.height == 5

assert rectangle.width == 3
assert rectangle.height == 7