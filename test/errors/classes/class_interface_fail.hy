// EXPECT_DIAGNOSTIC: Class 'Invisible' does not implement interface member 'Drawable.Draw'
interface Drawable
{
    void Draw()
}

class Invisible inherits Drawable
{
    Invisible() {}
}

// expect: Class 'Invisible' does not implement interface member 'Drawable.Draw'
