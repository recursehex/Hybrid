// Test cases for if-else statements and comparison operators

// If-else with integer comparison
int testFunc(int x) {
    if x == 0 {
        return 1
    } else {
        return 2
    }
}

// If-else-if chain
int grade(int score) {
    if score >= 90 {
        return 4  // A
    } else if score >= 80 {
        return 3  // B
    } else if score >= 70 {
        return 2  // C
    } else {
        return 1  // F
    }
}

// Testing all comparison operators
int compareTest(int a, int b) {
    if a == b {
        return 0
    }
    if a != b {
        return 1
    }
    if a < b {
        return 2
    }
    if a > b {
        return 3
    }
    if a <= b {
        return 4
    }
    if a >= b {
        return 5
    }
    return -1
}

// Float comparison
double floatCompare(double x, double y) {
    if x == y {
        return 1.0
    } else if x > y {
        return 2.0
    } else {
        return 0.0
    }
}

// Character comparison
bool charTest(char ch) {
    if ch == 'a' {
        return true
    } else if ch == 'b' {
        return false
    } else {
        return true
    }
}

// Boolean logic in conditions
int boolTest(bool flag1, bool flag2) {
    if flag1 == true {
        if flag2 == false {
            return 1
        } else {
            return 2
        }
    } else {
        return 0
    }
}

// Nested if statements
int nestedTest(int x, int y) {
    if x > 0 {
        if y > 0 {
            return 1
        } else {
            return 2
        }
    } else {
        if y > 0 {
            return 3
        } else {
            return 4
        }
    }
}