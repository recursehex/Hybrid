// Test compound assignment operators

// Basic variable compound assignments
int x = 10
x += 5  // x should be 15
x -= 3  // x should be 12
x *= 2  // x should be 24
x /= 4  // x should be 6
x %= 5  // x should be 1

// Test with different values
int a = 100
a += 50   // a = 150
a -= 25   // a = 125
a *= 2    // a = 250
a /= 5    // a = 50
a %= 7    // a = 1

// Test with expressions
int b = 20
b += (10 + 5)  // b = 35
b -= (5 * 2)   // b = 25
b *= (3 + 1)   // b = 100
b /= (20 / 4)  // b = 20
b %= (7 + 1)   // b = 4

// Test with arrays
int[] arr = [10, 20, 30, 40, 50]
arr[0] += 5   // arr[0] = 15
arr[1] -= 10  // arr[1] = 10
arr[2] *= 2   // arr[2] = 60
arr[3] /= 4   // arr[3] = 10
arr[4] %= 7   // arr[4] = 1

// Function using compound assignments
int compound_test(int val) {
    val += 10
    val *= 2
    val %= 15
    return val
}

// Test function
compound_test(5)  // (5+10)*2 % 15 = 30 % 15 = 0