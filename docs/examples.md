# Example Programs

This document contains complete example programs demonstrating various features of the Hybrid language.

## Basic Examples

### Hello World (with External Function)

```c
// hello.hy
extern int puts(char str)

int main() {
    puts("Hello, World!")
    return 0
}

main()
```

### Simple Calculator

```c
// calculator.hy
int add(int a, int b) { return a + b }
int sub(int a, int b) { return a - b }
int mul(int a, int b) { return a * b }
int div(int a, int b) { return a / b }

int calculate(int op, int x, int y) {
    if op == 1 { return add(x, y) }
    if op == 2 { return sub(x, y) }
    if op == 3 { return mul(x, y) }
    if op == 4 { return div(x, y) }
    return 0
}

// Usage
calculate(1, 10, 5)  // 15 (addition)
calculate(3, 4, 7)   // 28 (multiplication)
```

## Mathematical Functions

### Factorial

```c
// factorial.hy
int factorial(int n) {
    if n <= 1 {
        return 1
    }
    return n * factorial(n - 1)
}

// Calculate 5!
factorial(5)  // Returns 120
```

### Fibonacci

```c
// fibonacci.hy
int fibonacci(int n) {
    if n <= 0 { return 0 }
    if n == 1 { return 1 }
    return fibonacci(n - 1) + fibonacci(n - 2)
}

// Iterative version for better performance
int fibonacciIterative(int n) {
    if n <= 0 { return 0 }
    if n == 1 { return 1 }
    
    int prev = 0
    int curr = 1
    int i = 2
    
    while i <= n {
        int next = prev + curr
        prev = curr
        curr = next
        i = i + 1
    }
    
    return curr
}

fibonacci(10)         // Returns 55
fibonacciIterative(10) // Returns 55 (faster)
```

### Power Function

```c
// power.hy
int power(int base, int exp) {
    int result = 1
    int i = 0
    while i < exp {
        result = result * base
        i = i + 1
    }
    return result
}

power(2, 8)   // Returns 256
power(3, 4)   // Returns 81
```

## Array Operations

### Array Sum

```c
// array_sum.hy
int sumArray(int[] arr, int length) {
    int sum = 0
    int i = 0
    while i < length {
        sum = sum + arr[i]
        i = i + 1
    }
    return sum
}

int main() {
    int[] numbers = [1, 2, 3, 4, 5]
    return sumArray(numbers, 5)  // Returns 15
}
```

### Find Maximum

```c
// find_max.hy
int findMax(int[] arr, int length) {
    if length <= 0 { return 0 }
    
    int max = arr[0]
    int i = 1
    while i < length {
        if arr[i] > max {
            max = arr[i]
        }
        i = i + 1
    }
    return max
}

int[] scores = [85, 92, 78, 95, 88]
findMax(scores, 5)  // Returns 95
```

### Array Reverse

```c
// array_reverse.hy
void reverseArray(int[] arr, int length) {
    int start = 0
    int end = length - 1
    
    while start < end {
        // Swap elements
        int temp = arr[start]
        arr[start] = arr[end]
        arr[end] = temp
        
        start = start + 1
        end = end - 1
    }
    return
}

int[] nums = [1, 2, 3, 4, 5]
reverseArray(nums, 5)
// nums is now [5, 4, 3, 2, 1]
```

## Algorithms

### Linear Search

```c
// linear_search.hy
int linearSearch(int[] arr, int length, int target) {
    int i = 0
    while i < length {
        if arr[i] == target {
            return i  // Found at index i
        }
        i = i + 1
    }
    return -1  // Not found
}

int[] data = [10, 23, 45, 67, 89]
linearSearch(data, 5, 45)  // Returns 2
linearSearch(data, 5, 99)  // Returns -1
```

### Bubble Sort

```c
// bubble_sort.hy
void bubbleSort(int[] arr, int length) {
    int i = 0
    while i < length - 1 {
        int j = 0
        while j < length - i - 1 {
            if arr[j] > arr[j + 1] {
                // Swap
                int temp = arr[j]
                arr[j] = arr[j + 1]
                arr[j + 1] = temp
            }
            j = j + 1
        }
        i = i + 1
    }
    return
}

int[] unsorted = [64, 34, 25, 12, 22, 11, 90]
bubbleSort(unsorted, 7)
// unsorted is now [11, 12, 22, 25, 34, 64, 90]
```

## String and Character Operations

### Character Counter

```c
// char_counter.hy
int countChar(char[] str, int length, char target) {
    int count = 0
    int i = 0
    while i < length {
        if str[i] == target {
            count = count + 1
        }
        i = i + 1
    }
    return count
}

char[] text = ['h', 'e', 'l', 'l', 'o']
countChar(text, 5, 'l')  // Returns 2
```

## Complex Examples

### Grade Calculator

```c
// grade_calculator.hy
char getLetterGrade(int score) {
    if score >= 90 { return 'A' }
    if score >= 80 { return 'B' }
    if score >= 70 { return 'C' }
    if score >= 60 { return 'D' }
    return 'F'
}

float calculateAverage(int[] scores, int count) {
    int sum = 0
    int i = 0
    while i < count {
        sum = sum + scores[i]
        i = i + 1
    }
    return sum / count
}

int main() {
    int[] testScores = [92, 85, 78, 95, 88]
    float avg = calculateAverage(testScores, 5)
    char grade = getLetterGrade(avg)
    return grade  // Returns 'B' (87.6 average)
}
```

### Temperature Converter

```c
// temperature.hy
float celsiusToFahrenheit(float celsius) {
    return celsius * 9.0 / 5.0 + 32.0
}

float fahrenheitToCelsius(float fahrenheit) {
    return (fahrenheit - 32.0) * 5.0 / 9.0
}

// Convert temperatures
celsiusToFahrenheit(0.0)    // Returns 32.0
celsiusToFahrenheit(100.0)  // Returns 212.0
fahrenheitToCelsius(32.0)   // Returns 0.0
fahrenheitToCelsius(98.6)   // Returns 37.0
```

### Prime Number Checker

```c
// prime_checker.hy
bool isPrime(int n) {
    if n <= 1 { return false }
    if n == 2 { return true }
    if n % 2 == 0 { return false }
    
    int i = 3
    while i * i <= n {
        if n % i == 0 {
            return false
        }
        i = i + 2
    }
    return true
}

// Count primes up to n
int countPrimes(int n) {
    int count = 0
    int i = 2
    while i <= n {
        if isPrime(i) {
            count = count + 1
        }
        i = i + 1
    }
    return count
}

isPrime(17)      // Returns true
isPrime(24)      // Returns false
countPrimes(20)  // Returns 8 (2,3,5,7,11,13,17,19)
```

## Game Examples

### Number Guessing Game Logic

```c
// guess_game.hy
int checkGuess(int secret, int guess) {
    if guess == secret { return 0 }   // Correct
    if guess < secret { return -1 }   // Too low
    return 1                          // Too high
}

bool playRound(int secret, int guess) {
    int result = checkGuess(secret, guess)
    if result == 0 {
        return true  // Won
    }
    return false     // Keep playing
}

// Example game flow
int secret = 42
playRound(secret, 30)  // Returns false (too low)
playRound(secret, 50)  // Returns false (too high)  
playRound(secret, 42)  // Returns true (correct!)
```

## Utility Functions

### Min/Max Functions

```c
// minmax.hy
int min(int a, int b) {
    if a < b { return a }
    return b
}

int max(int a, int b) {
    if a > b { return a }
    return b
}

int clamp(int value, int minVal, int maxVal) {
    if value < minVal { return minVal }
    if value > maxVal { return maxVal }
    return value
}

min(10, 5)        // Returns 5
max(10, 5)        // Returns 10
clamp(15, 0, 10)  // Returns 10
clamp(-5, 0, 10)  // Returns 0
clamp(5, 0, 10)   // Returns 5
```

### Absolute Value

```c
// abs.hy
int abs(int x) {
    if x < 0 {
        return -x
    }
    return x
}

float fabs(float x) {
    if x < 0.0 {
        return -x
    }
    return x
}

abs(-10)     // Returns 10
abs(10)      // Returns 10
fabs(-3.14)  // Returns 3.14
```