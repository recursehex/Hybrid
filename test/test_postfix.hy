// Verify postfix operators work correctly

// Test basic postfix increment
int x = 5
int old_x = x++  // old_x should get 5, x becomes 6

// Verify old_x got the old value (5) and x was incremented to 6
old_x == 5 && x == 6

// Test basic postfix decrement  
int y = 10
int old_y = y--  // old_y should get 10, y becomes 9

// Verify old_y got the old value (10) and y was decremented to 9
old_y == 10 && y == 9

// Test difference between prefix and postfix
int a = 5
int b = 5
int prefix_result = ++a   // a becomes 6, prefix_result gets 6
int postfix_result = b++  // postfix_result gets 5, b becomes 6

// Verify results
prefix_result == 6 && postfix_result == 5 && a == 6 && b == 6

// Test with floats
float f = 2.5
float old_f = f++  // old_f should get 2.5, f becomes 3.5
old_f == 2.5 && f == 3.5