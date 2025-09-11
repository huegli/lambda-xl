
# Implementation Strategy for testing out the libatari800 library in Swift

## Step 1: Create an XCode project that outputs a "Hello World" string in Swift ✅

## Step 2: Add Objective-C Bridging header ✅
- Create a dummy C file that exports the following functions
  - A function that prints "Hello World" to StdOut
  - A function that returns <n>, where <n> is a parameter of the function
  - A function that takes a file name string as input, reads the file and outputs the first line to StdOut
  - A function that take a pointer to a struct of the following format as input and outputs the elements of the struct:
```
typedef struct {
    unsigned char byte;
    unsigned short word;
    uint32_t long;
} input_t
``` 
  - A function that allocates a struct of the following format on the heap and initializes all elements with dummy data:
```
typedef struct {
    union {
        uint32_t a;
        unsigned short b;
        unsigned char storage[6]
    };
    unsigned char bytes[16];
} state_t
  - A function that returns a pointer to the allocated struct of type state_t
  - A function that frees the memory allocated for the struct of type state_t
- In main.swift, call all of the above C functions using the Objective-C Bridging functionality
  - Using the returned pointer to a struct of type state_t, print out the values a, b and the 2nd element of storage as well as the first 8 elements of bytes

