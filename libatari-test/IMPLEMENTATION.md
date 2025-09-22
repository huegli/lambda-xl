
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

## Step 3: Add an Objective-C Bridging header for all the functions defined in the libatari800 library ✅
- The C include file for the libatari800 library can be found at ../atari800/src/libatari800/libatari800.h
- Steps to build ../atari800/src/libatari800.a
  1. cd ../atari800/
  2. ./configure --target=libatari800
  3. make
- Statically link ../atari800/libatari800.a into libatari-test
- In main.swift, call the libatari800_init function and the libatari800_exit function. Do not remove the existing content in main.swift
- For libatari800_init, use "-atari" as arguments

## Step 4: Make libatari800 library compilation part of the Swift project building ✅
- Create a build mechanism that first ensures that ../atari800/libatari800.a is compiled correctly as a library before building libatari-test
- Created `build_libatari800.sh` script to automatically build the libatari800 library with proper dependency checking
- Created `build.sh` wrapper script that builds libatari800 first, then builds the Swift project
- Created `run.sh` script for convenient build and run operations
- Updated Package.swift to use correct library path (`../atari800/src`)
- Updated BUILD.md with new build instructions

## Step 5: Replicate the functionality of libatari800_test.c ✅
- Refer to ../atari800/src/libatari800/libatari800_test.c
- Reimplement the same functionality calling the same functions from libatari800.h (library libatari800.a) in the main.swift file of libatari-test
- The libatari800_init should be called with "-atari" and use the libatari.cfg configuration file
- Verify that libatari-test works properly by making sure the output is similar to the one produced by the original libatari800_test.c file


## Step 6: Graphical output of the emulated screen ✅
- Remove the WAVWriter class and all functionality associated with it, do not check for a '-wav' command line argument ✅
- Assume 'showScreen' is always true ✅
- Remove all functionality from "print_hello_world" to "free_state_struct" ✅
- Rather than using the debug_screen to output the emulated screen accessed by libatari800_get_screen_ptr() using textual representation, create a SwiftUI window that displays the entire 384x240 byte content as follows: ✅
  - Each byte represents a pixel where the high 4 bits are the hue and the low 4 bits are the luminance ✅
  - Each emulated pixel should be represented by a 16x16 square ✅
- Update the SwiftUI window each time libatari800_next_frame is called ✅
- The calling frequency for libatari800_next_frame should be 60 Hz ✅

**Implementation Details:**
- Created `AtariEmulator` class as an `ObservableObject` to manage emulation state
- Implemented SwiftUI app structure with `AtariTestApp`, `ContentView`, and `AtariScreenView`
- Used Canvas in SwiftUI to render the 384x240 pixel screen buffer
- Pixel color decoding converts 4-bit hue and 4-bit luminance to HSB color space
- Timer runs at 60 Hz to call `libatari800_next_frame()` and update display
- Screen scales pixels appropriately to create visible 16x16 squares
- Removed all Step 2 bridge functions and WAV output functionality as requested

