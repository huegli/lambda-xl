import Foundation
import CBridge

// Call the C function to print Hello World
print_hello_world()

// Call the function that returns a number
let result = return_number(42)
print("Returned number: 0x\(String(result, radix: 16, uppercase: true))")

// Create a test file to read from
let testContent = "This is the first line\nThis is the second line"
let testFilePath = "/tmp/test_file.txt"
try? testContent.write(toFile: testFilePath, atomically: true, encoding: .utf8)

// Call the function to read first line of file
print("First line from file:")
read_file_first_line(testFilePath)

// Create and test input_t struct
var inputStruct = input_t(byte: 255, word: 65535, long_val: 4294967295)
print("Input struct contents:")
print_input_struct(&inputStruct)

// Allocate state_t struct and test it
guard let statePtr = allocate_state_struct() else {
    print("Failed to allocate state struct")
    exit(1)
}

// Access the state_t struct members
let stateStruct = statePtr.pointee
print("State struct contents:")
print("a: 0x\(String(stateStruct.a, radix: 16, uppercase: true))")
print("b: 0x\(String(stateStruct.b, radix: 16, uppercase: true))")
print("storage[2]: 0x\(String(stateStruct.storage.2, radix: 16, uppercase: true))")

// Print first 8 elements of bytes array
print("First 8 bytes:", terminator: " ")
for i in 0..<8 {
    let byteValue = withUnsafeBytes(of: stateStruct.bytes) { bytes in
        bytes[i]
    }
    print("0x\(String(byteValue, radix: 16, uppercase: true))", terminator: " ")
}
print()

// Free the allocated memory
free_state_struct(statePtr)

// Initialize libatari800 with specified arguments
let args = ["-atari"]
var cArgs = args.map { strdup($0) }

let initResult = libatari800_init(-1, &cArgs)
if initResult == 0 {
    print("libatari800 initialized successfully")
} else {
    print("libatari800 initialization failed with code: \(initResult)")
    if let errorMsg = libatari800_error_message() {
        print("Error message: \(String(cString: errorMsg))")
    }
}

// Exit libatari800
libatari800_exit()
print("libatari800 exited")

// Clean up allocated strings
for arg in cArgs {
    free(arg)
}
