import Foundation
import CBridge

// Function to display screen contents (equivalent to debug_screen in C)
func debugScreen() {
    // Print out portion of screen, assuming graphics 0 display list
    guard let screenPtr = libatari800_get_screen_ptr() else {
        print("Failed to get screen pointer")
        return
    }

    var screen = screenPtr.advanced(by: 384 * 24 + 24)

    for y in 0..<32 {
        for x in 8..<88 {
            let c = screen.advanced(by: x).pointee
            if c == 0 {
                print(" ", terminator: "")
            } else if c == 0x94 {
                print(".", terminator: "")
            } else if c == 0x9a {
                print("X", terminator: "")
            } else {
                print("?", terminator: "")
            }
        }
        print()
        screen = screen.advanced(by: 384)
    }
}

// WAV file writing functionality
class WAVWriter {
    private var file: FileHandle?
    private var bytesWritten: UInt32 = 0

    func open(_ fileName: String) -> Bool {
        let url = URL(fileURLWithPath: fileName)

        do {
            // Create the file and get a file handle
            FileManager.default.createFile(atPath: fileName, contents: nil, attributes: nil)
            file = try FileHandle(forWritingTo: url)

            // Write WAV header
            file?.write("RIFF".data(using: .ascii)!)
            writeUInt32(0) // length to be filled in later
            file?.write("WAVE".data(using: .ascii)!)

            file?.write("fmt ".data(using: .ascii)!)
            writeUInt32(16)
            writeUInt16(1)
            writeUInt16(UInt16(libatari800_get_num_sound_channels()))
            writeUInt32(UInt32(libatari800_get_sound_frequency()))
            writeUInt32(UInt32(libatari800_get_sound_frequency() * libatari800_get_sound_sample_size()))
            writeUInt16(UInt16(libatari800_get_num_sound_channels() * libatari800_get_sound_sample_size()))
            writeUInt16(UInt16(libatari800_get_sound_sample_size() * 8))

            file?.write("data".data(using: .ascii)!)
            writeUInt32(0) // length to be filled in later

            bytesWritten = 0
            return true
        } catch {
            print("Failed to open WAV file: \(error)")
            return false
        }
    }

    private func writeUInt16(_ value: UInt16) {
        var bytes = [UInt8]()
        bytes.append(UInt8(value & 0xff))
        bytes.append(UInt8((value >> 8) & 0xff))
        file?.write(Data(bytes))
    }

    private func writeUInt32(_ value: UInt32) {
        var bytes = [UInt8]()
        bytes.append(UInt8(value & 0xff))
        bytes.append(UInt8((value >> 8) & 0xff))
        bytes.append(UInt8((value >> 16) & 0xff))
        bytes.append(UInt8((value >> 24) & 0xff))
        file?.write(Data(bytes))
    }

    func write() -> Int {
        guard let file = file else { return 0 }

        let bufferLen = libatari800_get_sound_buffer_len()
        guard let buffer = libatari800_get_sound_buffer() else { return 0 }

        print("frame \(libatari800_get_frame_number()): writing \(bufferLen) bytes in sound buffer")

        let data = Data(bytes: buffer, count: Int(bufferLen))
        do {
            try file.write(contentsOf: data)
            bytesWritten += UInt32(bufferLen)
            return Int(bufferLen)
        } catch {
            print("Failed to write WAV data: \(error)")
            _ = close()
            return 0
        }
    }

    func close() -> Bool {
        guard let file = file else { return true }

        var success = true
        let aligned: UInt8 = (bytesWritten & 1) != 0 ? 1 : 0

        // Write alignment byte if needed
        if aligned != 0 {
            do {
                try file.write(contentsOf: Data([0]))
            } catch {
                success = false
            }
        }

        if success {
            // Update RIFF header size
            do {
                try file.seek(toOffset: 4)
                writeUInt32(bytesWritten + 36 + UInt32(aligned))

                // Update data chunk size
                try file.seek(toOffset: 40)
                writeUInt32(bytesWritten)
            } catch {
                success = false
            }
        }

        try? file.close()
        self.file = nil
        return success
    }
}

// Main program starts here
// Parse command line arguments
var saveWAV = false
var showScreen = true

for arg in CommandLine.arguments.dropFirst() {
    if arg == "-wav" {
        saveWAV = true
        showScreen = false
    }
}

// Call the C function to print Hello World (from Step 2)
print_hello_world()

// Call the function that returns a number (from Step 2)
let result = return_number(42)
print("Returned number: 0x\(String(result, radix: 16, uppercase: true))")

// Create a test file to read from (from Step 2)
let testContent = "This is the first line\nThis is the second line"
let testFilePath = "/tmp/test_file.txt"
try? testContent.write(toFile: testFilePath, atomically: true, encoding: .utf8)

// Call the function to read first line of file (from Step 2)
print("First line from file:")
read_file_first_line(testFilePath)

// Create and test input_t struct (from Step 2)
var inputStruct = input_t(byte: 255, word: 65535, long_val: 4294967295)
print("Input struct contents:")
print_input_struct(&inputStruct)

// Allocate state_t struct and test it (from Step 2)
guard let statePtr = allocate_state_struct() else {
    print("Failed to allocate state struct")
    exit(1)
}

// Access the state_t struct members (from Step 2)
let stateStruct = statePtr.pointee
print("State struct contents:")
print("a: 0x\(String(stateStruct.a, radix: 16, uppercase: true))")
print("b: 0x\(String(stateStruct.b, radix: 16, uppercase: true))")
print("storage[2]: 0x\(String(stateStruct.storage.2, radix: 16, uppercase: true))")

// Print first 8 elements of bytes array (from Step 2)
print("First 8 bytes:", terminator: " ")
for i in 0..<8 {
    let byteValue = withUnsafeBytes(of: stateStruct.bytes) { bytes in
        bytes[i]
    }
    print("0x\(String(byteValue, radix: 16, uppercase: true))", terminator: " ")
}
print()

// Free the allocated memory (from Step 2)
free_state_struct(statePtr)

// Initialize libatari800 with the same arguments as the C test
// C test uses NULL-terminated array, so we need to add NULL terminator
var testArgs = ["-atari"]
var cArgs = testArgs.map { strdup($0) }
cArgs.append(nil)  // Add NULL terminator like C version

let initResult = libatari800_init(-1, &cArgs)
if initResult != 0 {
    print("libatari800 initialization returned code: \(initResult)")
    if let errorMsg = libatari800_error_message() {
        print("Error message: \(String(cString: errorMsg))")
    }
    print("Error code from global: \(libatari800_error_code)")
    print("Proceeding anyway to see if emulation works...")
}

// Clear input array
var input = input_template_t()
libatari800_clear_input_array(&input)

// Initialize state structures
var state = emulator_state_t()

print("emulation: fps=\(libatari800_get_fps())")
print("sound: freq=\(libatari800_get_sound_frequency()), bytes/sample=\(libatari800_get_sound_sample_size()), channels=\(libatari800_get_num_sound_channels()), max buffer size=\(libatari800_get_sound_buffer_allocated_size())")

// Initialize WAV writer if needed
var wavWriter: WAVWriter?
if saveWAV {
    wavWriter = WAVWriter()
    if !wavWriter!.open("libatari800_test.wav") {
        print("Failed to open WAV file")
        exit(1)
    }
}

// Main emulation loop
while libatari800_get_frame_number() < 200 {
    libatari800_get_current_state(&state)

    // Get CPU and PC state using helper functions
    let cpuPtr = get_cpu_state(&state)
    let pcPtr = get_pc_state(&state)

    let cpu = cpuPtr!.pointee
    let pc = pcPtr!.pointee

    if showScreen {
        print(String(format: "frame %d: A=%02x X=%02x Y=%02x SP=%02x SR=%02x PC=%04x",
                     libatari800_get_frame_number(), cpu.A, cpu.X, cpu.Y, cpu.S, cpu.P, pc.PC))
    }

    libatari800_next_frame(&input)

    if libatari800_get_frame_number() > 100 {
        if showScreen {
            debugScreen()
        }
        input.keychar = UInt8(Character("A").asciiValue!)
    }

    if let wavWriter = wavWriter {
        _ = wavWriter.write()
    }
}

// Close WAV file if it was opened
if let wavWriter = wavWriter {
    _ = wavWriter.close()
}

// Print final state
libatari800_get_current_state(&state)
let finalCpuPtr = get_cpu_state(&state)
let finalPcPtr = get_pc_state(&state)

let finalCpu = finalCpuPtr!.pointee
let finalPc = finalPcPtr!.pointee

print(String(format: "frame %d: A=%02x X=%02x Y=%02x SP=%02x SR=%02x PC=%04x",
             libatari800_get_frame_number(), finalCpu.A, finalCpu.X, finalCpu.Y, finalCpu.S, finalCpu.P, finalPc.PC))

// Exit libatari800
libatari800_exit()

// Clean up allocated strings (skip the NULL terminator)
for arg in cArgs {
    if let ptr = arg {
        free(ptr)
    }
}
