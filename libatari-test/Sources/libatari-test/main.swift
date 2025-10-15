import SwiftUI
import Foundation
import AppKit
import CBridge

class AtariEmulator: ObservableObject {
    @Published var screenData: [UInt8] = Array(repeating: 0, count: 384 * 240)
    private var timer: Timer?
    private var isRunning = false
    private var input = input_template_t()
    private var state = emulator_state_t()
    private var lastFrameTime: CFAbsoluteTime = 0

    func start() {
        guard !isRunning else { return }

        // Initialize libatari800 with the same arguments as the C test
        let testArgs = ["-atari"]
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
        libatari800_clear_input_array(&input)

        print("emulation: fps=\(libatari800_get_fps())")
        print("sound: freq=\(libatari800_get_sound_frequency()), bytes/sample=\(libatari800_get_sound_sample_size()), channels=\(libatari800_get_num_sound_channels()), max buffer size=\(libatari800_get_sound_buffer_allocated_size())")

        isRunning = true

        // Start 60Hz timer on a background thread to avoid being blocked by UI rendering
        DispatchQueue.global(qos: .userInteractive).async { [weak self] in
            self?.timer = Timer.scheduledTimer(withTimeInterval: 1.0/60.0, repeats: true) { [weak self] _ in
                self?.updateFrame()
            }

            // Keep the background thread's RunLoop alive
            if let timer = self?.timer {
                RunLoop.current.add(timer, forMode: .common)
                RunLoop.current.run()
            }
        }

        // Clean up allocated strings (skip the NULL terminator)
        for arg in cArgs {
            if let ptr = arg {
                free(ptr)
            }
        }
    }

    func stop() {
        timer?.invalidate()
        timer = nil

        if isRunning {
            libatari800_exit()
            isRunning = false
        }
    }

    private func updateFrame() {
        guard isRunning else { return }

        let frameStartTime = CFAbsoluteTimeGetCurrent()
        let timeSinceLastFrame = lastFrameTime > 0 ? (frameStartTime - lastFrameTime) * 1000 : 0
        lastFrameTime = frameStartTime

        let startTime = CFAbsoluteTimeGetCurrent()
        libatari800_get_current_state(&state)

        // Get CPU and PC state using helper functions (for debugging if needed)
        let cpuPtr = get_cpu_state(&state)
        let pcPtr = get_pc_state(&state)

        let cpu = cpuPtr!.pointee
        let pc = pcPtr!.pointee


        // Only print debug info occasionally to avoid performance impact
//        if libatari800_get_frame_number() % 60 == 0 {
//        }

        libatari800_next_frame(&input)

        // Update screen data on main thread for SwiftUI
        DispatchQueue.main.async { [weak self] in
            self?.updateScreenData()
        }

        // Simulate key input after frame 100
        if libatari800_get_frame_number() > 100 {
            input.keychar = UInt8(Character("A").asciiValue!)
        }

        // Stop after 200 frames for now
        if libatari800_get_frame_number() >= 200 {
            stop()
        }
        
        let executionTime = (CFAbsoluteTimeGetCurrent() - startTime) * 1000 // Convert to milliseconds
        print(String(format: "frame %d: A=%02x X=%02x Y=%02x SP=%02x SR=%02x PC=%04x (one iteration took %.3f ms, time since last frame: %.3f ms)",
                     libatari800_get_frame_number(), cpu.A, cpu.X, cpu.Y, cpu.S, cpu.P, pc.PC, executionTime, timeSinceLastFrame))

    }

    private func updateScreenData() {
        guard let screenPtr = libatari800_get_screen_ptr() else {
            print("Failed to get screen pointer")
            return
        }

        // Efficiently copy the entire 384x240 screen buffer using bulk copy
        screenData.withUnsafeMutableBufferPointer { buffer in
            buffer.baseAddress?.initialize(from: screenPtr, count: 384 * 240)
        }
    }

    deinit {
        stop()
    }
}

struct AtariScreenView: View {
    @ObservedObject var emulator: AtariEmulator
    @State private var drawCount: Int = 0

    // Pre-computed lookup table for all 256 possible Atari pixel values as UInt32
    // Each UInt32 contains RGBA packed as a single 32-bit value for fast writes
    // Format: 0xAABBGGRR (little-endian RGBA)
    private static let colorLookupTable: [UInt32] = {
        var table = [UInt32]()
        table.reserveCapacity(256)

        for pixelByte in 0..<256 {
            let hue = (pixelByte >> 4) & 0x0F  // High 4 bits
            let luminance = pixelByte & 0x0F   // Low 4 bits

            // Convert to normalized values
            let h = Double(hue) / 15.0
            let s = 0.8
            let v = Double(luminance) / 15.0

            // Direct HSV to RGB conversion (faster than going through Color/NSColor)
            let c = v * s
            let x = c * (1.0 - abs((h * 6.0).truncatingRemainder(dividingBy: 2.0) - 1.0))
            let m = v - c

            var r: Double = 0, g: Double = 0, b: Double = 0
            let hPrime = h * 6.0

            if hPrime < 1.0 {
                r = c; g = x; b = 0
            } else if hPrime < 2.0 {
                r = x; g = c; b = 0
            } else if hPrime < 3.0 {
                r = 0; g = c; b = x
            } else if hPrime < 4.0 {
                r = 0; g = x; b = c
            } else if hPrime < 5.0 {
                r = x; g = 0; b = c
            } else {
                r = c; g = 0; b = x
            }

            let red = UInt32((r + m) * 255)
            let green = UInt32((g + m) * 255)
            let blue = UInt32((b + m) * 255)
            let alpha: UInt32 = 255

            // Pack RGBA into single UInt32 (little-endian: ABGR byte order)
            let packedColor = (alpha << 24) | (blue << 16) | (green << 8) | red
            table.append(packedColor)
        }

        return table
    }()

    var body: some View {
        let bodyStartTime = CFAbsoluteTimeGetCurrent()

        return GeometryReader { geometry in
            Canvas { context, size in
                let canvasStartTime = CFAbsoluteTimeGetCurrent()

                // Create bitmap data for the screen
                let width = 384
                let height = 240
                let bytesPerPixel = 4 // RGBA
                let bytesPerRow = width * bytesPerPixel
                let totalBytes = height * bytesPerRow

                var pixelData = [UInt8](repeating: 0, count: totalBytes)

                let conversionStartTime = CFAbsoluteTimeGetCurrent()

                // Ultra-fast conversion: write 32-bit pixels directly instead of 4 separate bytes
                pixelData.withUnsafeMutableBytes { pixelBuffer in
                    let pixelPtr = pixelBuffer.bindMemory(to: UInt32.self)

                    emulator.screenData.withUnsafeBufferPointer { atariBuffer in
                        guard let atariBase = atariBuffer.baseAddress else { return }

                        // Single write per pixel (32-bit) instead of 4 writes (8-bit each)
                        for i in 0..<(width * height) {
                            pixelPtr[i] = Self.colorLookupTable[Int(atariBase[i])]
                        }
                    }
                }

                let conversionTime = (CFAbsoluteTimeGetCurrent() - conversionStartTime) * 1000

                let imageCreationStartTime = CFAbsoluteTimeGetCurrent()

                // Create CGImage from bitmap data
                guard let dataProvider = CGDataProvider(data: Data(pixelData) as CFData),
                      let cgImage = CGImage(
                        width: width,
                        height: height,
                        bitsPerComponent: 8,
                        bitsPerPixel: 32,
                        bytesPerRow: bytesPerRow,
                        space: CGColorSpaceCreateDeviceRGB(),
                        bitmapInfo: CGBitmapInfo(rawValue: CGImageAlphaInfo.premultipliedLast.rawValue),
                        provider: dataProvider,
                        decode: nil,
                        shouldInterpolate: false,
                        intent: .defaultIntent
                      ) else { return }

                let imageCreationTime = (CFAbsoluteTimeGetCurrent() - imageCreationStartTime) * 1000

                let drawStartTime = CFAbsoluteTimeGetCurrent()

                // Draw the entire screen as one image, scaled to fit
                let destRect = CGRect(origin: .zero, size: size)
                context.draw(Image(cgImage, scale: 1.0, label: Text("Atari Screen")), in: destRect)

                let drawTime = (CFAbsoluteTimeGetCurrent() - drawStartTime) * 1000
                let totalCanvasTime = (CFAbsoluteTimeGetCurrent() - canvasStartTime) * 1000
                let totalBodyTime = (CFAbsoluteTimeGetCurrent() - bodyStartTime) * 1000

                // Print timing information
                print(String(format: "AtariScreenView draw #%d: conversion=%.3f ms, image_creation=%.3f ms, draw=%.3f ms, total_canvas=%.3f ms, total_body=%.3f ms",
                             drawCount, conversionTime, imageCreationTime, drawTime, totalCanvasTime, totalBodyTime))

                // Increment draw count for next frame
                DispatchQueue.main.async {
                    drawCount += 1
                }
            }
        }
        .aspectRatio(384.0/240.0, contentMode: .fit)
        .onAppear {
            emulator.start()
        }
        .onDisappear {
            emulator.stop()
        }
    }
}

struct ContentView: View {
    @StateObject private var emulator = AtariEmulator()

    var body: some View {
        VStack {
            Text("Atari 800 Emulator")
                .font(.title)
                .padding()

            AtariScreenView(emulator: emulator)
                .frame(width: 384 * 2, height: 240 * 2) // 16x16 scaling
                .border(Color.gray, width: 2)
                .padding()

            Text("Screen: 384x240 pixels")
                .font(.caption)
                .foregroundColor(.secondary)
        }
        .padding()
    }
}

@main
struct AtariTestApp: App {
    var body: some Scene {
        WindowGroup {
            ContentView()
        }
        .windowResizability(.contentSize)
    }
}
