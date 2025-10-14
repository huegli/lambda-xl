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

    var body: some View {
        GeometryReader { geometry in
            Canvas { context, size in
                // Create bitmap data for the screen
                let width = 384
                let height = 240
                let bytesPerPixel = 4 // RGBA
                let bytesPerRow = width * bytesPerPixel
                let totalBytes = height * bytesPerRow

                var pixelData = [UInt8](repeating: 0, count: totalBytes)

                // Convert Atari screen data to RGBA bitmap
                for y in 0..<height {
                    for x in 0..<width {
                        let atariPixel = emulator.screenData[y * width + x]
                        let color = decodePixelColor(atariPixel)

                        // Convert SwiftUI Color to RGBA components
                        let rgba = getRGBAComponents(from: color)
                        let pixelIndex = (y * width + x) * bytesPerPixel

                        pixelData[pixelIndex] = rgba.r     // Red
                        pixelData[pixelIndex + 1] = rgba.g // Green
                        pixelData[pixelIndex + 2] = rgba.b // Blue
                        pixelData[pixelIndex + 3] = rgba.a // Alpha
                    }
                }

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

                // Draw the entire screen as one image, scaled to fit
                let destRect = CGRect(origin: .zero, size: size)
                context.draw(Image(cgImage, scale: 1.0, label: Text("Atari Screen")), in: destRect)
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

    private func decodePixelColor(_ pixelByte: UInt8) -> Color {
        let hue = (pixelByte >> 4) & 0x0F  // High 4 bits
        let luminance = pixelByte & 0x0F   // Low 4 bits

        // Convert to normalized values (0.0 to 1.0)
        let normalizedHue = Double(hue) / 15.0
        let normalizedLuminance = Double(luminance) / 15.0

        // Create color using HSB color space
        // Multiply hue by 360 degrees for full color wheel
        return Color(hue: normalizedHue, saturation: 0.8, brightness: normalizedLuminance)
    }

    private func getRGBAComponents(from color: Color) -> (r: UInt8, g: UInt8, b: UInt8, a: UInt8) {
        // Convert SwiftUI Color to NSColor for component extraction
        let nsColor = NSColor(color)

        // Convert to RGB color space if needed
        guard let rgbColor = nsColor.usingColorSpace(.deviceRGB) else {
            return (0, 0, 0, 255) // Fallback to black
        }

        return (
            r: UInt8(rgbColor.redComponent * 255),
            g: UInt8(rgbColor.greenComponent * 255),
            b: UInt8(rgbColor.blueComponent * 255),
            a: UInt8(rgbColor.alphaComponent * 255)
        )
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
