import SwiftUI
import Foundation
import AppKit
import CBridge
import Metal
import MetalKit

@MainActor
class AtariEmulator: ObservableObject {
    @Published var screenData: [UInt8] = Array(repeating: 0, count: 384 * 240)
    
    // State accessed from background - safe with our controlled access pattern
    nonisolated(unsafe) private var emulationTask: Task<Void, Never>?
    nonisolated(unsafe) private var isRunning = false
    nonisolated(unsafe) private var input = input_template_t()
    nonisolated(unsafe) private var state = emulator_state_t()
    nonisolated(unsafe) private var lastFrameTime: CFAbsoluteTime = 0
    
    // Buffer for thread-safe screen updates
    nonisolated(unsafe) private var screenBuffer: [UInt8] = Array(repeating: 0, count: 384 * 240)

    func start() {
        guard !isRunning else { return }

        isRunning = true
        
        // Start emulation on a background task
        emulationTask = Task.detached { [weak self] in
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
                print("Proceeding anyway to see if emulation works...")
            }

            // Clear input array
            guard let self = self else { return }
            libatari800_clear_input_array(&self.input)

            print("emulation: fps=\(libatari800_get_fps())")
            print("sound: freq=\(libatari800_get_sound_frequency()), bytes/sample=\(libatari800_get_sound_sample_size()), channels=\(libatari800_get_num_sound_channels()), max buffer size=\(libatari800_get_sound_buffer_allocated_size())")

            // Clean up allocated strings (skip the NULL terminator)
            for arg in cArgs {
                if let ptr = arg {
                    free(ptr)
                }
            }
            
            // Run emulation loop at 60Hz
            await self.runEmulationLoop()
        }
    }

    func stop() {
        stopEmulation()
    }
    
    nonisolated private func stopEmulation() {
        emulationTask?.cancel()

        if isRunning {
            libatari800_exit()
            isRunning = false
        }
    }
    
    nonisolated private func runEmulationLoop() async {
        while !Task.isCancelled && isRunning {
            updateFrame()
            
            // Sleep for 60Hz timing (16.67ms)
            try? await Task.sleep(for: .milliseconds(16))
        }
    }

    nonisolated private func updateFrame() {
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

        libatari800_next_frame(&input)

        // Update screen data
        updateScreenData()

        // Simulate key input after frame 100
        if libatari800_get_frame_number() > 100 {
            input.keychar = UInt8(Character("A").asciiValue!)
        }

        // Stop after 200 frames for now
        if libatari800_get_frame_number() >= 200 {
            stopEmulation()
        }
        
        let executionTime = (CFAbsoluteTimeGetCurrent() - startTime) * 1000 // Convert to milliseconds
        print(String(format: "frame %d: A=%02x X=%02x Y=%02x SP=%02x SR=%02x PC=%04x (one iteration took %.3f ms, time since last frame: %.3f ms)",
                     libatari800_get_frame_number(), cpu.A, cpu.X, cpu.Y, cpu.S, cpu.P, pc.PC, executionTime, timeSinceLastFrame))
    }

    nonisolated private func updateScreenData() {
        guard let screenPtr = libatari800_get_screen_ptr() else {
            print("Failed to get screen pointer")
            return
        }

        // Copy screen data directly to our buffer (nonisolated, safe)
        screenBuffer.withUnsafeMutableBufferPointer { buffer in
            buffer.baseAddress?.initialize(from: screenPtr, count: 384 * 240)
        }
        
        // Update the published property on main queue
        DispatchQueue.main.async { [weak self] in
            guard let self = self else { return }
            // We know this is safe because we're on the main queue
            MainActor.assumeIsolated {
                self.screenData = self.screenBuffer
            }
        }
    }

    deinit {
        stopEmulation()
    }
}

// Metal-based renderer for Atari screen
class MetalAtariRenderer: NSObject, MTKViewDelegate {
    let device: MTLDevice
    let commandQueue: MTLCommandQueue
    let pipelineState: MTLRenderPipelineState
    let texture: MTLTexture
    let colorLookupBuffer: MTLBuffer
    
    weak var emulator: AtariEmulator?
    
    private var drawCount: Int = 0
    
    init?(emulator: AtariEmulator) {
        // Get default Metal device
        guard let device = MTLCreateSystemDefaultDevice() else {
            print("Metal is not supported on this device")
            return nil
        }
        
        self.device = device
        self.emulator = emulator
        
        // Create command queue
        guard let commandQueue = device.makeCommandQueue() else {
            print("Failed to create command queue")
            return nil
        }
        self.commandQueue = commandQueue
        
        // Create texture for Atari screen (384x240, single channel)
        let textureDescriptor = MTLTextureDescriptor.texture2DDescriptor(
            pixelFormat: .r8Unorm,
            width: 384,
            height: 240,
            mipmapped: false
        )
        textureDescriptor.usage = [.shaderRead]
        
        guard let texture = device.makeTexture(descriptor: textureDescriptor) else {
            print("Failed to create texture")
            return nil
        }
        self.texture = texture
        
        // Create color lookup table (256 RGBA colors)
        var colorLookup = [SIMD4<Float>]()
        colorLookup.reserveCapacity(256)
        
        for pixelByte in 0..<256 {
            let hue = (pixelByte >> 4) & 0x0F
            let luminance = pixelByte & 0x0F
            
            let h = Double(hue) / 15.0
            let s = 0.8
            let v = Double(luminance) / 15.0
            
            // HSV to RGB conversion
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
            
            let color = SIMD4<Float>(
                Float(r + m),
                Float(g + m),
                Float(b + m),
                1.0
            )
            colorLookup.append(color)
        }
        
        guard let colorLookupBuffer = device.makeBuffer(
            bytes: colorLookup,
            length: colorLookup.count * MemoryLayout<SIMD4<Float>>.stride,
            options: .storageModeShared
        ) else {
            print("Failed to create color lookup buffer")
            return nil
        }
        self.colorLookupBuffer = colorLookupBuffer
        
        // Load shaders and create pipeline
        // Try to load from bundle resource first
        let library: MTLLibrary
        if let bundle = Bundle.module.url(forResource: "Shaders", withExtension: "metal"),
           let source = try? String(contentsOf: bundle, encoding: .utf8),
           let lib = try? device.makeLibrary(source: source, options: nil) {
            library = lib
            print("Successfully loaded Metal shaders from bundle")
        } else if let defaultLib = device.makeDefaultLibrary() {
            library = defaultLib
            print("Successfully loaded default Metal library")
        } else {
            print("Failed to create shader library")
            return nil
        }
        
        guard let vertexFunction = library.makeFunction(name: "vertexShader"),
              let fragmentFunction = library.makeFunction(name: "fragmentShader") else {
            print("Failed to load shader functions")
            return nil
        }
        
        let pipelineDescriptor = MTLRenderPipelineDescriptor()
        pipelineDescriptor.vertexFunction = vertexFunction
        pipelineDescriptor.fragmentFunction = fragmentFunction
        pipelineDescriptor.colorAttachments[0].pixelFormat = .bgra8Unorm
        
        do {
            self.pipelineState = try device.makeRenderPipelineState(descriptor: pipelineDescriptor)
        } catch {
            print("Failed to create pipeline state: \(error)")
            return nil
        }
        
        super.init()
    }
    
    func mtkView(_ view: MTKView, drawableSizeWillChange size: CGSize) {
        // Handle resize if needed
    }
    
    func draw(in view: MTKView) {
        let frameStartTime = CFAbsoluteTimeGetCurrent()
        
        guard let emulator = emulator else { return }
        
        // Update texture with latest screen data
        let uploadStartTime = CFAbsoluteTimeGetCurrent()
        let region = MTLRegionMake2D(0, 0, 384, 240)
        emulator.screenData.withUnsafeBytes { bytes in
            texture.replace(region: region, mipmapLevel: 0, withBytes: bytes.baseAddress!, bytesPerRow: 384)
        }
        let uploadTime = (CFAbsoluteTimeGetCurrent() - uploadStartTime) * 1000
        
        // Create command buffer and render pass
        guard let commandBuffer = commandQueue.makeCommandBuffer(),
              let renderPassDescriptor = view.currentRenderPassDescriptor,
              let renderEncoder = commandBuffer.makeRenderCommandEncoder(descriptor: renderPassDescriptor) else {
            return
        }
        
        let renderStartTime = CFAbsoluteTimeGetCurrent()
        
        renderEncoder.setRenderPipelineState(pipelineState)
        renderEncoder.setFragmentTexture(texture, index: 0)
        renderEncoder.setFragmentBuffer(colorLookupBuffer, offset: 0, index: 0)
        
        // Draw full-screen quad (6 vertices = 2 triangles)
        renderEncoder.drawPrimitives(type: .triangle, vertexStart: 0, vertexCount: 6)
        
        renderEncoder.endEncoding()
        
        if let drawable = view.currentDrawable {
            commandBuffer.present(drawable)
        }
        
        commandBuffer.commit()
        
        let renderTime = (CFAbsoluteTimeGetCurrent() - renderStartTime) * 1000
        let totalTime = (CFAbsoluteTimeGetCurrent() - frameStartTime) * 1000
        
        print(String(format: "MetalAtariRenderer draw #%d: upload=%.3f ms, render=%.3f ms, total=%.3f ms",
                     drawCount, uploadTime, renderTime, totalTime))
        
        drawCount += 1
    }
}

// SwiftUI wrapper for Metal view
struct MetalAtariView: NSViewRepresentable {
    let emulator: AtariEmulator
    
    func makeNSView(context: Context) -> MTKView {
        let mtkView = MTKView()
        mtkView.device = MTLCreateSystemDefaultDevice()
        mtkView.clearColor = MTLClearColor(red: 0, green: 0, blue: 0, alpha: 1)
        mtkView.preferredFramesPerSecond = 60
        mtkView.enableSetNeedsDisplay = false
        mtkView.isPaused = false
        
        if let renderer = MetalAtariRenderer(emulator: emulator) {
            mtkView.delegate = renderer
            context.coordinator.renderer = renderer
        }
        
        return mtkView
    }
    
    func updateNSView(_ nsView: MTKView, context: Context) {
        // Update view if needed
    }
    
    func makeCoordinator() -> Coordinator {
        Coordinator()
    }
    
    class Coordinator {
        var renderer: MetalAtariRenderer?
    }
}

struct ContentView: View {
    @StateObject private var emulator = AtariEmulator()

    var body: some View {
        VStack {
            Text("Atari 800 Emulator (Metal)")
                .font(.title)
                .padding()

            MetalAtariView(emulator: emulator)
                .frame(width: 384 * 2, height: 240 * 2)
                .border(Color.gray, width: 2)
                .padding()
                .onAppear {
                    emulator.start()
                }
                .onDisappear {
                    emulator.stop()
                }

            Text("Screen: 384x240 pixels - Metal Rendering")
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
