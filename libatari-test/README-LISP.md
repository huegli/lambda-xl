# Atari 800 Emulator - LispWorks CAPI Version

This is a Common Lisp reimplementation of the libatari-test program using LispWorks CAPI (Common Application Programmer Interface) and FLI (Foreign Language Interface).

## Overview

This implementation replaces the Swift/Metal-based frame buffer display with CAPI-based graphics rendering while maintaining the same emulation functionality through the libatari800.dylib library.

## Key Components

### 1. FLI Definitions (`atari-emulator.lisp`)

The Foreign Language Interface definitions mirror all the C structures and functions from libatari800:

- **Type Definitions**: `ubyte`, `uword`, `ulong`
- **Structures**: `input-template-t`, `emulator-state-t`, `cpu-state-t`, `pc-state-t`
- **Functions**: All libatari800 API functions (init, next-frame, get-screen-ptr, etc.)

### 2. CAPI Window and Display

The `atari-display-interface` class provides:

- **Output Pane**: Canvas for rendering the 384x240 Atari screen
- **Display Callback**: `draw-atari-screen` renders the frame buffer with proper color conversion
- **Scaling**: 2x scaling by default (768x480 display window)
- **Status Display**: Title pane showing emulator name

### 3. Color Conversion

The Atari's palette format (4-bit hue + 4-bit luminance) is converted to RGB using HSV color space:

```lisp
(defun atari-byte-to-rgb (pixel-byte)
  ;; Extracts hue and luminance from byte
  ;; Converts HSV -> RGB
  ;; Returns RGB values in 0-255 range
  )
```

A 256-entry lookup table is pre-computed for performance.

### 4. Emulator State Management

The `emulator-state` class manages:

- **Running state**: Flag for emulation loop control
- **FLI structures**: Foreign memory for input and state
- **Screen buffer**: Local copy of 384x240 pixel data
- **Frame timing**: Tracking for 60 FPS target
- **Thread management**: Separate thread for emulation loop

### 5. Emulation Loop

The main emulation runs in a separate thread:

1. Call `libatari800-next-frame` to advance emulation
2. Copy screen data from C library to Lisp buffer
3. Request CAPI redraw on main thread
4. Sleep to maintain 60 FPS timing
5. Stop after 200 frames (configurable)

### 6. Thread Safety

- **Emulation thread**: Runs the emulation loop
- **Main thread**: Handles CAPI events and rendering
- **Communication**: Uses `capi:execute-with-interface` for thread-safe updates

## Prerequisites

1. **LispWorks**: Commercial Common Lisp implementation with CAPI
   - LispWorks 7.1 or later (64-bit)
   - Available at: http://www.lispworks.com/

2. **libatari800.dylib**: Built dynamic library
   - Must be present in the parent directory
   - Build using `build.sh` or `build_libatari800.sh`

3. **macOS**: Required for the .dylib format (could be adapted for Linux/Windows)

## Building and Running

### Using the Shell Script

```bash
chmod +x run-lisp.sh
./run-lisp.sh
```

The script will:
- Verify libatari800.dylib exists
- Find your LispWorks installation
- Load the emulator code
- Start the CAPI interface

### Manual Execution in LispWorks

1. Start LispWorks
2. Load the file:
   ```lisp
   (load "atari-emulator.lisp")
   ```
3. Switch to the package:
   ```lisp
   (in-package :atari-emulator)
   ```
4. Run the emulator:
   ```lisp
   (run-emulator)
   ```

### Creating a Standalone Application

You can create a standalone executable using LispWorks delivery:

```lisp
(load "atari-emulator.lisp")
(in-package :atari-emulator)

(lw:deliver 'main
            "AtariEmulator"
            5  ; Delivery level
            :interface :capi
            :keep-lisp-reader t)
```

## Architecture Comparison

### Swift/Metal Version
- **UI Framework**: SwiftUI + AppKit
- **Rendering**: Metal GPU-accelerated shaders
- **Language**: Swift 6.0
- **Concurrency**: Swift async/await
- **Performance**: GPU-based, very fast

### LispWorks/CAPI Version
- **UI Framework**: CAPI (native to LispWorks)
- **Rendering**: Software rendering via CAPI graphics
- **Language**: Common Lisp
- **Concurrency**: LispWorks multiprocessing
- **Performance**: CPU-based, adequate for 60 FPS at 2x scaling

## Features

### Implemented
- ✅ libatari800 library integration via FLI
- ✅ CAPI window with output pane
- ✅ Screen buffer rendering with color conversion
- ✅ 60 FPS emulation loop
- ✅ Frame timing and statistics
- ✅ CPU state display
- ✅ Threaded emulation
- ✅ Proper cleanup on exit

### Not Yet Implemented
- ⬜ Keyboard input mapping
- ⬜ Joystick support
- ⬜ Sound output
- ⬜ Disk image loading
- ⬜ Menu system
- ⬜ Save state functionality
- ⬜ Configuration options

## Adding Keyboard Input

To add keyboard input handling, extend the CAPI interface:

```lisp
(define-interface atari-display-interface ()
  ...
  (:panes
   (canvas output-pane
           :display-callback 'draw-atari-screen
           :input-model '((#\a handle-key-press)
                         (:key-press handle-key-press))
           ...))
  ...)

(defun handle-key-press (pane key)
  "Handle keyboard input"
  (let* ((interface (element-interface pane))
         (emulator (interface-emulator-state interface))
         (input (emulator-input-struct emulator)))
    ;; Map key to Atari key codes
    ;; Update input structure
    ...))
```

## Performance Notes

The software rendering approach is suitable for the Atari's 384x240 resolution at 2x or 3x scaling. For higher scaling factors, consider:

1. Using CAPI image caching
2. Implementing dirty rectangle tracking
3. Using external pixmap for double buffering
4. Reducing color conversion overhead

## Troubleshooting

### Library Loading Issues
If you get "Library not found" errors:
```lisp
;; Check library path
(fli:register-module :libatari800
                    :real-name "/full/path/to/libatari800.dylib"
                    :connection-style :immediate)
```

### Display Issues
If the window appears but shows no content:
- Check that libatari800 is initializing correctly
- Verify screen pointer is valid
- Enable debug output in `update-screen-buffer`

### Thread Issues
If the emulator hangs:
- Check that `mp:process-run-function` is working
- Verify `capi:execute-with-interface` is being called correctly
- Use LispWorks Process Browser to inspect threads

## References

- [LispWorks CAPI User Guide](http://www.lispworks.com/documentation/lw71/CAPRM/html/capiref.htm)
- [LispWorks FLI User Guide](http://www.lispworks.com/documentation/lw71/FLI/html/fli.htm)
- [libatari800 Documentation](../atari800/src/libatari800/libatari800.h)
- [Original Swift Implementation](Sources/libatari-test/main.swift)

## License

Same as the parent project.
