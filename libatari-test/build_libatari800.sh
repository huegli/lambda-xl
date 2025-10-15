#!/bin/bash

# Build script for libatari800 library
# This script builds both libatari800.a (static) and libatari800.dylib (dynamic)
# and copies them to the project root directory

set -e  # Exit on any error

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
ATARI800_DIR="$PROJECT_ROOT/atari800"
ATARI800_SRC_DIR="$ATARI800_DIR/src"
LIBATARI800_A_SRC="$ATARI800_SRC_DIR/libatari800.a"
LIBATARI800_DYLIB_SRC="$ATARI800_SRC_DIR/libatari800.dylib"
LIBATARI800_A_DEST="$PROJECT_ROOT/libatari800.a"
LIBATARI800_DYLIB_DEST="$PROJECT_ROOT/libatari800.dylib"

echo "Building libatari800 library..."

# Check if atari800 directory exists
if [ ! -d "$ATARI800_DIR" ]; then
    echo "Error: atari800 directory not found at $ATARI800_DIR"
    exit 1
fi

# Change to atari800 directory
cd "$ATARI800_DIR"

# Check if libraries already exist in destination and are up to date
NEEDS_REBUILD=false
if [ -f "$LIBATARI800_A_DEST" ] && [ -f "$LIBATARI800_DYLIB_DEST" ]; then
    echo "Checking if rebuild is needed..."
    
    # Check if any source files are newer than the destination libraries
    NEWEST_SOURCE=$(find src -name "*.c" -o -name "*.h" | xargs ls -t 2>/dev/null | head -1)
    if [ -n "$NEWEST_SOURCE" ]; then
        if [ "$NEWEST_SOURCE" -nt "$LIBATARI800_A_DEST" ] || [ "$NEWEST_SOURCE" -nt "$LIBATARI800_DYLIB_DEST" ]; then
            NEEDS_REBUILD=true
        fi
    fi
    
    if [ "$NEEDS_REBUILD" = false ]; then
        echo "Libraries are up to date at $PROJECT_ROOT"
        echo "  Static:  $(ls -lh "$LIBATARI800_A_DEST" | awk '{print $5}')"
        echo "  Dynamic: $(ls -lh "$LIBATARI800_DYLIB_DEST" | awk '{print $5}')"
        exit 0
    fi
else
    NEEDS_REBUILD=true
fi

echo "Building libatari800..."

# Configure if needed (check if Makefile exists and is recent)
if [ ! -f "Makefile" ] || [ "configure.ac" -nt "Makefile" ]; then
    echo "Configuring libatari800..."
    ./configure --target=libatari800
fi

# Build the static library
echo "Compiling libatari800.a..."
cd "$ATARI800_SRC_DIR"
make libatari800.a

# Verify the static library was created
if [ ! -f "$LIBATARI800_A_SRC" ]; then
    echo "Error: Failed to build libatari800.a"
    exit 1
fi

echo "libatari800.a built successfully"

# Build the dynamic library from the object files
echo "Building libatari800.dylib..."

# Get compiler and flags from Makefile
CC=$(grep "^CC = " Makefile | cut -d' ' -f3)
LIBS=$(grep "^LIBS = " Makefile | cut -d' ' -f3-)

# List of object files (same as used in libatari800.a)
DYLIB_OBJECTS=(
    libatari800/api.o libatari800/main.o libatari800/init.o
    libatari800/exit.o libatari800/input.o libatari800/video.o
    libatari800/statesav.o libatari800/sound.o
    afile.o antic.o atari.o binload.o cartridge.o cartridge_info.o
    cassette.o compfile.o cfg.o cpu.o crc32.o devices.o esc.o gtia.o
    img_tape.o log.o memory.o monitor.o pbi.o pia.o pokey.o
    roms/altirra_5200_os.o roms/altirra_5200_charset.o rtime.o sio.o
    sysrom.o util.o pokeysnd.o mzpokeysnd.o remez.o sound.o pokeyrec.o
    codecs/image.o codecs/image_pcx.o file_export.o codecs/container.o
    codecs/container_wav.o codecs/audio.o codecs/audio_pcm.o
    codecs/audio_adpcm.o codecs/audio_mulaw.o codecs/container_avi.o
    codecs/video.o codecs/video_mrle.o codecs/video_zmbv.o
    input.o statesav.o artifact.o colours.o colours_ntsc.o colours_pal.o
    colours_external.o screen.o cycle_map.o roms/altirraos_800.o
    roms/altirraos_xl.o roms/altirra_basic.o pbi_mio.o pbi_bb.o
    pbi_scsi.o netsio.o pbi_xld.o voicebox.o votrax.o votraxsnd.o
    ide.o rdevice.o
)

# Build the dylib
$CC -dynamiclib -o "$LIBATARI800_DYLIB_SRC" \
    "${DYLIB_OBJECTS[@]}" $LIBS \
    -install_name @rpath/libatari800.dylib \
    -current_version 5.2.0 -compatibility_version 5.0.0

# Verify the dynamic library was created
if [ ! -f "$LIBATARI800_DYLIB_SRC" ]; then
    echo "Error: Failed to build libatari800.dylib"
    exit 1
fi

echo "libatari800.dylib built successfully"

# Copy libraries to project root
echo ""
echo "Copying libraries to project root..."
cp "$LIBATARI800_A_SRC" "$LIBATARI800_A_DEST"
cp "$LIBATARI800_DYLIB_SRC" "$LIBATARI800_DYLIB_DEST"

echo "Libraries installed at $PROJECT_ROOT:"
echo "  Static:  libatari800.a ($(ls -lh "$LIBATARI800_A_DEST" | awk '{print $5}'))"
echo "  Dynamic: libatari800.dylib ($(ls -lh "$LIBATARI800_DYLIB_DEST" | awk '{print $5}'))"