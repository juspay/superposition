#!/bin/bash

# Script to set up provider binaries following the same platform-based approach as release.yaml

echo "$OSTYPE"
echo "$PATH" | grep -q '/nix/store'
in_nix=$?
if [ $in_nix == 0 ]; then
    echo "Inside nix shell, doing some stuff"
fi

TARGET_MODE=debug
JS_COPY_PATH="clients/javascript/open-feature-provider/dist/native-lib"
if [[ $1 == "js" ]]; then
    if [[ $2 == "bindings" ]]; then
        JS_COPY_PATH="clients/javascript/bindings/dist/native-lib"
    fi
    if [[ $3 == "release" ]]; then
        TARGET_MODE=release
    fi
    mkdir -p ${JS_COPY_PATH}
fi

# Determine platform and library details
PLATFORM=""
LIB_NAME=""
LIB_EXTENSION=""
TARGET_TRIPLE=""

if [[ "$OSTYPE" == "darwin"* ]]; then
    # macOS - determine architecture
    ARCH=$(uname -m)
    if [[ "$ARCH" == "arm64" ]]; then
        PLATFORM="darwin-aarch64"
        TARGET_TRIPLE="aarch64-apple-darwin"
    else
        PLATFORM="darwin-x86-64"
        TARGET_TRIPLE="x86_64-apple-darwin"
    fi
    LIB_NAME="libsuperposition_core"
    LIB_EXTENSION="dylib"
elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
    # Linux x86_64
    PLATFORM="linux-x86-64"
    TARGET_TRIPLE="x86_64-unknown-linux-gnu"
    LIB_NAME="libsuperposition_core"
    LIB_EXTENSION="so"
elif [[ "$OSTYPE" == "msys" || "$OSTYPE" == "cygwin" ]]; then
    # Windows
    PLATFORM="win32-x86-64"
    TARGET_TRIPLE="x86_64-pc-windows-msvc"
    LIB_NAME="superposition_core"
    LIB_EXTENSION="dll"
else
    echo "Unsupported OS type: $OSTYPE"
    exit 1
fi

echo "Detected platform: $PLATFORM"
echo "Target triple: $TARGET_TRIPLE"
echo "Library: $LIB_NAME.$LIB_EXTENSION"

# Set up copy paths based on provider type
COPY_PATH=""
if [[ $1 == "js" ]]; then
    COPY_PATH=${JS_COPY_PATH}
    FINAL_LIB_NAME="$LIB_NAME-$TARGET_TRIPLE.$LIB_EXTENSION"
elif [[ $1 == "py" ]]; then
    COPY_PATH="$UV_PROJECT_ENVIRONMENT/lib/python3.12/site-packages/superposition_bindings"
    FINAL_LIB_NAME="$LIB_NAME-$TARGET_TRIPLE.$LIB_EXTENSION"
elif [[ $1 == "kotlin" ]]; then
    # For Kotlin/Java, use platform-based directory structure like in release.yaml
    COPY_PATH="clients/java/bindings/src/main/resources/$PLATFORM"
    mkdir -p "$COPY_PATH"
    FINAL_LIB_NAME="$LIB_NAME.$LIB_EXTENSION"
else
    echo "Usage: $0 [js|py|kotlin]"
    echo "Unsupported provider type: $1"
    exit 1
fi

# Ensure target directory exists
mkdir -p "$COPY_PATH"

# Source library path
SOURCE_LIB="./target/${TARGET_MODE}/$LIB_NAME.$LIB_EXTENSION"

# Check if source library exists
if [[ ! -f "$SOURCE_LIB" ]]; then
    echo "Error: Source library not found at $SOURCE_LIB"
    echo "Make sure you have built the Rust project first with 'cargo build'"
    exit 1
fi

# Copy the library
DEST_PATH="$COPY_PATH/$FINAL_LIB_NAME"
echo "Copying $SOURCE_LIB to $DEST_PATH"
cp "$SOURCE_LIB" "$DEST_PATH"

if [[ $? -eq 0 ]]; then
    echo "Successfully copied native library for $1 provider"
    echo "Library location: $DEST_PATH"

    # List the contents of the target directory for verification
    echo ""
    echo "Contents of $COPY_PATH:"
    ls -la "$COPY_PATH"
else
    echo "Error: Failed to copy library"
    exit 1
fi
