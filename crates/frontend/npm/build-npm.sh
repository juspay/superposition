#!/bin/bash
set -e

# Build script for npm package
# This script builds the Superposition admin frontend and prepares it for npm publishing

echo "🔨 Building Superposition Admin NPM Package"
echo "============================================"
echo ""

# Navigate to frontend directory
cd "$(dirname "$0")/.."

# Check for required tools
echo "Checking for required tools..."
if ! command -v wasm-pack &> /dev/null; then
    echo "❌ Error: wasm-pack is not installed"
    echo "Install it with: cargo install wasm-pack"
    exit 1
fi

if ! command -v npx &> /dev/null; then
    echo "❌ Error: npx is not installed"
    echo "Install Node.js from: https://nodejs.org/"
    exit 1
fi

echo "✅ All required tools are available"
echo ""

# Build WASM with release optimizations
echo "📦 Building WebAssembly module (release mode)..."
wasm-pack build --target web --release --no-default-features --features 'hydrate'

if [ $? -ne 0 ]; then
    echo "❌ WASM build failed"
    exit 1
fi

echo "✅ WASM build complete"
echo ""

# Compile and minify CSS
echo "🎨 Compiling Tailwind CSS..."
npx tailwindcss -i ./styles/tailwind.css -o ./pkg/style.css --minify

if [ $? -ne 0 ]; then
    echo "❌ CSS compilation failed"
    exit 1
fi

echo "✅ CSS compilation complete"
echo ""

# Prepare npm package structure
echo "📁 Preparing npm package structure..."
rm -rf npm/pkg npm/assets
mkdir -p npm/pkg npm/assets

# Copy WASM artifacts
echo "   Copying WASM artifacts..."
cp pkg/frontend.js npm/pkg/
cp pkg/frontend_bg.wasm npm/pkg/
cp pkg/style.css npm/pkg/

# Copy assets
echo "   Copying assets..."
cp -a assets/. npm/assets/

# Copy license files if they exist
echo "   Copying license files..."
if [ -f "../../LICENSE-MIT" ]; then
    cp ../../LICENSE-MIT npm/
elif [ -f "../../LICENSE" ]; then
    cp ../../LICENSE npm/
fi

if [ -f "../../LICENSE-APACHE" ]; then
    cp ../../LICENSE-APACHE npm/
fi

echo "✅ Package structure prepared"
echo ""

# Calculate package size
PACKAGE_SIZE=$(du -sh npm | cut -f1)

echo "============================================"
echo "✨ NPM Package built successfully!"
echo "============================================"
echo ""
echo "📊 Package size: $PACKAGE_SIZE"
echo "📍 Package location: $(pwd)/npm/"
echo ""
echo "Next steps:"
echo "  1. Test the package:"
echo "     cd npm && npm pack"
echo "     # This creates a .tgz file you can test with npm install"
echo ""
echo "  2. Publish to npm:"
echo "     cd npm && npm publish"
echo ""
echo "  3. Test locally with the example:"
echo "     cd npm"
echo "     python3 -m http.server 8000"
echo "     # Open http://localhost:8000/example.html"
echo ""
