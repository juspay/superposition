#!/bin/bash
set -e

echo "🚀 Setting up Superposition HTML Integration Example"
echo "===================================================="
echo ""

# Check for Node.js
if ! command -v node &> /dev/null; then
    echo "❌ Node.js is not installed"
    echo "   Please install Node.js from https://nodejs.org/"
    exit 1
fi

echo "✅ Node.js found: $(node --version)"

# Check for npm
if ! command -v npm &> /dev/null; then
    echo "❌ npm is not installed"
    exit 1
fi

echo "✅ npm found: $(npm --version)"
echo ""

# Install dependencies
echo "📦 Installing dependencies..."
npm install

if [ $? -ne 0 ]; then
    echo "❌ Failed to install dependencies"
    exit 1
fi

echo ""
echo "✅ Setup complete!"
echo ""
echo "📖 Next steps:"
echo ""
echo "  1. Make sure Superposition backend is running:"
echo "     cd ../.. && make run"
echo ""
echo "  2. Start the example application:"
echo "     npm start"
echo ""
echo "  3. Open http://localhost:8000 in your browser"
echo ""
echo "📚 For more information, see README.md"
echo ""
