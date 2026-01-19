# Development Guide - Superposition Admin NPM Package

This guide explains how to build and develop the Superposition admin frontend npm package.

## Prerequisites

### Required Tools

1. **Rust toolchain** (with wasm32 target)
   ```bash
   curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
   rustup target add wasm32-unknown-unknown
   ```

2. **wasm-pack** (Rust to WebAssembly compiler)
   ```bash
   cargo install wasm-pack
   ```

3. **Node.js and npm** (for Tailwind CSS)
   ```bash
   # Install from https://nodejs.org/ or use your package manager
   # Minimum version: Node 14+
   ```

## Building the Package

### Option 1: Using the Makefile (Recommended)

From the repository root:

```bash
make npm-package
```

This will:
- Build the Rust/WASM frontend with release optimizations
- Compile and minify Tailwind CSS
- Copy all necessary files to `crates/frontend/npm/`
- Display next steps for testing and publishing

### Option 2: Using the Build Script

From the `crates/frontend` directory:

```bash
cd crates/frontend
./npm/build-npm.sh
```

### Option 3: Manual Build

```bash
cd crates/frontend

# 1. Build WASM
wasm-pack build --target web --release --no-default-features --features 'hydrate'

# 2. Compile CSS
npx tailwindcss -i ./styles/tailwind.css -o ./pkg/style.css --minify

# 3. Copy files
rm -rf npm/pkg npm/assets
mkdir -p npm/pkg npm/assets
cp pkg/frontend.js npm/pkg/
cp pkg/frontend_bg.wasm npm/pkg/
cp pkg/style.css npm/pkg/
cp -a assets/. npm/assets/
```

## Testing the Package

### Local Testing with npm pack

```bash
cd crates/frontend/npm
npm pack
```

This creates a `.tgz` file (e.g., `juspay-superposition-admin-0.97.0.tgz`) that you can install in any project:

```bash
# In your test project
npm install /path/to/juspay-superposition-admin-0.97.0.tgz
```

### Testing with the Example HTML

```bash
cd crates/frontend/npm
python3 -m http.server 8000
# or: npx http-server -p 8000
```

Then open http://localhost:8000/example.html in your browser.

**Note:** Make sure your Superposition backend is running and accessible at the configured host.

### Testing in a Real Project

Create a test HTML file:

```html
<!DOCTYPE html>
<html>
<head>
    <title>Test</title>
</head>
<body>
    <div id="superposition-admin"></div>
    <script type="module">
        import SuperpositionAdmin from './node_modules/@juspay/superposition-admin/index.js';

        const admin = new SuperpositionAdmin({
            host: 'http://localhost:8080'
        });

        admin.mount();
    </script>
</body>
</html>
```

## Development Workflow

### Making Changes to the Frontend

1. Make your changes in the Rust source files (`crates/frontend/src/**/*.rs`)
2. Rebuild the package:
   ```bash
   make npm-package
   # or: ./npm/build-npm.sh
   ```
3. Test the changes using the example.html
4. If testing in another project, re-pack and reinstall:
   ```bash
   cd npm
   npm pack
   cd /path/to/test/project
   npm install /path/to/superposition/crates/frontend/npm/juspay-superposition-admin-0.97.0.tgz
   ```

### Development Mode (Faster Iteration)

For faster iteration during development, you can use the dev build:

```bash
cd crates/frontend
wasm-pack build --target web --dev --no-default-features --features 'hydrate'
npx tailwindcss -i ./styles/tailwind.css -o ./pkg/style.css
```

Then copy the files to npm/pkg and test.

### Updating Styles

If you only change Tailwind CSS:

```bash
cd crates/frontend
npx tailwindcss -i ./styles/tailwind.css -o ./npm/pkg/style.css --minify
```

### Debugging

To enable debug output in the WASM module:

1. Build with debug symbols:
   ```bash
   wasm-pack build --target web --dev --no-default-features --features 'hydrate'
   ```

2. Check browser console for Rust panic messages (thanks to `console_error_panic_hook`)

3. Use browser DevTools to inspect WASM execution:
   - Chrome: DevTools → Sources → WASM modules
   - Firefox: DevTools → Debugger → WASM

## Publishing

### Pre-publish Checklist

- [ ] Version number updated in `package.json` and workspace `Cargo.toml`
- [ ] CHANGELOG updated with release notes
- [ ] All tests passing
- [ ] Package built with release mode
- [ ] Example.html tested and working
- [ ] README.md is up to date

### Publishing to npm

```bash
cd crates/frontend/npm

# Login to npm (first time only)
npm login

# Publish
npm publish
```

For scoped packages (`@juspay/superposition-admin`), ensure it's set to public:

```bash
npm publish --access public
```

### Dry Run

To test the publish process without actually publishing:

```bash
npm publish --dry-run
```

## Package Structure

```
npm/
├── index.js              # Main entry point (JS wrapper)
├── index.d.ts           # TypeScript definitions
├── package.json         # npm package metadata
├── README.md            # User documentation
├── DEVELOPMENT.md       # This file
├── build-npm.sh         # Build script
├── example.html         # Usage example
├── .npmignore          # Files to exclude from package
├── LICENSE              # License file
├── pkg/                 # Built artifacts (generated)
│   ├── frontend.js      # WASM module loader
│   ├── frontend_bg.wasm # WebAssembly binary
│   └── style.css        # Compiled Tailwind CSS
└── assets/              # Static assets (generated)
    ├── favicon.ico
    └── codicon.ttf
```

## Troubleshooting

### wasm-pack build fails

- Ensure you have the wasm32-unknown-unknown target:
  ```bash
  rustup target add wasm32-unknown-unknown
  ```
- Clear the target directory and try again:
  ```bash
  cargo clean
  ```

### CSS not compiling

- Ensure Tailwind is installed:
  ```bash
  npm install -g tailwindcss
  ```
- Check that `tailwind.config.js` exists in `crates/frontend/`

### Package size too large

The WASM binary is optimized with `wasm-pack --release`, which includes:
- Dead code elimination
- Optimization level 3
- LTO (Link Time Optimization)

For further size reduction, you could use `wasm-opt` from Binaryen:
```bash
wasm-opt -Oz -o optimized.wasm frontend_bg.wasm
```

## Contributing

When contributing to the npm package:

1. Make changes to source files in `crates/frontend/src/`
2. Update this documentation if adding new features
3. Test thoroughly with example.html
4. Update README.md if user-facing changes
5. Update version in package.json and Cargo.toml

## Resources

- [wasm-pack documentation](https://rustwasm.github.io/wasm-pack/)
- [Leptos documentation](https://leptos.dev/)
- [npm documentation](https://docs.npmjs.com/)
- [WebAssembly](https://webassembly.org/)
