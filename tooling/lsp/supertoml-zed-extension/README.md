# SuperTOML Analyzer Zed Extension

A Zed editor extension that provides language server support for TOML files using the `supertoml-analyzer` LSP server.

## Features

- **Diagnostics**: Real-time validation of TOML syntax and structure
- **Completions**: Auto-completion for TOML keys and values
- **Hover**: Documentation and type information on hover

## Prerequisites

Before using this extension, you need to build the `supertoml-analyzer` language server:

```bash
# From the superposition repository root
cargo build -p supertoml_lsp
```

This will create the `supertoml-analyzer` binary in `target/debug/` or `target/release/`.

## Installation

### Development Installation

1. Build the extension:
   ```bash
   cd tooling/lsp/zed-extension
   cargo build --release
   ```

2. Link or copy the extension to Zed's extensions directory:
   - **macOS**: `~/.zed/extensions/`
   - **Linux**: `~/.local/share/zed/extensions/`
   - **Windows**: `%APPDATA%\Zed\extensions\`

   Or use Zed's "Install Dev Extension" command pointing to this directory.

### From Source (Recommended for Development)

1. Open Zed
2. Open the command palette (Cmd/Ctrl+Shift+P)
3. Run "zed: install dev extension"
4. Select the `tooling/lsp/zed-extension` directory

## Configuration

The extension will automatically look for `supertoml-analyzer` in:

1. System PATH
2. `target/debug/supertoml-analyzer` relative to the workspace root
3. `target/release/supertoml-analyzer` relative to the workspace root

You can also add the binary to your PATH:

```bash
# Add to PATH (temporary)
export PATH="$PATH:/path/to/superposition/target/debug"

# Or create a symlink
ln -s /path/to/superposition/target/debug/supertoml-analyzer /usr/local/bin/supertoml-analyzer
```

### Zed Settings

You can customize the language server behavior in your Zed `settings.json`:

```json
{
  "languages": {
    "TOML": {
      "tab_size": 2,
      "language_servers": ["supertoml-analyzer"]
    }
  }
}
```

## Development

### Building

```bash
cargo build --release
```

### Testing

1. Build the LSP server: `cargo build -p supertoml_lsp`
2. Install the extension in Zed as a dev extension
3. Open a `.toml` file in Zed
4. The language server should start automatically

### Debugging

Check Zed's logs for any issues:
- **macOS**: `~/.zed/logs/`
- **Linux**: `~/.local/share/zed/logs/`

## File Structure

```
zed-extension/
├── extension.toml    # Extension metadata and language configuration
├── Cargo.toml        # Rust crate configuration
├── src/
│   └── lib.rs        # Extension implementation
└── README.md         # This file
```

## License

Apache-2.0