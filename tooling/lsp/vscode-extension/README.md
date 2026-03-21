# SuperTOML Analyzer - VS Code Extension

A Visual Studio Code extension that provides language server support for SuperTOML configuration files using the `supertoml-analyzer` LSP server.

## Features

- **Diagnostics**: Real-time validation of TOML syntax and structure
- **Completions**: Auto-completion for TOML keys and values
- **Hover**: Documentation and type information on hover
- **Syntax Highlighting**: Full TOML syntax highlighting with SuperTOML-specific enhancements

## Prerequisites

Before using this extension, you need to build the `supertoml-analyzer` language server:

```bash
# From the superposition repository root
cargo build -p supertoml_lsp
```

This will create the `supertoml-analyzer` binary in `target/debug/` or `target/release/`.

## Installation

### Method 1: Development Mode (Recommended for Testing)

1. **Build the LSP server**:
   ```bash
   cd /path/to/superposition
   cargo build -p supertoml_lsp
   ```

2. **Install extension dependencies**:
   ```bash
   cd tooling/lsp/vscode-extension
   npm install
   ```

3. **Compile the extension**:
   ```bash
   npm run compile
   ```

4. **Launch in VS Code Development Mode**:
   - Open the `tooling/lsp/vscode-extension` folder in VS Code
   - Press `F5` or go to **Run > Start Debugging**
   - This will open a new VS Code Extension Development Host window with the extension loaded

### Method 2: Install from VSIX Package

1. **Package the extension**:
   ```bash
   cd tooling/lsp/vscode-extension
   npm install
   npm run compile
   npm run package
   ```
   This creates a `.vsix` file in the current directory.

2. **Install the VSIX**:
   - Open VS Code
   - Go to **Extensions** (Ctrl/Cmd+Shift+X)
   - Click the `...` menu in the top-right of the Extensions panel
   - Select **Install from VSIX...**
   - Choose the generated `.vsix` file

### Method 3: Manual Installation

1. Copy the entire `vscode-extension` folder to:
   - **macOS/Linux**: `~/.vscode/extensions/superposition-supertoml-analyzer-0.1.0/`
   - **Windows**: `%USERPROFILE%\.vscode\extensions\superposition-supertoml-analyzer-0.1.0\`

2. Run `npm install && npm run compile` in that directory

3. Restart VS Code

## Configuration

The extension can be configured through VS Code settings:

### Settings

| Setting | Type | Default | Description |
|---------|------|---------|-------------|
| `superTOML.server.path` | string | `""` | Path to the supertoml-analyzer binary. If empty, the extension will try to find it in PATH or use the workspace target directory. |
| `superTOML.trace.server` | string | `"off"` | Traces the communication between VS Code and the language server. Options: `off`, `messages`, `verbose`. |
| `superTOML.diagnostics.enable` | boolean | `true` | Enable diagnostics for SuperTOML files. |
| `superTOML.completions.enable` | boolean | `true` | Enable auto-completions for SuperTOML files. |
| `superTOML.hover.enable` | boolean | `true` | Enable hover documentation for SuperTOML files. |

### Configuring the Server Path

If the extension cannot find the `supertoml-analyzer` binary automatically, you can specify the path in your VS Code settings:

1. Open VS Code Settings (Ctrl/Cmd+,)
2. Search for "SuperTOML"
3. Set `superTOML.server.path` to the full path of the binary

Or add to your `settings.json`:
```json
{
  "superTOML.server.path": "/path/to/superposition/target/debug/supertoml-analyzer"
}
```

## Testing the Extension

### Quick Test

1. **Build the LSP server**:
   ```bash
   cd /path/to/superposition
   cargo build -p supertoml_lsp
   ```

2. **Open the extension in development mode**:
   ```bash
   cd tooling/lsp/vscode-extension
   code .
   ```

3. **Start debugging**:
   - Press `F5` or go to **Run > Start Debugging**
   - A new VS Code window will open (Extension Development Host)

4. **Create a test file**:
   - In the new window, create a new file called `test.super.toml`
   - The extension should activate and provide syntax highlighting

5. **Verify LSP features**:
   - Type invalid TOML syntax and check for diagnostics (red squiggly lines)
   - Hover over keys to see hover information
   - Try typing to see completions

### Test with Existing SuperTOML Files

1. Open the superposition repository in the Extension Development Host window
2. Navigate to any `.super.toml` or `.stoml` file (e.g., `test.super.toml` in the root)
3. The language server should start and provide diagnostics

### Check Language Server Status

1. Open the **Output** panel (View > Output or Ctrl/Cmd+Shift+U)
2. Select "SuperTOML Analyzer" from the dropdown
3. You should see logs from the language server

### Debug the Extension

1. In the original VS Code window (not the Extension Development Host), you can set breakpoints in `src/extension.ts`
2. Use the Debug Console to see extension logs
3. The Extension Development Host window will show the extension behavior

## Troubleshooting

### Language Server Not Starting

1. **Check if the binary exists**:
   ```bash
   ls -la /path/to/superposition/target/debug/supertoml-analyzer
   ```

2. **Make sure it's executable**:
   ```bash
   chmod +x /path/to/superposition/target/debug/supertoml-analyzer
   ```

3. **Test the binary directly**:
   ```bash
   /path/to/superposition/target/debug/supertoml-analyzer --help
   ```

4. **Check the Output panel**:
   - Open View > Output
   - Select "SuperTOML Analyzer" from the dropdown
   - Look for error messages

### No Syntax Highlighting

1. Make sure the file has the correct extension (`.super.toml` or `.stoml`)
2. Check that the language mode is set to "SuperTOML" (shown in the bottom-right of VS Code)
3. If not, click the language mode indicator and select "SuperTOML"

### No Diagnostics or Completions

1. Check that the language server is running (see Output panel)
2. Verify that `superTOML.diagnostics.enable` and `superTOML.completions.enable` are set to `true`
3. Try restarting the language server:
   - Open Command Palette (Ctrl/Cmd+Shift+P)
   - Run "SuperTOML: Restart SuperTOML Language Server"

## Commands

| Command | Description |
|---------|-------------|
| `superTOML.restartServer` | Restart the SuperTOML Language Server |

## File Types

The extension activates for the following file types:
- Files with extension `.super.toml`
- Files with extension `.stoml`
- Files named `super.toml`

## Development

### Building from Source

```bash
# Install dependencies
npm install

# Compile TypeScript
npm run compile

# Watch for changes during development
npm run watch

# Package as VSIX
npm run package
```

### Project Structure

```
vscode-extension/
├── package.json              # Extension manifest
├── tsconfig.json             # TypeScript configuration
├── language-configuration.json # Language settings (brackets, comments)
├── syntaxes/
│   └── superTOML.tmLanguage.json # TextMate grammar for syntax highlighting
├── src/
│   └── extension.ts          # Main extension code
├── out/                      # Compiled JavaScript (generated)
└── README.md                 # This file
```

## License

Apache-2.0