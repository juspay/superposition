# SuperTOML Analyzer - Neovim Plugin

A Neovim plugin that provides Language Server Protocol (LSP) support for SuperTOML configuration files using the `supertoml-analyzer` language server.

## Features

- **Diagnostics**: Real-time validation of TOML syntax and structure
- **Completions**: Auto-completion for TOML keys and values
- **Hover**: Documentation and type information on hover
- **Go to Definition**: Navigate to key definitions
- **References**: Find all references to a key
- **Rename**: Rename keys across the file
- **Syntax Highlighting**: Full TOML syntax highlighting with SuperTOML-specific enhancements
- **TreeSitter Support**: Optional TreeSitter queries for enhanced highlighting and textobjects

## Requirements

- Neovim 0.9.0 or later
- The `supertoml-analyzer` binary (built from the superposition repository)

## Installation

### Building the Language Server

Before using this plugin, you need to build the `supertoml-analyzer` language server:

```bash
# From the superposition repository root
cargo build -p supertoml_lsp
```

This will create the `supertoml-analyzer` binary in `target/debug/` or `target/release/`.

### Plugin Installation

#### Using lazy.nvim

```lua
{
    "superposition/superposition",
    dir = "/path/to/superposition",
    rtp = "tooling/lsp/neovim-extension",
    config = function()
        require("supertoml-analyzer").setup({
            -- Optional configuration
            server = {
                path = "/path/to/supertoml-analyzer",  -- Auto-detected if nil
            },
            diagnostics = {
                enable = true,
            },
            completions = {
                enable = true,
            },
            hover = {
                enable = true,
            },
        })
    end,
    ft = { "superTOML" },
}
```

#### Using packer.nvim

```lua
use {
    "superposition/superposition",
    rtp = "tooling/lsp/neovim-extension",
    config = function()
        require("supertoml-analyzer").setup()
    end,
    ft = { "superTOML" },
}
```

#### Using vim-plug

```vim
Plug "superposition/superposition", { "rtp": "tooling/lsp/neovim-extension" }
```

Then add to your `init.lua`:
```lua
require("supertoml-analyzer").setup()
```

#### Local Development

For local development, you can use the plugin directly from the repository:

```lua
-- Using lazy.nvim with local path
{
    url = "/path/to/superposition/tooling/lsp/neovim-extension",
    name = "supertoml-analyzer",
    config = function()
        require("supertoml-analyzer").setup()
    end,
}
```

Or add to your `runtimepath`:
```vim
set rtp+=/path/to/superposition/tooling/lsp/neovim-extension
```

## Configuration

### Default Configuration

```lua
require("supertoml-analyzer").setup({
    server = {
        path = nil,      -- Auto-detect if nil
        cmd = nil,       -- Custom command (overrides path)
        env = nil,       -- Additional environment variables
        timeout = 10000, -- 10 seconds
    },
    diagnostics = {
        enable = true,
    },
    completions = {
        enable = true,
    },
    hover = {
        enable = true,
    },
    on_attach = nil,     -- Callback when LSP attaches to a buffer
    capabilities = nil,  -- Custom LSP capabilities
    log_level = "info",  -- "trace", "debug", "info", "warn", "error"
})
```

### Custom Server Path

If the binary is not in your PATH or the auto-detection doesn't work, specify the path:

```lua
require("supertoml-analyzer").setup({
    server = {
        path = "/path/to/superposition/target/debug/supertoml-analyzer",
    },
})
```

### Custom On Attach Callback

Add your own keymaps or settings when the LSP attaches:

```lua
require("supertoml-analyzer").setup({
    on_attach = function(client, bufnr)
        -- Your custom keymaps
        local opts = { buffer = bufnr, noremap = true, silent = true }
        vim.keymap.set("n", "gd", vim.lsp.buf.definition, opts)
        vim.keymap.set("n", "K", vim.lsp.buf.hover, opts)
        vim.keymap.set("n", "<leader>r", vim.lsp.buf.rename, opts)
    end,
})
```

### Integration with nvim-cmp

The plugin automatically detects and uses `nvim-cmp` capabilities if available:

```lua
-- No additional configuration needed
require("supertoml-analyzer").setup()
```

## Commands

| Command | Description |
|---------|-------------|
| `:SuperTOMLStart` | Start the language server |
| `:SuperTOMLStop` | Stop the language server |
| `:SuperTOMLRestart` | Restart the language server |
| `:SuperTOMLStatus` | Show language server status |
| `:SuperTOMLAttach` | Attach LSP to current buffer |
| `:SuperTOMLLog` | Open the LSP log file |
| `:SuperTOMLCheck` | Check health of SuperTOML setup |
| `:SuperTOMLSetup [config]` | Setup with optional config |

## Default Keymaps

When the LSP attaches to a SuperTOML buffer, the following keymaps are set:

| Keymap | Mode | Description |
|--------|------|-------------|
| `K` | n | Show hover documentation |
| `gd` | n | Go to definition |
| `gr` | n | Find references |
| `<leader>rn` | n | Rename symbol |
| `<leader>ca` | n | Code actions |
| `<leader>f` | n | Format buffer |
| `[d` | n | Previous diagnostic |
| `]d` | n | Next diagnostic |
| `<leader>e` | n | Show diagnostic float |

### Filetype-specific Keymaps

For SuperTOML files, additional keymaps are available:

| Keymap | Mode | Description |
|--------|------|-------------|
| `<C-t>` | i | Insert empty table `[]` |
| `<C-g>` | i | Insert inline table `{}` |
| `<C-a>` | i | Insert array `[]` |
| `<C-s>` | i | Insert string `""` |
| `]t` | n | Go to next table section |
| `[t` | n | Go to previous table section |
| `]a` | n | Go to next array table |
| `[a` | n | Go to previous array table |

## File Types

The plugin activates for the following file types:

- Files with extension `.super.toml`
- Files with extension `.stoml`
- Files named `super.toml`

## API

### Module Functions

```lua
local supertoml = require("supertoml-analyzer")

-- Setup the plugin
supertoml.setup({ /* config */ })

-- Start/stop/restart the language server
supertoml.start()
supertoml.stop()
supertoml.restart()

-- Attach to a buffer
supertoml.attach(bufnr)  -- bufnr is optional, defaults to current buffer

-- Check status
supertoml.is_running()  -- Returns boolean
supertoml.status()      -- Returns "running", "stopped", or "not started"

-- Get plugin info
supertoml.info()        -- Returns table with plugin info

-- Health check
supertoml.check_health()
```

### Submodules

For advanced usage, the internal modules are also accessible:

```lua
local config = require("supertoml-analyzer.config")
local lsp = require("supertoml-analyzer.lsp")

-- Get current configuration
local cfg = config.get()

-- Get server command
local cmd = config.get_server_cmd()

-- Get initialization options
local init_opts = config.get_init_options()

-- Check if buffer is SuperTOML
local is_super_toml = lsp.is_super_toml_buffer(bufnr)

-- Find root directory
local root = lsp.find_root_dir()
```

## TreeSitter Integration

The plugin includes TreeSitter queries for enhanced highlighting and textobjects. To use them, ensure you have the TOML parser installed:

```lua
-- Using nvim-treesitter
require("nvim-treesitter.configs").setup({
    ensure_installed = { "toml" },
})
```

### Available Queries

- `highlights.scm` - Enhanced syntax highlighting
- `textobjects.scm` - Text objects for selection and navigation
- `injections.scm` - Language injections for embedded languages

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

4. **Check LSP logs**:
   ```vim
   :SuperTOMLLog
   ```
   Or check `:LspLog`

### No Syntax Highlighting

1. Make sure the file has the correct extension (`.super.toml` or `.stoml`)
2. Check that the filetype is set correctly:
   ```vim
   :set filetype?
   ```
   Should show `filetype=superTOML`

3. Manually set the filetype:
   ```vim
   :set filetype=superTOML
   ```

### No Diagnostics or Completions

1. Check that the language server is running:
   ```vim
   :SuperTOMLStatus
   ```

2. Verify configuration settings are enabled:
   ```lua
   require("supertoml-analyzer").check_health()
   ```

3. Try restarting the language server:
   ```vim
   :SuperTOMLRestart
   ```

### Binary Not Found

If the plugin cannot find the `supertoml-analyzer` binary:

1. Set the path explicitly in your configuration:
   ```lua
   require("supertoml-analyzer").setup({
       server = {
           path = "/absolute/path/to/supertoml-analyzer",
       },
   })
   ```

2. Or add the binary to your PATH:
   ```bash
   export PATH="/path/to/superposition/target/debug:$PATH"
   ```

## Project Structure

```
neovim-extension/
├── lua/
│   └── supertoml-analyzer/
│       ├── init.lua        # Main entry point and public API
│       ├── config.lua      # Configuration management
│       └── lsp.lua         # LSP client setup and management
├── plugin/
│   └── init.lua            # Plugin initialization
├── ftplugin/
│   └── superTOML.lua       # Filetype-specific settings
├── ftdetect/
│   └── supertoml.lua       # Filetype detection
├── syntax/
│   └── superTOML.vim       # Vim syntax file
├── queries/
│   └── supertoml/
│       ├── highlights.scm  # TreeSitter highlights
│       ├── textobjects.scm # TreeSitter textobjects
│       └── injections.scm  # TreeSitter injections
├── README.md
└── .gitignore
```

## Development

### Testing Locally

1. Build the LSP server:
   ```bash
   cd /path/to/superposition
   cargo build -p supertoml_lsp
   ```

2. Start Neovim with the plugin:
   ```bash
   nvim --cmd "set rtp+=/path/to/superposition/tooling/lsp/neovim-extension"
   ```

3. Open a SuperTOML file and verify the LSP is working:
   ```vim
   :SuperTOMLStatus
   ```

### Debug Mode

Enable verbose logging for debugging:

```lua
require("supertoml-analyzer").setup({
    log_level = "debug",
})
```

Then check the LSP log:
```vim
:SuperTOMLLog
```

## License

Apache-2.0

## Related Projects

- [VS Code Extension](../vscode-extension/) - VS Code extension for SuperTOML
- [Zed Extension](../zed-extension/) - Zed editor extension for SuperTOML
- [LSP Server](../server/) - The SuperTOML language server