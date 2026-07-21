---
sidebar_position: 8
title: LSP Support
description: Language Server Protocol support for SuperTOML
---

# LSP Support

SuperTOML has a Language Server Protocol (LSP) server in this repository: `supertoml-analyzer`, built from the `supertoml_lsp` crate. Editor integrations live under `tooling/lsp/`.

## Features

### Autocomplete

Get intelligent suggestions while typing:

- **Dimension names**: Suggests available dimensions when editing `_context_`
- **Config keys**: Suggests valid configuration keys in overrides
- **Enum values**: Suggests valid enum values for dimensions and configs
- **Schema properties**: Suggests JSON Schema draft-7 properties

**Example:**

```toml
[dimensions]
city = { position = 4, schema = { type = "string", enum = ["Bangalore", "Delhi", "Chennai"] } }

[[overrides]]
_context_ = { city = "|" }  # Cursor here, autocomplete shows: Bangalore, Delhi, Chennai
```

### Validation

Real-time validation catches errors before you run your application:

- **Schema validation**: Values are validated against their schemas
- **Dimension validation**: Context values match dimension schemas
- **Key validation**: Override keys exist in default-configs
- **Position validation**: Dimension positions are unique
- **Cohort validation**: Cohort references are valid

**Example errors shown in editor:**

```toml
[default-configs]
per_km_rate = { value = -5.0, schema = { type = "number", minimum = 0 } }
# Error: Value -5.0 is less than minimum 0

[dimensions]
city = { position = 1, schema = { type = "string", enum = ["Bangalore", "Delhi", "Chennai"] } }

[[overrides]]
_context_ = { city = "Mumbai" }  # Error: "Mumbai" is not in enum ["Bangalore", "Delhi", "Chennai"]
per_km_rate = 25.0
```

### Hover Information

Hover over any element to see detailed information:

- **Config keys**: Shows schema, default value, and description
- **Dimensions**: Shows position, type, and enum values
- **Context values**: Shows the dimension schema and valid values

**Example:**

```toml
[default-configs]
per_km_rate = { value = 20.0, schema = { type = "number", minimum = 0 } }
# Hover over "per_km_rate" shows:
# ---
# **per_km_rate**
# Type: number
# Default: 20.0
# Constraints: minimum = 0
# ---
```

### Formatting

The server exposes document formatting and uses Taplo formatting for the full document.

### Diagnostics

Get detailed error messages and warnings:

- **Syntax errors**: Invalid TOML syntax
- **Schema errors**: Values don't match schemas
- **Reference errors**: Unknown dimensions or config keys
- **Structure errors**: Missing required sections, duplicate positions, invalid cohort references

## Editor Setup

### VS Code

Build the language server and run the checked-in extension from source:

```bash
cargo build -p supertoml_lsp
cd tooling/lsp/vscode-extension
npm install
npm run compile
```

Open `tooling/lsp/vscode-extension` in VS Code and start the extension development host, or package it with `npm run package` and install the generated VSIX.

**Extension features:**

- Syntax highlighting for SuperTOML
- LSP integration with diagnostics, completions, hover, and formatting
- Configuration snippets
- File icons

### Neovim

Use the checked-in Neovim plugin after building the language server:

```lua
{
    "superposition/superposition",
    dir = "/path/to/superposition",
    rtp = "tooling/lsp/neovim-extension",
    config = function()
        require("supertoml-analyzer").setup({
            server = {
                path = "/path/to/superposition/target/debug/supertoml-analyzer",
            },
        })
    end,
}
```

### Emacs

Use `lsp-mode` with the `supertoml-analyzer` binary:

```elisp
(use-package lsp-mode
  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "supertoml-analyzer")
                    :major-modes '(toml-mode)
                    :server-id 'supertoml))
  (add-hook 'toml-mode-hook #'lsp))
```

### Other Editors

Any editor with LSP support can use the SuperTOML language server:

1. Build or install the `supertoml-analyzer` binary
2. Configure your editor to use it for TOML files
3. The server communicates via stdio

## Language Server Configuration

### Initialization Options

Configure the language server during initialization:

```json
{
    "settings": {
        "superTOML.diagnostics.enable": true,
        "superTOML.completions.enable": true,
        "superTOML.hover.enable": true,
        "superTOML.server.path": "/path/to/superposition/target/debug/supertoml-analyzer"
    }
}
```

### Settings

| Setting                    | Type    | Default | Description                 |
| -------------------------- | ------- | ------- | --------------------------- |
| `superTOML.server.path` | string | `""` | Path to the `supertoml-analyzer` binary |
| `superTOML.trace.server` | string | `"off"` | VS Code language-client tracing |
| `superTOML.diagnostics.enable` | boolean | true | Enable diagnostics |
| `superTOML.completions.enable` | boolean | true | Enable completions |
| `superTOML.hover.enable` | boolean | true | Enable hover documentation |

## Validation Rules

The LSP validates the following:

### Structure Validation

- `[default-configs]` section exists
- `[dimensions]` section exists
- `[[overrides]]` is an array

### Default Configs Validation

- Every config has `value` and `schema`
- Values validate against schemas
- Schemas are valid JSON Schema

### Dimensions Validation

- Every dimension has `position` and `schema`
- Positions are unique
- Position 0 is conventionally left for experimentation/`variantIds` workflows
- Cohort references are valid

### Overrides Validation

- Every override has `_context_`
- Context dimensions are declared
- Context values match dimension schemas
- Override keys exist in default-configs
- Override values match config schemas

## Performance

The current server uses full-document sync and validates the current document text. For very large files, expect the full file to be parsed again after edits.

## Troubleshooting

### Language Server Not Starting

1. Check that `supertoml-analyzer` is built and in PATH, or configure the editor with its full path
2. Check editor logs for error messages
3. Verify the file type is recognized as TOML

### No Autocomplete

1. Ensure the file has a `[default-configs]` section
2. Check that `superTOML.completions.enable` is true
3. Verify schemas are valid JSON Schema

### Validation Not Working

1. Check that `superTOML.diagnostics.enable` is true
2. Look for syntax errors that prevent parsing
3. Check the output panel for language server errors

### Slow Performance

1. Reduce schema complexity where possible
2. Disable editor-side features you do not need
3. Check for syntax errors that force repeated failed parses

## Future Features

The LSP is actively developed with planned features:

- **Refactoring**: Rename dimensions and config keys
- **Find references**: Find all uses of a dimension or config
- **Code lenses**: Inline actions and information
- **Semantic highlighting**: Better syntax highlighting
- **Document symbols**: Outline view of the configuration

## Contributing

The SuperTOML LSP is open source. Contributions are welcome:

- Report bugs and request features on GitHub
- Submit pull requests for improvements
- Help with editor integration documentation
