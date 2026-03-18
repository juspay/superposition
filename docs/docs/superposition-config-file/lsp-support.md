---
sidebar_position: 8
title: LSP Support
description: Language Server Protocol support for SuperTOML
---

# LSP Support

SuperTOML has full Language Server Protocol (LSP) support, providing a rich editing experience in IDEs and text editors. The LSP enables real-time validation, autocompletion, hover information, and more.

## Features

### Autocomplete

Get intelligent suggestions while typing:

- **Dimension names**: Suggests available dimensions when editing `_context_`
- **Config keys**: Suggests valid configuration keys in overrides
- **Enum values**: Suggests valid enum values for dimensions and configs
- **Schema properties**: Suggests JSON Schema properties

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

### Diagnostics

Get detailed error messages and warnings:

- **Syntax errors**: Invalid TOML syntax
- **Schema errors**: Values don't match schemas
- **Reference errors**: Unknown dimensions or config keys
- **Logic errors**: Conflicting overrides, circular dependencies

### Go to Definition

Navigate to definitions:

- From a context dimension name to its dimension definition
- From an override key to its default-config definition
- From a cohort reference to its base dimension

### Code Actions

Quick fixes for common issues:

- **Add missing schema**: Add schema to config without one
- **Fix enum value**: Correct an invalid enum value
- **Add missing dimension**: Add an undeclared dimension

## Editor Setup

### VS Code

1. Install the SuperTOML extension from the marketplace
2. Open any `.toml` file with SuperTOML content
3. The extension automatically activates for files with `[default-configs]` section

**Extension features:**

- Syntax highlighting for SuperTOML
- Full LSP integration
- Configuration snippets
- File icons

### Neovim

Configure Neovim to use the SuperTOML language server:

```lua
-- Using nvim-lspconfig
local lspconfig = require('lspconfig')

lspconfig.supertoml.setup {
    filetypes = { 'toml' },
    root_dir = function(fname)
        return lspconfig.util.find_git_ancestor(fname)
    end,
}
```

### Emacs

Use `lsp-mode` with the SuperTOML language server:

```elisp
(use-package lsp-mode
  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "supertoml-lsp")
                    :major-modes '(toml-mode)
                    :server-id 'supertoml))
  (add-hook 'toml-mode-hook #'lsp))
```

### Other Editors

Any editor with LSP support can use the SuperTOML language server:

1. Install the `supertoml-lsp` binary
2. Configure your editor to use it for TOML files
3. The server communicates via stdio

## Language Server Configuration

### Initialization Options

Configure the language server during initialization:

```json
{
    "settings": {
        "supertoml": {
            "validation": {
                "enabled": true,
                "strict": false
            },
            "completion": {
                "enabled": true,
                "suggestValues": true
            },
            "hover": {
                "enabled": true,
                "showSchema": true
            }
        }
    }
}
```

### Settings

| Setting                    | Type    | Default | Description                 |
| -------------------------- | ------- | ------- | --------------------------- |
| `validation.enabled`       | boolean | true    | Enable real-time validation |
| `validation.strict`        | boolean | false   | Treat warnings as errors    |
| `completion.enabled`       | boolean | true    | Enable autocomplete         |
| `completion.suggestValues` | boolean | true    | Suggest enum values         |
| `hover.enabled`            | boolean | true    | Enable hover information    |
| `hover.showSchema`         | boolean | true    | Show schema in hover        |

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
- Position 0 is not used (reserved)
- Cohort references are valid

### Overrides Validation

- Every override has `_context_`
- Context dimensions are declared
- Context values match dimension schemas
- Override keys exist in default-configs
- Override values match config schemas

## Performance

The language server is optimized for large configuration files:

- **Incremental parsing**: Only re-parses changed sections
- **Caching**: Schemas and validations are cached
- **Lazy validation**: Only validates visible ranges
- **Background processing**: Heavy operations don't block the UI

### File Size Limits

| File Size     | Performance        |
| ------------- | ------------------ |
| < 100 KB      | Instant validation |
| 100 KB - 1 MB | < 100ms validation |
| 1 MB - 10 MB  | < 500ms validation |
| > 10 MB       | May require tuning |

### Optimizing Large Files

For very large configuration files:

1. **Split into multiple files**: Use includes or imports
2. **Reduce schema complexity**: Simpler schemas validate faster
3. **Disable strict mode**: Use `validation.strict: false`
4. **Increase memory**: Allocate more memory to the language server

## Troubleshooting

### Language Server Not Starting

1. Check that `supertoml-lsp` is installed and in PATH
2. Check editor logs for error messages
3. Verify the file type is recognized as TOML

### No Autocomplete

1. Ensure the file has a `[default-configs]` section
2. Check that `completion.enabled` is true
3. Verify schemas are valid JSON Schema

### Validation Not Working

1. Check that `validation.enabled` is true
2. Look for syntax errors that prevent parsing
3. Check the output panel for language server errors

### Slow Performance

1. Reduce file size by splitting
2. Disable features you don't need
3. Check for circular references in schemas
4. Increase language server memory

## Future Features

The LSP is actively developed with planned features:

- **Code formatting**: Auto-format SuperTOML files
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
