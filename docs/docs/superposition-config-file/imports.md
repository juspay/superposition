---
sidebar_position: 9
title: Imports
description: Split a SuperTOML config across files with main.stoml imports
---

# Imports

A SuperTOML config doesn't have to live in one file. `main.stoml` can import whole sections from typed files, so you can split default configs, dimensions and overrides however your team works.

```
config/
├── main.stoml                      # entry point; the only file that may import
├── pricing.default-configs.stoml   # only [default-configs]
├── geo.dimensions.stoml            # only [dimensions]
├── surge.overrides.stoml           # only [[overrides]]
└── city/
    └── blr.overrides.stoml         # subfolders are fine
```

```toml
# main.stoml: import lines go before any [section]
default-configs.import = ["pricing.default-configs.stoml"]
dimensions.import      = ["geo.dimensions.stoml"]
overrides.import       = ["surge.overrides.stoml", "city/blr.overrides.stoml"]
```

The result is exactly what you'd get by pasting every imported section into `main.stoml`, with overrides in the order they're listed.

## Rules

### Only `main.stoml` imports

Only a file named exactly `main.stoml` or `main.super.toml` may declare imports. Imports go one level deep: an imported file can't import anything.

### An import replaces the whole section

A section that has `import` may contain nothing else. A section without `import` stays inline in `main.stoml`, so you can mix the two:

```toml
# main.stoml
dimensions.import = ["geo.dimensions.stoml"]   # dimensions come from a file

[default-configs]                               # default configs stay inline
per_km_rate = { value = 20.0, schema = { type = "number" } }
```

All of these are errors:

```toml
dimensions.import = ["geo.dimensions.stoml"]
dimensions.city = { position = 1, schema = { type = "string" } }   # ✗ next to import

[dimensions]                                    # ✗ the section is already imported
city = { position = 1, schema = { type = "string" } }
```

The table form means the same thing as the dotted form, so this also works:

```toml
[dimensions]
import = ["geo.dimensions.stoml"]
```

:::info
An `import` key only counts as an import when its value is a list. A dimension or config key that happens to be named `import` (its value is a table) is still a dimension or config key.
:::

### Import lines go at the top

TOML attaches a dotted key to the last `[section]` header above it. An import line written after `[default-configs]` would define a default config named `dimensions`, so it's reported as an error: imports must come before any `[section]`.

### Imported files are typed by name

| File name | May only contain |
| --- | --- |
| `<name>.default-configs.stoml` | `[default-configs]` |
| `<name>.dimensions.stoml` | `[dimensions]` |
| `<name>.overrides.stoml` | `[[overrides]]` |

The `.super.toml` forms (`geo.dimensions.super.toml`) work too. A file listed under `dimensions.import` must be a `*.dimensions.stoml` file, and so on. Each file keeps its normal section header:

```toml
# geo.dimensions.stoml
[dimensions]
city = { position = 4, schema = { type = "string", enum = ["Bangalore", "Delhi"] } }
city_cohort = { position = 1, type = "LOCAL_COHORT:city", schema = { type = "string", enum = ["south", "otherwise"], definitions = { south = { in = [{ var = "city" }, ["Bangalore"]] } } } }
```

A dimensions or default-configs file must contain its section. An overrides file may be empty.

### Paths

- Paths are relative to `main.stoml`'s folder, and subfolders are fine.
- Absolute paths and `..` are not allowed, so a config folder stays self-contained.
- Use `/`, not `\`. `./a.overrides.stoml` and `a.overrides.stoml` are the same file, and listing a file twice is an error.

### Merging

- **Default configs and dimensions:** if two files define the same name, it's an error naming both files.
- **Overrides:** appended in the order of `overrides.import`. Priority still comes only from [dimension positions](./deterministic-resolution); for overrides with equal priority, list order breaks the tie, exactly as in a single file.

## Parsing from code

Imports need a file path to resolve against, so use `parse_toml_file`:

```rust
use std::path::Path;
use superposition_core::parse_toml_file;

let config = parse_toml_file(Path::new("config/main.stoml"))?;
```

A file without imports parses exactly as it does with `TomlFormat::parse_config`. Passing the text of a `main.stoml` that imports to `parse_config` (or `parse_toml_config`) fails with a clear error telling you to use `parse_toml_file`.

The Rust `FileDataSource` follows imports, reloads when any imported file changes, and watches the folders holding them.

## Errors

Errors point at the file and line they come from:

| Problem | Reported in |
| --- | --- |
| Bad path, missing file, wrong file type, duplicate import | `main.stoml`, on the import line |
| Something next to `import` in a section | `main.stoml`, on that key |
| Wrong section in an imported file, or an import inside one | The imported file |
| Same dimension or config key in two files | The second file, on the key |
| Schema, type or override errors | The file the value is in; override indices (`context[0]`) count within that file |

In code, errors from an imported file come back as `FormatError::InFile { file, error }`, and import-rule errors as `FormatError::ImportError { file, span, message }`. `FormatError::location()` gives the file and the underlying error.

## Editor support

The [language server](./lsp-support) treats `main.stoml` and its imported files as one group:

- An imported file finds its `main.stoml` in its own folder or a parent folder.
- Completion and hover in an overrides file know the dimensions and config keys defined in the other files.
- Editing one file re-checks the group, and errors show up in the file they belong to, even if it isn't open.
- A typed file that no `main.stoml` imports gets a warning and only TOML syntax checks.

## Limitations

- JSON configs can't import.
- Export (`TomlFormat::serialize`, the `/toml` endpoint) always writes one flattened file.
- Clients that pass config contents as a string, such as the Python `FileDataSource` and the language bindings, can't follow imports yet.
