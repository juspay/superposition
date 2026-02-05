# JavaScript TOML Binding Tests

This directory contains JavaScript/Node.js bindings for the TOML parsing functions using the C FFI (Foreign Function Interface) implementation.

> **Note**: JavaScript is not supported by uniffi, so these bindings use the `ffi_legacy` C FFI interface instead.

## Prerequisites

1. **Build the superposition_core library:**
   ```bash
   cargo build --release -p superposition_core
   ```

2. **Install Node.js dependencies:**
   ```bash
   cd clients/javascript/bindings
   npm install
   ```

## Running the Tests

```bash
npm test
```

Or run directly:
```bash
node test.js
```

## Architecture

The JavaScript bindings use:
- **C FFI Function**: `core_parse_toml_config` from `ffi_legacy.rs`

## API Reference

### `parseTomlConfig(tomlContent)`

Parses a TOML configuration string and returns structured data.

**Parameters:**
- `tomlContent` (string): TOML configuration string

**Returns:** Object with:
- `default_config` (Object): Map of key → JSON-encoded value
- `contexts_json` (string): JSON string containing array of contexts
- `overrides_json` (string): JSON string containing overrides map
- `dimensions_json` (string): JSON string containing dimensions map

**Example:**
```javascript
const { parseTomlConfig } = require('./index');

const toml = `
[default-config]
rate = { "value" = 10.0, "schema" = { "type" = "number" } }

[dimensions]
region = { schema = { "type" = "string" } }

[[context]]
_condition_ = { region = "us" }
rate = 15.0
`;

const result = parseTomlConfig(toml);
console.log(result.default_config);  // { rate: "10.0" }

const contexts = JSON.parse(result.contexts_json);
console.log(contexts);  // Array of context objects
```

### `evalTomlConfig(tomlContent, inputDimensions, mergeStrategy)`

Parses TOML and evaluates configuration based on input dimensions.

**Parameters:**
- `tomlContent` (string): TOML configuration string
- `inputDimensions` (Object): Dimension values as key-value pairs
- `mergeStrategy` (string): Merge strategy - `"merge"` or `"replace"`

**Returns:** Object with evaluated configuration (key-value pairs)

**Example:**
```javascript
const { evalTomlConfig } = require('./index');

const result = evalTomlConfig(
  tomlContent,
  { region: 'us', vehicle_type: 'cab' },
  'merge'
);

console.log(result.rate);  // "15.0"
```

## Test Coverage

The test suite demonstrates:

### 1. Parse TOML Configuration
- Validates TOML parsing into structured format
- Displays default config, contexts, overrides, and dimensions

### 2. Evaluate TOML with Dimensions
Tests 5 scenarios:
1. **Bike ride** - Single dimension
2. **Cab in Bangalore** - Two dimensions
3. **Delhi morning surge** - Three dimensions (hour=6)
4. **Delhi evening surge** - Three dimensions (hour=18)
5. **Auto ride** - Default configuration

### 3. Parse External File
- Demonstrates reading TOML from filesystem
- Parses `examples/superposition_toml_example/example.toml`

### 4. Error Handling
- Invalid TOML syntax
- Missing required sections

## Expected Output

When all tests pass:

```text
======================================================================
  TEST SUMMARY
======================================================================
  ✓ Parse TOML
  ✓ Eval TOML
  ✓ External File

  Total: 3/3 tests passed
======================================================================
```

## Merge Strategies

- `"merge"` (default): Merges override values with default configuration
- `"replace"`: Replaces entire configuration with override values

## Error Handling

Functions throw JavaScript `Error` objects on failure:

```javascript
try {
  const result = parseTomlConfig(invalidToml);
} catch (error) {
  console.error('Parsing failed:', error.message);
}
```

## Platform Support

The bindings automatically detect the platform and load the appropriate library:

- **macOS**: `libsuperposition_core.dylib`
- **Linux**: `libsuperposition_core.so`
- **Windows**: `superposition_core.dll`

## Using in Your Project

### Installation

```bash
npm install @superposition/toml-bindings
```

### Basic Usage

```javascript
const { parseTomlConfig, evalTomlConfig } = require('@superposition/toml-bindings');

// Parse TOML
const parsed = parseTomlConfig(tomlString);

// Evaluate with dimensions
const config = evalTomlConfig(
  tomlString,
  { city: 'Bangalore', vehicle_type: 'cab' },
  'merge'
);

console.log(config.per_km_rate);  // "22.0"
```

## Memory Management

The bindings handle memory management automatically:
- C strings returned from FFI functions are automatically freed after reading
- Error buffers are allocated and deallocated per function call
- No manual memory management required from JavaScript side

## TOML Structure

```toml
[default-config]
key1 = { "value" = <value>, "schema" = <json-schema> }

[dimensions]
dim1 = { schema = <json-schema> }

[[context]]
_condition_ = { dim1 = "value1" }
key1 = <override-value>

[[context]]
_condition_ = { dim1 = "value1", dim2 = "value2" }
key1 = <override-value>
```

See `test.js` for a complete ride-sharing pricing example.

## Technical Details

### C FFI Signatures

The bindings call these C functions:

```c
// Parse TOML configuration
char* core_parse_toml_config(
    const char* toml_content,
    char* error_buffer
);

// Free strings allocated by the library
void core_free_string(char* ptr);
```

### FFI Type Mappings

| Rust Type | C Type | JavaScript/FFI Type |
|-----------|--------|---------------------|
| `*const c_char` | `const char*` | `ref.types.CString` |
| `*mut c_char` | `char*` | `ref.refType(ref.types.CString)` |
| `void` | `void` | `'void'` |

## Troubleshooting

### Library Not Found

If you get "Library not found" errors:
1. Ensure you've built the Rust library: `cargo build --release -p superposition_core`
2. Check that the library exists in `target/release/`
3. Verify the library filename matches your platform

## Development

To modify the bindings:

1. **index.js**: Core FFI bindings and wrapper functions
2. **test.js**: Test suite
3. **package.json**: Dependencies and metadata

After making changes, run the tests to verify:
```bash
npm test
```
