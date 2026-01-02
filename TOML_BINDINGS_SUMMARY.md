# TOML Parsing Bindings - Summary

This document summarizes the TOML parsing functionality added to the `superposition_core` crate and the bindings created for Python, Java/Kotlin, and JavaScript.

## Overview

Two new functions have been added to parse and evaluate TOML configurations:

1. **Parse TOML** - Parses TOML into structured format (default config, contexts, overrides, dimensions)
2. **Eval TOML** - Parses and evaluates TOML with input dimensions to get final configuration

## Implementation Details

### Rust Implementation

**Location**: `crates/superposition_core/src/toml_parser.rs`

**Key Functions**:
- `parse(toml_content: &str) -> Result<ParsedTomlConfig, TomlParseError>`
- `eval_toml_config(toml_content: &str, input_dimensions: &Map<String, Value>, merge_strategy: MergeStrategy) -> Result<Map<String, Value>, String>`

**FFI Interfaces**:
- **uniffi** (`ffi.rs`): `ffi_parse_toml_config`, `ffi_eval_toml_config`
- **C FFI** (`ffi_legacy.rs`): `core_parse_toml_config`, `core_eval_toml_config`

### TOML Structure

```toml
[default-config]
key1 = { "value" = <value>, "schema" = <json-schema> }
key2 = { "value" = <value>, "schema" = <json-schema> }

[dimensions]
dim1 = { schema = <json-schema> }
dim2 = { schema = <json-schema> }

[context."dim1=value1"]
key1 = <override-value>

[context."dim1=value1; dim2=value2"]
key2 = <override-value>
```

## Language Bindings

### 1. Python Bindings (uniffi)

**Location**: `clients/python/bindings/`

**Files Created**:
- `test_toml_functions.py` - Comprehensive test suite
- `README_TOML_TESTS.md` - Documentation

**Installation**:
```bash
# Generate bindings
make uniffi-bindings

# Copy library
cp target/release/libsuperposition_core.dylib \
   clients/python/bindings/superposition_bindings/libsuperposition_core-aarch64-apple-darwin.dylib

# Run tests
cd clients/python/bindings
python3 test_toml_functions.py
```

**Usage**:
```python
from superposition_bindings.superposition_client import ffi_parse_toml_config, ffi_eval_toml_config

# Parse TOML
result = ffi_parse_toml_config(toml_content)

# Evaluate with dimensions
config = ffi_eval_toml_config(
    toml_content=toml_string,
    input_dimensions={"city": "Bangalore", "vehicle_type": "cab"},
    merge_strategy="merge"
)
```

**Test Results**: ✓ All 3 tests passed
- Parse TOML
- Eval TOML (5 scenarios)
- External File

### 2. Kotlin/Java Bindings (uniffi)

**Location**: `clients/java/bindings/`

**Files Created**:
- `src/test/kotlin/TomlFunctionsTest.kt` - JUnit test suite
- `README_TOML_TESTS.md` - Documentation
- `build.gradle.kts` - Updated with test dependencies

**Installation**:
```bash
# Generate bindings
make uniffi-bindings

# Run tests
cd clients/java/bindings
./gradlew test
```

**Usage**:
```kotlin
import uniffi.superposition_client.*

// Parse TOML
val result = ffiParseTomlConfig(tomlContent)

// Evaluate with dimensions
val config = ffiEvalTomlConfig(
    tomlContent = tomlString,
    inputDimensions = mapOf(
        "city" to "Bangalore",
        "vehicle_type" to "cab"
    ),
    mergeStrategy = "merge"
)
```

**Test Cases**: 9 test methods
- testParseTomlConfig
- testEvalTomlConfig_BikeRide
- testEvalTomlConfig_CabInBangalore
- testEvalTomlConfig_DelhiMorningSurge
- testEvalTomlConfig_DelhiEveningSurge
- testEvalTomlConfig_AutoRide
- testErrorHandling_InvalidToml
- testErrorHandling_MissingSection
- testMergeStrategy_Replace

### 3. JavaScript Bindings (C FFI)

**Location**: `clients/javascript/bindings/`

**Files Created**:
- `index.js` - FFI bindings using ffi-napi
- `test.js` - Test suite
- `example.js` - Simple usage example
- `package.json` - NPM package configuration
- `README_TOML_TESTS.md` - Documentation
- `.gitignore`

**Note**: Uses C FFI since uniffi doesn't support JavaScript

**Installation**:
```bash
# Build Rust library
cargo build --release -p superposition_core

# Install dependencies (requires Node.js 18 or 20)
cd clients/javascript/bindings
npm install

# Run tests
npm test

# Or run example
node example.js
```

**Usage**:
```javascript
const { parseTomlConfig, evalTomlConfig } = require('./index');

// Parse TOML
const result = parseTomlConfig(tomlContent);

// Evaluate with dimensions
const config = evalTomlConfig(
    tomlContent,
    { city: 'Bangalore', vehicle_type: 'cab' },
    'merge'
);
```

**Test Coverage**: 4 test suites
- Parse TOML Configuration
- Evaluate TOML with Input Dimensions (5 scenarios)
- Parse External TOML File
- Error Handling

## Common Test Scenarios

All binding tests use a consistent ride-sharing pricing example with 5 scenarios:

1. **Bike ride** - `vehicle_type=bike` → `per_km_rate=15.0`
2. **Cab in Bangalore** - `city=Bangalore, vehicle_type=cab` → `per_km_rate=22.0`
3. **Delhi morning surge** - `city=Delhi, vehicle_type=cab, hour_of_day=6` → `surge_factor=5.0`
4. **Delhi evening surge** - `city=Delhi, vehicle_type=cab, hour_of_day=18` → `surge_factor=5.0`
5. **Auto ride** - `vehicle_type=auto` → Uses defaults `per_km_rate=20.0`

## Merge Strategies

Both functions support two merge strategies:

- **`"merge"`** (default): Merges override values with default configuration
- **`"replace"`**: Replaces entire configuration with override values

## Error Handling

All bindings properly handle errors:

- **Python**: Raises `OperationError` exception
- **Kotlin/Java**: Throws `OperationException`
- **JavaScript**: Throws standard `Error` object

## Example TOML File

A complete example is available at:
`examples/superposition-toml-example/example.toml`

## Running All Tests

```bash
# Python
cd clients/python/bindings && python3 test_toml_functions.py

# Kotlin/Java
cd clients/java/bindings && ./gradlew test

# JavaScript (requires Node.js 18 or 20)
cd clients/javascript/bindings && npm install && npm test
```

## API Reference

### Parse Function

| Language | Function Name | Return Type |
|----------|---------------|-------------|
| Rust | `parse_toml_config` | `Result<ParsedTomlConfig, TomlParseError>` |
| Python | `ffi_parse_toml_config` | `ParsedTomlResult` |
| Kotlin | `ffiParseTomlConfig` | `ParsedTomlResult` |
| JavaScript | `parseTomlConfig` | `Object` |

**Returns**:
- `default_config`: Map of key → value
- `contexts_json`: JSON string with contexts array
- `overrides_json`: JSON string with overrides map
- `dimensions_json`: JSON string with dimensions map

### Eval Function

| Language | Function Name | Parameters |
|----------|---------------|------------|
| Rust | `eval_toml_config` | `toml_content, input_dimensions, merge_strategy` |
| Python | `ffi_eval_toml_config` | `toml_content, input_dimensions, merge_strategy` |
| Kotlin | `ffiEvalTomlConfig` | `tomlContent, inputDimensions, mergeStrategy` |
| JavaScript | `evalTomlConfig` | `tomlContent, inputDimensions, mergeStrategy` |

**Returns**: Map/Object of configuration key-value pairs

## Dependencies Added

### Rust
- `toml = "0.8"` - TOML parsing
- `blake3 = "1.5"` - Hashing for override IDs

### Python
- None (uses generated bindings)

### Kotlin/Java
- `junit:junit:4.13.2` - Testing
- `com.google.code.gson:gson:2.10.1` - JSON parsing

### JavaScript
- `ffi-napi` - FFI bindings
- `ref-napi` - Pointer handling
- `ref-array-napi` - Array handling

## Files Modified

- `crates/superposition_core/Cargo.toml` - Added dependencies
- `crates/superposition_core/src/lib.rs` - Exported new module
- `crates/superposition_core/src/ffi.rs` - Added uniffi functions
- `crates/superposition_core/src/ffi_legacy.rs` - Added C FFI functions
- `clients/java/bindings/build.gradle.kts` - Added test dependencies

## Files Created

Total: 11 new files

**Rust**:
1. `crates/superposition_core/src/toml_parser.rs` (567 lines)

**Python**:
2. `clients/python/bindings/test_toml_functions.py` (300+ lines)
3. `clients/python/bindings/README_TOML_TESTS.md`

**Kotlin/Java**:
4. `clients/java/bindings/src/test/kotlin/TomlFunctionsTest.kt` (250+ lines)
5. `clients/java/bindings/README_TOML_TESTS.md`

**JavaScript**:
6. `clients/javascript/bindings/index.js` (150+ lines)
7. `clients/javascript/bindings/test.js` (250+ lines)
8. `clients/javascript/bindings/example.js` (100+ lines)
9. `clients/javascript/bindings/package.json`
10. `clients/javascript/bindings/README_TOML_TESTS.md`
11. `clients/javascript/bindings/.gitignore`

## Next Steps

1. Run linting: `make check` ✓ (Already done, all passed)
2. Test Python bindings ✓ (All tests passed)
3. Test Java/Kotlin bindings (requires Gradle setup)
4. Test JavaScript bindings (requires Node.js 18/20)
5. Consider publishing bindings to package registries:
   - PyPI for Python
   - Maven Central for Java/Kotlin
   - npm for JavaScript

## Notes

- JavaScript bindings use C FFI (`ffi_legacy`) because uniffi doesn't support JavaScript
- Node.js v24+ has compatibility issues with ffi-napi; use Node.js 18 or 20 LTS
- All bindings follow the same test structure for consistency
- The TOML parser includes comprehensive error handling and validation
- Priority calculation uses bit-shift based on dimension position
- Override IDs are generated using BLAKE3 hashing
