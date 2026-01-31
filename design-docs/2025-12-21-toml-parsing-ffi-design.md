# TOML Parsing FFI Interface Design

**Date:** 2025-12-21
**Status:** Design Complete
**Author:** Claude Sonnet 4.5

## Overview

This design document outlines the implementation of TOML parsing functionality in the `superposition_core` crate with FFI (Foreign Function Interface) bindings. The feature enables external applications to parse TOML configuration files and resolve configurations through both traditional C FFI and uniffi interfaces.

## Background

The superposition system currently supports JSON-based configuration resolution through FFI interfaces. This enhancement adds TOML file format support, allowing users to define configurations, dimensions, and contexts in a more human-readable format while maintaining compatibility with existing resolution logic.

## Goals

1. Add TOML parsing capability to `superposition_core`
2. Provide both low-level (parse-only) and high-level (parse + evaluate) functions
3. Expose functionality through both existing FFI interfaces (C FFI and uniffi)
4. Maintain code quality, type safety, and memory safety standards
5. Provide detailed error messages for debugging

## Non-Goals

- Modifying existing configuration resolution logic
- Creating a new FFI interface (reuse existing ones)
- Supporting TOML file writing/generation

---

## Architecture

### Overall Design

The implementation consists of four main layers:

```
┌─────────────────────────────────────────────────────────┐
│  External Languages (C, Kotlin, Swift, etc.)           │
└─────────────────────────────────────────────────────────┘
                          ↓
┌─────────────────────────────────────────────────────────┐
│  FFI Layer (ffi.rs + ffi_legacy.rs)                    │
│  - core_parse_toml_config()                            │
│  - core_eval_toml_config()                             │
│  - ffi_parse_toml_config()                             │
│  - ffi_eval_toml_config()                              │
└─────────────────────────────────────────────────────────┘
                          ↓
┌─────────────────────────────────────────────────────────┐
│  Public API (lib.rs)                                   │
│  - parse_toml_config()                                 │
│  - eval_toml_config()                                  │
└─────────────────────────────────────────────────────────┘
                          ↓
┌─────────────────────────────────────────────────────────┐
│  Core Logic                                            │
│  - toml_parser module (new)                            │
│  - config::eval_config() (existing)                    │
└─────────────────────────────────────────────────────────┘
```

### Data Flow

**Low-level parsing:**
```
TOML String → toml_parser::parse() → ParsedTomlConfig {
    default_config: Map<String, Value>,
    contexts: Vec<Context>,
    overrides: HashMap<String, Overrides>,
    dimensions: HashMap<String, DimensionInfo>
}
```

**High-level evaluation:**
```
TOML String + Input Dimensions → parse() → eval_config() → Resolved Config
```

### Key Design Principles

- **Separation of concerns**: Parsing logic separate from evaluation logic
- **Reuse existing code**: Leverage `eval_config()` instead of duplicating
- **Consistent error handling**: Match existing FFI error patterns
- **Memory safety**: Proper C string lifecycle management
- **Type safety**: Use uniffi's automatic marshalling where possible

---

## Component Details

### 1. New Module: `toml_parser.rs`

**Location:** `crates/superposition_core/src/toml_parser.rs`

**Purpose:** Core TOML parsing logic, validation, and structure conversion.

#### Type Definitions

```rust
/// Parsed TOML configuration structure
pub struct ParsedTomlConfig {
    pub default_config: Map<String, Value>,
    pub contexts: Vec<Context>,
    pub overrides: HashMap<String, Overrides>,
    pub dimensions: HashMap<String, DimensionInfo>,
}

/// Detailed error type for TOML parsing
#[derive(Debug, Clone)]
pub enum TomlParseError {
    FileReadError(String),
    TomlSyntaxError(String),
    MissingSection(String),
    MissingField {
        section: String,
        key: String,
        field: String
    },
    InvalidContextExpression {
        expression: String,
        reason: String
    },
    UndeclaredDimension {
        dimension: String,
        context: String
    },
    InvalidOverrideKey {
        key: String,
        context: String
    },
    ConversionError(String),
}

impl Display for TomlParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::MissingSection(s) =>
                write!(f, "TOML parsing error: Missing required section '{}'", s),
            Self::MissingField { section, key, field } =>
                write!(f, "TOML parsing error: Missing field '{}' in section '{}' for key '{}'",
                    field, section, key),
            Self::InvalidContextExpression { expression, reason } =>
                write!(f, "TOML parsing error: Invalid context expression '{}': {}",
                    expression, reason),
            Self::UndeclaredDimension { dimension, context } =>
                write!(f, "TOML parsing error: Undeclared dimension '{}' used in context '{}'",
                    dimension, context),
            Self::InvalidOverrideKey { key, context } =>
                write!(f, "TOML parsing error: Override key '{}' not found in default-config (context: '{}')",
                    key, context),
            Self::TomlSyntaxError(e) =>
                write!(f, "TOML syntax error: {}", e),
            Self::ConversionError(e) =>
                write!(f, "TOML conversion error: {}", e),
            Self::FileReadError(e) =>
                write!(f, "File read error: {}", e),
        }
    }
}

impl std::error::Error for TomlParseError {}
```

#### Helper Functions

```rust
/// Convert TOML value to serde_json Value
fn toml_value_to_serde_value(toml_value: toml::Value) -> serde_json::Value {
    // Handle: String, Integer, Float, Boolean, Datetime, Array, Table
    // Special handling for NaN/Infinity in floats
}

/// Parse context expression string (e.g., "os=linux;region=us-east")
fn parse_context_expression(
    input: &str,
    dimensions: &HashMap<String, DimensionInfo>
) -> Result<Map<String, Value>, TomlParseError> {
    // Split by semicolons
    // Parse key=value pairs
    // Validate dimensions exist
    // Type conversion based on dimension schema
}

/// Hash a serde_json Value using BLAKE3
fn hash(val: &Value) -> String {
    let sorted = json_to_sorted_string(val);
    blake3::hash(sorted.as_bytes()).to_string()
}

/// Convert JSON to deterministic sorted string
fn json_to_sorted_string(v: &Value) -> String {
    // Ensure consistent hashing by sorting object keys
}

/// Compute priority based on dimension positions
fn compute_priority(
    context_map: &Map<String, Value>,
    dimensions: &HashMap<String, DimensionInfo>
) -> i32 {
    // Bit-shift calculation: sum of 2^position for each dimension
}
```

#### Main Parsing Function

```rust
pub fn parse(toml_content: &str) -> Result<ParsedTomlConfig, TomlParseError> {
    // 1. Parse TOML string
    let toml_table: toml::Table = toml::from_str(toml_content)
        .map_err(|e| TomlParseError::TomlSyntaxError(e.to_string()))?;

    // 2. Extract and validate "default-config" section
    let default_config = parse_default_config(&toml_table)?;

    // 3. Extract and validate "dimensions" section
    let dimensions = parse_dimensions(&toml_table)?;

    // 4. Extract and parse "context" section
    let (contexts, overrides) = parse_contexts(&toml_table, &default_config, &dimensions)?;

    Ok(ParsedTomlConfig {
        default_config,
        contexts,
        overrides,
        dimensions,
    })
}

fn parse_default_config(table: &toml::Table) -> Result<Map<String, Value>, TomlParseError> {
    let section = table.get("default-config")
        .ok_or(TomlParseError::MissingSection("default-config".into()))?
        .as_table()
        .ok_or(TomlParseError::ConversionError("default-config must be a table".into()))?;

    let mut result = Map::new();
    for (key, value) in section {
        let table = value.as_table()
            .ok_or(TomlParseError::ConversionError(
                format!("default-config.{} must be a table with 'value' and 'schema'", key)
            ))?;

        // Validate required fields
        if !table.contains_key("value") {
            return Err(TomlParseError::MissingField {
                section: "default-config".into(),
                key: key.clone(),
                field: "value".into(),
            });
        }
        if !table.contains_key("schema") {
            return Err(TomlParseError::MissingField {
                section: "default-config".into(),
                key: key.clone(),
                field: "schema".into(),
            });
        }

        let value = toml_value_to_serde_value(table["value"].clone());
        result.insert(key.clone(), value);
    }

    Ok(result)
}

fn parse_dimensions(table: &toml::Table) -> Result<HashMap<String, DimensionInfo>, TomlParseError> {
    let section = table.get("dimensions")
        .ok_or(TomlParseError::MissingSection("dimensions".into()))?
        .as_table()
        .ok_or(TomlParseError::ConversionError("dimensions must be a table".into()))?;

    let mut result = HashMap::new();
    let mut position = 1i32;

    for (key, value) in section {
        let table = value.as_table()
            .ok_or(TomlParseError::ConversionError(
                format!("dimensions.{} must be a table with 'schema'", key)
            ))?;

        if !table.contains_key("schema") {
            return Err(TomlParseError::MissingField {
                section: "dimensions".into(),
                key: key.clone(),
                field: "schema".into(),
            });
        }

        let schema = toml_value_to_serde_value(table["schema"].clone());
        let schema_map = ExtendedMap::try_from(schema)
            .map_err(|e| TomlParseError::ConversionError(format!("Invalid schema: {}", e)))?;

        let dimension_info = DimensionInfo {
            position,
            schema: schema_map,
            dimension_type: DimensionType::Regular {},
            dependency: Dependency::new(),
        };

        result.insert(key.clone(), dimension_info);
        position += 1;
    }

    Ok(result)
}

fn parse_contexts(
    table: &toml::Table,
    default_config: &Map<String, Value>,
    dimensions: &HashMap<String, DimensionInfo>
) -> Result<(Vec<Context>, HashMap<String, Overrides>), TomlParseError> {
    let section = table.get("context")
        .ok_or(TomlParseError::MissingSection("context".into()))?
        .as_table()
        .ok_or(TomlParseError::ConversionError("context must be a table".into()))?;

    let mut contexts = Vec::new();
    let mut overrides_map = HashMap::new();

    for (context_expr, override_values) in section {
        // Parse context expression
        let context_map = parse_context_expression(context_expr, dimensions)?;

        // Parse override values
        let override_table = override_values.as_table()
            .ok_or(TomlParseError::ConversionError(
                format!("context.{} must be a table", context_expr)
            ))?;

        let mut override_config = Map::new();
        for (key, value) in override_table {
            // Validate key exists in default_config
            if !default_config.contains_key(key) {
                return Err(TomlParseError::InvalidOverrideKey {
                    key: key.clone(),
                    context: context_expr.clone(),
                });
            }

            let serde_value = toml_value_to_serde_value(value.clone());
            override_config.insert(key.clone(), serde_value);
        }

        // Compute priority and hash
        let priority = compute_priority(&context_map, dimensions);
        let override_hash = hash(&serde_json::to_value(&override_config).unwrap());

        // Create Context
        let condition = Cac::<Condition>::try_from(context_map)
            .map_err(|e| TomlParseError::ConversionError(format!("Invalid condition: {}", e)))?;

        let context = Context {
            condition,
            id: override_hash.clone(),
            priority,
            override_with_keys: vec![],
            weight: 1,
        };

        // Create Overrides
        let overrides = Overrides {
            override_config,
        };

        contexts.push(context);
        overrides_map.insert(override_hash, overrides);
    }

    Ok((contexts, overrides_map))
}
```

---

### 2. Public API Functions (lib.rs)

**Location:** `crates/superposition_core/src/lib.rs`

```rust
mod toml_parser;

pub use toml_parser::{ParsedTomlConfig, TomlParseError};

/// Parse TOML configuration string into structured components
///
/// # Arguments
/// * `toml_content` - TOML string containing default-config, dimensions, and context sections
///
/// # Returns
/// * `Ok(ParsedTomlConfig)` - Successfully parsed configuration
/// * `Err(TomlParseError)` - Detailed error about what went wrong
///
/// # Example TOML Format
/// ```toml
/// [default-config]
/// timeout = { value = 30, schema = { type = "integer" } }
///
/// [dimensions]
/// os = { schema = { type = "string" } }
///
/// [context]
/// "os=linux" = { timeout = 60 }
/// ```
pub fn parse_toml_config(toml_content: &str) -> Result<ParsedTomlConfig, TomlParseError> {
    toml_parser::parse(toml_content)
}

/// Parse TOML configuration and evaluate with input dimensions
///
/// Combines parsing and evaluation in a single call for convenience.
///
/// # Arguments
/// * `toml_content` - TOML string with configuration
/// * `input_dimensions` - Map of dimension values for this evaluation
/// * `merge_strategy` - How to merge override values with defaults
///
/// # Returns
/// * `Ok(Map<String, Value>)` - Resolved configuration
/// * `Err(String)` - Error message
pub fn eval_toml_config(
    toml_content: &str,
    input_dimensions: &Map<String, Value>,
    merge_strategy: MergeStrategy,
) -> Result<Map<String, Value>, String> {
    let parsed = toml_parser::parse(toml_content)
        .map_err(|e| e.to_string())?;

    eval_config(
        &parsed.default_config,
        &parsed.contexts,
        &parsed.overrides,
        &parsed.dimensions,
        input_dimensions,
        merge_strategy,
    )
}
```

---

### 3. Traditional C FFI Interface (ffi.rs)

**Location:** `crates/superposition_core/src/ffi.rs`

```rust
/// Parse TOML configuration and return structured JSON
///
/// # Arguments
/// * `toml_content` - C string containing TOML configuration
/// * `ebuf` - Error buffer (2048 bytes) for error messages
///
/// # Returns
/// * Success: JSON string containing parsed structures
/// * Failure: NULL pointer, error written to ebuf
///
/// # JSON Output Format
/// ```json
/// {
///   "default_config": { "key": "value", ... },
///   "contexts": [ ... ],
///   "overrides": { "hash": { "key": "value" }, ... },
///   "dimensions": { "name": { "position": 1, "schema": {...} }, ... }
/// }
/// ```
///
/// # Memory Management
/// Caller must free the returned string using core_free_string()
#[no_mangle]
pub unsafe extern "C" fn core_parse_toml_config(
    toml_content: *const c_char,
    ebuf: *mut c_char,
) -> *mut c_char {
    let err_buffer = std::slice::from_raw_parts_mut(ebuf as *mut u8, 2048);

    // Null pointer check
    if toml_content.is_null() {
        write_error(err_buffer, "toml_content is null");
        return std::ptr::null_mut();
    }

    // Convert C string to Rust string
    let toml_str = match c_str_to_string(toml_content) {
        Ok(s) => s,
        Err(e) => {
            write_error(err_buffer, &format!("Invalid UTF-8 in toml_content: {}", e));
            return std::ptr::null_mut();
        }
    };

    // Parse TOML
    let parsed = match parse_toml_config(&toml_str) {
        Ok(p) => p,
        Err(e) => {
            write_error(err_buffer, &e.to_string());
            return std::ptr::null_mut();
        }
    };

    // Serialize to JSON
    let result = serde_json::json!({
        "default_config": parsed.default_config,
        "contexts": parsed.contexts,
        "overrides": parsed.overrides,
        "dimensions": parsed.dimensions,
    });

    let result_str = match serde_json::to_string(&result) {
        Ok(s) => s,
        Err(e) => {
            write_error(err_buffer, &format!("JSON serialization error: {}", e));
            return std::ptr::null_mut();
        }
    };

    // Convert to C string
    match CString::new(result_str) {
        Ok(c_str) => c_str.into_raw(),
        Err(e) => {
            write_error(err_buffer, &format!("CString conversion error: {}", e));
            std::ptr::null_mut()
        }
    }
}

/// Parse TOML configuration and evaluate with input dimensions
///
/// # Arguments
/// * `toml_content` - C string containing TOML configuration
/// * `input_dimensions_json` - C string with JSON object of dimension values
/// * `merge_strategy_json` - C string with merge strategy ("MERGE" or "REPLACE")
/// * `ebuf` - Error buffer (2048 bytes) for error messages
///
/// # Returns
/// * Success: JSON string with resolved configuration
/// * Failure: NULL pointer, error written to ebuf
///
/// # Example input_dimensions_json
/// ```json
/// { "os": "linux", "region": "us-east" }
/// ```
///
/// # Memory Management
/// Caller must free the returned string using core_free_string()
#[no_mangle]
pub unsafe extern "C" fn core_eval_toml_config(
    toml_content: *const c_char,
    input_dimensions_json: *const c_char,
    merge_strategy_json: *const c_char,
    ebuf: *mut c_char,
) -> *mut c_char {
    let err_buffer = std::slice::from_raw_parts_mut(ebuf as *mut u8, 2048);

    // Null pointer checks
    if toml_content.is_null() {
        write_error(err_buffer, "toml_content is null");
        return std::ptr::null_mut();
    }
    if input_dimensions_json.is_null() {
        write_error(err_buffer, "input_dimensions_json is null");
        return std::ptr::null_mut();
    }
    if merge_strategy_json.is_null() {
        write_error(err_buffer, "merge_strategy_json is null");
        return std::ptr::null_mut();
    }

    // Convert C strings
    let toml_str = match c_str_to_string(toml_content) {
        Ok(s) => s,
        Err(e) => {
            write_error(err_buffer, &format!("Invalid UTF-8 in toml_content: {}", e));
            return std::ptr::null_mut();
        }
    };

    // Parse input dimensions
    let input_dimensions: Map<String, Value> = match parse_json(input_dimensions_json) {
        Ok(v) => v,
        Err(e) => {
            write_error(err_buffer, &format!("Failed to parse input_dimensions_json: {}", e));
            return std::ptr::null_mut();
        }
    };

    // Parse merge strategy
    let merge_strategy: MergeStrategy = match parse_json(merge_strategy_json) {
        Ok(v) => v,
        Err(e) => {
            write_error(err_buffer, &format!("Failed to parse merge_strategy_json: {}", e));
            return std::ptr::null_mut();
        }
    };

    // Evaluate
    let result = match eval_toml_config(&toml_str, &input_dimensions, merge_strategy) {
        Ok(r) => r,
        Err(e) => {
            write_error(err_buffer, &e);
            return std::ptr::null_mut();
        }
    };

    // Serialize result
    let result_str = match serde_json::to_string(&result) {
        Ok(s) => s,
        Err(e) => {
            write_error(err_buffer, &format!("JSON serialization error: {}", e));
            return std::ptr::null_mut();
        }
    };

    // Convert to C string
    match CString::new(result_str) {
        Ok(c_str) => c_str.into_raw(),
        Err(e) => {
            write_error(err_buffer, &format!("CString conversion error: {}", e));
            std::ptr::null_mut()
        }
    }
}

// Helper function
fn write_error(buffer: &mut [u8], message: &str) {
    let bytes = message.as_bytes();
    let len = std::cmp::min(bytes.len(), buffer.len() - 1);
    buffer[..len].copy_from_slice(&bytes[..len]);
    buffer[len] = 0; // Null terminator
}
```

---

### 4. uniffi Interface (ffi_legacy.rs)

**Location:** `crates/superposition_core/src/ffi_legacy.rs`

```rust
/// Parsed TOML configuration result for FFI
///
/// Note: Complex structures are JSON-encoded as strings for uniffi compatibility
#[derive(uniffi::Record)]
pub struct ParsedTomlResult {
    /// Default configuration as a map of key -> JSON-encoded value
    pub default_config: HashMap<String, String>,
    /// Contexts array as JSON string
    pub contexts_json: String,
    /// Overrides map as JSON string
    pub overrides_json: String,
    /// Dimensions map as JSON string
    pub dimensions_json: String,
}

/// Parse TOML configuration string
///
/// # Arguments
/// * `toml_content` - TOML string with configuration
///
/// # Returns
/// * `Ok(ParsedTomlResult)` - Parsed configuration components
/// * `Err(OperationError)` - Detailed error message
///
/// # Example TOML
/// ```toml
/// [default-config]
/// timeout = { value = 30, schema = { type = "integer" } }
///
/// [dimensions]
/// os = { schema = { type = "string" } }
///
/// [context]
/// "os=linux" = { timeout = 60 }
/// ```
#[uniffi::export]
fn ffi_parse_toml_config(
    toml_content: String,
) -> Result<ParsedTomlResult, OperationError> {
    // Parse TOML
    let parsed = parse_toml_config(&toml_content)
        .map_err(|e| OperationError {
            message: e.to_string(),
        })?;

    // Convert default_config to HashMap<String, String> (JSON-encoded values)
    let default_config: HashMap<String, String> = parsed.default_config
        .into_iter()
        .map(|(k, v)| {
            let json_str = serde_json::to_string(&v).unwrap_or_else(|_| "null".to_string());
            (k, json_str)
        })
        .collect();

    // Serialize complex structures to JSON
    let contexts_json = serde_json::to_string(&parsed.contexts)
        .map_err(|e| OperationError {
            message: format!("Failed to serialize contexts: {}", e),
        })?;

    let overrides_json = serde_json::to_string(&parsed.overrides)
        .map_err(|e| OperationError {
            message: format!("Failed to serialize overrides: {}", e),
        })?;

    let dimensions_json = serde_json::to_string(&parsed.dimensions)
        .map_err(|e| OperationError {
            message: format!("Failed to serialize dimensions: {}", e),
        })?;

    Ok(ParsedTomlResult {
        default_config,
        contexts_json,
        overrides_json,
        dimensions_json,
    })
}

/// Parse TOML and evaluate configuration with input dimensions
///
/// # Arguments
/// * `toml_content` - TOML string with configuration
/// * `input_dimensions` - Map of dimension values
/// * `merge_strategy` - "MERGE" or "REPLACE"
///
/// # Returns
/// * `Ok(HashMap)` - Resolved configuration
/// * `Err(OperationError)` - Error message
#[uniffi::export]
fn ffi_eval_toml_config(
    toml_content: String,
    input_dimensions: HashMap<String, String>,
    merge_strategy: String,
) -> Result<HashMap<String, String>, OperationError> {
    // Convert input_dimensions from HashMap<String, String> to Map<String, Value>
    let dimensions_map: Map<String, Value> = input_dimensions
        .into_iter()
        .map(|(k, v)| {
            // Try to parse as JSON, fall back to string
            let value = serde_json::from_str(&v).unwrap_or_else(|_| Value::String(v));
            (k, value)
        })
        .collect();

    // Parse merge strategy
    let strategy: MergeStrategy = serde_json::from_str(&format!("\"{}\"", merge_strategy))
        .map_err(|e| OperationError {
            message: format!("Invalid merge strategy: {}", e),
        })?;

    // Evaluate
    let result = eval_toml_config(&toml_content, &dimensions_map, strategy)
        .map_err(|e| OperationError { message: e })?;

    // Convert result to HashMap<String, String>
    let result_map: HashMap<String, String> = result
        .into_iter()
        .map(|(k, v)| {
            let json_str = serde_json::to_string(&v).unwrap_or_else(|_| "null".to_string());
            (k, json_str)
        })
        .collect();

    Ok(result_map)
}
```

---

## Expected TOML Format

### Required Sections

#### 1. default-config
Defines the base configuration with schemas.

```toml
[default-config]
timeout = { value = 30, schema = { type = "integer", minimum = 0 } }
enabled = { value = true, schema = { type = "boolean" } }
api_endpoint = { value = "https://api.example.com", schema = { type = "string", pattern = "^https://" } }
```

**Requirements:**
- Each key MUST have both `value` and `schema` fields
- `value` can be any TOML type
- `schema` follows JSON Schema conventions

#### 2. dimensions
Defines available dimensions for context targeting.

```toml
[dimensions]
os = { schema = { type = "string", enum = ["linux", "windows", "macos"] } }
region = { schema = { type = "string" } }
version = { schema = { type = "string", pattern = "^\\d+\\.\\d+\\.\\d+$" } }
```

**Requirements:**
- Each dimension MUST have a `schema` field
- Schema validates values in context expressions
- Position is auto-assigned (1, 2, 3, ...)

#### 3. context
Defines context-based overrides.

```toml
[context]
"os=linux" = { timeout = 60 }
"os=linux;region=us-east" = { timeout = 90, enabled = false }
"os=windows;version=1.0.0" = { api_endpoint = "https://legacy.example.com" }
```

**Requirements:**
- Keys are context expressions: `"dim1=val1;dim2=val2"`
- Values override keys from `default-config`
- All dimensions in expressions must be declared in `[dimensions]`
- All override keys must exist in `[default-config]`

---

## Error Handling

### Error Categories

| Error Type | Description | Example |
|------------|-------------|---------|
| `TomlSyntaxError` | Invalid TOML syntax | `Unexpected character at line 5` |
| `MissingSection` | Required section missing | `Missing required section 'dimensions'` |
| `MissingField` | Required field in entry | `Missing field 'schema' in default-config for key 'timeout'` |
| `InvalidContextExpression` | Malformed context string | `Invalid context expression 'os=': Empty value after equals` |
| `UndeclaredDimension` | Dimension not in `[dimensions]` | `Undeclared dimension 'country' used in context 'country=US'` |
| `InvalidOverrideKey` | Override key not in `[default-config]` | `Override key 'port' not found in default-config` |
| `ConversionError` | Type conversion failure | `Cannot convert NaN to JSON number` |

### FFI Error Propagation

**C FFI (ffi.rs):**
- Error written to `ebuf` parameter (2048-byte buffer)
- Function returns NULL pointer
- Caller checks return value and reads `ebuf` on failure

**uniffi (ffi_legacy.rs):**
- Returns `Err(OperationError { message })`
- Target language receives native exception/error
- Error message contains full detail

---

## Dependencies

### New Dependencies

Add to `crates/superposition_core/Cargo.toml`:

```toml
[dependencies]
# Existing dependencies...
toml = "0.8"           # TOML parsing
blake3 = "1.5"         # Cryptographic hashing
itertools = "0.12"     # Sorted iteration
```

All other required types are already available through existing dependencies:
- `serde` / `serde_json` - JSON serialization
- `uniffi` - FFI bindings
- `superposition_types` - Core types (Context, DimensionInfo, etc.)

---

## Testing Strategy

### Unit Tests (toml_parser.rs)

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_valid_toml_parsing() {
        let toml = r#"
            [default-config]
            timeout = { value = 30, schema = { type = "integer" } }

            [dimensions]
            os = { schema = { type = "string" } }

            [context]
            "os=linux" = { timeout = 60 }
        "#;

        let result = parse(toml);
        assert!(result.is_ok());
        let parsed = result.unwrap();
        assert_eq!(parsed.default_config.len(), 1);
        assert_eq!(parsed.dimensions.len(), 1);
        assert_eq!(parsed.contexts.len(), 1);
    }

    #[test]
    fn test_missing_section_error() {
        let toml = r#"
            [default-config]
            timeout = { value = 30, schema = { type = "integer" } }
        "#;

        let result = parse(toml);
        assert!(matches!(result, Err(TomlParseError::MissingSection(_))));
    }

    #[test]
    fn test_missing_value_field() {
        let toml = r#"
            [default-config]
            timeout = { schema = { type = "integer" } }

            [dimensions]
            os = { schema = { type = "string" } }

            [context]
        "#;

        let result = parse(toml);
        assert!(matches!(result, Err(TomlParseError::MissingField { .. })));
    }

    #[test]
    fn test_undeclared_dimension() {
        let toml = r#"
            [default-config]
            timeout = { value = 30, schema = { type = "integer" } }

            [dimensions]
            os = { schema = { type = "string" } }

            [context]
            "region=us-east" = { timeout = 60 }
        "#;

        let result = parse(toml);
        assert!(matches!(result, Err(TomlParseError::UndeclaredDimension { .. })));
    }

    #[test]
    fn test_invalid_override_key() {
        let toml = r#"
            [default-config]
            timeout = { value = 30, schema = { type = "integer" } }

            [dimensions]
            os = { schema = { type = "string" } }

            [context]
            "os=linux" = { port = 8080 }
        "#;

        let result = parse(toml);
        assert!(matches!(result, Err(TomlParseError::InvalidOverrideKey { .. })));
    }

    #[test]
    fn test_type_conversions() {
        // Test all TOML types: String, Integer, Float, Boolean, Datetime, Array, Table
        // Verify correct conversion to serde_json::Value
    }

    #[test]
    fn test_priority_calculation() {
        // Verify bit-shift priority: os(pos=1) + region(pos=2) = 2^1 + 2^2 = 6
    }

    #[test]
    fn test_hash_consistency() {
        let val1 = json!({"a": 1, "b": 2});
        let val2 = json!({"b": 2, "a": 1});
        assert_eq!(hash(&val1), hash(&val2));
    }
}
```

### Integration Tests (tests/toml_integration_tests.rs)

```rust
#[test]
fn test_ffi_parse_toml_config() {
    let toml = CString::new(VALID_TOML).unwrap();
    let mut ebuf = vec![0u8; 2048];

    unsafe {
        let result = core_parse_toml_config(toml.as_ptr(), ebuf.as_mut_ptr() as *mut c_char);
        assert!(!result.is_null());

        let result_str = CStr::from_ptr(result).to_str().unwrap();
        let parsed: serde_json::Value = serde_json::from_str(result_str).unwrap();

        assert!(parsed["default_config"].is_object());
        assert!(parsed["contexts"].is_array());

        core_free_string(result);
    }
}

#[test]
fn test_ffi_eval_toml_config() {
    let toml = CString::new(VALID_TOML).unwrap();
    let dimensions = CString::new(r#"{"os": "linux"}"#).unwrap();
    let strategy = CString::new(r#""MERGE""#).unwrap();
    let mut ebuf = vec![0u8; 2048];

    unsafe {
        let result = core_eval_toml_config(
            toml.as_ptr(),
            dimensions.as_ptr(),
            strategy.as_ptr(),
            ebuf.as_mut_ptr() as *mut c_char,
        );

        assert!(!result.is_null());

        let result_str = CStr::from_ptr(result).to_str().unwrap();
        let config: HashMap<String, Value> = serde_json::from_str(result_str).unwrap();

        // Verify linux override was applied
        assert_eq!(config["timeout"].as_i64().unwrap(), 60);

        core_free_string(result);
    }
}

#[test]
fn test_uniffi_parse_toml() {
    let result = ffi_parse_toml_config(VALID_TOML.to_string());
    assert!(result.is_ok());

    let parsed = result.unwrap();
    assert!(!parsed.default_config.is_empty());
    assert!(!parsed.contexts_json.is_empty());
}

#[test]
fn test_uniffi_eval_toml() {
    let mut dims = HashMap::new();
    dims.insert("os".to_string(), "\"linux\"".to_string());

    let result = ffi_eval_toml_config(
        VALID_TOML.to_string(),
        dims,
        "MERGE".to_string(),
    );

    assert!(result.is_ok());
    let config = result.unwrap();
    assert!(config.contains_key("timeout"));
}

#[test]
fn test_end_to_end_resolution() {
    // Complete workflow: TOML → parse → eval → verify
}

#[test]
fn test_memory_safety() {
    // Valgrind/ASAN test: allocate/free many times
}
```

---

## Implementation Steps

### Phase 1: Core Parsing Module
1. Create `src/toml_parser.rs`
2. Define error types
3. Implement helper functions:
   - `toml_value_to_serde_value()`
   - `hash()`
   - `compute_priority()`
   - `parse_context_expression()`
4. Implement main `parse()` function
5. Add comprehensive unit tests
6. **Validation:** All unit tests pass

### Phase 2: Public API
1. Update `src/lib.rs`:
   - Export `toml_parser` module
   - Add `parse_toml_config()` function
   - Add `eval_toml_config()` function
2. Add integration tests in `tests/`
3. **Validation:** Integration tests pass

### Phase 3: C FFI Interface
1. Update `src/ffi.rs`:
   - Add `core_parse_toml_config()`
   - Add `core_eval_toml_config()`
2. Add FFI-specific tests
3. Regenerate C headers with cbindgen
4. Test from C client
5. **Validation:** C FFI tests pass, no memory leaks

### Phase 4: uniffi Interface
1. Update `src/ffi_legacy.rs`:
   - Define `ParsedTomlResult` record
   - Add `ffi_parse_toml_config()`
   - Add `ffi_eval_toml_config()`
2. Update `uniffi.toml` if needed
3. Regenerate uniffi bindings
4. Test from Kotlin/Swift if applicable
5. **Validation:** uniffi tests pass

### Phase 5: Build & Documentation
1. Update `Cargo.toml` with new dependencies
2. Update `CHANGELOG.md`
3. Add rustdoc comments to all public items
4. Add usage examples in module docs
5. Update README if needed
6. **Validation:** `cargo doc` succeeds, examples compile

### Phase 6: Final Validation
1. Run full test suite: `cargo test`
2. Run with sanitizers: `cargo test --target x86_64-unknown-linux-gnu` (with ASAN)
3. Verify C header generation: `cbindgen --config cbindgen.toml`
4. Verify uniffi bindings: `cargo run --bin uniffi-bindgen`
5. Performance smoke test (large TOML files)
6. **Validation:** All tests pass, no warnings, bindings generate correctly

---

## File Changes Summary

### New Files
- `crates/superposition_core/src/toml_parser.rs` (~500 lines)
- `crates/superposition_core/tests/toml_integration_tests.rs` (~300 lines)

### Modified Files
- `crates/superposition_core/src/lib.rs` (+30 lines)
  - Export toml_parser module
  - Add 2 public functions with docs
- `crates/superposition_core/src/ffi.rs` (+150 lines)
  - Add `core_parse_toml_config()`
  - Add `core_eval_toml_config()`
- `crates/superposition_core/src/ffi_legacy.rs` (+100 lines)
  - Add `ParsedTomlResult` record
  - Add `ffi_parse_toml_config()`
  - Add `ffi_eval_toml_config()`
- `crates/superposition_core/Cargo.toml` (+3 lines)
  - Add toml, blake3, itertools dependencies
- `crates/superposition_core/CHANGELOG.md` (+10 lines)
  - Document new feature

### Auto-Generated Files (Updated)
- C header file (via cbindgen)
- uniffi language bindings (Kotlin, Swift, etc.)

---

## Usage Examples

### Rust API

```rust
use superposition_core::{parse_toml_config, eval_toml_config, MergeStrategy};
use serde_json::{Map, Value};

// Low-level parsing
let toml_content = r#"
    [default-config]
    timeout = { value = 30, schema = { type = "integer" } }

    [dimensions]
    os = { schema = { type = "string" } }

    [context]
    "os=linux" = { timeout = 60 }
"#;

let parsed = parse_toml_config(toml_content)?;
println!("Contexts: {}", parsed.contexts.len());

// High-level evaluation
let mut input_dims = Map::new();
input_dims.insert("os".to_string(), Value::String("linux".to_string()));

let config = eval_toml_config(toml_content, &input_dims, MergeStrategy::MERGE)?;
println!("Resolved timeout: {}", config["timeout"]);
```

### C FFI

```c
#include "superposition_core.h"

char toml_content[] = "...";
char dimensions[] = "{\"os\": \"linux\"}";
char strategy[] = "\"MERGE\"";
char error_buf[2048] = {0};

char* result = core_eval_toml_config(toml_content, dimensions, strategy, error_buf);
if (result == NULL) {
    printf("Error: %s\n", error_buf);
} else {
    printf("Config: %s\n", result);
    core_free_string(result);
}
```

### Kotlin (uniffi)

```kotlin
val tomlContent = """
    [default-config]
    timeout = { value = 30, schema = { type = "integer" } }

    [dimensions]
    os = { schema = { type = "string" } }

    [context]
    "os=linux" = { timeout = 60 }
"""

val dimensions = mapOf("os" to "\"linux\"")
val config = ffiEvalTomlConfig(tomlContent, dimensions, "MERGE")
println("Resolved config: $config")
```

---

## Security Considerations

1. **Input Validation**
   - All TOML inputs are validated before processing
   - Schema validation prevents type confusion
   - Dimension validation prevents undefined references

2. **Memory Safety**
   - C FFI uses proper string lifecycle management
   - No buffer overflows (fixed-size error buffers)
   - All C strings properly null-terminated

3. **Error Information**
   - Detailed errors aid debugging but don't leak sensitive data
   - TOML syntax errors don't expose file paths

4. **DoS Prevention**
   - No recursion limits needed (TOML is flat)
   - Consider adding size limits for production use
   - Hash collisions handled by BLAKE3 cryptographic strength

---

## Future Enhancements

Potential future improvements (not in current scope):

1. **TOML File Watching**
   - Hot-reload configuration on file changes
   - Requires file system watcher integration

2. **TOML Generation**
   - Reverse operation: export current config to TOML
   - Useful for debugging/backup

3. **Validation Enhancements**
   - Schema validation during parsing (not just storage)
   - Cross-field validation rules

4. **Performance Optimizations**
   - Lazy parsing for large files
   - Cached parsing results

5. **Additional FFI Languages**
   - Python bindings via uniffi
   - Go bindings via cgo

---

## Success Criteria

The implementation is complete when:

1. ✅ All unit tests pass (toml_parser module)
2. ✅ All integration tests pass (FFI interfaces)
3. ✅ C header generates without errors
4. ✅ uniffi bindings generate for all target languages
5. ✅ No memory leaks detected (valgrind/ASAN)
6. ✅ Documentation builds without warnings
7. ✅ Example usage compiles and runs correctly
8. ✅ CHANGELOG updated
9. ✅ Code review completed
10. ✅ Backward compatibility maintained (existing APIs unchanged)

---

## References

- **TOML Specification:** https://toml.io/
- **uniffi Documentation:** https://mozilla.github.io/uniffi-rs/
- **BLAKE3 Hashing:** https://github.com/BLAKE3-team/BLAKE3
- **Reference Implementation:** https://github.com/juspay/superposition/tree/cac-toml/crates/superposition_toml

---

## Implementation Updates (2026-01-02)

This section documents significant updates and refinements made to the TOML parsing implementation after the initial design.

### 1. Mandatory Position Field for Dimensions

**Date:** 2026-01-02
**Status:** Implemented

#### Background
The initial implementation auto-assigned dimension positions sequentially (1, 2, 3...) based on their order in the TOML file. This approach was fragile and could lead to unintended priority changes if dimension order changed.

#### Changes Made

**Modified:** `crates/superposition_core/src/toml_parser.rs`

1. **Parsing Logic Update:**
   - Removed auto-assignment of positions
   - Added mandatory `position` field validation in `parse_dimensions()`
   - Returns `TomlParseError::MissingField` if position is absent

```rust
fn parse_dimensions(table: &toml::Table) -> Result<HashMap<String, DimensionInfo>, TomlParseError> {
    // ...
    for (key, value) in section {
        let table = value.as_table()?;

        // Require explicit position field
        if !table.contains_key("position") {
            return Err(TomlParseError::MissingField {
                section: "dimensions".into(),
                key: key.clone(),
                field: "position".into(),
            });
        }

        let position = table["position"].as_integer()? as i32;
        // ...
    }
}
```

2. **Updated TOML Format:**

```toml
[dimensions]
city = { position = 1, schema = { "type" = "string", "enum" = ["Bangalore", "Delhi"] } }
vehicle_type = { position = 2, schema = { "type" = "string", "enum" = ["auto", "cab", "bike"] } }
hour_of_day = { position = 3, schema = { "type" = "integer", "minimum" = 0, "maximum" = 23 }}
```

#### Benefits
- **Explicit Control:** Users explicitly define dimension priority
- **Stability:** Position doesn't change due to file reorganization
- **Clarity:** Intent is clear in the TOML file
- **Validation:** Parser enforces position presence

---

### 2. Duplicate Position Validation

**Date:** 2026-01-02
**Status:** Implemented

#### Background
Without duplicate position detection, multiple dimensions could have the same position value, leading to unpredictable priority calculations and context resolution behavior.

#### Changes Made

**Modified:** `crates/superposition_core/src/toml_parser.rs`

1. **New Error Variant:**

```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TomlParseError {
    // ... existing variants
    DuplicatePosition {
        position: i32,
        dimensions: Vec<String>,
    },
}

impl Display for TomlParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::DuplicatePosition { position, dimensions } => {
                write!(
                    f,
                    "TOML parsing error: Duplicate position {} found for dimensions: {}",
                    position,
                    dimensions.join(", ")
                )
            }
            // ... other variants
        }
    }
}
```

2. **Validation Logic:**

```rust
fn parse_dimensions(table: &toml::Table) -> Result<HashMap<String, DimensionInfo>, TomlParseError> {
    let mut position_to_dimensions: HashMap<i32, Vec<String>> = HashMap::new();

    for (key, value) in section {
        // ... validate and extract position
        let position = table["position"].as_integer()? as i32;

        // Track dimensions by position
        position_to_dimensions
            .entry(position)
            .or_insert_with(Vec::new)
            .push(key.clone());
    }

    // Check for duplicates
    for (position, dimensions) in position_to_dimensions {
        if dimensions.len() > 1 {
            return Err(TomlParseError::DuplicatePosition {
                position,
                dimensions,
            });
        }
    }

    // ... continue parsing
}
```

3. **Test Coverage:**

```rust
#[test]
fn test_duplicate_position_error() {
    let toml = r#"
        [default-config]
        key1 = { value = 10, schema = { type = "integer" } }

        [dimensions]
        city = { position = 1, schema = { "type" = "string" } }
        region = { position = 1, schema = { "type" = "string" } }

        [context]
    "#;

    let result = toml_parser::parse(toml);
    assert!(matches!(result, Err(TomlParseError::DuplicatePosition { .. })));
}
```

#### Benefits
- **Data Integrity:** Prevents ambiguous priority calculations
- **Early Detection:** Fails fast with clear error message
- **Debugging Aid:** Lists all conflicting dimensions

---

### 3. Haskell FFI Bindings

**Date:** 2026-01-02
**Status:** Implemented

#### Background
The project had bindings for Python, JavaScript/TypeScript, and Java/Kotlin, but lacked Haskell support despite having a Haskell bindings directory structure.

#### Changes Made

**New Files:**
- Test file: `clients/haskell/superposition-bindings/test/Main.hs`

**Modified Files:**
- `clients/haskell/superposition-bindings/lib/FFI/Superposition.hs`
- `clients/haskell/superposition-bindings/superposition-bindings.cabal`

1. **FFI Function Bindings (`FFI/Superposition.hs`):**

```haskell
foreign import capi "superposition_core.h core_parse_toml_config"
  parse_toml_config ::
    CString ->  -- toml_content
    CString ->  -- error-buffer
    IO CString  -- parsed config json

parseTomlConfig :: String -> IO (Either String String)
parseTomlConfig tomlContent = do
  ebuf <- callocBytes 2048
  tomlStr <- newCString tomlContent
  res <- parse_toml_config tomlStr ebuf
  err <- peekCAString ebuf
  let peekMaybe p | p /= nullPtr = Just <$> peekCAString p
                  | otherwise = pure Nothing
  result <- peekMaybe res
  free tomlStr
  free ebuf
  pure $ case (result, err) of
    (Just cfg, []) -> Right cfg
    (Nothing, []) -> Left "null pointer returned"
    _ -> Left err
```

2. **Test Suite (`test/Main.hs`):**

```haskell
main :: IO HUnit.Counts
main = do
  HUnit.runTestTT $
    HUnit.TestList
      [ HUnit.TestLabel "Valid Call" $ HUnit.TestCase validCall,
        HUnit.TestLabel "In-Valid Call" $ HUnit.TestCase invalidCall,
        HUnit.TestLabel "Parse TOML - Valid" $ HUnit.TestCase parseTomlValid,
        HUnit.TestLabel "Parse TOML - Invalid Syntax" $ HUnit.TestCase parseTomlInvalidSyntax,
        HUnit.TestLabel "Parse TOML - Missing Section" $ HUnit.TestCase parseTomlMissingSection,
        HUnit.TestLabel "Parse TOML - Missing Position" $ HUnit.TestCase parseTomlMissingPosition
      ]
```

3. **Build Configuration (`superposition-bindings.cabal`):**

```cabal
library
    exposed-modules:  FFI.Superposition
    build-depends:    base ^>=4.18.2.0
    default-extensions: CApiFFI
    extra-libraries: superposition_core
    include-dirs: ../../../target/include

test-suite superposition-bindings-test
    type:             exitcode-stdio-1.0
    main-is:          Main.hs
    build-depends:
        base ^>=4.18.2.0,
        HUnit,
        async,
        aeson,
        bytestring,
        superposition-bindings
```

#### Integration
- **Header File:** Uses uniffi-generated header from `target/include/superposition_core.h`
- **Library Path:** Requires `LIBRARY_PATH`, `LD_LIBRARY_PATH`, and `DYLD_LIBRARY_PATH` environment variables
- **Testing:** Integrated into `make bindings-test` target

#### Benefits
- **Complete Coverage:** All major language bindings now supported
- **Type Safety:** Haskell's strong type system provides additional safety
- **Functional Interface:** Natural fit for functional programming patterns

---

### 4. Platform-Specific Library Naming for Python Bindings

**Date:** 2026-01-02
**Status:** Implemented

#### Background
Initial implementation used simple library names (`libsuperposition_core.dylib`) for local testing, but CI packaging requires platform-specific names (`libsuperposition_core-aarch64-apple-darwin.dylib`). This mismatch created inconsistencies between local development and production packaging.

#### Changes Made

**Modified Files:**
- `uniffi/patches/python.patch`
- `Makefile` (bindings-test target)
- `.gitignore`

1. **Python Patch (`uniffi/patches/python.patch`):**

```python
def _uniffi_load_indirect():
    """
    Load the correct prebuilt dynamic library based on the current platform and architecture.
    """
    folder = os.path.dirname(__file__)

    triple_map = {
        ("darwin", "arm64"): "aarch64-apple-darwin.dylib",
        ("darwin", "x86_64"): "x86_64-apple-darwin.dylib",
        ("linux", "x86_64"): "x86_64-unknown-linux-gnu.so",
        ("win32", "x86_64"): "x86_64-pc-windows-msvc.dll",
    }

    triple = triple_map.get((sys.platform, platform.machine()))
    if not triple:
        raise RuntimeError(f"❌ Unsupported platform: {sys.platform} / {platform.machine()}")

    libname = f"libsuperposition_core-{triple}"
    libpath = os.path.join(folder, libname)
    if not os.path.exists(libpath):
        raise FileNotFoundError(f"❌ Required binary not found: {libpath}")

    return ctypes.cdll.LoadLibrary(libpath)
```

**Key Features:**
- Platform detection using `sys.platform` and `platform.machine()`
- Maps to rust target triple naming convention
- Clear error messages with platform information
- Validates library existence before loading

2. **Makefile Library Copy (`Makefile:413-424`):**

```makefile
@# Copy library to bindings directory for Python tests with platform-specific name
@if [ "$$(uname)" = "Darwin" ]; then \
    if [ "$$(uname -m)" = "arm64" ]; then \
        cp $(CARGO_TARGET_DIR)/release/libsuperposition_core.dylib clients/python/bindings/superposition_bindings/libsuperposition_core-aarch64-apple-darwin.dylib; \
    else \
        cp $(CARGO_TARGET_DIR)/release/libsuperposition_core.dylib clients/python/bindings/superposition_bindings/libsuperposition_core-x86_64-apple-darwin.dylib; \
    fi \
elif [ "$$(uname)" = "Linux" ]; then \
    cp $(CARGO_TARGET_DIR)/release/libsuperposition_core.so clients/python/bindings/superposition_bindings/libsuperposition_core-x86_64-unknown-linux-gnu.so; \
else \
    cp $(CARGO_TARGET_DIR)/release/superposition_core.dll clients/python/bindings/superposition_bindings/libsuperposition_core-x86_64-pc-windows-msvc.dll; \
fi
```

**Key Features:**
- OS detection with `uname`
- Architecture detection with `uname -m`
- Copies from simple name to platform-specific name
- Handles macOS (arm64/x86_64), Linux, and Windows

3. **Gitignore Update (`.gitignore`):**

```gitignore
# Dynamic libraries copied for testing
*.dylib
*.so
*.dll
```

#### Workflow

**Local Development:**
```bash
make bindings-test
# 1. Runs uniffi-bindings → generates Python bindings and applies patch
# 2. Copies library with platform-specific name
# 3. Python bindings load the platform-specific library
# 4. All tests pass ✅
```

**CI/Packaging:**
- Libraries packaged with platform-specific names (existing behavior)
- Python bindings (with patch applied) look for platform-specific names
- Works seamlessly in production ✅

#### Benefits
- **Consistency:** Local dev and CI use identical naming convention
- **Automation:** `make uniffi-bindings` applies patches automatically
- **Cross-Platform:** Handles macOS (both architectures), Linux, and Windows
- **No Manual Intervention:** Build system manages library placement

---

### 5. Unified Bindings Test Target

**Date:** 2026-01-02
**Status:** Implemented

#### Background
Testing bindings across multiple languages (Python, JavaScript, Java/Kotlin, Haskell) required running separate commands with complex environment setup. A unified test target was needed for CI integration.

#### Changes Made

**Modified:** `Makefile`

1. **New Target (`Makefile:407-446`):**

```makefile
# Target to run all TOML bindings tests
bindings-test: uniffi-bindings
	@echo ""
	@echo ""
	@echo "========================================"
	@echo "Running Python TOML binding tests"
	@echo "========================================"
	@# Copy library to bindings directory for Python tests with platform-specific name
	@if [ "$$(uname)" = "Darwin" ]; then \
		if [ "$$(uname -m)" = "arm64" ]; then \
			cp $(CARGO_TARGET_DIR)/release/libsuperposition_core.dylib clients/python/bindings/superposition_bindings/libsuperposition_core-aarch64-apple-darwin.dylib; \
		else \
			cp $(CARGO_TARGET_DIR)/release/libsuperposition_core.dylib clients/python/bindings/superposition_bindings/libsuperposition_core-x86_64-apple-darwin.dylib; \
		fi \
	elif [ "$$(uname)" = "Linux" ]; then \
		cp $(CARGO_TARGET_DIR)/release/libsuperposition_core.so clients/python/bindings/superposition_bindings/libsuperposition_core-x86_64-unknown-linux-gnu.so; \
	else \
		cp $(CARGO_TARGET_DIR)/release/superposition_core.dll clients/python/bindings/superposition_bindings/libsuperposition_core-x86_64-pc-windows-msvc.dll; \
	fi
	cd clients/python/bindings && python3 test_toml_functions.py
	@echo ""
	@echo "========================================"
	@echo "Running JavaScript/TypeScript TOML binding tests"
	@echo "========================================"
	cd clients/javascript/bindings && npm run build && node dist/test-toml.js
	@echo ""
	@echo "========================================"
	@echo "Running Java/Kotlin TOML binding tests"
	@echo "========================================"
	cd clients/java/bindings && SUPERPOSITION_LIB_PATH=$(CARGO_TARGET_DIR)/release gradle test
	@echo ""
	@echo "========================================"
	@echo "Running Haskell TOML binding tests"
	@echo "========================================"
	cd clients/haskell/superposition-bindings && \
		export LIBRARY_PATH=$(CARGO_TARGET_DIR)/release:$$LIBRARY_PATH && \
		export LD_LIBRARY_PATH=$(CARGO_TARGET_DIR)/release:$$LD_LIBRARY_PATH && \
		export DYLD_LIBRARY_PATH=$(CARGO_TARGET_DIR)/release:$$DYLD_LIBRARY_PATH && \
		echo "packages: ." > cabal.project.local && \
		cabal test --project-file=cabal.project.local && \
		rm -f cabal.project.local
	@echo ""
	@echo "========================================"
	@echo "All TOML binding tests passed!"
	@echo "========================================"
```

2. **Dependency:** Depends on `uniffi-bindings` target to ensure bindings are regenerated before testing

3. **Environment Variables:**
   - `SUPERPOSITION_LIB_PATH`: For Java/Kotlin tests (passed to Gradle)
   - `LIBRARY_PATH`, `LD_LIBRARY_PATH`, `DYLD_LIBRARY_PATH`: For Haskell tests

#### Usage

```bash
# Run all binding tests
make bindings-test

# Output includes:
# - Rust library build
# - uniffi binding generation
# - Python tests
# - JavaScript/TypeScript tests
# - Java/Kotlin tests
# - Haskell tests
# - Summary message
```

#### Benefits
- **Single Command:** One command runs all binding tests
- **CI Ready:** Suitable for GitHub Actions / CI pipelines
- **Environment Management:** Handles library paths automatically
- **Clear Output:** Sectioned output with progress indicators
- **Dependency Management:** Ensures bindings are current before testing

---

### 6. Removed Evaluated TOML Function

**Date:** 2026-01-02
**Status:** Removed

#### Background
The initial implementation included `eval_toml_config()` function combining TOML parsing and configuration evaluation in a single call. This was removed to maintain separation of concerns.

#### Changes Made

**Modified:** `crates/superposition_core/src/lib.rs`

**Removed Function:**
```rust
// REMOVED: This combined parse + eval in one function
pub fn eval_toml_config(
    toml_content: &str,
    input_dimensions: &Map<String, Value>,
    merge_strategy: MergeStrategy,
) -> Result<Map<String, Value>, String> {
    let parsed = toml_parser::parse(toml_content).map_err(|e| e.to_string())?;

    eval_config(
        (*parsed.default_configs).clone(),
        &parsed.contexts,
        &parsed.overrides,
        &parsed.dimensions,
        input_dimensions,
        merge_strategy,
        None,
    )
}
```

#### Rationale
1. **Separation of Concerns:** Parsing and evaluation are distinct operations
2. **Flexibility:** Users can parse once and evaluate multiple times with different inputs
3. **API Clarity:** Clear distinction between parsing (parse_toml_config) and evaluation (existing eval_config)
4. **Reduced Surface Area:** Smaller public API is easier to maintain

#### Migration Path
Users should now:
```rust
// Old: eval_toml_config(toml, dims, strategy)

// New: two-step process
let parsed = parse_toml_config(toml)?;
let config = eval_config(
    &parsed.default_configs,
    &parsed.contexts,
    &parsed.overrides,
    &parsed.dimensions,
    dims,
    strategy,
    None
)?;
```

---

## Updated File Changes Summary

### New Files (Total)
- `crates/superposition_core/src/toml_parser.rs` (~700 lines with validation)
- `clients/haskell/superposition-bindings/test/Main.hs` (~115 lines)
- `clients/python/bindings/superposition_bindings/.gitignore` (4 lines)

### Modified Files (Total)
- `crates/superposition_core/src/lib.rs`
  - Added toml_parser module export
  - Added parse_toml_config() function
  - Removed eval_toml_config() function
- `crates/superposition_core/src/toml_parser.rs`
  - Added mandatory position field validation
  - Added duplicate position detection
  - Added DuplicatePosition error variant
- `crates/superposition_core/src/ffi.rs`
  - Added core_parse_toml_config() C FFI function
- `clients/haskell/superposition-bindings/lib/FFI/Superposition.hs`
  - Added parseTomlConfig binding
- `clients/haskell/superposition-bindings/superposition-bindings.cabal`
  - Updated include-dirs to target/include
  - Added aeson, bytestring dependencies
- `Makefile`
  - Added bindings-test target with uniffi-bindings dependency
  - Updated Python test step with platform-specific library copy
  - Updated Haskell test configuration
  - Restored git apply step in uniffi-bindings target
- `uniffi/patches/python.patch`
  - Updated to use platform-specific library names
  - Added platform/architecture detection
- `.gitignore`
  - Added dynamic library patterns (*.dylib, *.so, *.dll)

### Test Coverage
- **Python:** 3 test functions with multiple assertions
- **JavaScript/TypeScript:** 3 test functions with error handling validation
- **Java/Kotlin:** 3 test functions via JUnit
- **Haskell:** 6 test cases (2 existing + 4 new TOML tests)

All tests validate:
- Valid TOML parsing
- External file parsing
- Invalid syntax error handling
- Missing section error handling
- Missing position field error handling
- Duplicate position detection (where applicable)

---

**End of Design Document**
