# JSON Configuration Format

Superposition supports JSON as an alternative to TOML for configuration files. This document describes the JSON format and how to use it.

## Overview

The JSON format uses the same data structure as the internal superposition model:

- **`default-configs`**: Map of configuration keys to their values and schemas
- **`dimensions`**: Map of dimension names to their definitions (position, schema, type)
- **`overrides`**: Array of context-conditional overrides

## Format Specification

```json
{
  "default-configs": {
    "<config-key>": {
      "value": <any>,
      "schema": <json-schema>
    }
  },
  "dimensions": {
    "<dimension-name>": {
      "position": <integer>,
      "schema": <json-schema>,
      "type": "REGULAR" | "LOCAL_COHORT:<dim>" | "REMOTE_COHORT:<dim>"
    }
  },
  "overrides": [
    {
      "_context_": { "<dimension>": <value> },
      "<config-key>": <override-value>
    }
  ]
}
```

## Complete Example

```json
{
  "default-configs": {
    "timeout": {
      "value": 30,
      "schema": { "type": "integer", "minimum": 0 }
    },
    "enabled": {
      "value": true,
      "schema": { "type": "boolean" }
    },
    "database": {
      "value": {
        "host": "localhost",
        "port": 5432,
        "ssl": false
      },
      "schema": {
        "type": "object",
        "properties": {
          "host": { "type": "string" },
          "port": { "type": "integer" },
          "ssl": { "type": "boolean" }
        }
      }
    }
  },
  "dimensions": {
    "environment": {
      "position": 1,
      "schema": {
        "type": "string",
        "enum": ["development", "staging", "production"]
      }
    },
    "region": {
      "position": 2,
      "schema": {
        "type": "string",
        "enum": ["us-east", "us-west", "eu-west"]
      }
    }
  },
  "overrides": [
    {
      "_context_": { "environment": "production" },
      "timeout": 60,
      "database": {
        "host": "prod.db.example.com",
        "ssl": true
      }
    },
    {
      "_context_": { "environment": "production", "region": "us-east" },
      "timeout": 90
    }
  ]
}
```

## API Usage

### Rust

```rust
use superposition_core::{parse_json_config, serialize_to_json};

// Parse JSON configuration
let config = parse_json_config(json_string)?;

// Access parsed configuration
println!("Default configs: {:?}", config.default_configs);
println!("Dimensions: {:?}", config.dimensions);
println!("Contexts: {:?}", config.contexts);

// Serialize back to JSON
let json_output = serialize_to_json(detailed_config)?;
```

### Java/Kotlin (via FFI)

```kotlin
import uniffi.superposition_client.*

// Parse JSON configuration
val config = ffiParseJsonConfig(jsonString)

// Access parsed configuration
config.defaultConfigs.forEach { (key, value) ->
    println("$key: $value")
}

config.contexts.forEach { context ->
    println("Context ID: ${context.id}")
    println("Priority: ${context.priority}")
}
```

## Comparison with TOML

### Advantages of JSON:
- Native support for all JSON types (objects, arrays, null)
- No special quoting rules required
- Widely supported by tools and editors
- Easier to generate programmatically
- Direct serde_json serialization (no intermediate conversion)

### Advantages of TOML:
- More human-readable for simple configurations
- Supports comments
- Better for version control diffs
- Native datetime type support

## Validation

Both JSON and TOML formats use identical validation logic:

1. **Default configs**: Values validated against their schemas
2. **Dimensions**: Schemas validated, cohort references checked
3. **Contexts**: All dimensions used must be declared
4. **Overrides**: Keys must exist in default-configs
5. **Positions**: No duplicate dimension positions allowed

## Migration from TOML

To convert TOML to JSON:

1. Parse the TOML file using existing tools
2. Serialize to JSON
3. Validate with Superposition

Example using Python:
```python
import toml
import json

with open('config.toml') as f:
    data = toml.load(f)

# Convert TOML structure to JSON structure
json_data = {
    "default-configs": data.get("default-configs", {}),
    "dimensions": data.get("dimensions", {}),
    "overrides": data.get("overrides", [])
}

with open('config.json', 'w') as f:
    json.dump(json_data, f, indent=2)
```

**Note**: Complex TOML structures may require manual review after conversion.

## Error Handling

JSON parsing uses the same error types as TOML:

```rust
pub enum FormatError {
    SyntaxError { format: String, message: String },
    InvalidDimension(String),
    UndeclaredDimension { dimension: String, context: String },
    InvalidOverrideKey { key: String, context: String },
    DuplicatePosition { position: i32, dimensions: Vec<String> },
    ValidationError { key: String, errors: String },
    // ...
}
```

All errors include the format name ("JSON") for context.

## Implementation Details

The JSON implementation:
- Uses `serde_json` for parsing/serialization
- Implements the `ConfigFormat` trait for consistency with TOML
- Shares validation logic via `validate_detailed_config()`
- Sorts contexts by priority after parsing (same as TOML)
- Supports all superposition features (cohorts, overrides, etc.)

## See Also

- [TOML Format Documentation](toml-config-format.md)
- [Example Configuration](examples/config.json)
- [Integration Tests](../crates/superposition_core/tests/format_integration.rs)
