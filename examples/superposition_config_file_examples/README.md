# Superposition Config File Examples

This example demonstrates how to use the `superposition_core` crate to parse and evaluate both TOML and JSON configuration files with context-based overrides.

## Overview

The example shows a ride-sharing pricing configuration with:
- **Default configuration**: Base rates for per-kilometer pricing and surge factors
- **Dimensions**: City, vehicle type, hour of day, and city cohort
- **Context-based overrides**: Different pricing for specific combinations of dimensions

Both TOML and JSON formats are demonstrated with identical configurations to show format equivalence.

## Running the Example

From the repository root:

```bash
cargo run -p superposition_config_file_examples
```

This will compile and run the example, demonstrating various pricing scenarios using both TOML and JSON formats.

## Example Output

The application demonstrates five different scenarios for each format:

1. **Bike ride** - Uses bike-specific rate (15.0 per km)
2. **Cab in Bangalore** - Uses Bangalore cab rate (22.0 per km)
3. **Cab in Delhi at 6 AM** - Applies morning surge (surge_factor = 5.0)
4. **Auto ride** - Uses default values (20.0 per km, no surge)
5. **Chennai ride** - Uses default values (city not in overrides)

## Configuration Files

### TOML Format (`example.toml`)

```toml
[default-configs]
per_km_rate = { "value" = 20.0, "schema" = { "type" = "number" } }
surge_factor = { "value" = 0.0, "schema" = { "type" = "number" } }

[dimensions]
city = { position = 4, schema = { "type" = "string", "enum" = ["Chennai", "Bangalore", "Delhi"] } }
vehicle_type = { position = 2, schema = { "type" = "string", "enum" = ["auto", "cab", "bike"] } }

[[overrides]]
_context_ = { vehicle_type = "cab" }
per_km_rate = 25.0
```

### JSON Format (`example.json`)

```json
{
  "default-configs": {
    "per_km_rate": {
      "value": 20.0,
      "schema": { "type": "number" }
    }
  },
  "dimensions": {
    "city": {
      "position": 4,
      "schema": {
        "type": "string",
        "enum": ["Chennai", "Bangalore", "Delhi"]
      }
    }
  },
  "overrides": [
    {
      "_context_": { "vehicle_type": "cab" },
      "per_km_rate": 25.0
    }
  ]
}
```

## Format Comparison

Both formats support identical features:
- Default configurations with JSON schema validation
- Dimensions with positions and schemas
- Context-based overrides
- Cohort dimensions (LOCAL_COHORT, REMOTE_COHORT)
- Priority-based override resolution

### TOML Advantages
- More human-readable for simple configs
- Supports comments
- Better for version control diffs
- Native datetime support

### JSON Advantages
- Native object/array support
- Easier to generate programmatically
- Direct serde_json serialization
- Widely supported by tools

## API Usage

### TOML
```rust
use superposition_core::parse_toml_config;

let parsed = parse_toml_config(&toml_content)?;
println!("Found {} contexts", parsed.contexts.len());
```

### JSON
```rust
use superposition_core::parse_json_config;

let parsed = parse_json_config(&json_content)?;
println!("Found {} contexts", parsed.contexts.len());
```

### Evaluation (Both Formats)
```rust
use superposition_core::{eval_config, MergeStrategy};
use serde_json::{Map, Value};

let mut dimensions = Map::new();
dimensions.insert("city".to_string(), Value::String("Delhi".to_string()));
dimensions.insert("vehicle_type".to_string(), Value::String("cab".to_string()));

let result = eval_config(
    default_configs,
    &config.contexts,
    &config.overrides,
    &config.dimensions,
    &dimensions,
    MergeStrategy::MERGE,
    None,
)?;
```

## Priority Calculation

When multiple contexts match, the one with higher priority wins. Priority is calculated using bit-shift based on dimension positions:
- `vehicle_type=cab` (position 2): priority = 2^2 = 4
- `city=Bangalore; vehicle_type=cab` (positions 4,2): priority = 2^4 + 2^2 = 20

Higher priority contexts override lower priority ones.

## Learn More

- [JSON Format Documentation](../../docs/json-config-format.md)
- [Unified Config Format Design](../../docs/plans/2025-02-15-unified-config-format.md)
