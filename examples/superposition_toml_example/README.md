# Superposition TOML Parser Example

This example demonstrates how to use the `superposition_core` crate to parse and evaluate TOML configuration files with context-based overrides.

## Overview

The example shows a ride-sharing pricing configuration with:
- **Default configuration**: Base rates for per-kilometer pricing and surge factors
- **Dimensions**: City, vehicle type, and hour of day
- **Context-based overrides**: Different pricing for specific combinations of dimensions

## Running the Example

From the repository root:

```bash
cargo run -p superposition_toml_example
```

This will compile and run the example, demonstrating various pricing scenarios.

## Example Output

The application demonstrates five different scenarios:

1. **Bike ride** - Uses bike-specific rate (15.0 per km)
2. **Cab in Bangalore** - Uses Bangalore cab rate (22.0 per km)
3. **Cab in Delhi at 6 AM** - Applies morning surge (surge_factor = 5.0)
4. **Cab in Delhi at 6 PM** - Applies evening surge (surge_factor = 5.0)
5. **Auto ride** - Uses default values (20.0 per km, no surge)

## TOML Configuration Structure

### Default Configuration
```toml
[default-config]
per_km_rate = { "value" = 20.0, "schema" = { "type" = "number" } }
surge_factor = { "value" = 0.0, "schema" = { "type" = "number" } }
```

Each configuration key requires:
- `value`: The default value
- `schema`: JSON schema for validation

### Dimensions
```toml
[dimensions]
city = { schema = { "type" = "string", "enum" = ["Bangalore", "Delhi"] } }
vehicle_type = { schema = { "type" = "string", "enum" = ["auto", "cab", "bike"] } }
hour_of_day = { schema = { "type" = "integer", "minimum" = 0, "maximum" = 23 }}
```

Dimensions define the variables that can be used in context expressions.

### Context-Based Overrides
```toml
[[context]]
_condition_ = { vehicle_type = "cab" }
per_km_rate = 25.0

[[context]]
_condition_ = { city = "Bangalore", vehicle_type = "cab" }
per_km_rate = 22.0
```

Contexts define overrides that apply when specific dimension values are present. Multiple dimensions can be combined by adding them to the `_condition_` table.

## API Usage

### Parsing Only
```rust
use superposition_core::parse_toml_config;

let parsed = parse_toml_config(&toml_content)?;
println!("Found {} contexts", parsed.contexts.len());
```

### Parse and Evaluate
```rust
use superposition_core::{eval_toml_config, MergeStrategy};
use serde_json::{Map, Value};

let mut dimensions = Map::new();
dimensions.insert("city".to_string(), Value::String("Delhi".to_string()));
dimensions.insert("vehicle_type".to_string(), Value::String("cab".to_string()));

let config = eval_toml_config(&toml_content, &dimensions, MergeStrategy::MERGE)?;
let rate = config.get("per_km_rate").unwrap();
```

## Priority Calculation

When multiple contexts match, the one with higher priority wins. Priority is calculated using bit-shift based on dimension positions:
- `vehicle_type=cab` (position 2): priority = 2^2 = 4
- `city=Bangalore; vehicle_type=cab` (positions 1,2): priority = 2^1 + 2^2 = 6

Higher priority contexts override lower priority ones.

## Learn More

See the [design document](../../design-docs/2025-12-21-toml-parsing-ffi-design.md) for complete implementation details.
