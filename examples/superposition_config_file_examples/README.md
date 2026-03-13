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

- `example.toml` — TOML format configuration
- `example.json` — JSON format configuration

Both files contain identical ride-sharing pricing configurations to demonstrate format equivalence. Refer to the actual files for the full format syntax.
