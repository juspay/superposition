# Superposition Local Provider Example

This example demonstrates how to use the Superposition Local Provider with different refresh strategies.

## Features

- **Command-line interface** to select refresh strategies
- **FileWatch mode** for real-time configuration monitoring
- **Support for all refresh strategies**: Manual, OnDemand, and FileWatch
- **Custom dimensions** via `--dimension` arguments
- **Flexible key evaluation** via `--keys` arguments
- **Dynamic context building** from command-line parameters
- **Auto-discovery of configuration keys** from TOML `default-config` section
- **Multi-type value support** (boolean, float, string) with automatic type detection

## Usage

### Basic Usage (Manual mode - default)
Auto-discovers all keys from the TOML configuration:
```bash
cargo run
# Output: festival_day: false, per_km_rate: 22.00, surge_factor: 1.00
```

### OnDemand Mode
```bash
cargo run -- --refresh-strategy on-demand
```

### FileWatch Mode
```bash
cargo run -- --refresh-strategy file-watch
```

### Custom Configuration File
```bash
cargo run -- --file-path /path/to/your/config.toml --refresh-strategy file-watch
```

### Custom Dimensions
Specify context dimensions using key-value pairs:
```bash
# Single dimension
cargo run -- --dimension vehicle_type=bike

# Multiple dimensions
cargo run -- --dimension city=Delhi --dimension vehicle_type=cab --dimension hour_of_day=18

# With specific keys to evaluate
cargo run -- --dimension city=Bangalore --dimension vehicle_type=cab --keys surge_factor,per_km_rate
```

### Custom Keys
Specify which configuration keys to evaluate. Supports both comma-separated values and multiple specifications:
```bash
# Evaluate only surge_factor
cargo run -- --keys surge_factor

# Evaluate multiple keys (comma-separated)
cargo run -- --keys surge_factor,per_km_rate

# Evaluate multiple keys (multiple specifications)
cargo run -- --keys surge_factor --keys per_km_rate

# Comma-separated with spaces
cargo run -- --keys "surge_factor, per_km_rate"

# Combine with dimensions
cargo run -- --dimension vehicle_type=auto --keys per_km_rate

# Mixed format (automatically deduplicated)
cargo run -- --keys surge_factor --keys per_km_rate,surge_factor
```

### Advanced Examples
```bash
# Auto-discovery with specific context
cargo run -- --dimension vehicle_type=bike
# Output: festival_day: false, per_km_rate: 15.00, surge_factor: 0.00

# Test specific boolean key
cargo run -- --keys festival_day --dimension city=Bangalore --dimension vehicle_type=cab
# Output: festival_day: false

# Test Delhi peak hour surge
cargo run -- --dimension city=Delhi --dimension vehicle_type=cab --dimension hour_of_day=18 --keys surge_factor,per_km_rate

# FileWatch mode with auto-discovery
cargo run -- --refresh-strategy file-watch --dimension city=Bangalore --dimension vehicle_type=cab

# Multiple keys with different value types
cargo run -- --keys festival_day,surge_factor --dimension vehicle_type=bike
# Output: festival_day: false, surge_factor: 0.00

# Comprehensive example with mixed types
cargo run -- --refresh-strategy on-demand --dimension city=Bangalore --dimension vehicle_type=cab
# Output: festival_day: false, per_km_rate: 22.00, surge_factor: 1.00
```

## Command-Line Arguments

### `--refresh-strategy` / `-r`
- **Options**: `manual`, `on-demand`, `file-watch`
- **Default**: `manual`
- **Description**: Controls how configuration is loaded and refreshed

### `--file-path` / `-f`
- **Type**: String
- **Default**: `example.cac.toml`
- **Description**: Path to the TOML configuration file

### `--dimension` / `-d`
- **Type**: Key-value pairs (format: `key=value`)
- **Multiple**: Yes (can be specified multiple times)
- **Description**: Context dimensions for configuration evaluation
- **Examples**: 
  - `--dimension city=Bangalore`
  - `--dimension vehicle_type=cab`
  - `--dimension hour_of_day=18`

### `--keys` / `-k`
- **Type**: Comma-separated strings or multiple specifications
- **Default**: Auto-discovered from TOML `default-config` section
- **Description**: Configuration keys to evaluate and display. Supports all value types (boolean, float, string)
- **Format Options**:
  - Comma-separated: `--keys key1,key2,key3`
  - Multiple specifications: `--keys key1 --keys key2`
  - Mixed format supported with automatic deduplication
- **Auto-Discovery**: When no keys specified, automatically discovers all keys from the `default-config` section of the TOML file
- **Examples**:
  - `--keys surge_factor`
  - `--keys surge_factor,per_km_rate,festival_day`
  - `--keys surge_factor --keys per_km_rate`
  - `--keys "festival_day, per_km_rate"` (boolean and float types)

## Refresh Strategies

### Manual
- Configuration is loaded once during initialization
- No automatic reloading
- Suitable for production environments where configuration changes are rare

### OnDemand
- Configuration is reloaded on every flag evaluation
- Higher I/O overhead but always up-to-date
- Suitable for development environments

### FileWatch
- Configuration is automatically reloaded when the file changes
- Runs continuously, printing flag values every 10 seconds
- Real-time updates when you modify the configuration file
- Perfect for testing and development

## Testing FileWatch Mode

1. Start the example in FileWatch mode:
   ```bash
   cargo run -- --refresh-strategy file-watch
   ```

2. The application will print current flag values every 10 seconds

3. In another terminal, modify the `example.cac.toml` file:
   ```bash
   # Change surge_factor from 1.0 to 2.5 for Bangalore cabs
   sed -i 's/surge_factor = 1.0/surge_factor = 2.5/' example.cac.toml
   ```

4. Watch the output update automatically with the new values

## Example Configuration

The `example.cac.toml` file contains configuration for a ride-sharing application with:
- **Default values**: `per_km_rate = 20.0`, `surge_factor = 0.0`, `festival_day = true`
- **Available keys** (auto-discovered):
  - `festival_day` (boolean): Whether it's a festival day
  - `per_km_rate` (float): Rate per kilometer
  - `surge_factor` (float): Surge pricing multiplier
- **Vehicle-specific rates**: Different rates for cab, bike, auto
- **Location-specific configurations**: Different values for different cities
- **Context-dependent behavior**: Values change based on city, vehicle type, and time
- **Multi-type support**: Demonstrates boolean, float, and string value types

## Output

The example automatically discovers and evaluates all configuration keys:
- `festival_day`: Boolean flag indicating whether it's a festival day
- `per_km_rate`: Float value for the base rate per kilometer  
- `surge_factor`: Float multiplier for surge pricing

Values are automatically formatted based on their type (boolean, float, string) and change based on the evaluation context (dimensions like city, vehicle_type, etc.).