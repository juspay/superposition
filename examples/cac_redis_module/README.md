# CAC Redis Module

A Redis module that integrates Superposition's Context-Aware Configuration (CAC) system directly into Redis, enabling configuration resolution through Redis commands with context-based logic evaluation.

## Overview

The `cac_redis_module` extends Redis with custom commands that allow clients to resolve configurations based on contextual data using Superposition's configuration engine. This module brings the power of context-aware configuration directly to Redis, making it accessible through standard Redis protocols.

## Features

- **Context-Aware Configuration**: Resolve configurations based on dynamic context conditions using JSONLogic
- **Redis Integration**: Access configuration resolution through Redis commands
- **Real-time Configuration**: Load and evaluate configurations from local JSON files
- **Multiple Merge Strategies**: Support for MERGE and REPLACE strategies when applying overrides
- **Error Handling**: Comprehensive error reporting with timestamps and detailed messages

## Architecture

The module consists of several key components:

- **Core Module** (`lib.rs`): Main Redis module implementation with custom commands
- **Configuration Engine** (`config.rs`): Local implementation of Superposition's configuration evaluation logic
- **Configuration Data** (`cac_config.json`): JSON file containing contexts, overrides, and default configurations

## Installation

### Prerequisites

- Rust 1.70 or later
- Redis server 6.0 or later
- Git (for dependencies)

### Building the Module

1. Clone the Superposition repository:
```bash
git clone https://github.com/juspay/superposition.git
cd superposition/examples/cac_redis_module
```

2. Build the Redis module:
```bash
cargo build --release
```

3. The compiled module will be available at `target/release/libredis_module_cac.so` (Linux) or `target/release/libredis_module_cac.dylib` (macOS).

### Loading the Module

Start Redis with the module loaded:

```bash
redis-server --loadmodule ./target/release/libredis_module_cac.so
```

Or load the module into a running Redis instance:
```bash
redis-cli> MODULE LOAD /path/to/libredis_module_cac.so
```

## Configuration

The module reads configuration from `src/cac_config.json`. This file contains:

### Structure

```json
{
  "contexts": [
    {
      "id": "context_id",
      "condition": {
        "and": [
          {
            "==": [
              {"var": "variable_name"},
              "expected_value"
            ]
          }
        ]
      },
      "priority": 0,
      "weight": 0,
      "override_with_keys": ["override_key_id"]
    }
  ],
  "overrides": {
    "override_key_id": {
      "config_key": "config_value"
    }
  },
  "default_configs": {
    "config_key": "default_value",
    "numeric_config": 123,
    "boolean_config": true
  }
}
```

### Configuration Elements

- **Contexts**: Define conditions using JSONLogic that determine when overrides should be applied
- **Overrides**: Key-value pairs that override default configurations when their associated contexts match
- **Default Configs**: Base configuration values used when no overrides apply

## Usage

### Available Commands

#### 1. CAC.HELLO
A simple greeting command for testing module functionality.

```bash
redis-cli> CAC.HELLO "World"
"Hello from CAC module, World!"
```

#### 2. CAC.EVAL
Evaluates configuration based on provided context data.

```bash
redis-cli> CAC.EVAL '{"newd": "hi", "d1": "d1"}'
```

**Response Format:**
```json
{
  "status": "success",
  "result": {
    "abcd": 2,
    "bool": true,
    "bool_key": false,
    "double": 1.2,
    "integer": 1,
    "new": "help",
    "new2": "hithere",
    "object": {
      "k1": {
        "k2": "v1"
      }
    },
    "string": "something"
  },
  "query_data": {
    "newd": "hi",
    "d1": "d1"
  },
  "timestamp": 1703123456,
  "eval_type": "config_evaluation_with_file"
}
```

#### 3. HELLO.MUL
A utility command that multiplies numbers (example from Redis module template).

```bash
redis-cli> HELLO.MUL 2 3 4
1) (integer) 2
2) (integer) 3
3) (integer) 4
4) (integer) 24
```

### Example Usage Scenarios

#### Basic Configuration Resolution

```bash
# Query with context that matches a condition
redis-cli> CAC.EVAL '{"d1": "d1"}'

# Query with context that doesn't match any condition
redis-cli> CAC.EVAL '{"unknown": "value"}'
```

#### Experiment Variant Selection

```bash
# Query with variant context for A/B testing
redis-cli> CAC.EVAL '{"d1": "d1", "variantIds": ["7346449145428840448-experimental"]}'
```

#### Numeric Context Evaluation

```bash
# Query with numeric conditions
redis-cli> CAC.EVAL '{"new104": 5}'
```

## Error Handling

The module provides detailed error messages for various scenarios:

### Invalid JSON Input
```json
{
  "status": "error",
  "message": "Invalid JSON: expected value at line 1 column 1",
  "input": "invalid_json",
  "timestamp": 1703123456
}
```

### Configuration Load Errors
```json
{
  "status": "error",
  "message": "Failed to load configuration: No such file or directory (os error 2)",
  "timestamp": 1703123456
}
```

### Invalid Query Data Type
```json
{
  "status": "error",
  "message": "Query data must be a JSON object",
  "received_type": "string",
  "timestamp": 1703123456
}
```

## Performance Considerations

- **File I/O**: Configuration is loaded from disk on each evaluation. For production use, consider caching mechanisms
- **JSON Parsing**: JSONLogic evaluation is performed for each context condition
- **Memory Usage**: Module keeps minimal state; most data is processed per request

## Development

### Dependencies

The module uses these key dependencies:

- `redis-module`: Redis module SDK for Rust
- `serde_json`: JSON serialization/deserialization
- `jsonlogic`: JSONLogic rule evaluation engine
- `superposition_types`: Superposition's type definitions

### Extending the Module

To add new commands:

1. Define the command function in `lib.rs`:
```rust
fn my_command(_: &Context, args: Vec<RedisString>) -> RedisResult {
    // Command implementation
    Ok("response".into())
}
```

2. Register the command in the `redis_module!` macro:
```rust
redis_module! {
    name: "cac",
    version: 1,
    commands: [
        ["my.command", my_command, "", 0, 0, 0],
    ],
}
```

### Testing

Test the module using Redis CLI:

```bash
# Test module loading
redis-cli> MODULE LIST

# Test basic functionality
redis-cli> CAC.HELLO "Test"

# Test configuration evaluation
redis-cli> CAC.EVAL '{"test": "value"}'
```

## Troubleshooting

### Module Won't Load
- Verify Redis version compatibility (6.0+)
- Check file permissions on the compiled module
- Ensure all dependencies are properly linked

### Configuration Errors
- Verify `cac_config.json` exists in the `src/` directory
- Validate JSON syntax in the configuration file
- Check file permissions for reading the configuration

### Evaluation Errors
- Ensure query data is valid JSON
- Verify context conditions use proper JSONLogic syntax
- Check that override keys referenced in contexts exist in the overrides section

## Use Cases

1. **Feature Flags**: Enable/disable features based on user attributes
2. **A/B Testing**: Route traffic to different configurations based on experiment variants
3. **Environment-Specific Configs**: Different settings for dev/staging/production
4. **User Personalization**: Customize application behavior based on user context
5. **Geographic Configuration**: Different settings based on user location

## Security Considerations

- The module reads configuration files from the local filesystem
- No authentication is performed on Redis commands (use Redis AUTH if needed)
- JSONLogic evaluation should be considered when exposing user-controllable context data

## License

This module is part of the Superposition project. Please refer to the main project's license terms.