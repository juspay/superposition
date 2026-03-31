---
name: superposition-config
description: Context-aware configuration management for Superposition. Use when creating dimensions, default configs, contexts, overrides, functions, or type templates. Helps validate configs with JSON Schema and custom JavaScript functions.
license: Apache-2.0
compatibility: Requires Superposition instance access (local or hosted)
metadata:
  author: juspay
  version: "1.0"
---

# Superposition Configuration Management

This skill helps you manage context-aware configurations in Superposition - the platform for safe configuration changes and experimentation.

## Quick Start

### Understanding the CAC Model

Context-Aware-Config (CAC) works like CSS for application configuration:

| CSS | CAC |
|-----|-----|
| `body { color: black; }` | `default { feature_flag: false }` |
| `.premium { color: gold; }` | `[user_tier="premium"] { discount: 20 }` |
| `#header { height: 100px; }` | `[city="Delhi"] { surge_factor: 1.5 }` |

### Core Concepts

1. **Default Configs** - Base configuration keys and their default values
2. **Dimensions** - Attributes that can change config values (e.g., city, user_tier, device_type)
3. **Context** - Key-value pairs matching dimension values (e.g., `{"city": "Delhi", "hour": 20}`)
4. **Overrides** - Different values applied when context matches
5. **Functions** - JavaScript validation and autocomplete logic
6. **Type Templates** - Reusable JSON Schema definitions

## Common Tasks

### 1. Create a Dimension

Dimensions define the axes along which your configuration varies.

**Simple dimension (enum):**
```bash
curl -X POST http://localhost:8080/dimension \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "dimension": "city",
    "description": "City where the user is located",
    "position": 1,
    "schema": {
      "type": "string",
      "enum": ["Bangalore", "Delhi", "Mumbai", "Chennai"]
    },
    "change_reason": "Adding city dimension for location-based pricing"
  }'
```

**Dimension with dependencies (hierarchical):**
```bash
curl -X POST http://localhost:8080/dimension \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "dimension": "zone",
    "description": "Zone within a city",
    "position": 3,
    "schema": { "type": "string" },
    "dependencies": ["city"],
    "change_reason": "Zone depends on city being set first"
  }'
```

### 2. Create Default Configuration

Default configs define all possible configuration keys with their base values.

```bash
curl -X POST http://localhost:8080/default-config \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "key": "per_km_rate",
    "value": { "value": 20.0 },
    "schema": { "type": "number", "minimum": 0 },
    "change_reason": "Base rate for ride hailing"
  }'
```

**With function validation:**
```bash
curl -X POST http://localhost:8080/default-config \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "key": "surge_factor",
    "value": { "value": 1.0 },
    "schema": { "type": "number", "minimum": 1.0, "maximum": 5.0 },
    "function_name": "validate_surge_pricing",
    "change_reason": "Surge multiplier with business rule validation"
  }'
```

### 3. Create Context with Overrides

Contexts define when to apply specific override values.

```bash
curl -X POST http://localhost:8080/context \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "context": {
      "city": "Delhi",
      "vehicle_type": "cab"
    },
    "override": {
      "per_km_rate": 25.0,
      "base_fare": 50.0
    },
    "change_reason": "Higher rates for cabs in Delhi"
  }'
```

### 4. Create a Validation Function

Functions validate dimension/config values using JavaScript.

```bash
curl -X POST http://localhost:8080/function \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "function_name": "validate_city_pricing",
    "code": "async function validate(key, value) {\n  const validCities = ['Bangalore', 'Delhi', 'Mumbai', 'Chennai'];\n  if (key === 'city') {\n    return validCities.includes(value);\n  }\n  return true;\n}",
    "change_reason": "Validate city is in allowed list"
  }'
```

**Publish the function (required before use):**
```bash
curl -X PATCH http://localhost:8080/function/validate_city_pricing \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "code": "async function validate(key, value) {\n  const validCities = ['Bangalore', 'Delhi', 'Mumbai', 'Chennai'];\n  if (key === 'city') {\n    return validCities.includes(value);\n  }\n  return true;\n}",
    "change_reason": "Publishing for production use"
  }'
```

### 5. Create Type Templates

Reusable JSON Schema definitions for consistent typing.

```bash
curl -X POST http://localhost:8080/types \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "type_name": "CurrencyAmount",
    "type_schema": {
      "type": "object",
      "properties": {
        "amount": { "type": "number", "minimum": 0 },
        "currency": { "type": "string", "enum": ["USD", "EUR", "INR", "GBP"] }
      },
      "required": ["amount", "currency"]
    },
    "change_reason": "Standard currency amount type"
  }'
```

## Context Condition Syntax

Contexts use simple key-value maps to match dimension values:

| Condition | Map Format |
|-----------|------------|
| city is Delhi | `{"city": "Delhi"}` |
| city is Delhi AND vehicle is cab | `{"city": "Delhi", "vehicle_type": "cab"}` |
| Multiple cities (OR logic) | Use separate contexts for each city |

## Dimension Position & Dependencies

Position determines evaluation order (lower = evaluated first):

| Position | Dimension | Dependencies |
|----------|-----------|--------------|
| 0 | `variantIds` | (reserved) |
| 1 | `region` | none |
| 2 | `city` | region |
| 3 | `zone` | city |

## Testing Configuration Resolution

Test what config values resolve for a given context:

```bash
curl -X POST http://localhost:8080/config \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
  "context": {
    "city": "Delhi",
    "vehicle_type": "cab",
    "hour_of_day": 20
  }
  }'
```

## TOML Configuration Format

For bulk configuration, use the TOML format:

```toml
[default-config]
per_km_rate = { value = 20.0, schema = { type = "number" } }
surge_factor = { value = 1.0, schema = { type = "number", minimum = 1 } }

[dimensions]
city = { schema = { type = "string", enum = ["Bangalore", "Delhi"] }, position = 1 }
vehicle_type = { schema = { type = "string", enum = ["auto", "cab", "bike"] }, position = 2 }
hour_of_day = { schema = { type = "integer", minimum = 0, maximum = 23 }, position = 3 }

[context.vehicle_type_cab]
condition = { vehicle_type = "cab" }
per_km_rate = 25.0

[context.delhi_evening]
condition = { city = "Delhi" }
surge_factor = 2.0
```

Import via API:
```bash
curl -X PUT http://localhost:8080/config \
  -H "Content-Type: application/toml" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  --data-binary @config.toml
```

## Related Skills

- [superposition-experiments](../superposition-experiments/) - Run A/B tests on your configurations
- [superposition-sdk](../superposition-sdk/) - Use SDK for programmatic config management
- [superposition-api](../superposition-api/) - Full REST API reference

See [references/REFERENCE.md](references/REFERENCE.md) for complete API examples.
