---
sidebar_position: 2
title: Format Specification
description: Complete syntax reference for SuperTOML configuration files
---

# Format Specification

SuperTOML configuration files consist of three main sections:

1. **`[default-configs]`** - Base configuration values with schemas
2. **`[dimensions]`** - Dimensions that define context segmentation
3. **`[[overrides]]`** - Context-specific configuration overrides

An optional **`[meta]`** table can precede them to mark the file as SuperTOML and record its format version.

## File Structure

### TOML Format

```toml
[meta]
type = "supertoml"
version = 1

[default-configs]
# Configuration keys with default values and schemas

[dimensions]
# Dimensions that segment your configuration

[[overrides]]
# First override
_context_ = { /* context condition */ }
# override values

[[overrides]]
# Second override
_context_ = { /* context condition */ }
# override values
```

### JSON Format

```json
{
  "default-configs": {
    "key": { "value": <default_value>, "schema": <json_schema>, "description": <string> }
  },
  "dimensions": {
    "dimension_name": { "position": <number>, "schema": <json_schema>, "description": <string> }
  },
  "overrides": [
    {
      "_context_": { /* context condition */ },
      "key": <override_value>
    }
  ]
}
```

---

## Meta Section

The optional `[meta]` table identifies the file as SuperTOML and states which version of the format it was written for. Exported SuperTOML always includes it.

### Syntax

```toml
[meta]
type = "supertoml"
version = 1
```

### Parameters

| Parameter | Required | Description |
|-----------|----------|-------------|
| `type` | Yes | Must be `"supertoml"` |
| `version` | Yes | SuperTOML format version, a positive integer. The current version is `1` |

A file that declares a version newer than the parser supports is rejected rather than parsed partially.

A file without `[meta]` is read as version `1`, the format that existed before the marker was introduced. This stays true as newer versions are released, so older files keep their meaning. The parser logs a warning for such files; add `[meta]` with `version = 1` to pin the version and silence it.

`[meta]` applies to the TOML format only; the JSON format has no equivalent.

---

## Default Configs Section

The `[default-configs]` section defines all possible configuration keys for your application, along with their default values and validation schemas.

### Syntax

```toml
[default-configs]
<key> = { value = <default_value>, schema = <json_schema> [, description = <string>] }
```

### Parameters

| Parameter     | Type        | Required        | Description                                                                          |
| ------------- | ----------- | --------------- | ----------------------------------------------------------------------------------- |
| `value`       | Any         | Yes             | The default value for this configuration key                                        |
| `schema`      | JSON Schema | Yes             | JSON Schema that validates the value                                                |
| `description` | String      | No (on import)  | Human-readable description. Optional on import (falls back to the key name when omitted); always emitted on export |

### Examples

#### Simple Types

```toml
[default-configs]
# String configuration
app_name = { value = "MyApp", schema = { type = "string" } }

# Number configuration
max_connections = { value = 100, schema = { type = "integer", minimum = 1, maximum = 1000 } }

# Boolean configuration
debug_mode = { value = false, schema = { type = "boolean" } }
```

#### Enum Values

```toml
[default-configs]
log_level = {
    value = "info",
    schema = {
        type = "string",
        enum = ["debug", "info", "warn", "error"]
    }
}
```

#### Complex Objects

```toml
[default-configs]
database_config = {
    value = { host = "localhost", port = 5432, pool_size = 10 },
    schema = {
        type = "object",
        properties = {
            host = { type = "string" },
            port = { type = "integer", minimum = 1, maximum = 65535 },
            pool_size = { type = "integer", minimum = 1 }
        },
        required = ["host", "port"]
    }
}
```

#### Arrays

```toml
[default-configs]
allowed_origins = {
    value = ["https://example.com"],
    schema = {
        type = "array",
        items = { type = "string", format = "uri" },
        minItems = 1
    }
}
```

---

## Dimensions Section

The `[dimensions]` section defines the attributes that segment your configuration. Each dimension has a position (for priority calculation) and a schema (for validation).

### Syntax

```toml
[dimensions]
<dimension_name> = { position = <number>, schema = <json_schema> [, description = <string>] [, type = <dimension_type>] }
```

### Parameters

| Parameter     | Type        | Required        | Description                                                                                |
| ------------- | ----------- | --------------- | ----------------------------------------------------------------------------------------- |
| `position`    | Integer     | Yes             | Position for priority calculation (higher = more specific)                                |
| `schema`      | JSON Schema | Yes             | Schema validating dimension values                                                        |
| `description` | String      | No (on import)  | Human-readable description. Optional on import (falls back to the dimension name when omitted); always emitted on export |
| `type`        | String      | No              | Dimension type: `"REGULAR"` (default), `"LOCAL_COHORT:<dim>"`, or `"REMOTE_COHORT:<dim>"` |

### Position and Priority

The `position` parameter determines the weight of a dimension in priority calculations:

- Weight = 2^position
- Higher positions contribute more to context priority
- Position 0 is reserved for `variantIds` (experimentation)
- Positions must be unique across dimensions

:::info
See [Deterministic Resolution](./deterministic-resolution) for details on how positions affect override priority.
:::

### Examples

#### Basic Dimensions

```toml
[dimensions]
city = {
    position = 4,
    schema = {
        type = "string",
        enum = ["Chennai", "Bangalore", "Delhi"]
    }
}

vehicle_type = {
    position = 2,
    schema = {
        type = "string",
        enum = ["auto", "cab", "bike"]
    }
}

hour_of_day = {
    position = 3,
    schema = {
        type = "integer",
        minimum = 0,
        maximum = 23
    }
}
```

#### Dimension with Numeric Range

```toml
[dimensions]
user_age_group = {
    position = 5,
    schema = {
        type = "integer",
        enum = [1, 2, 3, 4, 5]  # Age brackets
    }
}
```

#### Cohort Dimensions (Derived)

Cohort dimensions derive their values from other dimensions:

```toml
[dimensions]
# Base dimension
city = { position = 4, schema = { type = "string", enum = ["Bangalore", "Chennai", "Delhi", "Mumbai"] } }

# Derived cohort dimension
city_cohort = {
    position = 1,
    type = "LOCAL_COHORT:city",
    schema = {
        type = "string",
        enum = ["south", "north", "otherwise"],
        definitions = {
            south = { in = [{ var = "city" }, ["Bangalore", "Chennai"]] },
            north = { in = [{ var = "city" }, ["Delhi"]] }
        }
    }
}
```

:::info
See [Dimensions Deep Dive](./dimensions) for more on cohort dimensions.
:::

---

## Overrides Section

The `[[overrides]]` array defines context-specific configuration overrides. Each override has a `_context_` that specifies when it applies.

### Syntax

```toml
[[overrides]]
_context_ = { <dimension> = <value>, ... }
<config_key> = <override_value>
<config_key> = <override_value>
```

### Context Matching

A context matches when **all** specified dimensions match the runtime context:

- Equality matching: `city = "Bangalore"` matches when city equals "Bangalore"
- Multiple dimensions: All must match (AND logic)
- Unspecified dimensions: Wildcard (matches any value)

### Examples

#### Single Dimension Override

```toml
[[overrides]]
_context_ = { vehicle_type = "bike" }
per_km_rate = 15.0
```

When `vehicle_type` is "bike", `per_km_rate` becomes 15.0.

#### Multi-Dimension Override

```toml
[[overrides]]
_context_ = { city = "Delhi", vehicle_type = "cab", hour_of_day = 18 }
surge_factor = 5.0
```

When all three dimensions match, `surge_factor` becomes 5.0.

#### Multiple Overrides for Same Key

```toml
[[overrides]]
_context_ = { vehicle_type = "cab" }
per_km_rate = 25.0

[[overrides]]
_context_ = { city = "Bangalore", vehicle_type = "cab" }
per_km_rate = 22.0
```

The more specific context (city + vehicle_type) has higher priority.

#### Multiple Config Keys in One Override

```toml
[[overrides]]
_context_ = { vehicle_type = "auto" }
per_km_rate = 100.0
surge_factor = 1.0
```

Both values are overridden when the context matches.

---

## Complete Example

```toml
# Ride-hailing pricing configuration

[meta]
type = "supertoml"
version = 1

[default-configs]
per_km_rate = { value = 20.0, schema = { type = "number", minimum = 0 } }
surge_factor = { value = 0.0, schema = { type = "number", minimum = 0 } }
base_fare = { value = 50.0, schema = { type = "number", minimum = 0 } }

[dimensions]
city = {
    position = 4,
    schema = { type = "string", enum = ["Chennai", "Bangalore", "Delhi"] }
}
vehicle_type = {
    position = 2,
    schema = { type = "string", enum = ["auto", "cab", "bike"] }
}
hour_of_day = {
    position = 3,
    schema = { type = "integer", minimum = 0, maximum = 23 }
}
city_cohort = {
    position = 1,
    type = "LOCAL_COHORT:city",
    schema = {
        type = "string",
        enum = ["south", "otherwise"],
        definitions = {
            south = { in = [{ var = "city" }, ["Bangalore", "Chennai"]] }
        }
    }
}

# Bike rides are cheaper
[[overrides]]
_context_ = { vehicle_type = "bike" }
per_km_rate = 15.0

# Cab rides have premium pricing
[[overrides]]
_context_ = { vehicle_type = "cab" }
per_km_rate = 25.0

# Bangalore cabs have specific rate
[[overrides]]
_context_ = { city = "Bangalore", vehicle_type = "cab" }
per_km_rate = 22.0

# Early morning surge in Delhi
[[overrides]]
_context_ = { city = "Delhi", vehicle_type = "cab", hour_of_day = 6 }
surge_factor = 5.0

# Evening surge in Delhi
[[overrides]]
_context_ = { city = "Delhi", vehicle_type = "cab", hour_of_day = 18 }
surge_factor = 5.0

# South India cohort pricing
[[overrides]]
_context_ = { city_cohort = "south" }
base_fare = 40.0
```

---

## JSON Equivalent

The same configuration in JSON format:

```json
{
    "default-configs": {
        "per_km_rate": { "value": 20.0, "schema": { "type": "number", "minimum": 0 } },
        "surge_factor": { "value": 0.0, "schema": { "type": "number", "minimum": 0 } },
        "base_fare": { "value": 50.0, "schema": { "type": "number", "minimum": 0 } }
    },
    "dimensions": {
        "city": {
            "position": 4,
            "schema": { "type": "string", "enum": ["Chennai", "Bangalore", "Delhi"] }
        },
        "vehicle_type": {
            "position": 2,
            "schema": { "type": "string", "enum": ["auto", "cab", "bike"] }
        },
        "hour_of_day": {
            "position": 3,
            "schema": { "type": "integer", "minimum": 0, "maximum": 23 }
        },
        "city_cohort": {
            "position": 1,
            "type": "LOCAL_COHORT:city",
            "schema": {
                "type": "string",
                "enum": ["south", "otherwise"],
                "definitions": {
                    "south": { "in": [{ "var": "city" }, ["Bangalore", "Chennai"]] }
                }
            }
        }
    },
    "overrides": [
        { "_context_": { "vehicle_type": "bike" }, "per_km_rate": 15.0 },
        { "_context_": { "vehicle_type": "cab" }, "per_km_rate": 25.0 },
        { "_context_": { "city": "Bangalore", "vehicle_type": "cab" }, "per_km_rate": 22.0 },
        { "_context_": { "city": "Delhi", "vehicle_type": "cab", "hour_of_day": 6 }, "surge_factor": 5.0 },
        { "_context_": { "city": "Delhi", "vehicle_type": "cab", "hour_of_day": 18 }, "surge_factor": 5.0 },
        { "_context_": { "city_cohort": "south" }, "base_fare": 40.0 }
    ]
}
```

---

## Validation Rules

### Meta Validation

1. `[meta]` is optional (a file without it is read as version `1`), but when present it must have both `type` and `version`
2. `type` must be `"supertoml"`
3. `version` must be between `1` and the latest version the parser supports

### Default Configs Validation

1. Every config key must have both `value` and `schema`
2. The `value` must validate against the `schema`
3. Schema must be a valid JSON Schema

### Dimensions Validation

1. Every dimension must have `position` and `schema`
2. Positions must be unique
3. Position 0 is reserved for `variantIds`
4. Cohort dimensions must reference existing dimensions
5. Cohort dimensions must have position ≤ their referenced dimension

### Overrides Validation

1. Every override must have a `_context_`
2. Context dimensions must be declared in `[dimensions]`
3. Context values must validate against dimension schemas
4. Override keys must exist in `[default-configs]`
5. Override values must validate against their schemas
