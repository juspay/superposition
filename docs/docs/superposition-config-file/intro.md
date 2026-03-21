---
sidebar_position: 1
title: Introduction to SuperTOML
description: Type-safe, cascading configuration files for modern applications
---

# Superposition Config File (SuperTOML)

SuperTOML is an extension of [TOML](https://toml.io/) that addresses the challenges of managing configuration at scale. It adds two powerful capabilities to standard TOML:

1. **Type Safety via JSON Schema** - Every configuration value is validated against a schema, catching errors before they reach production
2. **Cascading Configuration** - A CSS-inspired model for overriding configurations based on application context

## Why SuperTOML?

TOML configuration usually starts simple, but it doesn't stay that way for long. As systems grow, you end up dealing with:

- **Multiple environments** (development, staging, production)
- **Multiple tenants** (different customers with different needs)
- **Feature rollouts** (canary releases, A/B tests)
- **Context-specific overrides** (per-region, per-user-segment settings)

At that point, plain TOML falls short. You either:

- Duplicate configs everywhere, leading to maintenance nightmares
- Move logic into application code, which becomes hard to reason about

SuperTOML solves this by bringing **type-safety** and **cascading** directly into the configuration file format.

## Key Features

### Type Safety

Every configuration value has an associated JSON Schema that validates it:

```toml
[default-configs]
per_km_rate = { value = 20.0, schema = { type = "number", minimum = 0 } }
surge_factor = { value = 0.0, schema = { type = "number", minimum = 0, maximum = 10 } }

[dimensions]
city = { position = 3, schema = { type = "string", enum = ["Chennai", "Bangalore", "Delhi"] } }
vehicle_type = { position = 1, schema = { type = "string", enum = ["auto", "cab", "bike"] } }
hour_of_day = { position = 2, schema = { type = "integer", minimum = 0, maximum = 23 } }
```

This extends Rust-like type-safety to configuration files, regardless of which programming language consumes them.

### Cascading Overrides

SuperTOML uses a CSS-inspired cascading model. You define default values, then override them for specific contexts:

```toml
[default-configs]
per_km_rate = { value = 20.0, schema = { type = "number" } }

[dimensions]
city = { position = 2, schema = { type = "string", enum = ["Chennai", "Bangalore", "Delhi"] } }
vehicle_type = { position = 1, schema = { type = "string", enum = ["auto", "cab", "bike"] } }

[[overrides]]
_context_ = { vehicle_type = "cab" }
per_km_rate = 25.0

[[overrides]]
_context_ = { city = "Bangalore", vehicle_type = "cab" }
per_km_rate = 22.0
```

When a cab ride happens in Bangalore, the most specific override (Bangalore + cab) wins.

### Deterministic Resolution

SuperTOML resolves configuration values deterministically:

- Each dimension has a **position** that determines its weight
- Contexts with more dimensions (or higher-weight dimensions) have higher priority
- Conflicts are resolved unambiguously - no random behavior

### Fully TOML Compatible

SuperTOML files are valid TOML files. Any TOML parser can read them, though the type-safety and cascading features are only available through Superposition libraries.

## Quick Example

Here's a complete SuperTOML file for a ride-hailing application:

```toml
[default-configs]
per_km_rate = { value = 20.0, schema = { type = "number" } }
surge_factor = { value = 0.0, schema = { type = "number" } }

[dimensions]
city = { position = 3, schema = { type = "string", enum = ["Chennai", "Bangalore", "Delhi"] } }
vehicle_type = { position = 1, schema = { type = "string", enum = ["auto", "cab", "bike"] } }
hour_of_day = { position = 2, schema = { type = "integer", minimum = 0, maximum = 23 } }

[[overrides]]
_context_ = { vehicle_type = "cab" }
per_km_rate = 25.0

[[overrides]]
_context_ = { vehicle_type = "bike" }
per_km_rate = 15.0

[[overrides]]
_context_ = { city = "Delhi", vehicle_type = "cab", hour_of_day = 18 }
surge_factor = 5.0
```

## JSON Format

SuperTOML configurations can also be written in JSON - both formats are equivalent:

```json
{
    "default-configs": {
        "per_km_rate": { "value": 20.0, "schema": { "type": "number" } }
    },
    "dimensions": {
        "vehicle_type": { "position": 1, "schema": { "type": "string", "enum": ["cab"] } }
    },
    "overrides": [{ "_context_": { "vehicle_type": "cab" }, "per_km_rate": 25.0 }]
}
```

## Language Support

SuperTOML can be consumed from multiple programming languages:

| Language   | Library                  | Description                     |
| ---------- | ------------------------ | ------------------------------- |
| Rust       | `superposition_provider` | Native Rust implementation      |
| JavaScript | `superposition-provider` | OpenFeature compatible provider |
| Python     | `superposition_provider` | OpenFeature compatible provider |
| Java       | `openfeature-provider`   | OpenFeature compatible provider |

All languages support the same configuration format and resolution logic.

## LSP Support

SuperTOML has full Language Server Protocol support, providing:

- **Autocomplete** - Suggestions for dimension names, config keys, and enum values
- **Validation** - Real-time schema validation with error diagnostics
- **Hover Information** - Documentation and type information on hover
- **Go to Definition** - Navigate to dimension or config definitions

This makes editing SuperTOML files as comfortable as working with code in an IDE.

## Next Steps

- [Format Specification](./format-specification) - Complete syntax reference
- [Type Safety](./type-safety) - Deep dive into JSON Schema validation
- [Cascading Model](./cascading-model) - How overrides cascade
- [Deterministic Resolution](./deterministic-resolution) - Priority and conflict resolution
- [Examples](./examples) - Complete working examples
- [Config File Compatibility](./config-file-compatibility) - Common Linux/macOS configs as SuperTOML
