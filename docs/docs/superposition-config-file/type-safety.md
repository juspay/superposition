---
sidebar_position: 3
title: Type Safety
description: JSON Schema validation for configuration values
---

# Type Safety

SuperTOML brings type-safety to configuration files through [JSON Schema](https://json-schema.org/). Every configuration value and dimension is validated against a schema, catching errors before they reach production.

## Why Type Safety Matters

In an AI-powered world where code is increasingly generated, type-safety is like immunity. Configuration files deserve the same level of type-safety as your programs because:

1. **Configuration bugs are runtime bugs** - A typo in a config file can crash your application
2. **Configs change independently** - They're not version-controlled alongside code
3. **Multiple teams edit configs** - Without validation, mistakes are inevitable
4. **AI generates configs too** - Type-safety ensures generated configs are valid

## Schema Definition

Every configuration value in SuperTOML has an associated [JSON Schema](https://json-schema.org/):

```toml
[default-configs]
per_km_rate = {
    value = 20.0,
    schema = { type = "number", minimum = 0, maximum = 100 }
}
```

The schema validates:

- The default value itself
- Any override values for this key
- Values returned from resolution

## JSON Schema Types

### Primitive Types

```toml
[default-configs]
# String
app_name = { value = "MyApp", schema = { type = "string" } }

# Integer
port = { value = 8080, schema = { type = "integer", minimum = 1, maximum = 65535 } }

# Number (float)
rate = { value = 1.5, schema = { type = "number", minimum = 0 } }

# Boolean
enabled = { value = true, schema = { type = "boolean" } }

# Null
optional_value = { value = "null", schema = { type = "null" } }
```

### String Constraints

```toml
[default-configs]
# Email format
admin_email = {
    value = "admin@example.com",
    schema = { type = "string", format = "email" }
}

# Pattern matching
api_key = {
    value = "sk_test_1234567890",
    schema = { type = "string", pattern = "^sk_[a-z]+_[a-zA-Z0-9]+$" }
}

# Length constraints
description = {
    value = "A short description",
    schema = { type = "string", minLength = 10, maxLength = 500 }
}
```

### Numeric Constraints

```toml
[default-configs]
# Integer with range
priority = {
    value = 5,
    schema = { type = "integer", minimum = 1, maximum = 10 }
}

# Number with exclusive bounds
discount = {
    value = 0.1,
    schema = {
        type = "number",
        exclusiveMinimum = 0,
        exclusiveMaximum = 1
    }
}

# Multiple of
batch_size = {
    value = 100,
    schema = { type = "integer", multipleOf = 10 }
}
```

### Enumerations

```toml
[default-configs]
# String enum
log_level = {
    value = "info",
    schema = {
        type = "string",
        enum = ["debug", "info", "warn", "error"]
    }
}

# Integer enum
status_code = {
    value = 200,
    schema = { type = "integer", enum = [200, 201, 400, 404, 500] }
}
```

### Arrays

```toml
[default-configs]
# Array of strings
allowed_hosts = {
    value = ["localhost", "example.com"],
    schema = {
        type = "array",
        items = { type = "string" },
        minItems = 1
    }
}

# Array with unique items
tags = {
    value = ["production", "critical"],
    schema = {
        type = "array",
        items = { type = "string" },
        uniqueItems = true
    }
}

# Array with length bounds
features = {
    value = ["feature_a", "feature_b"],
    schema = {
        type = "array",
        items = { type = "string" },
        minItems = 0,
        maxItems = 10
    }
}
```

### Objects

```toml
[default-configs]
database = {
    value = { host = "localhost", port = 5432, ssl = true },
    schema = {
        type = "object",
        properties = {
            host = { type = "string" },
            port = { type = "integer", minimum = 1, maximum = 65535 },
            ssl = { type = "boolean" },
            pool_size = { type = "integer", default = 10 }
        },
        required = ["host", "port"],
        additionalProperties = false
    }
}
```

### Nested Objects

```toml
[default-configs]
server = {
    value = {
        host = "0.0.0.0",
        port = 8080,
        tls = { enabled = true, cert_path = "/certs/server.crt" }
    },
    schema = {
        type = "object",
        properties = {
            host = { type = "string" },
            port = { type = "integer" },
            tls = {
                type = "object",
                properties = {
                    enabled = { type = "boolean" },
                    cert_path = { type = "string" },
                    key_path = { type = "string" }
                },
                required = ["enabled"]
            }
        },
        required = ["host", "port"]
    }
}
```

## Combining Schemas

### All Of

All conditions must be satisfied:

```toml
[default-configs]
username = {
    value = "john_doe",
    schema = {
        allOf = [
            { type = "string", minLength = 3 },
            { type = "string", maxLength = 20 },
            { type = "string", pattern = "^[a-z][a-z0-9_]*$" }
        ]
    }
}
```

### Any Of

At least one condition must be satisfied:

```toml
[default-configs]
identifier = {
    value = "user-123",
    schema = {
        anyOf = [
            { type = "string", format = "uuid" },
            { type = "string", pattern = "^[a-z]+-[0-9]+$" }
        ]
    }
}
```

### One Of

Exactly one condition must be satisfied:

```toml
[default-configs]
port_config = {
    value = 8080,
    schema = {
        oneOf = [
            { type = "integer", minimum = 1, maximum = 65535 },
            { type = "string", enum = ["any", "none"] }
        ]
    }
}
```

### Not

Must not match the schema:

```toml
[default-configs]
not_admin = {
    value = "user",
    schema = {
        type = "string",
        not = { enum = ["admin", "root", "superuser"] }
    }
}
```

## Conditional Schemas

Use `if`, `then`, and `else` for conditional validation:

```toml
[default-configs]
payment_config = {
    value = { method = "card", card_number = "4111111111111111" },
    schema = {
        type = "object",
        properties = {
            method = { type = "string", enum = ["card", "bank"] }
        },
        if = { properties = { method = { const = "card" } } },
        then = {
            properties = {
                card_number = { type = "string", pattern = "^[0-9]{16}$" }
            },
            required = ["card_number"]
        },
        else = {
            properties = {
                account_number = { type = "string" }
            },
            required = ["account_number"]
        }
    }
}
```

## Schema References

Use `$ref` and `definitions` for reusable schemas:

```toml
[default-configs]
address = {
    value = { street = "123 Main St", city = "Bangalore", zip = "560001" },
    schema = {
        type = "object",
        properties = {
            street = { type = "string" },
            city = { type = "string" },
            zip = { "$ref" = "#/definitions/zipCode" }
        },
        definitions = {
            zipCode = { type = "string", pattern = "^[0-9]{6}$" }
        }
    }
}
```

## Type Templates

Superposition supports reusable type templates that can be referenced across configurations:

```toml
# Type templates are defined at the workspace level
# and can be reused across multiple configs

[default-configs]
user_email = {
    value = "user@example.com",
    schema = { "$ref" = "Email" }  # References a type template
}

admin_email = {
    value = "admin@example.com",
    schema = { "$ref" = "Email" }  # Same template, different config
}
```

### Built-in Type Templates

| Template  | Schema                  |
| --------- | ----------------------- |
| `Number`  | `{ "type": "integer" }` |
| `Decimal` | `{ "type": "number" }`  |
| `Boolean` | `{ "type": "boolean" }` |
| `String`  | `{ "type": "string" }`  |

## Validation in Action

### At Parse Time

When parsing a SuperTOML file, all values are validated against their schemas:

```toml
[default-configs]
# This will fail validation - negative value violates minimum = 0
per_km_rate = { value = -5.0, schema = { type = "number", minimum = 0 } }
```

**Error:**

```
ValidationError: default-configs.per_km_rate
  - Value -5.0 is less than minimum 0
```

### For Overrides

Override values are validated against the config's schema:

```toml
[default-configs]
per_km_rate = { value = 20.0, schema = { type = "number", minimum = 0, maximum = 100 } }

[dimensions]
vehicle_type = { position = 1, schema = { type = "string", enum = ["auto", "cab", "bike"] } }

[[overrides]]
_context_ = { vehicle_type = "cab" }
# This will fail - value exceeds maximum
per_km_rate = 150.0
```

**Error:**

```
ValidationError: context[0].per_km_rate
  - Value 150.0 is greater than maximum 100
```

### For Dimensions

Dimension values in contexts are validated against dimension schemas:

```toml
[default-configs]
per_km_rate = { value = 20.0, schema = { type = "number", minimum = 0, maximum = 100 } }

[dimensions]
hour_of_day = { position = 1, schema = { type = "integer", minimum = 0, maximum = 23 } }

[[overrides]]
# This will fail - 25 exceeds maximum 23
_context_ = { hour_of_day = 25 }
per_km_rate = 30.0
```

**Error:**

```
ValidationError: context[0]._context_.hour_of_day
  - Value 25 is greater than maximum 23
```

## Best Practices

### 1. Always Set Constraints

Don't just use `{ "type": "number" }` - add bounds:

```toml
# ❌ Too permissive
rate = { value = 0.5, schema = { type = "number" } }

# ✅ Bounded
rate = { value = 0.5, schema = { type = "number", minimum = 0, maximum = 1 } }
```

### 2. Use Enums for Fixed Values

When values are from a known set, use enums:

```toml
# ❌ String without constraints
environment = { value = "production", schema = { type = "string" } }

# ✅ Enum for known values
environment = {
    value = "production",
    schema = { type = "string", enum = ["development", "staging", "production"] }
}
```

### 3. Document with Descriptions

Add descriptions to schemas for documentation:

```toml
[default-configs]
api_timeout = {
    value = 30,
    schema = {
        type = "integer",
        minimum = 1,
        maximum = 300,
        description = "API timeout in seconds. Must be between 1 and 300."
    }
}
```

### 4. Use Type Templates for Consistency

Define common types once and reuse:

```toml
# Define once
[default-configs]
primary_email = { value = "primary@example.com", schema = { "$ref": "Email" } }
secondary_email = { value = "secondary@example.com", schema = { "$ref": "Email" } }
support_email = { value = "support@example.com", schema = { "$ref": "Email" } }
```

### 5. Validate Object Structures Strictly

Use `required` and `additionalProperties`:

```toml
# ❌ Loose validation
database = {
    value = { host = "localhost" },
    schema = { type = "object" }
}

# ✅ Strict validation
database = {
    value = { host = "localhost", port = 5432 },
    schema = {
        type = "object",
        properties = {
            host = { type = "string" },
            port = { type = "integer", minimum = 1, maximum = 65535 }
        },
        required = ["host", "port"],
        additionalProperties = false
    }
}
```

## JSON Schema Resources

- [JSON Schema Specification](https://json-schema.org/specification)
- [JSON Schema Validation](https://json-schema.org/understanding-json-schema/reference)
- [Online Schema Validator](https://www.jsonschemavalidator.net/)
