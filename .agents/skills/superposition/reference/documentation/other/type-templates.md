---
sidebar_position: 2
title: Type Templates
---

Type Templates are reusable JSON Schema definitions that serve as standardized data type specifications for configuration values. They provide a way to define and enforce consistent data types and validation rules across the entire configuration management system.

#### Purpose and Benefits

Type Templates enable:

- **Reusability**: Define complex data types once and use them across multiple configurations
- **Consistency**: Standardized type definitions across all configurations in your organization
- **Validation**: Built-in JSON Schema validation prevents invalid data entry
- **UI Generation**: Automatic form generation based on type definitions
- **Maintainability**: Centralized type definitions make schema updates easier

#### Built-in Type Templates

Superposition comes with several predefined type templates:

```json
{
  "Number": { "type": "integer" },
  "Decimal": { "type": "number" },
  "Boolean": { "type": "boolean" },
  "Enum": { "type": "string", "enum": ["android", "ios"] },
  "Pattern": { "type": "string", "pattern": ".*" }
}
```

#### Creating Custom Type Templates

You can create custom type templates for complex data validation requirements:

**Email Template:**
```json
{
  "type": "string",
  "format": "email",
  "pattern": "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"
}
```

**Geographic Coordinate Template:**
```json
{
  "type": "object",
  "properties": {
    "latitude": {
      "type": "number",
      "minimum": -90,
      "maximum": 90
    },
    "longitude": {
      "type": "number", 
      "minimum": -180,
      "maximum": 180
    }
  },
  "required": ["latitude", "longitude"]
}
```

**Currency Amount Template:**
```json
{
  "type": "object",
  "properties": {
    "amount": {
      "type": "number",
      "minimum": 0
    },
    "currency": {
      "type": "string",
      "enum": ["USD", "EUR", "INR", "GBP"]
    }
  },
  "required": ["amount", "currency"]
}
```

#### Usage in Configuration

Type templates are used when defining:

1. **Default Configurations**: Specify the data type and validation rules for configuration keys
2. **Dimension Schemas**: Define the allowed values and validation for dimension attributes
3. **Override Validation**: Ensure override values conform to the expected data types

**Example Usage in Default Config:**
```toml
[default-config]
per_km_rate = { "value" = 20.0, "schema" = { "$ref": "#/types/Decimal" } }
user_location = { "value" = {"lat": 0.0, "lng": 0.0}, "schema" = { "$ref": "#/types/GeographicCoordinate" } }
```

#### Type Template Management

Type templates support full lifecycle management:

- **Creation**: Define new type templates with JSON Schema validation
- **Versioning**: Track changes with audit trail (created_by, last_modified_by, change_reason)
- **Updates**: Modify existing templates with automatic validation
- **Dependencies**: Templates can reference other templates for composition
- **Deletion**: Remove unused templates with dependency checking