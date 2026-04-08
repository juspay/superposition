---
sidebar_position: 3
title: Context Aware Config
description: Context Aware Config component of Superposition platform
---


# Context Aware Config
---

Context Aware Config (abbreviated as CAC) is the foundational service of the Superposition Platform.  It is the configuration management system that can override configuration values under certain domain contexts.

  - [Concepts](#concepts)
    - [Default Configs](#default-configs)
    - [Dimensions](#dimensions)
    - [Context](#context)
    - [Overrides](#overrides)
    - [Functions](#functions)
    - [Type Templates](#type-templates)
  - [How CAC Works](#how-cac-works)

## Concepts
---

Context-Aware-Config is made of 6 key abstractions - default-configs, dimensions, context, overrides, functions, and type templates.


### Default Configs

Default Configs is the set of all possible configuration keys of your application along with their default values.  One may think of default configs as the best assumption we can make about our configs.

Let us use a [simple cab ride-hailing application](https://github.com/juspay/superposition/tree/main/examples/superposition-demo-app) that operates in different cities and capture some attributes that we might need configurable at a per city level.

1.  `per_distance_unit_rate`
2.  `surge_factor`

CAC supports configurations written using the [TOML format](https://toml.io/en/).  This section and the following sections will use the TOML format to illustrate each abstraction.

```toml
[default-config]
per_km_rate = { "value" = 20.0, "schema" = { "type" = "number" } }
surge_factor = { "value" = 0.0, "schema" = { "type" = "number" } }
```

### Dimensions

Dimensions are attributes of your domain that can potentially govern the values that a particular configuration can take. They define the segmentation criteria used to create different contexts for configuration overrides.

In our example application, dimensions that could change the configuration values could be one or more of the following:

1. `city` - city where the user is hailing the ride - e.g. Bangalore (India), Delhi (India)
2. `vehicle_type` - type of vehicle  the user is hailing - e.g. cab (4-wheeler), auto (3-wheeler), bike (2-wheeler)
3. `hour_of_day` - a number ranging between 0 and 23 denoting the hour of the day the ride is taken

The value of the default configuration could change based on which city the user is hailing the ride and the hour of the day.

```toml
[dimensions]
city = { schema = { "type" = "string", "enum" = ["Bangalore", "Delhi"] } }
vehicle_type = { schema = { "type" = "string", "enum" = [
    "auto",
    "cab",
    "bike",
] } }
hour_of_day = { schema = { "type" = "integer", "minimum" = 0, "maximum" = 23 }}
```

#### Dimension Properties

Each dimension has several key properties that control its behavior:

- **Schema**: JSON Schema definition that validates the values this dimension can accept
- **Position**: Hierarchical ordering that determines evaluation precedence (position 0 is reserved for `variantIds`)
- **Dependencies**: Other dimensions that this dimension depends on for evaluation
- **Functions**: Optional validation and autocomplete functions for custom logic
- **Description**: Human-readable explanation of the dimension's purpose

#### Dimension Hierarchy and Dependencies

Dimensions can have dependencies on other dimensions, creating a hierarchy that ensures proper evaluation order:

```toml
[dimensions]
# Base dimension - no dependencies
region = { schema = { "type" = "string", "enum" = ["north", "south"] } }

# Depends on region
city = { 
    schema = { "type" = "string", "enum" = ["delhi", "mumbai", "bangalore", "hyderabad"] },
    dependencies = ["region"]
}

# Depends on city
zone = { 
    schema = { "type" = "string" },
    dependencies = ["city"]
}
```

This hierarchy ensures that:
- `region` is evaluated first (lowest position)
- `city` is evaluated after `region` is available
- `zone` is evaluated after both `region` and `city` are available

#### Built-in Dimensions

Every workspace automatically includes the `variantIds` dimension:

- **variantIds**: Reserved dimension (position 0) used for experimentation and A/B testing
- **Schema**: Array of strings representing experiment variant identifiers
- **Usage**: Automatically populated during experiment evaluation

#### Dimension Validation

Dimensions support multiple validation approaches:

1. **JSON Schema Validation**: Basic type checking, enums, ranges, patterns
2. **Function Validation**: Custom JavaScript functions for complex business logic
3. **Dependency Validation**: Ensures dependent dimensions have valid values

**Example with Function Validation:**
```toml
[dimensions]
user_tier = { 
    schema = { "type" = "string", "enum" = ["bronze", "silver", "gold", "platinum"] },
    function_name = "validate_user_tier"
}
```

The `validate_user_tier` function could perform additional checks like verifying the user actually has the claimed tier based on external data.

#### Advanced Dimension Features

**Autocomplete Support:**
```toml
[dimensions]
city_code = {
    schema = { "type" = "string", "pattern" = "^[A-Z]{3}$" },
    autocomplete_function_name = "suggest_city_codes"
}
```

**Complex Schema Types:**
```toml
[dimensions]
geo_location = {
    schema = {
        "type" = "object",
        "properties" = {
            "lat" = { "type" = "number", "minimum" = -90, "maximum" = 90 },
            "lng" = { "type" = "number", "minimum" = -180, "maximum" = 180 }
        },
        "required" = ["lat", "lng"]
    }
}
```

### Context

A Context is a logical expression built using dimensions as variables.  In CAC configuration files, this expression is parsed using the [pest crate](https://crates.io/crates/pest).

```pest
expression = { SOI ~ whitespace* ~ logical ~ whitespace* ~ EOI }

logical = _{ logical_or }
logical_or = { logical_and ~ (whitespace* ~ "||" ~ whitespace* ~ logical_and)* }
logical_and = { comparison ~ (whitespace* ~ "&&" ~ whitespace* ~ comparison)* }

comparison = { term ~ whitespace* ~ comparison_operator ~ whitespace* ~ term }
comparison_operator = { ">=" | "<=" | "<" | ">" | "==" | "!=" }
term = { bool_literal | string_literal | float | integer | dimension }

string_literal = @{ "'" ~ char+ ~ "'" }
bool_literal = @{ "true" | "false" }
integer = @{ ASCII_DIGIT+ }
float = @{ ASCII_DIGIT+ ~ "." ~ ASCII_DIGIT+ }
char = { ASCII_ALPHANUMERIC | "." | "_" }
dimension = @{ "$" ~ char+ }

whitespace = _{ " " | "\t" | "\n" }
```

Examples of contexts:

- `[context."$vehicle_type == 'cab'"]`
- `[context."$city == 'Delhi' && $vehicle_type == 'cab' && $hour_of_day <= 6"]`

### Overrides

Overrides are a subset of the configurations from Default Config typically with different values.  Overrides are always associated with contexts and are applied when a context evaluates to `true`. 

Example:

Let us configure different overrides for configuration keys under different contexts in our example.

```toml
[context."$vehicle_type == 'cab'"]
per_km_rate = 25.0

[context."$vehicle_type == 'bike'"]
per_km_rate = 15.0

[context."$city == 'Bangalore' && $vehicle_type == 'cab'"]
per_km_rate = 22.0

[context."$city == 'Delhi' && $vehicle_type == 'cab' && $hour_of_day >= 18"]
surge_factor = 5.0

[context."$city == 'Delhi' && $vehicle_type == 'cab' && $hour_of_day <= 6"]
surge_factor = 5.0
```

### Functions

Functions in Superposition are JavaScript code snippets that provide custom validation and autocomplete functionality for configuration dimensions and default configs. They enable dynamic validation rules and intelligent form suggestions that go beyond basic JSON Schema validation.

#### Function Types

Superposition supports two types of functions:

1. **Validation Functions** - Provide custom validation logic for dimension values and configuration keys
2. **Autocomplete Functions** - Generate dynamic suggestions for form fields based on context

#### Validation Functions

Validation functions allow you to implement complex business logic for validating dimension values or configuration values that cannot be expressed through JSON Schema alone.

**Function Signature:**
```javascript
async function validate(key, value) {
    // Custom validation logic here
    // Return true if valid, false if invalid
    return boolean;
}
```

**Example - Validating City Codes:**
```javascript
async function validate(key, value) {
    // Validate that city codes follow specific business rules
    const validCities = ['BLR', 'DEL', 'MUM', 'HYD'];
    if (key === 'city_code') {
        return validCities.includes(value) && value.length === 3;
    }
    return true;
}
```

#### Autocomplete Functions

Autocomplete functions provide dynamic suggestions for form fields, enabling intelligent data entry and reducing errors.

**Function Signature:**
```javascript
async function autocomplete(name, prefix, environment) {
    // Generate suggestions based on name, prefix, and environment
    // Return array of suggestion strings
    return string[];
}
```

**Example - City Suggestions:**
```javascript
async function autocomplete(name, prefix, environment) {
    if (name === 'city' && prefix.length >= 2) {
        const cities = ['Bangalore', 'Delhi', 'Mumbai', 'Hyderabad', 'Chennai'];
        return cities.filter(city => 
            city.toLowerCase().startsWith(prefix.toLowerCase())
        );
    }
    return [];
}
```

#### Function Lifecycle

Functions have a draft and published lifecycle:

- **Draft Stage**: Functions can be created, edited, and tested without affecting live configurations
- **Testing**: Functions can be tested in both draft and published stages to verify behavior
- **Publishing**: Draft functions can be published to become the active version used in validation
- **Execution**: Functions run in a secure Node.js sandbox with controlled timeouts and limited system access

#### Security and Sandboxing

Functions execute in a secure environment with:
- 10-second execution timeout limits
- Isolated Node.js worker processes  
- Limited environment variable access
- No file system access beyond execution context
- Access to HTTP libraries for external API calls (via axios)

### Type Templates

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

## How CAC Works
---

This section shows a complete configuration and how 

- Your application adds the CAC client as a library
- At runtime, your application will provide a context to the client for evaluation. Using our ride hailing example from earlier, the context provided by your application would look like:
    ```json
    {
        "city": "Delhi", 
        "vehicle_type": "cab", 
        "hour_of_day": 19
    }
    ```
- CAC will apply all contexts it has available, and return the final configuration for the application. If no rule applies for a configuration, it's default value will be returned. From our earlier examples (check Default Config and Overrides section) the configuration returned would be
    ```json
    {
        "per_km_rate": 25.0,
        "surge_factor": 5.0
    }
    ```

### Analogy with CSS 

For people familiar with how CSS works - the following table comparing CAC to CSS will help them grasp CAC's structure quickly

| CSS | Context Aware Configuration |
|-----|---------------------------|
| `body {`<br/>&nbsp;&nbsp;`color: black;`<br/>`}` | `default {`<br/>&nbsp;&nbsp;`color: black`<br/>`}` |
| `#main {`<br/>&nbsp;&nbsp;`color: green;`<br/>`}` | `[id="main"] {`<br/>&nbsp;&nbsp;`color: green`<br/>`}` |
| `.module {`<br/>&nbsp;&nbsp;`color: blue;`<br/>`}` | `[class="module"] {`<br/>&nbsp;&nbsp;`color: blue`<br/>`}` |
| `#main .module {`<br/>&nbsp;&nbsp;`color: orange`<br/>`}` | `[id="main, class="module"] {`<br/>&nbsp;&nbsp;`color: orange`<br/>`}` |

The following mapping of concepts between CAC and CSS will also be useful:

| CSS | Context Aware Configuration |
|-----|---------------------------|
| default stylesheet | default config |
| property | configuration key |
| value | value (typed) |
| selector | context<br/>(variables involved in contexts are called dimensions) |
| set of property:values | override |
| selector + set of property:value | contextual override |

## Complete CAC configuration file in TOML format
The complete context-aware-configuration file for the above example is shown below:

```toml
[default-config]
per_km_rate = { "value" = 20.0, "schema" = { "type" = "number" } }
surge_factor = { "value" = 0.0, "schema" = { "type" = "number" } }

[dimensions]
city = { schema = { "type" = "string", "enum" = ["Bangalore", "Delhi"] } }
vehicle_type = { schema = { "type" = "string", "enum" = [ "auto", "cab", "bike", ] } }
hour_of_day = { schema = { "type" = "integer", "minimum" = 0, "maximum" = 23 }}

[context."$vehicle_type == 'cab'"]
per_km_rate = 25.0

[context."$vehicle_type == 'bike'"]
per_km_rate = 15.0

[context."$city == 'Bangalore' && $vehicle_type == 'cab'"]
per_km_rate = 22.0

[context."$city == 'Delhi' && $vehicle_type == 'cab' && $hour_of_day >= 18"]
surge_factor = 5.0

[context."$city == 'Delhi' && $vehicle_type == 'cab' && $hour_of_day <= 6"]
surge_factor = 5.0
```

This file is also available in the caclang crate - [example.cac.toml](example.cac.toml).
