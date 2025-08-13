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
  - [How CAC Works](#how-cac-works)

## Concepts
---

Context-Aware-Config is made of 4 key abstractions - default-configs, dimensions, context and overrides.


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

Dimensions are typically attributes of your domain which can potentially govern the values that a particular configuration can take.

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
