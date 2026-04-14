---
sidebar_position: 7
title: Context Expressions
description: Defining context conditions in SuperTOML
---

# Context Expressions

Context expressions define when an override should apply. In SuperTOML, contexts are defined using the `_context_` object with equality matching.

## Context Matching with `_context_`

The `_context_` object defines when an override applies:

```toml
[[overrides]]
_context_ = { vehicle_type = "cab" }
per_km_rate = 25.0
```

This override applies when the runtime context has `vehicle_type = "cab"`.

### Multiple Conditions

All conditions in `_context_` must match (AND logic):

```toml
[[overrides]]
_context_ = { city = "Bangalore", vehicle_type = "cab" }
per_km_rate = 22.0
```

This applies when **both** `city = "Bangalore"` AND `vehicle_type = "cab"`.

### How Matching Works

A context matches when:

- All dimension values in `_context_` equal the corresponding runtime values
- Unspecified dimensions act as wildcards (match any value)

```toml
# Matches any cab ride, regardless of city or time
[[overrides]]
_context_ = { vehicle_type = "cab" }
per_km_rate = 25.0

# Matches only Bangalore cabs
[[overrides]]
_context_ = { city = "Bangalore", vehicle_type = "cab" }
per_km_rate = 22.0
```

## Limitations of `_context_`

The `_context_` object only supports **equality matching**. It does not support:

- Inequality (`!=`)
- Numeric comparisons (`<`, `>`, `<=`, `>=`)
- OR logic
- Range checks

For these use cases, use **LOCAL_COHORT dimensions** with JSONLogic definitions.

## Complex Conditions with LOCAL_COHORT

When you need conditions beyond simple equality, create a LOCAL_COHORT dimension that derives its value using JSONLogic.

### What is JSONLogic?

[JSONLogic](https://jsonlogic.com/) is a rules engine that allows you to define conditional logic as JSON. SuperTOML uses JSONLogic in the `definitions` field of cohort dimension schemas.

### Common JSONLogic Operations

| Operation        | Description                     |
| ---------------- | ------------------------------- |
| Equality         | Check if `values are equal`     |
| Inequality       | Check if `values are not equal` |
| Greater than     | Check if `value1 > value2`      |
| Less than        | Check if `value1 < value2`      |
| Greater or equal | Check if `value1 >= value2`     |
| Less or equal    | Check if `value1 <= value2`     |
| In list          | Check if `value is in array`    |
| AND              | Both conditions must be true    |
| OR               | Either condition must be true   |
| NOT              | Negate a condition              |

### JSONLogic Syntax Examples

The following examples show JSONLogic syntax. Use these patterns in the `definitions` field:

```text
Equality:        {"==": [value1, value2]}
Inequality:      {"!=": [value1, value2]}
Greater than:    {">": [value1, value2]}
Less than:       {"<": [value1, value2]}
Greater/equal:   {">=": [value1, value2]}
Less/equal:      {"<=": [value1, value2]}
In list:         {"in": [value, array]}
AND:             {"and": [condition1, condition2]}
OR:              {"or": [condition1, condition2]}
NOT:             {"!": condition}
```

### Referencing Dimensions

To reference a dimension value in JSONLogic, use the "var" operator:

```text
{"var": "dimension_name"}
```

Example - check if hour_of_day >= 18:

```text
{">=": [{"var": "hour_of_day"}, 18]}
```

## Examples

### Numeric Range: Time Periods

Create a cohort for time periods (morning rush, evening rush, off-peak):

```toml
[dimensions]
hour_of_day = {
    position = 3,
    schema = { type = "integer", minimum = 0, maximum = 23 }
}

time_period = {
    position = 1,
    type = "LOCAL_COHORT:hour_of_day",
    schema = {
        type = "string",
        enum = ["morning_rush", "evening_rush", "off_peak"],
        definitions = {
            morning_rush = { "and": [
                { ">=": [{ "var": "hour_of_day" }, 7] },
                { "<=": [{ "var": "hour_of_day" }, 10] }
            ]},
            evening_rush = { "and": [
                { ">=": [{ "var": "hour_of_day" }, 17] },
                { "<=": [{ "var": "hour_of_day" }, 21] }
            ]}
        }
    }
}

# Use the cohort in overrides
[[overrides]]
_context_ = { time_period = "morning_rush" }
surge_factor = 1.5

[[overrides]]
_context_ = { time_period = "evening_rush" }
surge_factor = 2.0
```

### Inequality: Excluding Values

Create a cohort that excludes a specific city:

```toml
[dimensions]
city = {
    position = 4,
    schema = { type = "string", enum = ["Bangalore", "Delhi", "Chennai", "Mumbai"] }
}

is_not_delhi = {
    position = 1,
    type = "LOCAL_COHORT:city",
    schema = {
        type = "string",
        enum = ["yes", "no"],
        definitions = {
            yes = { "!=": [{ "var": "city" }, "Delhi"] }
        }
    }
}

[[overrides]]
_context_ = { is_not_delhi = "yes" }
base_fare = 45.0
```

### OR Logic: Multiple Cities

Create a cohort for a region (multiple cities):

```toml
[dimensions]
city = {
    position = 4,
    schema = { type = "string", enum = ["Bangalore", "Chennai", "Delhi", "Mumbai"] }
}

region = {
    position = 1,
    type = "LOCAL_COHORT:city",
    schema = {
        type = "string",
        enum = ["south", "north", "west", "otherwise"],
        definitions = {
            south = { "in": [{ "var": "city" }, ["Bangalore", "Chennai"]] },
            north = { "in": [{ "var": "city" }, ["Delhi"]] },
            west = { "in": [{ "var": "city" }, ["Mumbai"]] }
        }
    }
}

[[overrides]]
_context_ = { region = "south" }
per_km_rate = 18.0

[[overrides]]
_context_ = { region = "north" }
per_km_rate = 22.0
```

### Complex Conditions: Peak Hours in Specific City

Combine multiple conditions:

```toml
[dimensions]
city = { position = 4, schema = { type = "string" } }
hour_of_day = { position = 3, schema = { type = "integer", minimum = 0, maximum = 23 } }

delhi_peak_hours = {
    position = 1,
    type = "LOCAL_COHORT:city",
    schema = {
        type = "string",
        enum = ["yes", "no"],
        definitions = {
            yes = { "and": [
                { "==": [{ "var": "city" }, "Delhi"] },
                { "or": [
                    { "and": [
                        { ">=": [{ "var": "hour_of_day" }, 8] },
                        { "<=": [{ "var": "hour_of_day" }, 10] }
                    ]},
                    { "and": [
                        { ">=": [{ "var": "hour_of_day" }, 18] },
                        { "<=": [{ "var": "hour_of_day" }, 21] }
                    ]}
                ]}
            ]}
        }
    }
}

[[overrides]]
_context_ = { delhi_peak_hours = "yes" }
surge_factor = 2.5
```

### Range Check: Age Groups

```toml
[dimensions]
user_age = {
    position = 5,
    schema = { type = "integer", minimum = 0, maximum = 120 }
}

age_group = {
    position = 1,
    type = "LOCAL_COHORT:user_age",
    schema = {
        type = "string",
        enum = ["youth", "adult", "senior"],
        definitions = {
            youth = { "<": [{ "var": "user_age" }, 25] },
            senior = { ">=": [{ "var": "user_age" }, 60] }
        }
    }
}

[[overrides]]
_context_ = { age_group = "youth" }
discount = 0.2

[[overrides]]
_context_ = { age_group = "senior" }
discount = 0.3
```

## Best Practices

### 1. Use `_context_` for Simple Equality

```toml
# Good: Simple equality check
[[overrides]]
_context_ = { vehicle_type = "cab" }
per_km_rate = 25.0
```

### 2. Use Cohorts for Complex Conditions

```toml
# Good: Complex condition via cohort
[dimensions]
hour_of_day = { position = 3, schema = { type = "integer", minimum = 0, maximum = 23 } }

is_peak_hour = {
    position = 1,
    type = "LOCAL_COHORT:hour_of_day",
    schema = {
        type = "string",
        enum = ["yes", "no"],
        definitions = {
            yes = { "or": [
                { "and": [
                    { ">=": [{ "var": "hour_of_day" }, 7] },
                    { "<=": [{ "var": "hour_of_day" }, 10] }
                ]},
                { "and": [
                    { ">=": [{ "var": "hour_of_day" }, 17] },
                    { "<=": [{ "var": "hour_of_day" }, 21] }
                ]}
            ]}
        }
    }
}

[[overrides]]
_context_ = { is_peak_hour = "yes" }
surge_factor = 1.5
```

### 3. Name Cohorts Meaningfully

```toml
# Good: Clear cohort name
is_premium_user = { ... }
delhi_peak_hours = { ... }
south_region = { ... }

# Avoid: Unclear names
condition_1 = { ... }
check = { ... }
```

### 4. Always Include a Default Value

Cohorts should have a default value for unmatched cases:

```toml
schema = {
    type = "string",
    enum = ["yes", "no"],  # "no" is the default for unmatched
    definitions = {
        yes = { ... }  # Only define the "yes" condition
    }
}
```

### 5. Keep Cohort Position Lower Than Base

```toml
# Good: Cohort position (1) < base position (4)
city = { position = 4, ... }
region = { position = 1, type = "LOCAL_COHORT:city", ... }
```

## Validation

Context conditions are validated at parse time:

1. **Dimension existence**: All dimensions in `_context_` must be declared
2. **Value validity**: Values must match dimension schemas
3. **Cohort definitions**: JSONLogic must be valid and reference existing dimensions

### Example Errors

```toml
# Error: Unknown dimension
[[overrides]]
_context_ = { unknown_dimension = "value" }
per_km_rate = 25.0
# Error: Dimension 'unknown_dimension' not found

# Error: Invalid enum value
[dimensions]
city = { position = 4, schema = { type = "string", enum = ["Bangalore", "Delhi"] } }

[[overrides]]
_context_ = { city = "Mumbai" }
per_km_rate = 25.0
# Error: Value "Mumbai" not in enum ["Bangalore", "Delhi"]
```

## Summary

| Condition Type                            | Method                                        |
| ----------------------------------------- | --------------------------------------------- |
| Simple equality                           | Use `_context_` directly                      |
| Inequality (`!=`)                         | Create LOCAL_COHORT with `!=` JSONLogic       |
| Numeric comparison (`<`, `>`, `<=`, `>=`) | Create LOCAL_COHORT with comparison JSONLogic |
| OR logic                                  | Create LOCAL_COHORT with `or` JSONLogic       |
| Range check                               | Create LOCAL_COHORT with `and` + comparisons  |
| Membership check                          | Create LOCAL_COHORT with `in` JSONLogic       |

The `_context_` object with equality matching is the primary way to define conditions. For complex conditions, create LOCAL_COHORT dimensions using JSONLogic as the definition language.
