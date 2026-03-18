---
sidebar_position: 6
title: Dimensions
description: Understanding dimensions and cohort dimensions in SuperTOML
---

# Dimensions

Dimensions are the segmentation criteria that define how configuration overrides are applied. They represent the attributes of your application context that can influence configuration values.

## Basic Dimensions

A basic dimension has a position and a schema:

```toml
[dimensions]
city = {
    position = 4,
    schema = {
        type = "string",
        enum = ["Bangalore", "Delhi", "Chennai", "Mumbai"]
    }
}
```

### Dimension Properties

| Property   | Type        | Required | Description                                                               |
| ---------- | ----------- | -------- | ------------------------------------------------------------------------- |
| `position` | Integer     | Yes      | Determines weight in priority calculation                                 |
| `schema`   | JSON Schema | Yes      | Validates dimension values                                                |
| `type`     | String      | No       | Dimension type: `REGULAR`, `LOCAL_COHORT:<dim>`, or `REMOTE_COHORT:<dim>` |

### Position

The position determines how much weight this dimension contributes to context priority:

- Weight = 2^position
- Higher positions = more influence on priority
- Position 0 is reserved for `variantIds` (experimentation)
- Positions must be unique

:::info
See [Deterministic Resolution](./deterministic-resolution) for details on priority calculation.
:::

### Schema

The schema validates dimension values in contexts:

```toml
[dimensions]
# String with enum
vehicle_type = {
    position = 2,
    schema = {
        type = "string",
        enum = ["auto", "cab", "bike"]
    }
}

# Integer with range
hour_of_day = {
    position = 3,
    schema = {
        type = "integer",
        minimum = 0,
        maximum = 23
    }
}

# String with pattern
user_id = {
    position = 5,
    schema = {
        type = "string",
        pattern = "^usr_[a-zA-Z0-9]+$"
    }
}
```

## Cohort Dimensions

Cohort dimensions are **derived dimensions** whose values are computed from other dimensions. They allow you to create logical groupings without requiring explicit values in the runtime context.

### LOCAL_COHORT

A `LOCAL_COHORT` dimension derives its value from another dimension using rules defined in its schema:

```toml
[dimensions]
# Base dimension
city = {
    position = 4,
    schema = { type = "string", enum = ["Bangalore", "Chennai", "Delhi", "Mumbai"] }
}

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

#### How LOCAL_COHORT Works

1. The cohort dimension references a base dimension (`city` in this case)
2. The `definitions` object maps cohort values to conditions
3. At runtime, the cohort value is computed from the base dimension value
4. If no condition matches, the value is "otherwise"

**Example:**

| Runtime `city` | Computed `city_cohort` |
| -------------- | ---------------------- |
| Bangalore      | south                  |
| Chennai        | south                  |
| Delhi          | north                  |
| Mumbai         | otherwise              |

#### Cohort Position Constraint

A cohort dimension's position must be **less than or equal to** its referenced dimension's position:

```toml
# ✅ Valid: cohort position (1) ≤ city position (4)
city = { position = 4, ... }
city_cohort = { position = 1, type = "LOCAL_COHORT:city", ... }

# ❌ Invalid: cohort position (5) > city position (4)
city = { position = 4, ... }
city_cohort = { position = 5, type = "LOCAL_COHORT:city", ... }
```

This ensures cohorts don't override their base dimension's influence.

#### Cohort Schema Structure

The cohort schema must have:

1. `type`: "string"
2. `enum`: List of possible cohort values (including a default like "otherwise")
3. `definitions`: Mapping of cohort values to conditions

```toml
schema = {
    type = "string",
    enum = ["value1", "value2", "otherwise"],
    definitions = {
        value1 = <condition>,
        value2 = <condition>
        # "otherwise" is the implicit default
    }
}
```

### Condition Syntax

Cohort conditions use JSON Logic-like expressions:

#### In (Membership)

Check if a dimension value is in a list:

```toml
south = { in = [{ var = "city" }, ["Bangalore", "Chennai"]] }
```

This checks if `city` is in the list `["Bangalore", "Chennai"]`.

#### Comparison

Compare dimension values:

```toml
peak_hours = { ">=" = [{ var = "hour_of_day" }, 18] }
```

#### Logical Operators

Combine conditions:

```toml
premium_zone = {
    and = [
        { in = [{ var = "city" }, ["Bangalore", "Delhi"]] },
        { ">=" = [{ var = "hour_of_day" }, 18] }
    ]
}
```

### Use Cases for Cohorts

#### 1. Geographic Grouping

Group cities into regions:

```toml
[dimensions]
city = { position = 4, schema = { type = "string", enum = ["Bangalore", "Chennai", "Delhi", "Mumbai", "Kolkata"] } }

region = {
    position = 1,
    type = "LOCAL_COHORT:city",
    schema = {
        type = "string",
        enum = ["south", "north", "east", "otherwise"],
        definitions = {
            south = { in = [{ var = "city" }, ["Bangalore", "Chennai"]] },
            north = { in = [{ var = "city" }, ["Delhi"]] },
            east = { in = [{ var = "city" }, ["Kolkata"]] }
        }
    }
}

[[overrides]]
_context_ = { region = "south" }
per_km_rate = 18.0  # South India has different pricing
```

#### 2. Time-based Grouping

Group hours into time periods:

```toml
[dimensions]
hour_of_day = { position = 3, schema = { type = "integer", minimum = 0, maximum = 23 } }

time_period = {
    position = 1,
    type = "LOCAL_COHORT:hour_of_day",
    schema = {
        type = "string",
        enum = ["morning_rush", "evening_rush", "off_peak"],
        definitions = {
            morning_rush = { and = [{ ">=" = [{ var = "hour_of_day" }, 7] }, { "<=" = [{ var = "hour_of_day" }, 9] }] },
            evening_rush = { and = [{ ">=" = [{ var = "hour_of_day" }, 17] }, { "<=" = [{ var = "hour_of_day" }, 20] }] }
        }
    }
}

[[overrides]]
_context_ = { time_period = "morning_rush" }
surge_factor = 1.5

[[overrides]]
_context_ = { time_period = "evening_rush" }
surge_factor = 2.0
```

#### 3. User Tier Grouping

Group users into tiers based on behavior:

```toml
[dimensions]
user_segment = { position = 5, schema = { type = "string" } }

tier = {
    position = 1,
    type = "LOCAL_COHORT:user_segment",
    schema = {
        type = "string",
        enum = ["premium", "regular", "new"],
        definitions = {
            premium = { in = [{ var = "user_segment" }, ["gold", "platinum", "diamond"]] },
            new = { in = [{ var = "user_segment" }, ["trial", "onboarding"]] }
        }
    }
}

[[overrides]]
_context_ = { tier = "premium" }
support_priority = "high"
```

### REMOTE_COHORT

A `REMOTE_COHORT` dimension derives its value from an external service:

```toml
[dimensions]
user_id = { position = 5, schema = { type = "string" } }

user_cohort = {
    position = 1,
    type = "REMOTE_COHORT:user_id",
    schema = {
        type = "string",
        enum = ["high_value", "standard", "churn_risk"]
    }
}
```

The cohort value is fetched from an external service at resolution time, allowing for dynamic segmentation based on real-time data.

:::note
REMOTE_COHORT requires backend service integration. The cohort value is resolved by calling an external API with the base dimension value.
:::

## Dimension Dependencies

Cohort dimensions create dependencies between dimensions:

```toml
[dimensions]
city = { position = 4, schema = { ... } }           # No dependencies
city_cohort = { position = 1, type = "LOCAL_COHORT:city", ... }  # Depends on city
```

The dependency graph ensures:

1. Base dimensions are resolved before their cohorts
2. Cohort values are computed from resolved base values
3. Circular dependencies are not allowed

## Multiple Cohorts

You can have multiple cohort dimensions deriving from the same base:

```toml
[dimensions]
city = { position = 4, schema = { type = "string", enum = ["Bangalore", "Delhi", "Mumbai"] } }

# Geographic region cohort
region = {
    position = 1,
    type = "LOCAL_COHORT:city",
    schema = {
        type = "string",
        enum = ["south", "north", "west"],
        definitions = {
            south = { in = [{ var = "city" }, ["Bangalore"]] },
            north = { in = [{ var = "city" }, ["Delhi"]] }
        }
    }
}

# Market tier cohort
market_tier = {
    position = 2,
    type = "LOCAL_COHORT:city",
    schema = {
        type = "string",
        enum = ["tier1", "tier2"],
        definitions = {
            tier1 = { in = [{ var = "city" }, ["Bangalore", "Delhi", "Mumbai"]] }
        }
    }
}
```

## Using Cohorts in Overrides

Cohort dimensions can be used in context matching just like regular dimensions:

```toml
[[overrides]]
_context_ = { region = "south" }
per_km_rate = 18.0

[[overrides]]
_context_ = { market_tier = "tier1" }
service_fee = 10.0

[[overrides]]
_context_ = { region = "south", market_tier = "tier1" }
# Both cohort conditions must match
premium_service = true
```

## Cohorts vs Regular Dimensions

| Aspect       | Regular Dimension         | Cohort Dimension                  |
| ------------ | ------------------------- | --------------------------------- |
| Value source | Provided at runtime       | Computed from base dimension      |
| Schema       | Validates input           | Defines computation rules         |
| Position     | Any unique position       | Must be ≤ base dimension position |
| Use case     | Direct context attributes | Derived groupings                 |

## Best Practices

### 1. Always Include a Default Cohort Value

```toml
# ✅ Good: "otherwise" catches unclassified values
enum = ["south", "north", "otherwise"]

# ❌ Bad: No default, unclassified values cause issues
enum = ["south", "north"]
```

### 2. Use Lower Positions for Cohorts

Cohorts should have less weight than their base dimensions:

```toml
city = { position = 4, ... }
city_cohort = { position = 1, ... }  # Lower position = lower weight
```

This ensures specific city overrides take precedence over cohort overrides.

### 3. Document Cohort Logic

Add comments explaining cohort groupings:

```toml
# South India: Bangalore, Chennai - similar market dynamics
# North India: Delhi - different regulatory environment
# Otherwise: All other cities use default pricing
```

### 4. Avoid Overlapping Cohort Conditions

```toml
# ❌ Bad: Overlapping conditions - which wins?
definitions = {
    south = { in = [{ var = "city" }, ["Bangalore", "Chennai", "Delhi"]] },
    north = { in = [{ var = "city" }, ["Delhi", "Mumbai"]] }
}

# ✅ Good: Mutually exclusive conditions
definitions = {
    south = { in = [{ var = "city" }, ["Bangalore", "Chennai"]] },
    north = { in = [{ var = "city" }, ["Delhi", "Mumbai"]] }
}
```

### 5. Use Cohorts for Logical Groupings

Cohorts are ideal for:

- Geographic regions
- Time periods
- User segments
- Product categories
- Market tiers

Avoid using cohorts for:

- Simple value transformations (use functions instead)
- Dynamic data (use REMOTE_COHORT)
- Complex business logic (keep it simple)

## Validation Rules

1. **Position uniqueness**: No two dimensions can have the same position
2. **Position 0 reserved**: Reserved for `variantIds`
3. **Cohort reference**: Must reference an existing dimension
4. **Cohort position**: Must be ≤ referenced dimension's position
5. **Schema validity**: All schemas must be valid JSON Schema
6. **Cohort schema structure**: Must have `type`, `enum`, and `definitions`
