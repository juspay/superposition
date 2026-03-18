---
sidebar_position: 4
title: Cascading Model
description: CSS-inspired configuration overrides
---

# Cascading Model

SuperTOML uses a cascading model for configuration overrides, inspired by [CSS (Cascading Style Sheets)](https://developer.mozilla.org/en-US/docs/Web/CSS/Cascade). Just as CSS allows styles to cascade from general to specific selectors, SuperTOML allows configuration values to cascade from general to specific contexts.

## The CSS Analogy

In CSS, styles cascade based on selector specificity:

```css
/* General rule */
p {
    color: black;
}

/* More specific rule */
p.highlight {
    color: blue;
}

/* Most specific rule */
p#intro.highlight {
    color: red;
}
```

A paragraph with `id="intro"` and `class="highlight"` gets `color: red` because that rule is most specific.

SuperTOML applies the same principle to configuration:

```toml
# General rule
[[overrides]]
_context_ = { vehicle_type = "cab" }
per_km_rate = 25.0

# More specific rule
[[overrides]]
_context_ = { city = "Bangalore", vehicle_type = "cab" }
per_km_rate = 22.0

# Most specific rule
[[overrides]]
_context_ = { city = "Bangalore", vehicle_type = "cab", hour_of_day = 18 }
per_km_rate = 30.0
```

A cab ride in Bangalore at 6 PM gets `per_km_rate = 30.0` because that context is most specific.

## How Cascading Works

### 1. Start with Defaults

All resolution begins with the default configuration:

```toml
[default-configs]
per_km_rate = { value = 20.0, schema = { type = "number" } }
surge_factor = { value = 0.0, schema = { type = "number" } }
```

For any context, these are the base values.

### 2. Find Matching Contexts

When resolving configuration for a runtime context, find all overrides whose `_context_` matches:

**Runtime context:**

```json
{ "city": "Bangalore", "vehicle_type": "cab", "hour_of_day": 18 }
```

**Matching overrides:**

```toml
# ✅ Matches (vehicle_type matches)
[[overrides]]
_context_ = { vehicle_type = "cab" }
per_km_rate = 25.0

# ✅ Matches (city AND vehicle_type match)
[[overrides]]
_context_ = { city = "Bangalore", vehicle_type = "cab" }
per_km_rate = 22.0

# ✅ Matches (all three match)
[[overrides]]
_context_ = { city = "Bangalore", vehicle_type = "cab", hour_of_day = 18 }
surge_factor = 5.0

# ❌ Does NOT match (city is different)
[[overrides]]
_context_ = { city = "Delhi", vehicle_type = "cab" }
per_km_rate = 28.0

# ❌ Does NOT match (hour_of_day is different)
[[overrides]]
_context_ = { hour_of_day = 6 }
surge_factor = 3.0
```

### 3. Sort by Priority

Matching contexts are sorted by priority. Higher priority contexts override lower priority ones.

Priority is calculated based on:

- **Number of dimensions** - More dimensions = higher priority
- **Dimension positions** - Higher positions contribute more weight

:::info
See [Deterministic Resolution](./deterministic-resolution) for the exact priority formula.
:::

### 4. Apply Overrides in Order

Overrides are applied from lowest to highest priority. Later overrides replace earlier values:

```
Default:      per_km_rate = 20.0
Override 1:   per_km_rate = 25.0  (vehicle_type = cab)
Override 2:   per_km_rate = 22.0  (city = Bangalore, vehicle_type = cab)

Final:        per_km_rate = 22.0
```

## Visual Example: Ride-Hailing App

Consider a ride-hailing app with this configuration:

```toml
[default-configs]
per_km_rate = { value = 20.0, schema = { type = "number" } }
surge_factor = { value = 0.0, schema = { type = "number" } }

[dimensions]
city = { position = 4, schema = { type = "string", enum = ["Bangalore", "Delhi", "Chennai"] } }
vehicle_type = { position = 2, schema = { type = "string", enum = ["auto", "cab", "bike"] } }
hour_of_day = { position = 3, schema = { type = "integer", minimum = 0, maximum = 23 } }

[[overrides]]
_context_ = { vehicle_type = "cab" }
per_km_rate = 25.0

[[overrides]]
_context_ = { vehicle_type = "bike" }
per_km_rate = 15.0

[[overrides]]
_context_ = { city = "Bangalore", vehicle_type = "cab" }
per_km_rate = 22.0

[[overrides]]
_context_ = { city = "Delhi", vehicle_type = "cab", hour_of_day = 18 }
surge_factor = 5.0
```

### Scenario 1: Bike in Any City

**Context:** `{ vehicle_type: "bike" }`

| Override      | Context                                        | Matches? | Priority |
| ------------- | ---------------------------------------------- | -------- | -------- |
| Default       | -                                              | -        | 0        |
| bike          | `{ vehicle_type = "bike" }`                    | ✅       | 4        |
| cab           | `{ vehicle_type = "cab" }`                     | ❌       | -        |
| Bangalore cab | `{ city = "Bangalore", vehicle_type = "cab" }` | ❌       | -        |

**Result:** `per_km_rate = 15.0`, `surge_factor = 0.0`

### Scenario 2: Cab in Bangalore

**Context:** `{ city: "Bangalore", vehicle_type: "cab" }`

| Override      | Context                                        | Matches? | Priority |
| ------------- | ---------------------------------------------- | -------- | -------- |
| Default       | -                                              | -        | 0        |
| cab           | `{ vehicle_type = "cab" }`                     | ✅       | 4        |
| bike          | `{ vehicle_type = "bike" }`                    | ❌       | -        |
| Bangalore cab | `{ city = "Bangalore", vehicle_type = "cab" }` | ✅       | 20       |

**Result:** `per_km_rate = 22.0` (Bangalore cab override wins), `surge_factor = 0.0`

### Scenario 3: Cab in Delhi at 6 PM

**Context:** `{ city: "Delhi", vehicle_type: "cab", hour_of_day: 18 }`

| Override      | Context                                                      | Matches? | Priority |
| ------------- | ------------------------------------------------------------ | -------- | -------- |
| Default       | -                                                            | -        | 0        |
| cab           | `{ vehicle_type = "cab" }`                                   | ✅       | 4        |
| bike          | `{ vehicle_type = "bike" }`                                  | ❌       | -        |
| Bangalore cab | `{ city = "Bangalore", vehicle_type = "cab" }`               | ❌       | -        |
| Delhi evening | `{ city = "Delhi", vehicle_type = "cab", hour_of_day = 18 }` | ✅       | 28       |

**Result:** `per_km_rate = 25.0` (cab override), `surge_factor = 5.0` (Delhi evening override)

## Benefits of Cascading

### 1. Reduced Configuration Sprawl

Instead of duplicating configuration for every combination:

```toml
# ❌ Without cascading - lots of duplication
[cab_bangalore]
per_km_rate = 22.0
surge_factor = 0.0

[cab_delhi]
per_km_rate = 25.0
surge_factor = 0.0

[cab_chennai]
per_km_rate = 25.0
surge_factor = 0.0

[bike_bangalore]
per_km_rate = 15.0
surge_factor = 0.0

# ... and so on for every combination
```

You define defaults and only override what changes:

```toml
# ✅ With cascading - minimal duplication
[default-configs]
per_km_rate = { value = 20.0, schema = { type = "number" } }
surge_factor = { value = 0.0, schema = { type = "number" } }

[[overrides]]
_context_ = { vehicle_type = "cab" }
per_km_rate = 25.0

[[overrides]]
_context_ = { vehicle_type = "bike" }
per_km_rate = 15.0

[[overrides]]
_context_ = { city = "Bangalore", vehicle_type = "cab" }
per_km_rate = 22.0
```

### 2. Reasonable Defaults

Every configuration has a fallback:

```toml
# Default applies to any city not explicitly configured
[default-configs]
per_km_rate = { value = 20.0, schema = { type = "number" } }

# Only override for specific cities
[[overrides]]
_context_ = { city = "Bangalore" }
per_km_rate = 22.0
```

A new city automatically gets sensible defaults.

### 3. Incremental Specificity

Add specificity only when needed:

```toml
# Start with vehicle type override
[[overrides]]
_context_ = { vehicle_type = "cab" }
per_km_rate = 25.0

# Later, add city-specific override for Bangalore
[[overrides]]
_context_ = { city = "Bangalore", vehicle_type = "cab" }
per_km_rate = 22.0

# Even later, add time-specific override
[[overrides]]
_context_ = { city = "Bangalore", vehicle_type = "cab", hour_of_day = 18 }
surge_factor = 5.0
```

### 4. Clear Override Hierarchy

The configuration file clearly shows what overrides what:

```toml
# General → Specific hierarchy is visible in the file
[[overrides]]
_context_ = { vehicle_type = "cab" }          # Level 1: Vehicle type
per_km_rate = 25.0

[[overrides]]
_context_ = { city = "Bangalore" }            # Level 1: City
per_km_rate = 21.0

[[overrides]]
_context_ = { city = "Bangalore", vehicle_type = "cab" }  # Level 2: Combination
per_km_rate = 22.0
```

## Anti-Patterns to Avoid

### 1. Overriding Everything

Don't override every combination:

```toml
# ❌ Bad: Overriding for every city-vehicle combination
[[overrides]]
_context_ = { city = "Bangalore", vehicle_type = "auto" }
per_km_rate = 18.0

[[overrides]]
_context_ = { city = "Bangalore", vehicle_type = "cab" }
per_km_rate = 22.0

[[overrides]]
_context_ = { city = "Bangalore", vehicle_type = "bike" }
per_km_rate = 15.0

[[overrides]]
_context_ = { city = "Delhi", vehicle_type = "auto" }
per_km_rate = 18.0

# ... many more
```

Instead, use sensible defaults and override only exceptions:

```toml
# ✅ Good: Default per vehicle type, exceptions per city
[[overrides]]
_context_ = { vehicle_type = "auto" }
per_km_rate = 18.0

[[overrides]]
_context_ = { vehicle_type = "cab" }
per_km_rate = 25.0

[[overrides]]
_context_ = { vehicle_type = "bike" }
per_km_rate = 15.0

# Only override where needed
[[overrides]]
_context_ = { city = "Bangalore", vehicle_type = "cab" }
per_km_rate = 22.0
```

### 2. Conflicting Overrides at Same Priority

Avoid overrides that conflict at the same priority level:

```toml
# ❌ Bad: Same priority, different values
[[overrides]]
_context_ = { city = "Bangalore" }
per_km_rate = 22.0

[[overrides]]
_context_ = { vehicle_type = "cab" }
per_km_rate = 25.0

# For Bangalore cab: which one wins?
# Both have same priority (one dimension each)
```

Resolution: Make one more specific:

```toml
# ✅ Good: Clear priority
[[overrides]]
_context_ = { vehicle_type = "cab" }
per_km_rate = 25.0

[[overrides]]
_context_ = { city = "Bangalore", vehicle_type = "cab" }
per_km_rate = 22.0  # This wins for Bangalore cabs
```

### 3. Missing Defaults

Always define defaults for all configs:

```toml
# ❌ Bad: No default for surge_factor
[default-configs]
per_km_rate = { value = 20.0, schema = { type = "number" } }

[[overrides]]
_context_ = { city = "Delhi" }
surge_factor = 5.0  # What about other cities?
```

```toml
# ✅ Good: Default for all configs
[default-configs]
per_km_rate = { value = 20.0, schema = { type = "number" } }
surge_factor = { value = 0.0, schema = { type = "number" } }

[[overrides]]
_context_ = { city = "Delhi" }
surge_factor = 5.0  # Other cities get 0.0
```

## Comparison with Other Approaches

| Approach                   | Pros                                      | Cons                                  |
| -------------------------- | ----------------------------------------- | ------------------------------------- |
| **SuperTOML Cascading**    | Deterministic, declarative, no code logic | Learning curve for priority rules     |
| **Environment Files**      | Simple, familiar                          | Duplication, no type safety           |
| **Code-based Logic**       | Flexible                                  | Hard to reason about, scattered logic |
| **Database Configuration** | Dynamic                                   | Hard to version, no type safety       |
| **Feature Flags**          | Good for experiments                      | Not ideal for static configuration    |

SuperTOML cascading combines the best of these: declarative like env files, flexible like code, and type-safe like a database schema.
