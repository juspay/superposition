---
sidebar_position: 5
title: Deterministic Resolution
description: How SuperTOML resolves configuration values deterministically
---

# Deterministic Resolution

SuperTOML resolves configuration values **deterministically**. Given the same configuration file and runtime context, the resolved values are always the same. There is no randomness, no undefined behavior, and no ambiguity in conflict resolution.

## Why Determinism Matters

Deterministic resolution is critical for:

1. **Reproducibility** - The same inputs always produce the same outputs
2. **Debuggability** - You can trace exactly why a value was chosen
3. **Testing** - Configuration behavior is predictable in tests
4. **Confidence** - No surprises in production

## The Priority Formula

When multiple overrides match a context, SuperTOML uses a priority formula to determine which values win.

### Weight Calculation

Each dimension has a **position**, and each position contributes a **weight** to the context priority:

```
Weight = 2^position
```

For example:

| Position | Weight |
| -------- | ------ |
| 1        | 2      |
| 2        | 4      |
| 3        | 8      |
| 4        | 16     |
| 5        | 32     |

### Context Priority

A context's priority is the **sum of weights** of all dimensions in that context:

```
Priority = Sum of (2^position) for each dimension in context
```

### Example Calculation

Given these dimensions:

```toml
[dimensions]
city = { position = 4, schema = { type = "string" } }
vehicle_type = { position = 2, schema = { type = "string" } }
hour_of_day = { position = 3, schema = { type = "integer" } }
```

Context priorities:

| Context                                                      | Dimensions                           | Calculation     | Priority |
| ------------------------------------------------------------ | ------------------------------------ | --------------- | -------- |
| `{ vehicle_type = "cab" }`                                   | vehicle_type (pos 2)                 | 2^2 = 4         | 4        |
| `{ city = "Bangalore" }`                                     | city (pos 4)                         | 2^4 = 16        | 16       |
| `{ city = "Bangalore", vehicle_type = "cab" }`               | city (4), vehicle_type (2)           | 16 + 4 = 20     | 20       |
| `{ city = "Delhi", vehicle_type = "cab", hour_of_day = 18 }` | city (4), vehicle_type (2), hour (3) | 16 + 4 + 8 = 28 | 28       |

## Resolution Algorithm

### Step 1: Find Matching Contexts

Find all overrides where the `_context_` is a subset of the runtime context:

```toml
# Runtime context
{ city = "Bangalore", vehicle_type = "cab", hour_of_day = 14 }

# Matching overrides
[[overrides]]
_context_ = { vehicle_type = "cab" }  # ✅ Matches

[[overrides]]
_context_ = { city = "Bangalore", vehicle_type = "cab" }  # ✅ Matches

[[overrides]]
_context_ = { city = "Delhi" }  # ❌ Does not match
```

### Step 2: Calculate Priorities

Calculate the priority for each matching context:

```toml
# Dimensions
city = { position = 4, ... }      # Weight: 16
vehicle_type = { position = 2, ... }  # Weight: 4
hour_of_day = { position = 3, ... }  # Weight: 8
```

| Override      | Context                                        | Priority |
| ------------- | ---------------------------------------------- | -------- |
| Default       | -                                              | 0        |
| cab           | `{ vehicle_type = "cab" }`                     | 4        |
| Bangalore cab | `{ city = "Bangalore", vehicle_type = "cab" }` | 20       |

### Step 3: Sort by Priority

Sort matching contexts by priority (ascending):

```
Priority 0:  Default
Priority 4:  { vehicle_type = "cab" }
Priority 20: { city = "Bangalore", vehicle_type = "cab" }
```

### Step 4: Apply Overrides in Order

Apply overrides from lowest to highest priority. Later values replace earlier ones:

```
per_km_rate:
  Default (0):     20.0
  Override (4):    25.0  (vehicle_type = cab)
  Override (20):   22.0  (city = Bangalore, vehicle_type = cab)

  Final: 22.0
```

## The Importance of Position

The position of each dimension determines its contribution to priority. **Higher positions mean more weight**.

### Why Position Matters

Consider this scenario:

```toml
[dimensions]
city = { position = 4, schema = { type = "string" } }
vehicle_type = { position = 2, schema = { type = "string" } }

[[overrides]]
_context_ = { city = "Bangalore" }
per_km_rate = 21.0

[[overrides]]
_context_ = { vehicle_type = "cab" }
per_km_rate = 25.0
```

For a cab in Bangalore, which override wins?

- `{ city = "Bangalore" }` has priority 2^4 = 16
- `{ vehicle_type = "cab" }` has priority 2^2 = 4

**City wins** because it has a higher position. This is intentional: city is considered more significant than vehicle type.

### Designing Position Hierarchy

When designing your dimensions, assign higher positions to more significant dimensions:

```toml
[dimensions]
# Lower positions = less significant
variant = { position = 0, ... }      # Experimentation (reserved)
tier = { position = 1, ... }         # User tier
vehicle_type = { position = 2, ... } # Vehicle category
hour_of_day = { position = 3, ... }  # Time-based
region = { position = 4, ... }       # Geographic region
city = { position = 5, ... }         # Specific city
tenant = { position = 6, ... }       # Tenant/customer
```

With this hierarchy:

- A tenant-specific override beats a city-specific one
- A city-specific override beats a region-specific one
- A region-specific override beats a vehicle-type-specific one

## Conflict Resolution

### When Contexts Have Equal Priority

If two matching contexts have the same priority, **file order determines the winner**. The later override wins:

```toml
[[overrides]]
_context_ = { city = "Bangalore" }
per_km_rate = 21.0

[[overrides]]
_context_ = { vehicle_type = "cab" }  # Same priority if positions are equal
per_km_rate = 25.0  # This wins (appears later)
```

:::caution
Avoid equal-priority conflicts by designing distinct positions for your dimensions.
:::

### When Multiple Overrides Affect the Same Key

If multiple overrides at different priorities affect the same key, the highest priority wins:

```toml
[[overrides]]
_context_ = { vehicle_type = "cab" }
per_km_rate = 25.0

[[overrides]]
_context_ = { city = "Bangalore", vehicle_type = "cab" }
per_km_rate = 22.0

[[overrides]]
_context_ = { city = "Bangalore", vehicle_type = "cab", hour_of_day = 18 }
per_km_rate = 30.0
```

For Bangalore cab at 6 PM:

- Priority 4: `per_km_rate = 25.0`
- Priority 20: `per_km_rate = 22.0`
- Priority 28: `per_km_rate = 30.0`

**Result: `per_km_rate = 30.0`** (highest priority)

### When Overrides Affect Different Keys

Overrides can affect different keys without conflict:

```toml
[[overrides]]
_context_ = { vehicle_type = "cab" }
per_km_rate = 25.0

[[overrides]]
_context_ = { city = "Delhi", vehicle_type = "cab" }
surge_factor = 5.0
```

For a cab in Delhi:

- `per_km_rate = 25.0` (from vehicle_type override)
- `surge_factor = 5.0` (from Delhi cab override)

Both are applied; they affect different keys.

## Complete Example

```toml
[default-configs]
per_km_rate = { value = 20.0, schema = { type = "number" } }
surge_factor = { value = 0.0, schema = { type = "number" } }
base_fare = { value = 50.0, schema = { type = "number" } }

[dimensions]
city = { position = 4, schema = { type = "string" } }
vehicle_type = { position = 2, schema = { type = "string" } }
hour_of_day = { position = 3, schema = { type = "integer" } }

[[overrides]]
_context_ = { vehicle_type = "bike" }
per_km_rate = 15.0

[[overrides]]
_context_ = { vehicle_type = "cab" }
per_km_rate = 25.0

[[overrides]]
_context_ = { city = "Bangalore", vehicle_type = "cab" }
per_km_rate = 22.0

[[overrides]]
_context_ = { city = "Delhi", vehicle_type = "cab", hour_of_day = 6 }
surge_factor = 5.0

[[overrides]]
_context_ = { city = "Delhi", vehicle_type = "cab", hour_of_day = 18 }
surge_factor = 5.0

[[overrides]]
_context_ = { city = "Delhi" }
base_fare = 60.0
```

### Resolution Table

| Runtime Context                                              | Matching Overrides (Priority)           | Result                                                         |
| ------------------------------------------------------------ | --------------------------------------- | -------------------------------------------------------------- |
| `{ vehicle_type = "bike" }`                                  | bike (4)                                | `per_km_rate = 15.0`, `surge_factor = 0.0`, `base_fare = 50.0` |
| `{ city = "Bangalore", vehicle_type = "cab" }`               | cab (4), Bangalore cab (20)             | `per_km_rate = 22.0`, `surge_factor = 0.0`, `base_fare = 50.0` |
| `{ city = "Delhi", vehicle_type = "cab", hour_of_day = 18 }` | cab (4), Delhi (16), Delhi evening (28) | `per_km_rate = 25.0`, `surge_factor = 5.0`, `base_fare = 60.0` |
| `{ city = "Chennai", vehicle_type = "auto" }`                | (none)                                  | `per_km_rate = 20.0`, `surge_factor = 0.0`, `base_fare = 50.0` |

## Debugging Resolution

To understand why a particular value was resolved:

1. **List matching contexts** - Which `_context_` entries match your runtime context?
2. **Calculate priorities** - What is the priority of each matching context?
3. **Sort by priority** - Which context has the highest priority?
4. **Check the override** - What value does the highest-priority context set?

### Example Debug Trace

**Runtime context:** `{ city = "Delhi", vehicle_type = "cab", hour_of_day = 18 }`

```
Step 1: Find matching contexts
  - { vehicle_type = "cab" } ✅
  - { city = "Delhi" } ✅
  - { city = "Delhi", vehicle_type = "cab", hour_of_day = 18 } ✅

Step 2: Calculate priorities
  - { vehicle_type = "cab" }: 2^2 = 4
  - { city = "Delhi" }: 2^4 = 16
  - { city = "Delhi", vehicle_type = "cab", hour_of_day = 18 }: 2^4 + 2^2 + 2^3 = 28

Step 3: Sort by priority (ascending)
  - Priority 4: { vehicle_type = "cab" }
  - Priority 16: { city = "Delhi" }
  - Priority 28: { city = "Delhi", vehicle_type = "cab", hour_of_day = 18 }

Step 4: Apply overrides
  per_km_rate:
    - Default: 20.0
    - Priority 4: 25.0 (vehicle_type = cab)
    - Priority 28: (not set, keeps 25.0)
    Final: 25.0

  surge_factor:
    - Default: 0.0
    - Priority 28: 5.0
    Final: 5.0

  base_fare:
    - Default: 50.0
    - Priority 16: 60.0 (city = Delhi)
    Final: 60.0
```

## Mathematical Properties

### Uniqueness

Given a configuration file and a runtime context, the resolved configuration is **unique**. There is no ambiguity.

### Composability

Contexts combine additively:

```
Priority(A ∪ B) = Priority(A) + Priority(B)
```

If context A has priority 4 and context B has priority 16, then the combined context has priority 20.

### Monotonicity

Adding dimensions to a context can only **increase** its priority:

```
If A ⊆ B, then Priority(A) ≤ Priority(B)
```

If context A is a subset of context B, then B's priority is at least as high as A's.

## Position Assignment Guidelines

1. **Reserve position 0** for `variantIds` (experimentation)
2. **Use gaps** between positions to allow future dimensions:
    ```toml
    [dimensions]
    region = { position = 10, ... }
    city = { position = 20, ... }
    neighborhood = { position = 30, ... }
    ```
3. **Higher position = more specific** - More specific dimensions should have higher positions
4. **Document your hierarchy** - Make the intended priority order clear

## Summary

| Concept          | Formula/Rule                                    |
| ---------------- | ----------------------------------------------- |
| Dimension weight | 2^position                                      |
| Context priority | Sum of weights of all dimensions in context     |
| Resolution       | Apply overrides from lowest to highest priority |
| Conflicts        | Higher priority wins; file order breaks ties    |
| Position design  | Higher position = more significant dimension    |

The deterministic nature of SuperTOML resolution means you can always predict and explain why a configuration value was chosen. This transparency is essential for debugging and maintaining complex configuration systems.
