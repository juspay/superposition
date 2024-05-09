# Context Aware Config
---

Context Aware Config (abbreviated as CAC) is the foundational service of the Superposition Platform.  It is the configuration management system that can override configuration values under certain domain contexts.

- [Context Aware Config](#context-aware-config)
  - [Concepts](#concepts)
    - [Default Configs](#default-configs)
    - [Dimensions](#dimensions)
    - [Context](#context)
    - [Overrides](#overrides)
  - [How CAC Works](#how-cac-works)

## Concepts
---

Context-Aware-Config is made of 4 key abstractions default-configs, dimensions, context and overrides.


### Default Configs

Default Configs is the set of all possible configuration keys of your application along with their default values.  One may think of default configs as the best assumption we can make about our configs.

Let us use a [simple cab ride-hailing application](https://github.com/knutties/superposition-demo-app/) that operates in different cities and capture some attributes that we might need configurable at a per city level.

1.  `base_rate` - 100 INR
2.  `per_distance_unit_rate` - 10 INR

### Dimensions

Dimensions are typically attributes of your domain which can potentially govern the values that a particular configuration can take.

In our example application, dimensions that could change the configuration values could be one or more of the following:

1. `city` - city where the user is hailing the ride - e.g. Bangalore (India), Chennai (India), Seattle (USA)
2. `hour_of_day` - a number ranging between 0 and 23 denoting the hour of the day the ride is taken

The value of the default configuration could change based on which city the user is hailing the ride and the hour of the day.

### Context

A Context is a logical expression built using dimensions as variables. It can be defined using the following [EBNF notation](https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_form)

```
context = context <logical-operator> context
context = dimension <relational-operator> value
logical-operator = AND
relational-operator - IS | HAS | BETWEEN
```

- `IS`: an equality operator
- `HAS`: similar to the IN operator
- `BETWEEN` (inclusive): a relational operator that checks if a provided value is between `value`

The `logical-operator` is typically AND to keep context evaluation and comprehension overhead simple.  While other `logical-operator` can be used in CAC - we strongly recommend against it to keep configuration override comprehension simple.

Examples of contexts:

- `[city IS "Bangalore"]`
- `[hour_of_day IS 8]`
- `[city IS "Bangalore" AND hour_of_day IS 8]`

### Overrides

Overrides are a subset of the configurations from Default Config typically with different values.  Overrides are always associated with contexts and are applied when a context evaluates to `true`. 

Example:

Let us configure different overrides for configuration keys under different contexts in our example.

```
[city IS "Bangalore"]
base_rate = 25
per_distance_unit_rate = 10

[city IS "Chennai"]
base_rate = 30
per_distance_unit_rate = 20

[city IS "Bangalore", hour_of_day IS "8"]
base_rate = 50
per_distance_unit_rate = 30

```

## How CAC Works
---

This section shows a complete configuration and how 

- Your application adds the CAC client as a library
- At runtime, your application will provide a context to the client for evaluation. Using our ride hailing example from earlier, the context provided by your application would look like:
    ```
    [city IS "Bangalore", hour_of_day IS 8]
    ```
- CAC will apply all contexts it has available, and return the final configuration for the application. If no rule applies for a configuration, it's default value will be returned. From our earlier examples (check Default Config and Overrides section) the configuration returned would be
    ```
    {
        "base_rate": 50,
        "per_distance_unit_rate": 30
    }
    ```