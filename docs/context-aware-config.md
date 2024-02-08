# Context Aware Config
---

Context Aware Config (abbreviated as CAC) is the foundational service of the Superposition Platform that is configuration management system that can override config values under certain domain contexts. CAC makes it easy to configure your applications and make sure that you can change these configurations without causing issues in your application

- [Context Aware Config](#context-aware-config)
  - [Concepts](#concepts)
    - [Default Configs](#default-configs)
    - [Dimensions](#dimensions)
    - [Context](#context)
    - [Overrides](#overrides)
  - [How CAC Works](#how-cac-works)

## Concepts
---
### Default Configs

Default Configs are a fundamental concept to CAC. Default Configs are key value pairs that help CAC model the entity/configuration it is trying to manage and serve. Think of default configs as the best assumption we can make about our configs.

Example:
Using our car classification example, we can establish defaults like:
1. Number of doors - 4 (most cars do have 4 doors)
2. Engine - Internal Combustion
3. Chassis - Hatchback
4. Color - Silver 
5. interiors - leather

Again remember, these are our base assumptions that we provide to CAC about our configuration

### Dimensions

Dimensions are typically attributes of your domain which can potentially govern values that a particular configuration takes.

Example:

Let us assume we are modelling a system for buying cars, the dimensions you might use are:
   1. Engine horsepower
   2. manufacturer
   3. model

Contexts build upon Dimensions

### Context

A Context is a logical expression built using dimensions as variables. It takes the form:

`Dimension operator value`

Operators are:
- IS: an equality operator
- HAS: similar to the IN operator
- BETWEEN (inclusive): an operator that checks if a provided value is between `value`

When multiple Context are defined together, they are evaluated as AND - if a context is evaluated as false, then the entire rule will not apply.

Examples:

- color IS "red"
- manufacturer IS "hyundai"
- chassis HAS "hatchback"

### Overrides

Overrides are a subset of the configuration from Default Config typically with different values. Overrides are always associated with Contexts and are applied when a Context is evaluated to `true`. 

Example:

lets take an example to configure Tesla cars

```
[manufacturer IS "Tesla"]
engine = "EV"

[manufacturer IS "Tesla", model IS "cybertruck"]
chassis = "truck"

[manufacturer IS "Tesla", model IS "roadster"]
chassis = "sport"

[manufacturer IS "Tesla", model IS "Y"]
chassis = "sedan"
```

## How CAC Works
---

- Your application adds the CAC client as a library
- At runtime, your application will provide a context to the client for evaluation. Using our car example from earlier, the context provided by your application would look like:

    ```
    {
        manufacturer: "Tesla",
        "model": "Y"
    }
    ```
- CAC will apply all contexts it has available, and return the final configuration for the application. If no rule applies for a configuration, it's default value will be returned. From our earlier examples (check Default Config and Overrides section) the configuration returned would be

    ```
    engine = "EV"
    chassis = "sedan"
    number_of_doors = 4
    color = "silver"
    interiors = "leather"
    ```