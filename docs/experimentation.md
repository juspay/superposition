# Experimentation

## Introduction

The **Experimentation** component enables one to run A/B/../Z testing for your
configurations in equi-sized groups/cohorts. It works on top of
**Context-Aware-Config**.  The Experimentation component can also be used as a
release system for your configuration changes which becomes an A/B test with
two variants - the current value and new value.

## Concepts

### Experiment
An experiment, as the name suggests, enables you to test and evaluate the
behaviour of the system for different values of the same configuration. An
experiment can have exactly one **CONTROL** variant and `n` **EXPERIMENTAL**
variants, with each variant overriding/changing same set of keys in
configuration. An experiment's scope can be controlled by declaring the
context, which chalks out the sample set for the experiment from the
population.

For example in our ride-hailing example, we may want to run an experiment for
the 8th hour of the day in the city of Bangalore.  We can do that by specifying
a context for the experiment.
```
[city IS "Bangalore", hour_of_day IS "8"]
```

### Variant
A variant in an experiment, represent one of the `n` tests. In simple terms,
its a collection of key-value pairs, where each key is one of your already
defined **default-config** key, and the value can be any valid new/old
configuration value for the key adhering to it type constraints. There are two kinds of variants:

1. **CONTROL**: It conceptually represents the current state of the configuration. 
2. **EXPERIMENTAL**: The experimental variant lets you define the newer value for the **default-config** keys.

#### Control Variant
```json
    {
        "base_rate": 50,
        "per_distance_unit_rate": 30
    }
```

The context for the above configuration is setup using a special additional dimension called `variantIds`
```
[city IS "Bangalore", hour_of_day IS 8, variantId IS "control-variant-for-experiment-1"]
```

#### Experimental Variant
```json
    {
        "base_rate": 45,
        "per_distance_unit_rate": 30
    }
```
The context for the above configuration is setup using a special additional dimension called `variantIds`
```
[city IS "Bangalore", hour_of_day IS 8, variantId IS "experimental-variant-for-experiment-1"]
```

> [!NOTE]
> Note how the control and experimental configuration add additional conditions to the base context of the experiment.


### Experiment's Traffic Percentage
This defines the traffic size for each variant of the experiment, for instance
if traffic percentage is `13%` and there are `4` variants in the experiment,
    this makes each variant of the experiment receive `13%` of the entire
    traffic and in entirety `13 * 4 = 52%` of the total traffic. 
