---
slug: superposition-genesis
title: Superposition - The Genesis
description:  This blog post talks about the genesis of Superposition from a first principles perspective - what made us build it and the thought process behind why it was structured the way it was.
tags: [superposition, genesis, introduction]
---
This blog post covers the genesis of Superposition from a first principles perspective - why we built it and the thought process behind how the core of Superposition is designed.

## How did it all start?

There were many applications in Juspay that needed runtime configuration management systems.  Many systems relied on json files deployed on S3.  There were just too many issues in this model of serving configuration files.  No problem modelling of the configuration values, no tooling to safely edit and roll out those changes in a safe manner and challenges in keeping multiple files updated at scale.  Some of these configuration changes were the cause of production outages as well.  Superposition started off as a system to address this use-case.

<!-- truncate -->

## Safety first
With safety being a top priority, we anchored on the following three core principles:

1. Safely change configuration values at runtime, supporting the following:
    1. Type safety
    2. Custom validations
    3. Staggered roll-out

If our system could factor in the above - we would have done well.

Safety aside, another salient requirement in our applications was that under some defined domain context - the configurations took a different value i.e. in other words the value needs to be overridden under come conditions.  This was either managed with control blocks in code (simple if-else statements) or by suffixing/prefixing the key with a string that represents the context and defining a new row for that key with the overridden value.  For e.g. if a configuration was called `notification_enabled` with a default value of `false` and we had to enable it only for say `iOS` users, one would create a key call `iOS_notification_enabled` and set it to `true`.  In most cases, when no override exists for a specific context, the typical expectation is to fallback to a default value.  This approach doesn't scale well once the configuration started to depend on multiple contexts as managing the ordering of the suffices gets complicated.

## Cascading configurations
We added this as one of the core problems that the new configuration system should address as we felt it was too salient a feature for a configuration system to ignore.

1. Safely change configuration values at runtime, supporting the following:
    1. Type safety
    2. Custom validations
    3. Staggered roll-out
2. **Cascading configuration values**

Once we decided to support cascading configuration values, we had to solve for how to formally define contexts under which configuration values could change.

And so we decided to model the concept of `contexts`.  Contexts are simple boolean expressions involving key attributes that matter to your domain.  We chose to call these key attributes `dimensions`.  Dimensions are also defined with their associated types to ensure that values set for dimensions in contexts are validated and type-safe.

To illustrate the above in a step by step fashion, we will make use of the TOML representation of Superposition configurations.  As a concrete example, let us say your application is available across Android, iOS and Web - you might want to create a dimension which is an enum holding one of three possible values: `ios`, `android` and `web`.  Below is an example that shows a configuration key having a default value under a section title `default-config`, but having an override when `platform` is `web`.    Note the definition of the types for the values of the `default-config` and `dimensions` to support our type-safety requirement. 
```toml
[default-config]
notification_enabled = { "value" = false, "schema" = { "type" = "boolean" } }

[dimensions]
platform = { schema = { "type" = "string", "enum" = ["ios", "android", "web"] } }

[[overrides]]
_context_ = { platform = "web" }
notification_enabled = true
```

## Disambiguating conflicting contexts
Now let us say your application needs to disable notifications when users use the Edge browser.  To facilitate this override, you would create a new dimension called `browser` with the following as possible values: `["chrome", "safari", "edge", "arc", "brave"]` and add a corresponding override section with a context using the newly added `browser` dimension.
```toml
[default-config]
notification_enabled = { "value" = false, "schema" = { "type" = "boolean" } }

[dimensions]
platform = { schema = { "type" = "string", "enum" = ["ios", "android", "web"] } }
browser = { schema = { "type" = "string", "enum" = ["chrome", "safari", "edge",] } }

[[overrides]]
_context_ = { platform = "web" }
notification_enabled = true

[[overrides]]
_context_ = { browser = "edge" }
notification_enabled = false
```
To the astute observer, it will become apparent that we have landed ourselves in a bit of a soup - if someone accesses your application over the `web` from an `edge` browser both the contexts evaluate to `true`.  So which configuration should apply?  The easiest choice would have been to decide that latest one applies.  The latest one wins approach brings with it a couple of key challenges: 

* Firstly, it brings in a lack of predictability when multiple people can modify configurations and might not share the same idea of prioritizing the overrides.
* Secondly, the latest wins approach does not scale well when the number of the overrides grow - it is very hard to manage relative priority of the overrides even at a small cardinality of 10s of overrides and becomes error-prone.

Such an approach fundamentally affects the core requirement of the configuration system - safety.  To address this, we decided to force the user to tie-break between the dimensions i.e. explicitly decide their priority depending on their domain contexts.  While this choice might seem constraining - we believe it brings in the clarity and communicates clearly to all users of the configuration system what the relative priorities are.  It makes it easy to author an override without worrying about where to place the override.  We also believe at a deeper level most systems end-up having a implicit priority between these dimensions in code if it is not explicitly stated or captured.

So we refined the definition of what we wanted to build.  We wanted to build a configuration system that would enable the following:

1. Safely change configuration values at runtime, supporting the following:
    1. Type safety
    2. Custom validations
    3. Staggered roll-out
2. **Unambiguous cascading of configuration values**

Now we enhance our earlier example to explicitly tie-break between the dimensions using a field called `position` in the definition of the dimension.   Higher the position, higher the weightage for that dimension.  So in the following example the override with the `browser` context check triumphs even though it appears before the one with the `platform` context.

```toml
[default-config]
notification_enabled = { "value" = false, "schema" = { "type" = "boolean" } }

[dimensions]
platform = { position = 1, schema = { "type" = "string", "enum" = ["ios", "android", "web"] } }
browser = { position = 2, schema = { "type" = "string", "enum" = ["chrome", "safari", "edge",] } }

[[overrides]]
_context_ = { browser = "edge" }
notification_enabled = false

[[overrides]]
_context_ = { platform = "web" }
notification_enabled = true
```
## Cascading configurations and staggering the change
As we did this - we noticed an interesting nexus between cascading configurations and the need to be able to stagger configuration changes.  Staggering of configuration changes can be treated as an experimental override if we capture the experimental variation as part of the context.  Hence, the cascading nature of the configuration was not just an independent requirement, it also formed the basis for supporting configuration staggering.  To illustrate this, let us say we want to enable notifications for the `edge` browser - but stagger the change.

```toml
[default-config]
notification_enabled = { "value" = false, "schema" = { "type" = "boolean" } }

[dimensions]
bucket = { position = 1, schema = { "type" = "string" } }
platform = { position = 2, schema = { "type" = "string", "enum" = ["ios", "android", "web"] } }
browser = { position = 3, schema = { "type" = "string", "enum" = ["chrome", "safari", "edge",] } }

[[overrides]]
_context_ = { browser = "edge" }
notification_enabled = false

[[overrides]]
_context_ = { platform = "web" }
notification_enabled = true

[[overrides]]
_context_ = { browser = "edge", bucket="old"} # acts as the control bucket for the configuration change
notification_enabled = false

[[overrides]]
_context_ = { browser = "edge", bucket="new"} # acts as the experimental variant for the configuration change
notification_enabled = true
```
**Note:** In the above example - when a particular flow is assigned to the `new` bucket vs the `old` is not dictated by the core configuration system.  It is typically done by an experimentation assignment system while finally delegating the configuration resolution to the core configuration system.  

We finalized the above to be the core of our configuration system and it has served us well.

## Inspiration
We would be failing in our duty if we do not call out pieces of technology from where we drew our inspiration to build Superposition:

1. [Yahoo Configuration Bundles](http://github.com/yahoo/ycb) (for the concept of dimensions/overrides)
2. [Cascading Style Sheets](https://developer.mozilla.org/en/docs/Web/CSS) (for the concept of specificity)

#### P.S.
_While we illustrated the core capabilities of Superposition using a toml representation above, for a full fledged cloud application, Superposition is available as a deployable service.  We will get into the architecture of the Superposition service in a following post._
