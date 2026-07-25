---
slug: superposition-genesis
title: Superposition - its genesis?
description: 
tags: [superposition, genesis, introduction]
---

This blog post talks about the genesis of Superposition from a first principles perspective - what made us build it and the thought process behind why it was structured the way it was.

There were many applications in Juspay that needed runtime configuration management systems.  Many systems relied on json files deployed on S3.  There were just too many issues in this model of serving configuration files.  No problem modelling of the configuration values, no tooling to safely edit and roll out those changes in a safe manner.  Some of these configuration changes were the cause of production outages as well.  Superposition started off as a system to address this use-case.

<!-- truncate -->

With safety being a top priority, we anchored on the following three core safety principles:

1. Safely change configuration values at runtime, supporting the following:
    1. Type safety
    2. Custom validations
    3. Staggered roll-out

If our system could factor in the above - we would have done well.

Safety aside, another salient requirement in our applications was that under some domain context - the configurations took a different value i.e. in other words the value needs to be overridden under come conditions.  This is either managed with control blocks in code (simple if-else statements) or by suffixing/prefixing the key with a string that represents the context and defining a new row for that key with the overridden value.  In most cases, when no override exists for a specific context, the typical expectation is to fallback to a default value.

We added this as one of the core problems that the new configuration system should address.

1. Safely change configuration values at runtime, supporting the following:
    1. Type safety
    2. Custom validations
    3. Staggered roll-out
2. **Cascading configuration values**

Once we decided to support cascading configuration values, we had to solve for how to define contexts under which configuration values could change and how do we disambiguate between competing contexts that might apply for a single domain flow.

And so we decided to model the concept of `contexts`.  Contexts are simple boolean expressions involving key attributes that matter to your domain.  We chose to call these key attributes `dimensions`.  Dimensions are also defined with their associated types to ensure that values set for dimensions in contexts are validated and type-safe.  As a concrete example, let us say your application is available across Android, iOS and Web - you might want to create a dimension which is an enum holding one of three possible values: `ios`, `android` and `web`.  Below is an example that shows a configuration key having a default value, but having an override when `platform` is `web`.  
```toml
[default-config]
notification_enabled = { "value" = false, "schema" = { "type" = "boolean" } }

[dimensions]
platform = { schema = { "type" = "string", "enum" = ["ios", "android", "web"] } }

[[overrides]]
_context_ = { platform = "web" }
notification_enabled = true
```

Now let us say your application needs to disable notifications when users use the Edge browser.  To facilitate this override, you would create a new dimension called browser with the following as possible values: 
```
chrome, safari, edge, arc, brave
```

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
With this we have landed ourselves in a bit of a soup - if someone accesses your application over the `web` from an `edge` browser both the contexts evaluate to `true`.  So which configuration should apply?  The easiest choice would have been the latest applies.  We did not want to do this as it brings in a lack of predictability when multiple people can modify configurations and might not share the same idea of prioritizing the overrides.  Secondly, the latest wins approach does not scale well when the number of the overrides grow - it is very hard to manage relative priority of the overrides even at a small cardinality of 10s of overrides and becomes error-prone.  Such an approach fundamentally affects the core requirement of the configuration system - safety.  The key insight here being complex systems tend to become unsafe and hence we had to simplify this.  We decided to force the user to tie-break between the dimensions i.e. explicitly decide their priority based on their domain contexts.  While this choice might seem constraining - we believe it brings in the clarity and communicates clearly to all users of the configuration system that the relative priorities are.  It makes it easy to author an override without worrying about its relative prioritization.  We also believe at a deeper level most systems end-up having a priority between these dimensions implicitly in code if it is not explicitly captured.

So we refined the definition of what we wanted to build.  We wanted to build a configuration system that would enable the following:

1. Safely change configuration values at runtime, supporting the following:
    1. Type safety
    2. Custom validations
    3. Staggered roll-out
2. **Unambiguous cascading of configuration values**

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
So in the above example the override with the `browser` context check triumphs even though it appears before the one with the `platform` context.  This was achieved by explicitly declaring a position in the definition of the dimensions.

As we did this - we noticed an interesting nexus between cascading configurations and the need to be able to stagger configuration changes.  Staggering of configuration changes can be treated as an experimental override if we make the experimental variation as part of the context.  Hence, the cascading nature of the configuration was not just an independent requirement, it also formed the basis for supporting configuration staggering.
