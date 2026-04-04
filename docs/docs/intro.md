---
sidebar_position: 1
title: Introduction
slug: /
---
Superposition is a configuration and experimentation management platform that allows software teams to manage their configurations safely and allows them to run multi-variate experiments on those configurations. Superposition places a strong emphasis on safety of configuration changes.  

## Why use Superposition?

The most common alternatives to Superposition are

- Environment Variables (ENVs)
- Database Tables
- Feature Flag systems

In this section, lets talk about when you would want to use Superposition over these options.

### Cascading

The big differentiator for Superposition is its ability to cascade configurations based on the context matched from the query sent by the application. You can learn more about [overrides add proper link](link), which reduces the number of configurations you need to create to achieve a desired state. [Cascading Style Sheets (CSS)](https://juspay.io/superposition/docs/basic-concepts/context-aware-config#analogy-with-css) is a good mental model to understand this behaviour. 

As far as we know, no other way to configuration complex applications have this feature. This is why we built Superposition.

### Strong Typing

Superposition supports strong and flexible typing via jsonschema. 

- ENVs

    These kinds of type validations are impossible in ENVs, where the application must catch type mismatches at runtime - the worst place to catch type issues. This often leads to app crashes

- Databases

    Databases have types, but jsonschema is more flexible than a standard database type. For example, in postgres the JSON and JSONB accept any valid JSON as input. The validation generally sits in your application to ensure the JSON conforms to a shape that suits your needs. In jsonschema, you can [embed this in the type](https://json-schema.org/understanding-json-schema/reference/object#additionalproperties):
    ```
    {
    "type": "object",
    "properties": {
        "number": { "type": "number" },
        "street_name": { "type": "string" },
        "street_type": { "enum": ["Street", "Avenue", "Boulevard"] }
    },
    "additionalProperties": false
    }
    ```

- Feature Flag Systems

    Different Systems have different support for types, some even use jsonschema


### Correctness 

Even if the type of a config value is correct, is the value appropriate? For example, how do you an ensure a URL whose type is string is actually valid? Superposition does this with [functions](functions)

- ENVs

    The application needs to validate correctness

- Database

    The application needs to validate correctness

- Feature Flag Systems

    Depends on the system used

### Staggering Releases

Superposition lets you stagger configuration changes, letting you catch issues early or experiment with a particular value and making sure it achieves your desired outcome. 

- Envs & Databases

    These are generally changes that immediately impact 100% of users and if staggering exists then the application manages the release

- Feature Flag Systems

    Depends on the system used

This comprehensive setup gives applications a robust platform to roll-out changes safely.

## Getting started

### Setup the server
The fastest way to setup Superposition is to use the following docker command which runs Superposition and its dependencies (a Postgres database) locally:

```
docker run -p 8080:8080 ghcr.io/juspay/superposition-demo:latest
```

Once you run this command, you can access the Superposition admin interface at `localhost:8080`. Play around to understand Superposition better, then dive into the documentation below!

### Integrating Superposition in your application

Once you have played with the Superposition admin interface, you may want to consume the configuration in your application.  Superposition is [OpenFeature](https://openfeature.dev/docs/reference/concepts/provider) compatible.  OpenFeature allows your application code to remain agnostic of the underlying configuration/feature-flag platform (like [open telemetry](https://opentelemetry.io/) for telemetry).  The [quick start guide](https://juspay.io/superposition/docs/quick_start) has details on how to integrate and consume configurations (setup in Superposition) in your application using the Superposition Open Feature provider.

## Superposition Providers

Superposition comes with a variety of providers with openfeature compatibility and with support for multiple programming languages to interact with the Superposition platform.

1. `sdk` - this library contains methods to interact with the control plane of Superposition to manage configurations and experiments.  In short, all APIs supported by the Superposition platform can be invoked using the sdk (built using [AWS' Smithy IDL](https://smithy.io)).
2. `provider` (or open-feature-provider) - this open feature compatible library is meant to be used by the applications that consume configurations hosted in Superposition.  This has support for in-memory-caching and period polling based refresh of configuration in the application.

The following matrix contains the languages in which the above client libraries are available:

| Language       | sdk | provider |
|----------------|-----|----------|
| Rust           | [![Crates.io Version](https://img.shields.io/crates/v/superposition_sdk?color=green&label=superposition_sdk)](https://crates.io/crates/superposition_sdk) | [![Crates.io Version](https://img.shields.io/crates/v/superposition_provider?color=green&label=superposition_provider)](https://crates.io/crates/superposition_provider) |
| Javascript     | [![NPM Version](https://img.shields.io/npm/v/superposition-sdk?color=green&label=superposition-sdk)](https://www.npmjs.com/package/superposition-sdk) |  [![NPM Version](https://img.shields.io/npm/v/superposition-provider?color=green&label=superposition-provider)](https://www.npmjs.com/package/superposition-provider) |
| Python         | [![PyPI - Version](https://img.shields.io/pypi/v/superposition_sdk?color=green&label=superposition_sdk)](https://pypi.org/project/superposition-sdk/) | [![PyPI - Version](https://img.shields.io/pypi/v/superposition_provider?color=green&label=superposition_provider)](https://pypi.org/project/superposition-provider/) |
| Java           | [![Maven Central Version](https://img.shields.io/maven-central/v/io.juspay.superposition/sdk?label=io.juspay.superposition.sdk&color=green)](https://central.sonatype.com/artifact/io.juspay.superposition/sdk) | [![Maven Central Version](https://img.shields.io/maven-central/v/io.juspay.superposition/openfeature-provider?label=io.juspay.superposition.openfeature-provider&color=green)](https://central.sonatype.com/artifact/io.juspay.superposition/openfeature-provider)|
| Haskell        | ![Static Badge](https://img.shields.io/badge/SDK-Github-8A2BE2?style=flat&logoColor=purple&label=SDK&color=8A2BE2&cacheSeconds=30&link=https%3A%2F%2Fgithub.com%2Fjuspay%2Fsuperposition%2Ftree%2Fmain%2Fclients%2Fhaskell%2Fsdk) | ![Static Badge](https://img.shields.io/badge/SDK-Github-8A2BE2?style=flat&logoColor=purple&label=Provider&color=8A2BE2&cacheSeconds=30&link=https%3A%2F%2Fgithub.com%2Fjuspay%2Fsuperposition%2Ftree%2Fmain%2Fclients%2Fhaskell%2Fopen-feature-provider) |
| Go             | TBD |    TBD   |

## When to not use Superposition

You should not use Superposition when the following conditions are met:

- Superposition is great when you have a lot of users/tenants that have the same config but some diverge slightly. If you have a case that is the inverse of this, you should consider other options before superposition

## Applications using Superposition

Superposition comes as a shot in the arm for any application that needs safe and flexible configurability.  We have built applications that leverage the Superposition Platform across different parts of the software stack 
    - Frontend
        - [Dynamic Payment Fields](https://github.com/juspay/superposition/tree/main/examples/dynamic-payment-fields)
        - Internal Juspay Dashboards 
    - Backend
        - [Airborne](https://airborne.juspay.in/)
        - [Hyperswitch](https://hyperswitch.io/) 
    - Infrastructure
        - [K8S Staggered Release](https://github.com/juspay/superposition/tree/main/examples/k8s-staggered-releaser)
        - [Superfuse](https://github.com/juspay/superfuse)
    - Storage
        - [Redis Module](https://github.com/juspay/superposition/tree/main/examples/cac_redis_module)

## Key highlights

* **Admin UI** - Out of the box administration (and tools) UI for configurations and experiments
* **Rich API support** - every action on the platform to manage configurations / experiments is supported with an accompanying API (and in the SDK as well)
* **Safe configuration changes** - support canary testing for releasing configuration changes using experiments
* **Type/Validation support** - Comprehensive type support using json-schema and custom validator function support for configuration values
* **Multi-tenant support** - a single deployment allows multiple tenants to manage their configurations/experiments in a completely isolated manner
* **Authn/Authz support** - RBAC support to decide can make configuration/experimentation changes

## Email us
* [superposition@juspay.in](mailto:superposition@juspay.in)

