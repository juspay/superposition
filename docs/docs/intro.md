---
sidebar_position: 1
title: Introduction
slug: /
---
Superposition is a configuration and experimentation management platform that allows software teams to manage their configurations safely and allows them to run multi-variate experiments on those configurations. Superposition places a strong emphasis on safety of configuration changes.  It does this by the following:

* supporting strong typing of configuration values (via [json-schema](https://json-schema.org/))
* custom validation functions (written in Javascript) that can validate configuration values using state-less logic or validate them against external data-sources
* supporting staggering configuration changes via experiments.
* allows configurations to cascade from least specific to most specific contexts [much like CSS](https://juspay.io/superposition/docs/basic-concepts/context-aware-config#analogy-with-css)

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

## Superposition Clients

Superposition comes with a variety of clients supported in multiple programming languages to interact with the Superposition platform.

1. `sdk` - this library contains methods to interact with the control plane of Superposition to manage configurations and experiments.  In short, all APIs supported by the Superposition platform can be invoked using the sdk (built using [AWS' Smithy IDL](https://smithy.io)).
2. `provider` (or open-feature-provider) - this open feature compatible library is meant to be used by the applications that consume configurations hosted in Superposition.  This has support for in-memory-caching and period polling based refresh of configuration in the application.

The following matrix contains the languages in which the above client libraries are available:

| Language       | sdk | provider |
|----------------|-----|----------|
| Rust           | [![Crates.io Version](https://img.shields.io/crates/v/superposition_sdk?color=green&label=superposition_sdk)](https://crates.io/crates/superposition_sdk) | [![Crates.io Version](https://img.shields.io/crates/v/superposition_provider?color=green&label=superposition_provider)](https://crates.io/crates/superposition_provider) |
| Javascript     | [![NPM Version](https://img.shields.io/npm/v/superposition-sdk?color=green&label=superposition-sdk)](https://www.npmjs.com/package/superposition-sdk) |  [![NPM Version](https://img.shields.io/npm/v/superposition-provider?color=green&label=superposition-provider)](https://www.npmjs.com/package/superposition-provider) |
| Python         | [![PyPI - Version](https://img.shields.io/pypi/v/superposition_sdk?color=green&label=superposition_sdk)](https://pypi.org/project/superposition-sdk/) | [![PyPI - Version](https://img.shields.io/pypi/v/superposition_provider?color=green&label=superposition_provider)](https://pypi.org/project/superposition-provider/) |
| Java           | [![Maven Central Version](https://img.shields.io/maven-central/v/io.juspay.superposition/sdk?color=green)](https://central.sonatype.com/artifact/io.juspay.superposition/sdk) | [![Maven Central Version](https://img.shields.io/maven-central/v/io.juspay.superposition/openfeature-provider?color=green)](https://central.sonatype.com/artifact/io.juspay.superposition/openfeature-provider)|
| Haskell        | WIP |    WIP   |
| Go             | TBD |    TBD   |

## Deeper dive into Superposition

For a deeper dive into the under-pinnings of Superposition, development setup, API docs - you can go over the following links.
1. Conceptual docs on two foundational services of Superposition:
    * [Context-Aware-Configuration](https://juspay.io/superposition/docs/basic-concepts/context-aware-config)
    * [Experimentation](https://juspay.io/superposition/docs/basic-concepts/experimentation)
3. [Development setup](https://juspay.io/superposition/docs/setup)
4. API Ref (TODO:)

## Applications using Superposition

Superposition comes as a shot in the arm for any application that needs safe and flexible configurability.  We have built applications that leverage the Superposition across different parts of the software stack - [frontend](examples/dynamic-payment-fields), backend, [infra](examples/k8s-staggered-releaser) and [storage](examples/cac_redis_module).

## Key highlights

* **Admin UI** - Out of the box administration (and tools) UI for configurations and experiments
* **Rich API support** - every action on the platform to manage configurations / experiments is supported with an accompanying API (and in the SDK as well)
* **Safe configuration changes** - support canary testing for releasing configuration changes using experiments
* **Type/Validation support** - Comprehensive type support using json-schema and custom validator function support for configuration values
* **Multi-tenant support** - a single deployment allows multiple tenants to manage their configurations/experiments in a completely isolated manner
* **Authn/Authz support** - RBAC support to decide can make configuration/experimentation changes

## Email us
* [superposition@juspay.in](mailto:superposition@juspay.in)
