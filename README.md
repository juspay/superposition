# Superposition

<p align="center">
<img src="https://juspay.io/images/superposition/logo.jpg" alttext="Superposition Logo" width="400">
</p>

![GitHub License](https://img.shields.io/github/license/juspay/superposition)
![Dynamic TOML Badge](https://img.shields.io/badge/dynamic/toml?url=https%3A%2F%2Fraw.githubusercontent.com%2Fjuspay%2Fsuperposition%2Frefs%2Fheads%2Fmain%2FCargo.toml&query=workspace.package.version&label=version&color=green)
[![GitHub Release Date](https://img.shields.io/github/release-date-pre/juspay/superposition)](https://github.com/juspay/superposition/releases) 
![GitHub Actions Workflow Status](https://img.shields.io/github/actions/workflow/status/juspay/superposition/release.yaml)

[![Crates.io Downloads (latest version)](https://img.shields.io/crates/dv/superposition_provider?label=openfeature-provider%40crates.io)](https://crates.io/crates/superposition_provider)
[![Crates.io Downloads (latest version)](https://img.shields.io/crates/dv/superposition_sdk?label=superposition%20sdk%40crates.io)](https://crates.io/crates/superposition_sdk)
[![NPM Downloads](https://img.shields.io/npm/dm/superposition-provider?label=openfeature%20provider%40npm)](https://www.npmjs.com/package/superposition-provider)
[![NPM Downloads](https://img.shields.io/npm/dm/superposition-sdk?label=superposition%20sdk%40npm)
](https://www.npmjs.com/package/superposition-sdk)


![X (formerly Twitter) Follow](https://img.shields.io/twitter/follow/superpositionJP)
[![Discord badge](https://img.shields.io/discord/1280216553350107258?label=Discord&logo=Discord)](https://discord.gg/jNeUJR9Bwr) 


Superposition is a configuration and experimentation management platform that allows software teams to manage their configurations (any data-type) safely.  It allows teams to run multi-variate experiments on these configuration values.  Superposition places a strong emphasis on safety of configuration changes.  It does this by supporting strong typing (via [json-schema](https://json-schema.org/)), custom validation functions (with an embedded JS runtime) and supporting staggering configuration changes via experiments.  This gives applications robust platform to roll-out changes safely.

The Superposition platform comprises of two components:

* **Context-Aware-Configuration** - a flexible configuration management system that supports contextual overrides for configuration keys
* **Experimentation** - a experimentation management system that allows supplying different configuration values to equal-sized cohorts (facilitating A/B/...Z testing)

## Getting started

### Setup the server
The fastest way to setup Superposition is to use the following docker command which runs Superposition and its dependencies (a Postgres database) locally:

```
docker run -p 8080:8080 ghcr.io/juspay/superposition-demo:latest
```

Once you run this command, you can access the Superposition admin interface at `localhost:8080`. Play around to understand Superposition better, then dive into the documentation below!

### Integrating Superposition in your application

Once you have played with the Superposition admin interface, you may want to consume the configuration in your application.  Superposition is [OpenFeature](https://openfeature.dev/docs/reference/concepts/provider) compatible.  OpenFeature allows your application code to remain agnostic of the underlying configuration platform (like [open telemetry](https://opentelemetry.io/) for telemetry).  The [quick start guide](https://juspay.io/superposition/docs/quick_start) has details on how to integrate and consume configurations setup in Superposition in your application using the Superposition Open Feature provider.

## Superposition Clients

The Superposition platform provides a variety of clients to interact with the Superposition platform.

1. sdk - this library contains methods to interact with the control plane of Superposition to manage configurations and experiments.  In short, all APIs supported by the Superposition platform can be invoked using the sdk (built using [AWS' Smithy IDL](https://smithy.io)).
2. provider (or open-feature-provider) - this open feature compatible library is meant to be used by the applications that consume configurations hosted in Superposition.  This has support for in-memory-caching and period polling based refresh of configuration in the application.
3. bindings - this library contains the core configuration resolution logic - which is wrapped by the provider in an open-feature compatible format

## Applications using Superposition

Superposition comes as a shot in the arm for any application that needs safe and flexibile configurability.  We have build many internal applications that leverage the Superposition across different parts of the software stack - frontend, backend, infra, storage.

## More links
1. Conceptual docs
    * [Context-Aware-Configuration](https://juspay.io/superposition/docs/basic-concepts/context-aware-config)
    * [Experimentation](https://juspay.io/superposition/docs/basic-concepts/experimentation)
3. [Development setup](https://juspay.io/superposition/docs/setup)
4. API Ref (TBD)
5. [TOML formatted Context-Aware-Configuration example](docs/docs/experimental/cac-toml.md)

## Key highlights
* **Admin UI** - Out of the box administration (and tools) UI for configurations and experiments
* **Rich API support** - every action on the platform to manage configurations / experiments is supported with an accompanying API
* **Safe configuration changes** - support canary testing for releasing configuration changes using experiments
* **Type/Validation support** - Comprehensive type support using json-schema and custom validator function support for configuration values
* **Multi-tenant support** - a single deployment allows multiple tenants to manage their configurations/experiments in a completely isolated manner
* **Authn/Authz support** - control who can make configuration/experimentation changes

## Email us
* [superposition@juspay.in](mailto:superposition@juspay.in)
