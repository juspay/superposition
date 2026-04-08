---
sidebar_position: 1
title: Overview
slug: /providers/openfeature
---

# Superposition OpenFeature Providers

OpenFeature provider implementations for [Superposition](https://github.com/juspay/superposition), enabling feature flag management, context-aware configuration, and experimentation in multiple programming languages.

## Features

- 🚩 **Feature Flag Management** — Full OpenFeature compatibility for boolean, string, integer, float, and object flags
- 🎯 **Context-Aware Configuration** — Dynamic configuration based on user context and dimensions
- ⚡ **Multiple Refresh Strategies** — Polling and on-demand configuration fetching
- 🔄 **Real-time Updates** — Automatic configuration refresh with configurable polling
- 🛡️ **Error Handling** — Graceful fallbacks and stale data usage on errors
- 🧪 **Experimentation** — Built-in support for A/B testing and feature experiments
- 🔌 **Pluggable Data Sources** — HTTP server, local files, or custom data sources

## Supported Languages

| Language   | Package                                                                                   | Docs                                  |
| ---------- | ----------------------------------------------------------------------------------------- | ------------------------------------- |
| Rust       | [`superposition_provider`](https://crates.io/crates/superposition_provider)               | [Rust Provider](./rust)               |
| Python     | [`superposition-provider`](https://pypi.org/project/superposition-provider/)              | [Python Provider](./python)           |
| JavaScript | [`superposition-provider`](https://www.npmjs.com/package/superposition-provider)          | [JavaScript Provider](./javascript)   |
| Java       | [`io.juspay.superposition:openfeature-provider`](https://central.sonatype.com/artifact/io.juspay.superposition/openfeature-provider) | [Java Provider](./java) |

## Prerequisites

Before using any provider, ensure that:

1. **Superposition server is running** — See [Quick Start](../../quick_start)
2. **Valid credentials** are configured — `token`, `org_id`, `workspace_id`
3. **Feature flags / default configs** are set up in your Superposition workspace

## Common Concepts

### Provider Architecture

All providers offer two approaches to flag evaluation:

| Variant | Description | When to Use |
| ------- | ----------- | ----------- |
| **`LocalResolutionProvider`** | Fetches config from a data source (HTTP server or local file), caches locally, and evaluates flags in-process. **Recommended for most use cases.** | Production applications — low latency, supports offline fallback, supports all refresh strategies |
| **`SuperpositionAPIProvider`** | A stateless remote provider that calls the Superposition server on every evaluation. No local caching. | Serverless, low-traffic, or always-latest requirements |

The `LocalResolutionProvider` uses pluggable **data sources** (e.g. `HttpDataSource`, `FileDataSource`) and supports an optional **fallback data source** for resilient initialization.

### Evaluation Context

All providers use an **evaluation context** — a set of key-value pairs (dimensions) used to resolve the correct configuration:

- **`targeting_key`** — Used for experiment variant bucketing (typically a user ID)
- **Custom attributes** — Map to your Superposition dimensions (e.g. `city`, `os`, `customers`)

### Refresh Strategies

Providers support multiple refresh strategies for keeping configuration up to date:

| Strategy      | Description                                                          | Availability  |
| ------------- | -------------------------------------------------------------------- | ------------- |
| **Polling**   | Periodically fetches config updates at a fixed interval              | All languages |
| **On-Demand** | Fetches on first access, then caches with a configurable TTL         | All languages |
| **Watch**     | Uses file-system notifications to reload on file changes             | Rust only     |
| **Manual**    | No automatic refresh; user triggers refresh explicitly               | Rust only     |

### Experimentation

When experimentation options are configured, the provider will:

1. Fetch running experiments from the server
2. Use the `targeting_key` to determine which variant a user belongs to
3. Apply experiment overrides to the resolved configuration

All providers support configurable `ExperimentationOptions` including refresh strategy, evaluation cache, and a default toss value.
