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
- ⚡ **Multiple Refresh Strategies** — Polling and on-demand configuration fetching across providers, with watch/manual support where implemented
- 🔄 **Real-time Updates** — Automatic configuration refresh with configurable polling
- 🛡️ **Error Handling** — Provider-specific fallback and stale-cache behavior where implemented
- 🧪 **Experimentation** — Support for A/B testing and feature experiments where enabled by the provider
- 🔌 **Provider-Specific Data Sources** — HTTP server, local files, or custom data sources depending on the language implementation

## Supported Languages

| Language   | Package                                                                                   | Docs                                  |
| ---------- | ----------------------------------------------------------------------------------------- | ------------------------------------- |
| Rust       | [`superposition_provider`](https://crates.io/crates/superposition_provider)               | [Rust Provider](/docs/providers/openfeature/rust)               |
| Python     | [`superposition-provider`](https://pypi.org/project/superposition-provider/)              | [Python Provider](/docs/providers/openfeature/python)           |
| JavaScript | [`superposition-provider`](https://www.npmjs.com/package/superposition-provider)          | [JavaScript Provider](/docs/providers/openfeature/javascript)   |
| Java       | `io.juspay.superposition.openfeature:superposition-provider` | [Java Provider](/docs/providers/openfeature/java) |
| Haskell    | [`superposition-open-feature-provider`](https://hackage.haskell.org/package/superposition-open-feature-provider) | [Haskell Provider](/docs/providers/openfeature/haskell) |

## Prerequisites

Before using any provider, ensure that:

1. **Superposition server is running** — See [Quick Start](/docs/quick_start)
2. **Valid credentials** are configured — `token`, `org_id`, `workspace_id`
3. **Feature flags / default configs** are set up in your Superposition workspace

## Common Concepts

### Provider Architecture

Provider architecture varies by language. Rust and Python currently expose the
newer data-source provider API with `LocalResolutionProvider` and
`SuperpositionAPIProvider`. JavaScript, Java, and Haskell currently expose a
single language-specific provider API backed by the Superposition HTTP API and
local provider caches.

| Variant | Description | When to Use |
| ------- | ----------- | ----------- |
| **`LocalResolutionProvider`** | Fetches config from a data source, caches locally, and evaluates flags in-process. Available in languages that have adopted the data-source provider API. | Production applications that need low-latency local evaluation |
| **`SuperpositionAPIProvider`** | A stateless remote provider that calls the Superposition server on every evaluation. No local caching. Available only in providers that implement it. | Serverless, low-traffic, or always-latest requirements |
| **`SuperpositionProvider` / `SuperpositionOpenFeatureProvider`** | Language-specific provider wrappers used by current JavaScript, Java, and Haskell providers. | Applications using the current single-provider APIs |

Where available, `LocalResolutionProvider` uses pluggable **data sources**
(for example `HttpDataSource` and `FileDataSource`) and may support an optional
**fallback data source** for resilient initialization. Check the language page
for the exact API surface.

### Evaluation Context

All providers use an **evaluation context** — a set of key-value pairs (dimensions) used to resolve the correct configuration:

- **`targeting_key`** — Used for experiment variant bucketing (typically a user ID)
- **Custom attributes** — Map to your Superposition dimensions (e.g. `city`, `os`, `customers`)

### Refresh Strategies

Providers support multiple refresh strategies for keeping configuration up to date:

| Strategy      | Description                                                          | Availability  |
| ------------- | -------------------------------------------------------------------- | ------------- |
| **Polling**   | Periodically fetches config updates at a fixed interval              | All documented providers |
| **On-Demand / lazy fetch** | Fetches on first access, or refreshes from a TTL-backed cache where implemented | Language-specific; see each provider page |
| **Watch**     | Uses file-system notifications to reload on file changes             | Rust and Python |
| **Manual**    | No automatic refresh; user triggers refresh explicitly               | Rust and Python |

### Experimentation

When experimentation support is configured, providers generally:

1. Fetch running experiments from the server
2. Use the `targeting_key` to determine which variant a user belongs to
3. Apply experiment overrides to the resolved configuration

The exact experimentation options vary by language. Rust and Python expose
`ExperimentationOptions` with refresh and cache settings. JavaScript uses an
`experimentationOptions` object, Java uses nested
`SuperpositionProviderOptions.ExperimentationOptions`, and Haskell currently
uses `experimentationRefreshOptions` without evaluation-cache or default-toss
options.
