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

### Evaluation Context

All providers use an **evaluation context** — a set of key-value pairs (dimensions) used to resolve the correct configuration:

- **`targeting_key`** — Used for experiment variant bucketing (typically a user ID)
- **Custom attributes** — Map to your Superposition dimensions (e.g. `city`, `os`, `customers`)

### Refresh Strategies

All providers support at least two refresh strategies:

| Strategy      | Description                                                          |
| ------------- | -------------------------------------------------------------------- |
| **Polling**   | Periodically fetches config updates at a fixed interval              |
| **On-Demand** | Fetches on first access, then caches with a configurable TTL         |

The Rust provider additionally supports **Watch** (file-system change notifications) and **Manual** (user-triggered refresh).

### Experimentation

When experimentation options are configured, the provider will:

1. Fetch running experiments from the server
2. Use the `targeting_key` to determine which variant a user belongs to
3. Apply experiment overrides to the resolved configuration
