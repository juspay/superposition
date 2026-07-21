---
sidebar_position: 4
title: JavaScript
---

# JavaScript - Superposition OpenFeature Provider

The JavaScript package currently exposes one Node.js OpenFeature provider:
`SuperpositionProvider`.

It fetches configuration from the Superposition HTTP API, initializes a native
resolver cache through `superposition-bindings`, and evaluates OpenFeature
values from that local cache.

The JavaScript provider supports:

- OpenFeature evaluation for boolean, string, number, and object values
- Full resolved config lookup through `resolveAllConfigDetails`
- Polling refresh for configuration
- Optional experimentation refresh
- Provider cleanup through `OpenFeature.close()` or provider close hooks

It does not currently export `LocalResolutionProvider`,
`SuperpositionAPIProvider`, `HttpDataSource`, `FileDataSource`, local-file
resolution, watch refresh, manual refresh, or pluggable data sources.

**npm:** [`superposition-provider`](https://www.npmjs.com/package/superposition-provider)

## Installation

```bash
npm install @openfeature/server-sdk superposition-provider
```

:::note
You need a running Superposition server. See [Quick Start](../../quick_start) for setup instructions.
:::

## Quick Start

```javascript
import { OpenFeature } from "@openfeature/server-sdk";
import { SuperpositionProvider } from "superposition-provider";

async function main() {
    const provider = new SuperpositionProvider({
        endpoint: "http://localhost:8080",
        token: "your-api-token",
        org_id: "localorg",
        workspace_id: "test",
        refreshStrategy: {
            interval: 60000,
            timeout: 30000,
        },
    });

    await OpenFeature.setProviderAndWait(provider);
    const client = OpenFeature.getClient();

    const context = {
        targetingKey: "user-42",
        city: "Berlin",
        os: "android",
    };

    const currency = await client.getStringValue("currency", "USD", context);
    console.log("currency:", currency);

    const price = await client.getNumberValue("price", 0, context);
    console.log("price:", price);

    const darkMode = await client.getBooleanValue("dark_mode", false, context);
    console.log("dark_mode:", darkMode);

    const allConfig = await provider.resolveAllConfigDetails({}, context);
    console.log("All config:", allConfig);

    await OpenFeature.close();
}

main().catch(console.error);
```

## Provider Options

Create a provider with `new SuperpositionProvider(options)`.

| Field | Type | Required | Description |
| ----- | ---- | -------- | ----------- |
| `endpoint` | `string` | Yes | Superposition server URL |
| `token` | `string` | Yes | Authentication token |
| `org_id` | `string` | Yes | Organisation ID |
| `workspace_id` | `string` | Yes | Workspace ID |
| `httpClient` | `any` | No | Custom Smithy request handler |
| `refreshStrategy` | `RefreshStrategy` | No | Configuration refresh behavior |
| `experimentationOptions` | `ExperimentationOptions` | No | Enables experiment and experiment-group refresh |
| `fallbackConfig` | `ConfigData` | No | Present on the TypeScript interface, but not automatically installed as fallback data by current provider initialization |
| `evaluationCache` | `EvaluationCacheOptions` | No | Present on the TypeScript interface; current configuration evaluation does not apply this cache option |

```javascript
const provider = new SuperpositionProvider({
    endpoint: "http://localhost:8080",
    token: "your-api-token",
    org_id: "localorg",
    workspace_id: "test",
    refreshStrategy: { interval: 60000, timeout: 30000 },
});
```

## Refresh Strategies

The current TypeScript `RefreshStrategy` type is a union of two object shapes:

```typescript
type PollingStrategy = {
    interval: number;
    timeout?: number;
};

type OnDemandStrategy = {
    ttl: number;
    timeout?: number;
    use_stale_on_error?: boolean;
};
```

For configuration refresh, the current `ConfigurationClient` starts background
polling when `refreshStrategy` has an `interval` field. Without an interval, it
fetches configuration lazily when the provider evaluates config.

```javascript
// Poll configuration every 60 seconds.
refreshStrategy: {
    interval: 60000,
    timeout: 30000,
}
```

For experimentation refresh, both polling and TTL-based on-demand strategies are
handled by `ExperimentationClient`.

## Experimentation

Enable experimentation by passing `experimentationOptions`:

```javascript
const provider = new SuperpositionProvider({
    endpoint: "http://localhost:8080",
    token: "your-api-token",
    org_id: "localorg",
    workspace_id: "test",
    refreshStrategy: { interval: 60000, timeout: 30000 },
    experimentationOptions: {
        refreshStrategy: {
            interval: 60000,
            timeout: 30000,
        },
    },
});
```

Experimentation uses `context.targetingKey` for bucketing. The current
`ExperimentationOptions` interface also includes `evaluationCache` and
`defaultIdentifier`, but those are provider-specific options rather than the
Rust/Python `default_toss` API.

## Evaluation Context

OpenFeature context properties other than `targetingKey`, `timestamp`, and
internal `__` fields are passed to Superposition as dimensions when they are
simple serializable values.

```javascript
const context = {
    targetingKey: "user-42",
    city: "Berlin",
    os: "ios",
    customers: "platinum",
};

const value = await client.getStringValue("currency", "USD", context);
```

## Supported Value Types

| Method | Return type |
| ------ | ----------- |
| `getBooleanValue` | `boolean` |
| `getStringValue` | `string` |
| `getNumberValue` | `number` |
| `getObjectValue` | object / JSON-like value |

For the full resolved configuration, call the provider directly:

```javascript
const allConfig = await provider.resolveAllConfigDetails({}, context);
```

## Current Limitations

The following provider features are not implemented in JavaScript yet:

- `LocalResolutionProvider`
- `SuperpositionAPIProvider`
- `HttpDataSource` and `FileDataSource`
- Local SuperTOML or JSON file resolution
- Pluggable data sources
- Watch refresh
- Manual refresh
- Automatic fallback data source initialization
