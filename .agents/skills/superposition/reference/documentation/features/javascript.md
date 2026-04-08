---
sidebar_position: 4
title: JavaScript
---

# JavaScript — Superposition OpenFeature Provider

The JavaScript provider is an OpenFeature-compatible provider for Superposition that runs in Node.js. It offers two provider variants:

- **`LocalResolutionProvider`** — Fetches config from a data source (HTTP server), caches it locally, and evaluates flags in-process. Supports polling and on-demand refresh strategies. This is the **recommended provider** for most use cases.
- **`SuperpositionAPIProvider`** — A stateless remote provider that makes an HTTP API call to the Superposition server on every evaluation. No local caching — useful for serverless or low-traffic scenarios.

**npm:** [`superposition-provider`](https://www.npmjs.com/package/superposition-provider)

## Installation

```bash
npm install @openfeature/server-sdk superposition-provider
```

:::note
You need a running Superposition server. See [Quick Start](../../quick_start) for setup instructions.
:::

## Quick Start

This is the most common usage — the provider connects to a Superposition server via HTTP, polls for config updates, and evaluates flags locally.

```javascript
import { OpenFeature } from "@openfeature/server-sdk";
import {
    LocalResolutionProvider,
    HttpDataSource,
    SuperpositionOptions,
} from "superposition-provider";

async function main() {
    // 1. Create an HTTP data source pointing to your Superposition server
    const httpSource = new HttpDataSource(
        new SuperpositionOptions({
            endpoint: "http://localhost:8080",
            token: "your-api-token",
            orgId: "localorg",
            workspaceId: "test",
        })
    );

    // 2. Create the provider with a polling refresh strategy
    const provider = new LocalResolutionProvider({
        dataSource: httpSource,
        refreshStrategy: {
            type: "polling",
            interval: 60000, // milliseconds between polls
            timeout: 30000,  // HTTP request timeout in milliseconds
        },
    });

    // 3. Register with OpenFeature and wait for initialization
    await OpenFeature.setProviderAndWait(provider);
    console.log("Provider initialized successfully");

    // 4. Create a client
    const client = OpenFeature.getClient();

    // 5. Define evaluation context (dimensions)
    const context = {
        targetingKey: "user-42",
        city: "Berlin",
        os: "android",
    };

    // 6. Evaluate feature flags
    const stringVal = await client.getStringValue("currency", "USD", context);
    console.log("currency:", stringVal);

    const numberVal = await client.getNumberValue("price", 0, context);
    console.log("price:", numberVal);

    const boolVal = await client.getBooleanValue("dark_mode", false, context);
    console.log("dark_mode:", boolVal);

    // 7. Cleanup
    await OpenFeature.close();
}

main().catch(console.error);
```

## Configuration Options

### `SuperpositionOptions`

Connection options shared by `HttpDataSource` and `SuperpositionAPIProvider`:

| Field         | Type     | Required | Description                   |
| ------------- | -------- | -------- | ----------------------------- |
| `endpoint`    | `string` | Yes      | Superposition server URL      |
| `token`       | `string` | Yes      | Authentication token (bearer) |
| `orgId`       | `string` | Yes      | Organisation ID               |
| `workspaceId` | `string` | Yes      | Workspace ID                  |

```javascript
const options = new SuperpositionOptions({
    endpoint: "http://localhost:8080",
    token: "your-api-token",
    orgId: "localorg",
    workspaceId: "test",
});
```

### Refresh Strategies

```javascript
// Polling — periodically fetches updates from the server
{
    type: "polling",
    interval: 60000, // milliseconds between polls (default: 60000)
    timeout: 30000,  // HTTP request timeout in milliseconds (default: 30000)
}

// On-Demand — fetches on first access, then caches with a TTL
{
    type: "on-demand",
    ttl: 300,                // cache TTL in seconds (default: 300)
    useStaleOnError: true,   // serve stale data on fetch error (default: true)
    timeout: 30000,          // HTTP timeout in milliseconds (default: 30000)
}
```

### `ExperimentationOptions`

| Field              | Type                     | Required | Description                        |
| ------------------ | ------------------------ | -------- | ---------------------------------- |
| `refreshStrategy`  | `RefreshStrategy`        | Yes      | How experiment data is refreshed   |
| `evaluationCache`  | `EvaluationCacheOptions` | No       | Cache for experiment evaluations   |
| `defaultToss`      | `number`                 | No       | Default toss value for experiments |

```javascript
const experimentationOptions = {
    refreshStrategy: {
        type: "polling",
        interval: 5000,
        timeout: 2000,
    },
    evaluationCache: {
        ttl: 300,
        size: 1000,
    },
    defaultToss: 50,
};
```

### `EvaluationCacheOptions`

| Field  | Type     | Default | Description                     |
| ------ | -------- | ------- | ------------------------------- |
| `ttl`  | `number` | `60`    | Cache time-to-live in seconds   |
| `size` | `number` | `500`   | Maximum number of cache entries |

## Provider Variants

### 1. `LocalResolutionProvider` (Recommended)

Fetches config from a pluggable data source (HTTP), caches locally, and evaluates flags in-process. Accepts an optional fallback data source.

```javascript
import {
    LocalResolutionProvider,
    HttpDataSource,
    SuperpositionOptions,
} from "superposition-provider";

const httpSource = new HttpDataSource(
    new SuperpositionOptions({
        endpoint: "http://localhost:8080",
        token: "token",
        orgId: "localorg",
        workspaceId: "dev",
    })
);

const provider = new LocalResolutionProvider({
    dataSource: httpSource,
    refreshStrategy: {
        type: "polling",
        interval: 30000,
        timeout: 10000,
    },
    experimentationOptions: {
        refreshStrategy: {
            type: "polling",
            interval: 5000,
            timeout: 2000,
        },
        evaluationCache: { ttl: 300, size: 1000 },
        defaultToss: 50,
    },
});

await OpenFeature.setProviderAndWait(provider);
const client = OpenFeature.getClient();

// Resolve all config at once (calls provider directly)
const context = { targetingKey: "user-1234", city: "Berlin", os: "android" };
const allConfig = await provider.resolveAllConfigDetails({}, context);
console.log("All config:", allConfig);
```

**Key capabilities:**

- **Pluggable data sources** — use `HttpDataSource` for server-backed resolution
- **Optional fallback** — provide a secondary data source that is used when the primary source fails
- **Full config resolution** — resolve all features at once via `resolveAllConfigDetails()`

### 2. `SuperpositionAPIProvider` (Remote / Stateless)

A stateless provider that calls the Superposition server on every evaluation. No local caching — each flag evaluation makes an HTTP request. Best for serverless, low-traffic, or scenarios where you always want the latest config.

```javascript
import { OpenFeature } from "@openfeature/server-sdk";
import { SuperpositionAPIProvider, SuperpositionOptions } from "superposition-provider";

const provider = new SuperpositionAPIProvider(
    new SuperpositionOptions({
        endpoint: "http://localhost:8080",
        token: "token",
        orgId: "localorg",
        workspaceId: "dev",
    })
);

await OpenFeature.setProviderAndWait(provider);
const client = OpenFeature.getClient();

const context = { targetingKey: "user-42", city: "Berlin" };
const value = await client.getStringValue("currency", "USD", context);
console.log("currency:", value);
```

## Evaluation Context

```javascript
const context = {
    targetingKey: "user-42",  // Used for experiment bucketing
    city: "Berlin",
    os: "ios",
    customers: "platinum",
};

const value = await client.getStringValue("currency", "USD", context);
```

- **`targetingKey`** — Used for experiment variant bucketing. Typically a user ID.
- All other keys map to your Superposition dimensions.

## Supported Value Types

| Method            | Return Type | Description             |
| ----------------- | ----------- | ----------------------- |
| `getBooleanValue` | `boolean`   | Boolean flag evaluation |
| `getStringValue`  | `string`    | String flag evaluation  |
| `getNumberValue`  | `number`    | Number flag evaluation  |
| `getObjectValue`  | `object`    | Object/JSON evaluation  |

The `LocalResolutionProvider` also supports resolving all features:

```javascript
// Resolve all config at once
const allConfig = await provider.resolveAllConfigDetails({}, context);
```

:::tip
Use `provider.resolveAllConfigDetails({}, context)` to get the entire resolved configuration in one call. This calls the provider directly, bypassing the OpenFeature client.
:::

## ES Modules Setup

Make sure your project uses ES modules. Add this to your `package.json`:

```json
{
  "type": "module"
}
```

## Dependencies

The JavaScript provider depends on:

- [`@openfeature/server-sdk`](https://www.npmjs.com/package/@openfeature/server-sdk) — OpenFeature Server SDK
- `superposition-sdk` — Smithy-generated client for Superposition API
- Native bindings for local config resolution (bundled)

## Full Example

See the integration test: [`clients/javascript/provider-sdk-tests/index.js`](https://github.com/juspay/superposition/blob/main/clients/javascript/provider-sdk-tests/index.js)
