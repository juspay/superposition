---
sidebar_position: 4
title: JavaScript
---

# JavaScript — Superposition OpenFeature Provider

The JavaScript provider is an OpenFeature-compatible provider for Superposition that runs in Node.js. It uses the Superposition SDK for server communication and native bindings for local config resolution.

- **npm:** [`superposition-provider`](https://www.npmjs.com/package/superposition-provider)

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
    // 1. Configure provider options
    const config = {
        endpoint: "http://localhost:8080",
        token: "your-api-token",
        org_id: "localorg",
        workspace_id: "test",
    };

    // 2. Create and initialize the provider
    const provider = new SuperpositionProvider(config);
    await OpenFeature.setProviderAndWait(provider);
    console.log("Provider initialized successfully");

    // 3. Create a client
    const client = OpenFeature.getClient();

    // 4. Define evaluation context (dimensions)
    const context = {
        city: "Berlin",
        os: "android",
        targetingKey: "user-123",
    };

    // 5. Evaluate feature flags
    const boolVal = await client.getBooleanValue("dark_mode", false, context);
    console.log("dark_mode:", boolVal);

    const stringVal = await client.getStringValue("currency", "USD", context);
    console.log("currency:", stringVal);

    const numberVal = await client.getNumberValue("price", 0, context);
    console.log("price:", numberVal);

    // 6. Resolve all config at once (calls provider directly)
    const allConfig = await provider.resolveAllConfigDetails({}, context);
    console.log("All config:", allConfig);

    // 7. Cleanup
    await OpenFeature.close();
}

main().catch(console.error);
```

## Configuration Options

### `SuperpositionProviderOptions`

| Field                    | Type                       | Required | Description                              |
| ------------------------ | -------------------------- | -------- | ---------------------------------------- |
| `endpoint`               | `string`                   | Yes      | Superposition server URL                 |
| `token`                  | `string`                   | Yes      | Authentication token (bearer)            |
| `org_id`                 | `string`                   | Yes      | Organisation ID                          |
| `workspace_id`           | `string`                   | Yes      | Workspace ID                             |
| `fallbackConfig`         | `any`                      | No       | Fallback config when server is unreachable |
| `evaluationCache`        | `EvaluationCacheOptions`   | No       | Evaluation result cache settings         |
| `refreshStrategy`        | `RefreshStrategy`          | No       | How configs are refreshed                |
| `experimentationOptions` | `ExperimentationOptions`   | No       | Experimentation / A/B testing settings   |
| `httpClient`             | custom HTTP client         | No       | Override the default HTTP transport      |

### Refresh Strategies

```javascript
// Polling — periodic background refresh
const config = {
    // ...connection options...
    refreshStrategy: {
        interval: 60000, // milliseconds between polls
        timeout: 30000,  // HTTP request timeout in milliseconds
    },
};

// On-Demand — fetch on access, cache with TTL
const config = {
    // ...connection options...
    refreshStrategy: {
        ttl: 300,                   // cache TTL in seconds
        useStaleOnError: true,      // serve stale data on fetch error
        timeout: 30000,             // HTTP timeout in milliseconds
    },
};
```

### `ExperimentationOptions`

```javascript
const config = {
    // ...connection options...
    experimentationOptions: {
        refreshStrategy: {
            interval: 10000,  // poll every 10 seconds
            timeout: 5000,
        },
        evaluationCache: {
            ttl: 300,
            size: 1000,
        },
        defaultToss: 50,
    },
};
```

### `EvaluationCacheOptions`

| Field  | Type     | Default | Description                     |
| ------ | -------- | ------- | ------------------------------- |
| `ttl`  | `number` | —       | Cache time-to-live in seconds   |
| `size` | `number` | —       | Maximum number of cache entries |

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

## With Experimentation

```javascript
const config = {
    endpoint: "http://localhost:8080",
    token: "your-api-token",
    org_id: "localorg",
    workspace_id: "test",
    experimentationOptions: {
        refreshStrategy: {
            interval: 5000,
            timeout: 2000,
        },
    },
};

const provider = new SuperpositionProvider(config);
await OpenFeature.setProviderAndWait(provider);
```

## Accessing the Configuration Client

The provider exposes the underlying `ConfigurationClient` for advanced use cases:

```javascript
const configClient = provider.getConfigurationClient();
```

## Supported Value Types

| Method            | Return Type | Description             |
| ----------------- | ----------- | ----------------------- |
| `getBooleanValue` | `boolean`   | Boolean flag evaluation |
| `getStringValue`  | `string`    | String flag evaluation  |
| `getNumberValue`  | `number`    | Number flag evaluation  |
| `getObjectValue`  | `object`    | Object/JSON evaluation  |

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
