# Superposition Provider

Superposition provider is an openfeature provider that works with [Superposition](https://juspay.io/open-source/superposition) to fetch feature flags, configurations, and experiment variants from a Superposition server, store it in-memory and do configuration resolutions based on dynamic contexts. Read the [docs](https://juspay.io/open-source/superposition/docs) for more details.

## Getting started

Install the provider
```
npm install superposition-provider
```

> **Note:** You will need to boot up Superposition before running the client code. Check the docs on how to get started with Superposition.

## Initialization

To initialize the Superposition provider, you need to create a configuration. Create the provider object and then, you can set the provider using OpenFeature's API.

```javascript
import { OpenFeature } from '@openfeature/server-sdk';
import { SuperpositionProvider } from 'superposition-provider';

// create a simple configuration object, for all options check the provider documentation
const config = {
  endpoint: "http://localhost:8080",
  token: "your-token-here",
  org_id: "localorg",
  workspace_id: "test",
};
const provider = new SuperpositionProvider(config);
console.log("Provider created successfully");

// Initialize the provider
await OpenFeature.setProviderAndWait(provider);
console.log("Provider initialized successfully");
```

## Usage

Once the provider is initialized, you can evaluate feature flags and configurations using the OpenFeature client.

```javascript
const client = OpenFeature.getClient();
const context = {
  d1: "d1",
};

console.log("Testing feature flags...");

const boolValue = await client.getBooleanValue("bool", false, context);
console.log("Boolean flag 'bool':", boolValue);

const stringValue = await client.getStringValue(
  "string",
  "default",
  context,
);
console.log("String flag 'string':", stringValue);

const numberValue = await client.getNumberValue("number_key", 0, context);
console.log("Number flag 'number_key':", numberValue);

// Test resolving all config
const allConfig = await provider.resolveAllConfigDetails({}, context);
console.log("All config:", allConfig);
```

