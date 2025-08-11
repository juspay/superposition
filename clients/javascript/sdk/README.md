# Superposition SDK

Superposition SDK is a JavaScript client for the Superposition platform, designed to facilitate programmatic integration of all Superposition's API capabilities in JavaScript applications. Read the complete documentation at [Superposition SDK Documentation](https://juspay.io/superposition/docs).

## Installation

Install the Superposition SDK using npm:

```bash
npm install superposition_sdk
```

## Initialization

```javascript
import { SuperpositionClient } from "superposition-sdk";

let config = {
    endpoint: "http://localhost:8080",
    token: {
        token: "your-token-here",
    },
};
let client = new SuperpositionClient(config);
```

## Usage

The SDK provides commands for every API call that superposition supports. Below is an example of how to use the SDK to list default configs.

```javascript

import { SuperpositionClient, ListDefaultConfigsCommand } from "superposition-sdk";

let config = {
    endpoint: "http://localhost:8080",
    token: {
        token: "your-token-here",
    },
};
let client = new SuperpositionClient(config);

let list_default_configs = async (client) => {
  const cmd = new ListDefaultConfigsCommand({
    workspace_id: "workspace_id",
    org_id: "organisation_id",
  });
  let response = await client.send(cmd);
  console.log(JSON.stringify(response.data));
};

await list_default_configs(client);
```
