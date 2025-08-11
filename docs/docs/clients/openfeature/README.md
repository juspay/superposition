# Superposition OpenFeature Clients

OpenFeature provider implementations for [Superposition](https://github.com/juspay/superposition), enabling feature flag management and context-aware configuration in multiple programming languages.

## Features

- 🚩 **Feature Flag Management**: Full OpenFeature compatibility for boolean, string, integer, float, and object flags
- 🎯 **Context-Aware Configuration**: Dynamic configuration based on user context and dimensions
- ⚡ **Multiple Refresh Strategies**: Polling and on-demand configuration fetching
- 🔄 **Real-time Updates**: Automatic configuration refresh with polling strategy
- 🛡️ **Error Handling**: Graceful fallbacks and stale data usage on errors
- 🧪 **Experimentation**: Built-in support for A/B testing and feature experiments

## Supported Languages

- [Python](#python-client)
- [JavaScript/Node.js](#javascript-client)

---

## Python Client

### Installation

```bash
pip install "superposition_provider @ git+https://github.com/juspay/superposition.git@main#subdirectory=clients/python/provider"
```

### Quick Start

> **Note:** You will need to boot up Superposition before running the client code.

```python
import asyncio

from superposition_provider.types import ExperimentationOptions, SuperpositionProviderOptions, PollingStrategy
from openfeature import api
from superposition_provider.provider import SuperpositionProvider
from openfeature.evaluation_context import EvaluationContext


async def test_config():
    """Integration test function - enhanced version of original test"""
    print("Testing SuperpositionProvider integration...")

    config_options = SuperpositionProviderOptions(
        endpoint="http://localhost:8080",
        token="token",
        org_id="localorg", 
        workspace_id="test",
        refresh_strategy=PollingStrategy(
            interval=5,  # Poll every 5 seconds
            timeout=3    # Timeout after 3 seconds
        ),
        fallback_config=None,
        evaluation_cache_options=None,
        experimentation_options=ExperimentationOptions(
            refresh_strategy=PollingStrategy(
                interval=5,  # Poll every 5 seconds
                timeout=3    # Timeout after 3 seconds
            )
        )
    )
    
    provider = SuperpositionProvider(provider_options=config_options)
    ctx = EvaluationContext(
        targeting_key="25",  # Using a targeting key for experiment variant decider
        attributes={'d1': 'd1'}
    )
    
    try:
        # Initialize provider
        await provider.initialize(context=ctx)
        api.set_provider(provider)
        client = api.get_client()

        bool_val = client.get_boolean_details(
            flag_key="bool",
            default_value=True,
            evaluation_context=ctx
        )
        # Note: If you want the whole config, you can directly use the provider itself
        resp = provider.resolve_all_config_details({}, ctx)
        print(f"Response for all config: {resp}")

        print("Successfully resolved boolean flag details:", bool_val)
        
    except Exception as e:
        print(f"Test failed with error: {e}")
    finally:
        # Cleanup
        if hasattr(provider, 'shutdown'):
            await provider.shutdown()


if __name__ == "__main__":
    asyncio.run(test_config())
```

### Running the Python Example

```bash
python demo.py
```

---

## JavaScript Client

### Installation

1. Initialize your Node.js project (if not already done):
   ```bash
   npm init -y
   ```

2. Install dependencies:
   ```bash
   npm install @openfeature/server-sdk@^1.13.0
   npm install github:juspay/superposition#feat/typescript-exp-client
   ```

3. Update your `package.json` to use ES modules:
   ```json
   {
     "type": "module",
     "dependencies": {
       "@openfeature/server-sdk": "^1.13.0",
       "@superposition/typescript": "github:juspay/superposition#feat/typescript-exp-client"
     }
   }
   ```

### Quick Start

> **Note:** You will need to boot up Superposition before running the client code.

Create a file named `demo.js`:

```javascript
import { OpenFeature } from '@openfeature/server-sdk';
import { createRequire } from 'module';
const require = createRequire(import.meta.url);
const { SuperpositionProvider } = require('@superposition/typescript/clients/javascript/open-feature-provider/dist/index.js');

async function runDemo() {
    const config = {
        endpoint: "http://localhost:8080",
        token: "your-token-here",
        org_id: "localorg",
        workspace_id: "dev"
    };

    try {
        const provider = new SuperpositionProvider(config);
        console.log("Provider created successfully");

        // Initialize the provider
        await OpenFeature.setProviderAndWait(provider);
        console.log("Provider initialized successfully");

        const client = OpenFeature.getClient();
        const context = {
            d1: 'd1'
        };

        console.log("Testing feature flags...");

        const boolValue = await client.getBooleanValue('bool', false, context);
        console.log("Boolean flag 'bool':", boolValue);

        const stringValue = await client.getStringValue('string', 'default', context);
        console.log("String flag 'string':", stringValue);

        const numberValue = await client.getNumberValue('number_key', 0, context);
        console.log("Number flag 'number_key':", numberValue);

        // Test resolving all config
        const allConfig = await provider.resolveAllConfigDetails({}, context);
        console.log("All config:", allConfig);

    } catch (error) {
        console.error("Error running demo:", error);
    } finally {
        try {
            await OpenFeature.close();
            console.log("OpenFeature closed successfully");
        } catch (closeError) {
            console.error("Error closing OpenFeature:", closeError);
        }
    }
}

console.log("Starting Superposition OpenFeature demo (JavaScript)...");
runDemo();
```

### Running the JavaScript Example

```bash
node demo.js
```

---

## Configuration

### Refresh Strategies

#### Polling Strategy
Automatically fetches configuration at regular intervals:

**Python:**
```python
refresh_strategy = PollingStrategy(
    interval=60,    # Poll every 60 seconds
    timeout=30      # Request timeout in seconds
)
```

**JavaScript:**
```javascript
const config = {
    endpoint: "http://localhost:8080",
    token: "your-token-here",
    org_id: "your-org",
    workspace_id: "your-workspace",
    polling_interval: 60000,  // Poll every 60 seconds (in milliseconds)
    timeout: 30000           // Request timeout in milliseconds
};
```

#### On-Demand Strategy
Fetches configuration only when needed, with TTL-based caching:

**Python:**
```python
refresh_strategy = OnDemandStrategy(
    ttl=300,                    # Cache for 5 minutes
    use_stale_on_error=True,    # Use cached data if fetch fails
    timeout=30                  # Request timeout in seconds
)
```

## Context and Dimensions

Superposition supports context-aware configuration through dimensions. Pass relevant context when evaluating flags:

**Python:**
```python
# User context
user_context = EvaluationContext(attributes={
    "user_id": "user123",
    "email": "user@example.com",
})

# Geographic context
geo_context = EvaluationContext(attributes={
    "country": "US",
    "region": "west",
    "city": "san_francisco"
})

# Evaluate with context
result = await client.get_boolean_details("new_checkout_flow", False, user_context)
```

**JavaScript:**
```javascript
// User context
const userContext = {
    user_id: "user123",
    email: "user@example.com"
};

// Geographic context
const geoContext = {
    country: "US",
    region: "west",
    city: "san_francisco"
};

// Evaluate with context
const result = await client.getBooleanValue("new_checkout_flow", false, userContext);
```

## Prerequisites

Before running any examples, ensure that:

1. **Superposition server is running** on `http://localhost:8080`
2. **Valid credentials** are configured (token, org_id, workspace_id)
3. **Feature flags are set up** in your Superposition workspace

## Troubleshooting

- **Connection errors**: Verify that the Superposition server is running and accessible
- **Authentication errors**: Check that your token and organization/workspace IDs are correct
- **Module import errors (JavaScript)**: Ensure you're using ES modules (`"type": "module"` in package.json)
- **Missing flags**: Verify that the feature flags you're testing exist in your workspace