# Superposition Provider

Superposition provider is an openfeature provider that works with [Superposition](https://juspay.io/open-source/superposition) to fetch feature flags, configurations, and experiment variants from a Superposition server, store it in-memory and do configuration resolutions based on dynamic contexts. Read the [docs](https://juspay.io/open-source/superposition/docs) for more details.

### Installation

Install the provider

```bash
pip install openfeature-sdk
pip install superposition-provider
```

> **Note:** You will need to boot up Superposition before running the client code. Check the docs on how to get started with Superposition.

## Initialization

To initialize the Superposition provider, you need to create a configuration. Create the provider object and then, you can set the provider using OpenFeature's API.

```python
from openfeature import api
from superposition_provider.provider import SuperpositionProvider
from superposition_provider.types import ExperimentationOptions, SuperpositionProviderOptions, PollingStrategy

config_options = SuperpositionProviderOptions(
  endpoint="http://localhost:8080",
  token="api-token",
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
# Initialize provider
await provider.initialize(context=ctx)
api.set_provider(provider)
```

## Usage

Once the provider is initialized, you can evaluate feature flags and configurations using the OpenFeature client.

```python
client = api.get_client()

ctx = EvaluationContext(
  targeting_key="25",  # Using a targeting key for experiment variant decider
  attributes={'d1': 'd1'}
)

bool_val = client.get_boolean_details(
    flag_key="bool",
    default_value=True,
    evaluation_context=ctx
)
# Note: If you want the whole config, you can directly use the provider itself
resp = provider.resolve_all_config_details({}, ctx)
print(f"Response for all config: {resp}")

print("Successfully resolved boolean flag details:", bool_val)
```

