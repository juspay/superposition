# Superposition OpenFeature Python Client

A Python implementation of the [OpenFeature](https://openfeature.dev/) provider for [Superposition](https://github.com/juspay/superposition), enabling feature flag management and context-aware configuration.

## Features

- 🚩 **Feature Flag Management**: Full OpenFeature compatibility for boolean, string, integer, float, and object flags
- 🎯 **Context-Aware Configuration**: Dynamic configuration based on user context and dimensions
- ⚡ **Multiple Refresh Strategies**: Polling and on-demand configuration fetching
- 🔄 **Real-time Updates**: Automatic configuration refresh with polling strategy
- 🛡️ **Error Handling**: Graceful fallbacks and stale data usage on errors
- 🧪 **Experimentation**: Built-in support for A/B testing and feature experiments

## Installation

```bash
pip install "superposition_provider @ git+https://github.com/juspay/superposition.git@main#subdirectory=clients/python/provider"
```

## Quick Start

Note: You will have to boot up superposition, then you run and test with the below code

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
        #Note: If you want the whole config , you can directly use the provider itself
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

## Configuration


### Refresh Strategies

#### Polling Strategy
Automatically fetches configuration at regular intervals:

```python
refresh_strategy = PollingStrategy(
    interval=60,    # Poll every 60 seconds
    timeout=30      # Request timeout in seconds
)
```

#### On-Demand Strategy
Fetches configuration only when needed, with TTL-based caching:

```python
refresh_strategy = OnDemandStrategy(
    ttl=300,                    # Cache for 5 minutes
    use_stale_on_error=True,    # Use cached data if fetch fails
    timeout=30                  # Request timeout in seconds
)
```


## Context and Dimensions

Superposition supports context-aware configuration through dimensions. Pass relevant context when evaluating flags:

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