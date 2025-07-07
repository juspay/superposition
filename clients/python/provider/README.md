# Superposition OpenFeature Python Client

A Python implementation of the [OpenFeature](https://openfeature.dev/) provider for [Superposition](https://github.com/juspay/superposition), enabling feature flag management and context-aware configuration.

## Features

- 🚩 **Feature Flag Management**: Full OpenFeature compatibility for boolean, string, integer, float, and object flags
- 🎯 **Context-Aware Configuration**: Dynamic configuration based on user context and dimensions
- ⚡ **Multiple Refresh Strategies**: Polling and on-demand configuration fetching
- 🗄️ **Evaluation Caching**: Configurable caching with TTL support
- 🔄 **Real-time Updates**: Automatic configuration refresh with polling strategy
- 🛡️ **Error Handling**: Graceful fallbacks and stale data usage on errors
- 🧪 **Experimentation**: Built-in support for A/B testing and feature experiments

## Installation

```bash
pip install "superposition_provider @ git+https://github.com/juspay/superposition.git@main#subdirectory=clients/python/provider"
```

## Quick Start

```python
import asyncio
from openfeature import api
from openfeature.evaluation_context import EvaluationContext
from superposition_openfeature import SuperpositionProvider, SuperpositionProviderOptions, PollingStrategy

async def main():
    # Configure the provider
    provider_options = SuperpositionProviderOptions(
        endpoint="https://your-superposition-instance.com",
        token="your-api-token",
        org_id="your-org-id",
        workspace_id="your-workspace-id",
        refresh_strategy=PollingStrategy(interval=60)  # Poll every 60 seconds
    )
    
    # Set up the provider
    provider = SuperpositionProvider(provider_options)
    await provider.initialize()
    
    # Register with OpenFeature
    api.set_provider(provider)
    
    # Get a client
    client = api.get_client("my-app")
    
    # Evaluate feature flags
    context = EvaluationContext(attributes={
        "user_id": "123",
        "country": "\"US\"",
        "subscription_tier": "\"premium\""
    })
    
    # Boolean flag
    is_feature_enabled = await client.get_boolean_value("new_feature", False, context)
    
    # String flag
    theme = await client.get_string_value("ui_theme", "default", context)
    
    # Integer flag
    max_items = await client.get_integer_value("max_items_per_page", 10, context)
    
    # Object flag
    config = await client.get_object_value("feature_config", {}, context)
    
    print(f"Feature enabled: {is_feature_enabled}")
    print(f"Theme: {theme}")
    print(f"Max items: {max_items}")
    print(f"Config: {config}")

if __name__ == "__main__":
    asyncio.run(main())
```

## Configuration

### Provider Options

The `SuperpositionProviderOptions` class configures the provider behavior:

```python
from superposition_openfeature import (
    SuperpositionProviderOptions,
    PollingStrategy,
    OnDemandStrategy,
    EvaluationCacheOptions,
    ExperimentationOptions
)

provider_options = SuperpositionProviderOptions(
    # Required: Superposition service configuration
    endpoint="https://your-superposition-instance.com",
    token="your-api-token",
    org_id="your-org-id", 
    workspace_id="your-workspace-id",
    
    # Required: How often to refresh configuration
    refresh_strategy=PollingStrategy(interval=60, timeout=30),
    
    # Optional: Caching configuration
    evaluation_cache_options=EvaluationCacheOptions(
        ttl=300,  # Cache for 5 minutes
        size=1000  # Max 1000 cache entries
    ),
    
    # Optional: Fallback configuration when service is unavailable
    fallback_config={
        "default_theme": "light",
        "max_retries": 3
    },
    
    # Optional: Experimentation settings
    experimentation_options=ExperimentationOptions(
        refresh_strategy=PollingStrategy(interval=30),
        default_toss=-1  # Default experiment assignment
    )
)
```

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

### Evaluation Cache Options

Configure client-side caching for improved performance:

```python
cache_options = EvaluationCacheOptions(
    ttl=300,     # Time-to-live in seconds
    size=1000    # Maximum number of cached evaluations
)
```

## Context and Dimensions

Superposition supports context-aware configuration through dimensions. Pass relevant context when evaluating flags:

```python
# User context
user_context = EvaluationContext(attributes={
    "user_id": "\"user123\"",
    "email": "\"user@example.com\"",
})

# Geographic context
geo_context = EvaluationContext(attributes={
    "country": "\"US\"",
    "region": "\"west\"",
    "city": "\"san_francisco\""
})



# Evaluate with context
result = await client.get_boolean_value("new_checkout_flow", False, user_context)
```