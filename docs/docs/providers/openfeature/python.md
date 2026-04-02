---
sidebar_position: 3
title: Python
---

# Python — Superposition OpenFeature Provider

The Python provider is an OpenFeature-compatible provider for Superposition. It supports remote config fetching, experimentation, polling/on-demand refresh, fallback configs, and evaluation caching.

- **PyPI:** [`superposition-provider`](https://pypi.org/project/superposition-provider/)

## Installation

```bash
pip install openfeature-sdk
pip install superposition-provider
```

:::note
You need a running Superposition server. See [Quick Start](../../quick_start) for setup instructions.
:::

## Quick Start

```python
import asyncio
from openfeature import api
from openfeature.evaluation_context import EvaluationContext
from superposition_provider.provider import SuperpositionProvider
from superposition_provider.types import (
    ExperimentationOptions,
    SuperpositionProviderOptions,
    PollingStrategy,
)


async def main():
    # 1. Configure provider options
    config_options = SuperpositionProviderOptions(
        endpoint="http://localhost:8080",
        token="your-api-token",
        org_id="localorg",
        workspace_id="test",
        refresh_strategy=PollingStrategy(
            interval=5,   # Poll every 5 seconds
            timeout=3,    # HTTP timeout in seconds
        ),
        fallback_config=None,
        evaluation_cache_options=None,
        experimentation_options=ExperimentationOptions(
            refresh_strategy=PollingStrategy(
                interval=5,
                timeout=3,
            )
        ),
    )

    # 2. Create the provider
    provider = SuperpositionProvider(provider_options=config_options)

    # 3. Set up evaluation context (dimensions + targeting key)
    ctx = EvaluationContext(
        targeting_key="25",   # Used for experiment variant bucketing
        attributes={"city": "Berlin", "os": "android"},
    )

    # 4. Initialize the provider (async — starts polling, fetches initial config)
    await provider.initialize(context=ctx)

    # 5. Register with OpenFeature
    api.set_provider(provider)
    client = api.get_client()

    # 6. Evaluate individual feature flags
    bool_val = client.get_boolean_details(
        flag_key="dark_mode",
        default_value=False,
        evaluation_context=ctx,
    )
    print(f"dark_mode = {bool_val.value}")

    string_val = client.get_string_details(
        flag_key="currency",
        default_value="USD",
        evaluation_context=ctx,
    )
    print(f"currency = {string_val.value}")

    int_val = client.get_integer_details(
        flag_key="price",
        default_value=0,
        evaluation_context=ctx,
    )
    print(f"price = {int_val.value}")

    float_val = client.get_float_details(
        flag_key="discount_rate",
        default_value=0.0,
        evaluation_context=ctx,
    )
    print(f"discount_rate = {float_val.value}")

    # 7. Resolve all config values at once (bypasses OpenFeature, calls provider directly)
    all_config = provider.resolve_all_config_details({}, ctx)
    print(f"All config: {all_config}")

    # 8. Cleanup — stops polling, releases resources
    await provider.shutdown()


if __name__ == "__main__":
    asyncio.run(main())
```

## Configuration Options

### `SuperpositionProviderOptions`

| Field                      | Type                                  | Required | Description                                |
| -------------------------- | ------------------------------------- | -------- | ------------------------------------------ |
| `endpoint`                 | `str`                                 | Yes      | Superposition server URL                   |
| `token`                    | `str`                                 | Yes      | Authentication token (bearer)              |
| `org_id`                   | `str`                                 | Yes      | Organisation ID                            |
| `workspace_id`             | `str`                                 | Yes      | Workspace ID                               |
| `refresh_strategy`         | `PollingStrategy \| OnDemandStrategy` | Yes      | How configs are refreshed                  |
| `fallback_config`          | `Optional[Dict[str, Any]]`           | No       | Fallback config when server is unreachable |
| `evaluation_cache_options` | `Optional[EvaluationCacheOptions]`    | No       | Evaluation result cache settings           |
| `experimentation_options`  | `Optional[ExperimentationOptions]`    | No       | A/B testing settings                       |

### Refresh Strategies

**Polling** — periodically fetches config updates in the background:

```python
from superposition_provider.types import PollingStrategy

PollingStrategy(
    interval=5,   # seconds between polls
    timeout=3,    # HTTP request timeout in seconds
)
```

**On-Demand** — fetches on first access, then caches with a TTL:

```python
from superposition_provider.types import OnDemandStrategy

OnDemandStrategy(
    ttl=60,                    # cache TTL in seconds
    use_stale_on_error=False,  # serve stale data on fetch error
    timeout=3,                 # HTTP request timeout in seconds
)
```

### `ExperimentationOptions`

| Field                      | Type                                  | Required | Description                            |
| -------------------------- | ------------------------------------- | -------- | -------------------------------------- |
| `refresh_strategy`         | `PollingStrategy \| OnDemandStrategy` | Yes      | How experiment data is refreshed       |
| `evaluation_cache_options` | `Optional[EvaluationCacheOptions]`    | No       | Cache for experiment evaluations       |
| `default_toss`             | `int`                                 | No       | Default toss value (default: `-1`)     |

### `EvaluationCacheOptions`

| Field  | Type            | Default | Description                     |
| ------ | --------------- | ------- | ------------------------------- |
| `ttl`  | `Optional[int]` | `None`  | Cache time-to-live in seconds   |
| `size` | `Optional[int]` | `None`  | Maximum number of cache entries |

## Evaluation Context

The evaluation context passes dimensions for config resolution and the `targeting_key` for experiment bucketing.

```python
from openfeature.evaluation_context import EvaluationContext

ctx = EvaluationContext(
    targeting_key="user-42",          # Used for experiment variant selection
    attributes={
        "city": "Berlin",
        "os": "ios",
        "customers": "platinum",
    },
)
```

- **`targeting_key`** — Maps to the toss value for experiment bucketing. Typically a user ID or session ID.
- **`attributes`** — Key-value pairs matching your Superposition dimensions.

## Provider Lifecycle

```python
# Create
provider = SuperpositionProvider(provider_options=config_options)

# Initialize (async — starts polling, fetches initial config)
await provider.initialize(context=ctx)

# Register with OpenFeature
api.set_provider(provider)
client = api.get_client()

# ... evaluate flags ...

# Shutdown (stops polling, releases resources)
await provider.shutdown()
```

### Provider Status

| Status       | Meaning                                    |
| ------------ | ------------------------------------------ |
| `NOT_READY`  | Provider created but not yet initialized   |
| `READY`      | Provider initialized and ready for use     |
| `ERROR`      | Initialization failed                      |
| `FATAL`      | Unrecoverable error during shutdown        |

## Supported Value Types

| Method                       | Return Type                    | Description                |
| ---------------------------- | ------------------------------ | -------------------------- |
| `get_boolean_details`        | `FlagResolutionDetails[bool]`  | Boolean flag evaluation    |
| `get_string_details`         | `FlagResolutionDetails[str]`   | String flag evaluation     |
| `get_integer_details`        | `FlagResolutionDetails[int]`   | Integer flag evaluation    |
| `get_float_details`          | `FlagResolutionDetails[float]` | Float flag evaluation      |
| `resolve_object_details`     | `FlagResolutionDetails[Any]`   | Object/JSON evaluation     |
| `resolve_all_config_details` | `Dict[str, Any]`              | All resolved configs       |

:::tip
Use `provider.resolve_all_config_details({}, ctx)` to get the entire resolved configuration in one call. This bypasses the OpenFeature client and calls the provider directly.
:::

## With Experimentation

```python
config_options = SuperpositionProviderOptions(
    endpoint="http://localhost:8080",
    token="your-api-token",
    org_id="localorg",
    workspace_id="test",
    refresh_strategy=PollingStrategy(interval=5, timeout=3),
    experimentation_options=ExperimentationOptions(
        refresh_strategy=PollingStrategy(interval=5, timeout=3),
        evaluation_cache_options=EvaluationCacheOptions(ttl=300, size=1000),
        default_toss=50,
    ),
)
```

When experimentation is enabled, the provider will:

1. Fetch running experiments from the server
2. Use the `targeting_key` from the evaluation context to determine which variant a user belongs to
3. Apply experiment overrides to the resolved configuration

## Error Handling

The provider handles errors gracefully:

- If the server is unreachable during initialization, the provider status is set to `ERROR`
- If a `fallback_config` is provided, it will be used when the server is unreachable
- Flag evaluation returns the `default_value` when a flag is not found

```python
try:
    await provider.initialize(context=ctx)
except Exception as e:
    print(f"Failed to initialize: {e}")
    # Provider status will be ERROR
    # If fallback_config was provided, it will be used
```

## Dependencies

The Python provider depends on:

- [`openfeature-sdk`](https://pypi.org/project/openfeature-sdk/) — OpenFeature Python SDK
- `superposition_py_sdk` — Smithy-generated client for Superposition API
- `superposition_py_cac_client` — Native FFI bindings (via Rust) for local config resolution

## Full Example

See the integration test: [`clients/python/provider-sdk-tests/main.py`](https://github.com/juspay/superposition/blob/main/clients/python/provider-sdk-tests/main.py)
