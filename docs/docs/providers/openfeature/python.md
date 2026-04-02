---
sidebar_position: 3
title: Python
---

# Python — Superposition OpenFeature Provider

The Python provider is an OpenFeature-compatible provider for Superposition. It offers two provider variants:

- **`LocalResolutionProvider`** — Fetches config from a data source (HTTP server), caches it locally, and evaluates flags in-process. Supports polling and on-demand refresh strategies. This is the **recommended provider** for most use cases.
- **`SuperpositionAPIProvider`** — A stateless remote provider that makes an HTTP API call to the Superposition server on every evaluation. No local caching — useful for serverless or low-traffic scenarios.

**PyPI:** [`superposition-provider`](https://pypi.org/project/superposition-provider/)

## Installation

```bash
pip install openfeature-sdk superposition-provider
```

:::note
You need a running Superposition server. See [Quick Start](../../quick_start) for setup instructions.
:::

## Quick Start

This is the most common usage — the provider connects to a Superposition server via HTTP, polls for config updates, and evaluates flags locally.

```python
import asyncio
from openfeature import api
from openfeature.evaluation_context import EvaluationContext
from superposition_provider import (
    LocalResolutionProvider,
    HttpDataSource,
    SuperpositionOptions,
    PollingStrategy,
    RefreshStrategy,
)


async def main():
    # 1. Create an HTTP data source pointing to your Superposition server
    http_source = HttpDataSource(SuperpositionOptions(
        endpoint="http://localhost:8080",
        token="your-api-token",
        org_id="localorg",
        workspace_id="test",
    ))

    # 2. Create the provider with a polling refresh strategy
    provider = LocalResolutionProvider(
        data_source=http_source,
        fallback_source=None,  # no fallback data source
        refresh_strategy=RefreshStrategy.Polling(PollingStrategy(
            interval=60,   # seconds between polls
            timeout=30,    # HTTP request timeout in seconds
        )),
    )

    # 3. Set up evaluation context (dimensions + targeting key)
    ctx = EvaluationContext(
        targeting_key="user-42",
        attributes={"city": "Berlin", "os": "android"},
    )

    # 4. Initialize the provider (async — starts polling, fetches initial config)
    await provider.initialize(context=ctx)

    # 5. Register with OpenFeature
    api.set_provider(provider)
    client = api.get_client()

    # 6. Evaluate feature flags
    string_val = client.get_string_details("currency", "USD", ctx)
    print(f"currency = {string_val.value}")

    int_val = client.get_integer_details("price", 0, ctx)
    print(f"price = {int_val.value}")

    bool_val = client.get_boolean_details("dark_mode", False, ctx)
    print(f"dark_mode = {bool_val.value}")

    float_val = client.get_float_details("discount_rate", 0.0, ctx)
    print(f"discount_rate = {float_val.value}")

    # 7. Cleanup — stops polling, releases resources
    await provider.shutdown()


if __name__ == "__main__":
    asyncio.run(main())
```

## Configuration Options

### `SuperpositionOptions`

Connection options shared by `HttpDataSource` and `SuperpositionAPIProvider`:

| Field          | Type  | Required | Description                   |
| -------------- | ----- | -------- | ----------------------------- |
| `endpoint`     | `str` | Yes      | Superposition server URL      |
| `token`        | `str` | Yes      | Authentication token (bearer) |
| `org_id`       | `str` | Yes      | Organisation ID               |
| `workspace_id` | `str` | Yes      | Workspace ID                  |

```python
options = SuperpositionOptions(
    endpoint="http://localhost:8080",
    token="your-api-token",
    org_id="localorg",
    workspace_id="test",
)
```

### Refresh Strategies

```python
# Polling — periodically fetches updates from the server
RefreshStrategy.Polling(PollingStrategy(
    interval=60,   # seconds between polls (default: 60)
    timeout=30,    # HTTP request timeout in seconds (default: 30)
))

# On-Demand — fetches on first access, then caches with a TTL
RefreshStrategy.OnDemand(OnDemandStrategy(
    ttl=300,                 # cache TTL in seconds (default: 300)
    use_stale_on_error=True, # serve stale data on fetch error (default: True)
    timeout=30,              # HTTP timeout in seconds (default: 30)
))
```

### `ExperimentationOptions`

| Field              | Type                               | Required | Description                        |
| ------------------ | ---------------------------------- | -------- | ---------------------------------- |
| `refresh_strategy` | `RefreshStrategy`                  | Yes      | How experiment data is refreshed   |
| `evaluation_cache` | `Optional[EvaluationCacheOptions]` | No       | Cache for experiment evaluations   |
| `default_toss`     | `Optional[int]`                    | No       | Default toss value for experiments |

```python
exp_options = ExperimentationOptions(
    refresh_strategy=RefreshStrategy.Polling(PollingStrategy(
        interval=5,
        timeout=3,
    )),
    evaluation_cache=EvaluationCacheOptions(ttl=300, size=1000),
    default_toss=50,
)
```

### `EvaluationCacheOptions`

| Field  | Type            | Default | Description                     |
| ------ | --------------- | ------- | ------------------------------- |
| `ttl`  | `Optional[int]` | `60`    | Cache time-to-live in seconds   |
| `size` | `Optional[int]` | `500`   | Maximum number of cache entries |

## Provider Variants

### 1. `LocalResolutionProvider` (Recommended)

Fetches config from a pluggable data source (HTTP), caches locally, and evaluates flags in-process. Accepts an optional fallback data source.

```python
from superposition_provider import (
    LocalResolutionProvider,
    HttpDataSource,
    SuperpositionOptions,
    PollingStrategy,
    RefreshStrategy,
    ExperimentationOptions,
    EvaluationCacheOptions,
)
from openfeature.evaluation_context import EvaluationContext

http_source = HttpDataSource(SuperpositionOptions(
    endpoint="http://localhost:8080",
    token="token",
    org_id="localorg",
    workspace_id="dev",
))

provider = LocalResolutionProvider(
    data_source=http_source,
    fallback_source=None,
    refresh_strategy=RefreshStrategy.Polling(PollingStrategy(interval=30, timeout=10)),
    experimentation_options=ExperimentationOptions(
        refresh_strategy=RefreshStrategy.Polling(PollingStrategy(interval=5, timeout=3)),
        evaluation_cache=EvaluationCacheOptions(ttl=300, size=1000),
        default_toss=50,
    ),
)

# Initialize the provider (fetches initial config)
await provider.initialize(context=EvaluationContext())

# Resolve all config values at once
ctx = EvaluationContext(
    targeting_key="user-1234",
    attributes={"city": "Berlin", "os": "android"},
)
all_config = provider.resolve_all_config_details({}, ctx)
print(f"All config: {all_config}")

# Cleanup
await provider.shutdown()
```

**Key capabilities:**

- **Pluggable data sources** — use `HttpDataSource` for server-backed resolution
- **Optional fallback** — provide a secondary data source that is used when the primary source fails
- **Async lifecycle** — `initialize()` and `shutdown()` are async, supporting non-blocking I/O

### 2. `SuperpositionAPIProvider` (Remote / Stateless)

A stateless provider that calls the Superposition server on every evaluation. No local caching — each flag evaluation makes an HTTP request. Best for serverless, low-traffic, or scenarios where you always want the latest config.

```python
from openfeature import api
from openfeature.evaluation_context import EvaluationContext
from superposition_provider import SuperpositionAPIProvider, SuperpositionOptions

provider = SuperpositionAPIProvider(SuperpositionOptions(
    endpoint="http://localhost:8080",
    token="token",
    org_id="localorg",
    workspace_id="dev",
))

# Initialize and register with OpenFeature
await provider.initialize(context=EvaluationContext())
api.set_provider(provider)
client = api.get_client()

ctx = EvaluationContext(
    targeting_key="user-42",
    attributes={"city": "Berlin"},
)

currency = client.get_string_details("currency", "USD", ctx)
print(f"currency = {currency.value}")
```

## Provider Lifecycle

```python
# Create
provider = LocalResolutionProvider(
    data_source=http_source,
    fallback_source=None,
    refresh_strategy=RefreshStrategy.Polling(PollingStrategy(interval=60, timeout=30)),
)

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

## Evaluation Context

Pass dimensions and a targeting key for experiment bucketing:

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

## Supported Value Types

| Method                       | Return Type                    | Description             |
| ---------------------------- | ------------------------------ | ----------------------- |
| `get_boolean_details`        | `FlagResolutionDetails[bool]`  | Boolean flag evaluation |
| `get_string_details`         | `FlagResolutionDetails[str]`   | String flag evaluation  |
| `get_integer_details`        | `FlagResolutionDetails[int]`   | Integer flag evaluation |
| `get_float_details`          | `FlagResolutionDetails[float]` | Float flag evaluation   |
| `resolve_object_details`     | `FlagResolutionDetails[Any]`   | Object/JSON evaluation  |

The `LocalResolutionProvider` also supports resolving all features:

```python
# Resolve all config values at once
all_config = provider.resolve_all_config_details({}, ctx)
```

:::tip
Use `provider.resolve_all_config_details({}, ctx)` to get the entire resolved configuration in one call. This bypasses the OpenFeature client and calls the provider directly.
:::

## Error Handling

The provider handles errors gracefully:

- If the server is unreachable during initialization, the provider status is set to `ERROR`
- If a fallback data source is provided, it will be used when the primary source fails
- Flag evaluation returns the `default_value` when a flag is not found

```python
try:
    await provider.initialize(context=ctx)
except Exception as e:
    print(f"Failed to initialize: {e}")
    # Provider status will be ERROR
    # If fallback_source was provided, it will be used
```

## Dependencies

The Python provider depends on:

- [`openfeature-sdk`](https://pypi.org/project/openfeature-sdk/) — OpenFeature Python SDK
- `superposition_py_sdk` — Smithy-generated client for Superposition API
- `superposition_py_cac_client` — Native FFI bindings (via Rust) for local config resolution

## Full Example

See the integration test: [`clients/python/provider-sdk-tests/main.py`](https://github.com/juspay/superposition/blob/main/clients/python/provider-sdk-tests/main.py)
