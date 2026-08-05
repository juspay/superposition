---
sidebar_position: 3
title: Python
---

# Python - Superposition OpenFeature Provider

The Python package currently exposes two OpenFeature-compatible providers:

- **`LocalResolutionProvider`** - Fetches config through a data source, caches it locally, and evaluates flags in-process through native bindings.
- **`SuperpositionAPIProvider`** - Calls the Superposition API directly for remote evaluation and does not keep a local config cache.

The local provider supports HTTP and file data sources, optional fallback data
sources, polling, on-demand, watch, and manual refresh strategies.

**PyPI:** [`superposition-provider`](https://pypi.org/project/superposition-provider/)

## Installation

```bash
pip install openfeature-sdk superposition-provider
```

:::note
You need a running Superposition server. See [Quick Start](../../quick_start) for setup instructions.
:::

## Quick Start

```python
import asyncio

from openfeature import api
from openfeature.evaluation_context import EvaluationContext
from superposition_provider import LocalResolutionProvider, HttpDataSource
from superposition_provider.types import SuperpositionOptions, PollingStrategy


async def main():
    options = SuperpositionOptions(
        endpoint="http://localhost:8080",
        token="your-api-token",
        org_id="localorg",
        workspace_id="test",
    )

    provider = LocalResolutionProvider(
        primary_source=HttpDataSource(options),
        refresh_strategy=PollingStrategy(interval=60, timeout=30),
    )

    context = EvaluationContext(
        targeting_key="user-42",
        attributes={"city": "Berlin", "os": "android"},
    )

    await provider.initialize(context)
    api.set_provider(provider)
    client = api.get_client()

    currency = client.get_string_details("currency", "USD", context)
    print(f"currency = {currency.value}")

    price = client.get_integer_details("price", 0, context)
    print(f"price = {price.value}")

    dark_mode = client.get_boolean_details("dark_mode", False, context)
    print(f"dark_mode = {dark_mode.value}")

    all_config = provider.resolve_all_features(context)
    print(f"All config = {all_config}")

    await provider.shutdown()


if __name__ == "__main__":
    asyncio.run(main())
```

## Connection Options

`SuperpositionOptions` is used by `HttpDataSource` and
`SuperpositionAPIProvider`.

| Field | Type | Required | Description |
| ----- | ---- | -------- | ----------- |
| `endpoint` | `str` | Yes | Superposition server URL |
| `token` | `str` | Yes | Authentication token |
| `org_id` | `str` | Yes | Organisation ID |
| `workspace_id` | `str` | Yes | Workspace ID |

```python
options = SuperpositionOptions(
    endpoint="http://localhost:8080",
    token="your-api-token",
    org_id="localorg",
    workspace_id="test",
)
```

## Refresh Strategies

The current Python API uses dataclass strategy objects directly:

```python
from superposition_provider.types import (
    PollingStrategy,
    OnDemandStrategy,
    WatchStrategy,
    ManualStrategy,
)

# Poll every 60 seconds.
PollingStrategy(interval=60, timeout=30)

# Refresh when cached data is older than the TTL.
OnDemandStrategy(ttl=300, use_stale_on_error=True, timeout=30)

# Watch a file-backed data source.
WatchStrategy(debounce_ms=500)

# No automatic refresh; call refresh() yourself.
ManualStrategy()
```

There is a `RefreshStrategy` type alias, but there are no
`RefreshStrategy.Polling(...)` or `RefreshStrategy.OnDemand(...)` constructors.

## Local Provider

`LocalResolutionProvider` takes a primary data source, an optional fallback data
source, and a refresh strategy.

```python
from openfeature.evaluation_context import EvaluationContext
from superposition_provider import LocalResolutionProvider, HttpDataSource
from superposition_provider.types import SuperpositionOptions, PollingStrategy

provider = LocalResolutionProvider(
    primary_source=HttpDataSource(SuperpositionOptions(
        endpoint="http://localhost:8080",
        token="token",
        org_id="localorg",
        workspace_id="dev",
    )),
    refresh_strategy=PollingStrategy(interval=30, timeout=10),
)

await provider.initialize(EvaluationContext())

context = EvaluationContext(
    targeting_key="user-1234",
    attributes={"dimension": "d2"},
)

all_config = provider.resolve_all_features(context)
variants = await provider.get_applicable_variants(context)

await provider.shutdown()
```

**Key capabilities:**

- **HTTP data source** - `HttpDataSource(SuperpositionOptions(...))`
- **File data source** - `FileDataSource("config.toml")`
- **Optional fallback** - pass `fallback_source=...`
- **Manual refresh** - call `await provider.refresh()`
- **Async lifecycle** - `initialize()` and `shutdown()` are async

## Local File Resolution

Resolve config from a local TOML or JSON file without a server:

```python
from openfeature.evaluation_context import EvaluationContext
from superposition_provider import FileDataSource, LocalResolutionProvider
from superposition_provider.types import OnDemandStrategy

provider = LocalResolutionProvider(
    primary_source=FileDataSource("config.toml"),
    refresh_strategy=OnDemandStrategy(ttl=60),
)

await provider.initialize(EvaluationContext())

context = EvaluationContext(
    attributes={"os": "linux", "city": "Boston"},
)

config = provider.resolve_all_features(context)
timeout = provider.resolve_integer_details("timeout", 0, context)

await provider.shutdown()
```

:::note
`FileDataSource` does not support experiments. Use an HTTP-backed source when
experiment metadata is required.
:::

## HTTP with File Fallback

Use a Superposition server as the primary source and a local file as fallback:

```python
from openfeature.evaluation_context import EvaluationContext
from superposition_provider import FileDataSource, HttpDataSource, LocalResolutionProvider
from superposition_provider.types import SuperpositionOptions, PollingStrategy

options = SuperpositionOptions(
    endpoint="http://localhost:8080",
    token="token",
    org_id="localorg",
    workspace_id="dev",
)

provider = LocalResolutionProvider(
    primary_source=HttpDataSource(options),
    fallback_source=FileDataSource("config.toml"),
    refresh_strategy=PollingStrategy(interval=10, timeout=10),
)

await provider.initialize(EvaluationContext())

context = EvaluationContext(
    targeting_key="user-456",
    attributes={"os": "linux", "city": "Berlin"},
)

config = provider.resolve_all_features(context)
currency = provider.resolve_string_details("currency", "No value", context)

await provider.shutdown()
```

## Remote Provider

`SuperpositionAPIProvider` performs remote API evaluation and keeps no local
config cache. Its current lifecycle methods are synchronous.

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

context = EvaluationContext(
    targeting_key="user-42",
    attributes={"city": "Berlin"},
)

provider.initialize(context)
api.set_provider(provider)
client = api.get_client()

currency = client.get_string_details("currency", "USD", context)
print(f"currency = {currency.value}")

provider.shutdown()
```

Use the async provider-specific methods such as
`resolve_all_features_with_filter_async` when you want direct access to remote
all-config resolution.

## Evaluation Context

Pass dimensions and a targeting key for experiment bucketing:

```python
from openfeature.evaluation_context import EvaluationContext

context = EvaluationContext(
    targeting_key="user-42",
    attributes={
        "city": "Berlin",
        "os": "ios",
        "customers": "platinum",
    },
)
```

- **`targeting_key`** - Used for experiment variant selection.
- **`attributes`** - Key-value pairs matching your Superposition dimensions.

## Supported Value Types

| Method | Return Type |
| ------ | ----------- |
| `resolve_boolean_details` / `get_boolean_details` | `FlagResolutionDetails[bool]` |
| `resolve_string_details` / `get_string_details` | `FlagResolutionDetails[str]` |
| `resolve_integer_details` / `get_integer_details` | `FlagResolutionDetails[int]` |
| `resolve_float_details` / `get_float_details` | `FlagResolutionDetails[float]` |
| `resolve_object_details` | `FlagResolutionDetails[Any]` |

The provider-specific full-config API is:

```python
all_config = provider.resolve_all_features(context)
filtered = provider.resolve_all_features_with_filter(context, ["payment."])
```

Async variants are also available:

```python
all_config = await provider.resolve_all_features_async(context)
```

## Current Limitations

- `LocalResolutionProvider` does not take an `experimentation_options` argument today.
- `ExperimentationOptions` is available in `types.py` for compatibility paths, but it is not wired into the `LocalResolutionProvider` constructor.
- File-backed sources do not support experiments.
