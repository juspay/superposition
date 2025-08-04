### Installation

```bash
pip install superposition_provider
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
        endpoint="http://localhost:8080", # or any valid superposition endpoint
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
