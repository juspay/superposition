import asyncio

from .provider import SuperpositionProvider
from .types import SuperpositionProviderOptions, PollingStrategy
from openfeature import api
from openfeature.evaluation_context import EvaluationContext

async def test_config():
    print("🧪 Testing improved ConfigurationClient...")


    config_options = SuperpositionProviderOptions(
        endpoint="http://localhost:8080",
        token="token",
        org_id="localorg",
        workspace_id="test",
        refresh_strategy=PollingStrategy(
            interval=5,  # Poll every 5 seconds
            timeout=3  # Timeout after 30 seconds
        ),
        fallback_config=None, # No fallback config for this test
        evaluation_cache_options=None,  # No cache options for this test  # Poll every 5 seconds
        experimentation_options=None  # No experimentation options for this test
    )
    provider = SuperpositionProvider(provider_options=config_options)
    ctx = EvaluationContext(
        targeting_key="",  # e.g. your user ID
        attributes={"newd": "\"hi\""}
    )
    await provider.initialize(context=ctx)
    api.set_provider(provider)
    cac_client = api.get_client()

    show = cac_client.get_boolean_details("bool_key", True, ctx)
    print(f"Boolean flag 'bool_key': {show.value} (details: {show})")
    await asyncio.sleep(15)

    show = cac_client.get_boolean_details("bool_key", True, ctx)
    print(f"Boolean flag 'bool_key': {show.value} (details: {show})")



if __name__ == "__main__":
    asyncio.run(test_config())


