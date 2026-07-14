"""The ON_DEMAND TTL is measured from the last *check*, not the config's last *change*.

An HTTP data source stamps ConfigData.fetched_at with the server's last-modified — when the config
last changed. Driving the TTL off that made a config which had been stable for longer than the TTL
permanently "stale": every evaluation fired a fetch, the 304 that came back left the timestamp
untouched, and the next evaluation fired another. The more stable the config, the more load.

FileDataSource stamps datetime.now() instead, which is why the file-based paths never showed this.
ServerLike below reproduces the HTTP behaviour.

Run standalone: python test_ondemand_ttl.py
"""

import asyncio
import time
from datetime import datetime, timedelta, timezone
from typing import Any, Dict, List, Optional

from openfeature.evaluation_context import EvaluationContext

from superposition_provider.data_source import (
    ConfigData,
    FetchResponse,
    SuperpositionDataSource,
)
from superposition_provider.local_provider import LocalResolutionProvider
from superposition_provider.types import OnDemandStrategy

from superposition_bindings.superposition_types import Config

# The config last changed an hour ago — and is perfectly current.
SERVER_LAST_MODIFIED = datetime.now(timezone.utc) - timedelta(hours=1)


class ServerLike(SuperpositionDataSource):
    """Behaves like HttpDataSource: fetched_at is the server's last-modified, repeats get a 304."""

    def __init__(self):
        self.fetches = 0

    async def fetch_filtered_config(
        self,
        context: Optional[Dict[str, Any]] = None,
        prefix_filter: Optional[List[str]] = None,
        if_modified_since: Optional[datetime] = None,
    ) -> FetchResponse[ConfigData]:
        self.fetches += 1
        if if_modified_since is not None:
            return FetchResponse.not_modified()
        config = Config(
            contexts=[],
            overrides={},
            default_configs={"currency": '"Rupee"'},
            dimensions={},
        )
        return FetchResponse.data(ConfigData(data=config, fetched_at=SERVER_LAST_MODIFIED))

    async def fetch_active_experiments(self, if_modified_since=None):
        raise NotImplementedError

    async def fetch_candidate_active_experiments(self, context=None, prefix_filter=None, if_modified_since=None):
        raise NotImplementedError

    async def fetch_matching_active_experiments(self, context=None, prefix_filter=None, if_modified_since=None):
        raise NotImplementedError

    def supports_experiments(self) -> bool:
        return False

    async def close(self) -> None:
        pass


def _provider(ttl_ms: int):
    source = ServerLike()
    provider = LocalResolutionProvider(
        source,
        refresh_strategy=OnDemandStrategy(ttl_milliseconds=ttl_ms, timeout_milliseconds=5_000),
    )
    return source, provider


def test_an_unchanged_config_is_not_refetched_within_the_ttl():
    async def run():
        source, provider = _provider(60_000)
        await provider.initialize(EvaluationContext())
        after_init = source.fetches

        for _ in range(5):
            await provider.resolve_all_features_async(EvaluationContext())

        assert source.fetches == after_init, (
            f"evaluations inside the TTL hit the source {source.fetches - after_init} times; "
            "however old the config is, they should hit it zero times"
        )
        await provider.shutdown()

    asyncio.run(run())


def test_the_ttl_still_expires_and_a_304_restarts_it():
    async def run():
        source, provider = _provider(100)
        await provider.initialize(EvaluationContext())
        after_init = source.fetches

        # Inside the TTL: free.
        await provider.resolve_all_features_async(EvaluationContext())
        assert source.fetches == after_init

        time.sleep(0.2)

        # Past the TTL: exactly one re-check.
        await provider.resolve_all_features_async(EvaluationContext())
        assert source.fetches == after_init + 1, "an expired TTL must re-check the source"

        # The 304 it got back is a successful check, so the clock restarts and this is free.
        await provider.resolve_all_features_async(EvaluationContext())
        assert source.fetches == after_init + 1, "a 304 is a successful check and must restart the TTL"

        # And the cache kept serving the last known good value throughout.
        flags = await provider.resolve_all_features_async(EvaluationContext())
        assert flags["currency"] == "Rupee"

        await provider.shutdown()

    asyncio.run(run())


if __name__ == "__main__":
    cases = [v for k, v in sorted(globals().items()) if k.startswith("test_")]
    for case in cases:
        case()
        print(f"ok  {case.__name__}")
    print(f"\n{len(cases)} passed")
