"""
LocalResolutionProvider - Local in-process evaluation with caching.

Implements feature resolution with configurable refresh strategies and
support for primary + fallback data sources.
"""

import logging
import asyncio
import json
import weakref
from datetime import datetime, timezone
from typing import Dict, List, Optional, Any, Tuple, Union, Sequence, Mapping

from openfeature.provider import (
    AbstractProvider,
    Metadata,
    ProviderStatus,
)
from openfeature.evaluation_context import EvaluationContext
from openfeature.event import ProviderEventDetails
from openfeature.flag_evaluation import FlagResolutionDetails

from superposition_bindings.superposition_client import ProviderCache
from superposition_bindings.superposition_types import MergeStrategy

from . import FetchResponse
from .data_source import SuperpositionDataSource, ConfigData, ExperimentData
from .errors import SuperpositionError
from .interfaces import AllFeatureProvider, FeatureExperimentMeta
from .types import RefreshStrategy, OnDemandStrategy, WatchStrategy, PollingStrategy, ManualStrategy, default_on_demand_strategy

logger = logging.getLogger(__name__)

class LocalResolutionProvider(AbstractProvider, AllFeatureProvider, FeatureExperimentMeta, SuperpositionDataSource):
    """Local in-process OpenFeature provider with caching and refresh strategies.

    Features:
    - Configurable refresh strategies (Polling, OnDemand, Watch, Manual)
    - Primary + fallback data sources
    - Atomic cache updates via thread-safe references
    - FFI-based local evaluation for performance
    """

    def __init__(
        self,
        primary_source: SuperpositionDataSource,
        fallback_source: Optional[SuperpositionDataSource] = None,
        refresh_strategy: Optional[RefreshStrategy] = None,
    ):
        """Initialize local resolution provider.

        Args:
            primary_source: Primary data source for config/experiments.
            fallback_source: Optional fallback data source.
            refresh_strategy: How often to refresh data. Defaults to on-demand.
        """
        self.primary_source = primary_source
        self.fallback_source = fallback_source
        # Built per instance, not in the signature: a default argument is evaluated once at
        # import, so every provider in the process would otherwise share one strategy object.
        self.refresh_strategy = refresh_strategy or default_on_demand_strategy()

        self.metadata = Metadata(name="LocalResolutionProvider")
        self.status = ProviderStatus.NOT_READY
        self.global_context = EvaluationContext()

        # Caches (atomic updates via simple assignments)
        self.cached_config: Optional[ConfigData] = None
        self.cached_experiments: Optional[ExperimentData] = None
        self.ffi_cache: Optional[ProviderCache] = None

        # When each cache was last successfully checked against its source, by the local clock.
        #
        # Deliberately not ConfigData.fetched_at: for an HTTP source that is the *server's*
        # last-modified — when the config last *changed*, not when we last *looked*. Driving the
        # ON_DEMAND TTL off it meant a config stable for longer than the TTL was permanently
        # "stale", so every evaluation fired a fetch; the 304 that came back left the timestamp
        # untouched, so the next evaluation fired another. A perfectly stable config produced
        # maximum load, which is the opposite of what ON_DEMAND is for.
        #
        # Advanced on every successful check, *including* a 304.
        self.config_checked_at: Optional[datetime] = None
        self.experiments_checked_at: Optional[datetime] = None

        # Background task for refresh strategy
        self._background_task: Optional[asyncio.Task] = None

    async def initialize(self, context: EvaluationContext):
        """Initialize the provider.

        Fetches initial config and experiments, starts refresh strategy.

        Args:
            context: Global evaluation context.
        """
        # Single-shot: a provider is initialized once and then served. Re-initializing a live
        # provider would overwrite the background-task handle, orphaning the running polling/watch
        # loop, so it is refused. A fresh or previously-failed provider proceeds.
        if self.status in (ProviderStatus.READY, ProviderStatus.STALE):
            logger.warning(
                "LocalResolutionProvider already initialized; ignoring initialize(). "
                "Providers are single-shot — build a new instance."
            )
            return

        try:
            logger.info("Initializing LocalResolutionProvider...")
            self.status = ProviderStatus.NOT_READY
            self.global_context = context

            # Create FFI cache
            self.ffi_cache = ProviderCache()

            # Fetch initial config (required)
            await self._fetch_and_cache_config(init=True)

            # Fetch initial experiments (best-effort)
            await self._fetch_and_cache_experiments(init=True)

            # Start refresh strategy
            await self._start_refresh_strategy()

            self.status = ProviderStatus.READY
            logger.info("LocalResolutionProvider initialized successfully")
        except Exception as e:
            logger.error(f"Failed to initialize LocalResolutionProvider: {e}")
            self.status = ProviderStatus.ERROR
            raise

    async def shutdown(self):
        """Shutdown the provider and stop all background tasks."""
        logger.info("Shutting down LocalResolutionProvider...")

        # Cancel background tasks
        if self._background_task:
            self._background_task.cancel()
            try:
                await self._background_task
            except asyncio.CancelledError:
                pass

        # Close data sources
        try:
            await self.primary_source.close()
        except Exception as e:
            logger.warning(f"Error closing primary data source: {e}")

        if self.fallback_source:
            try:
                await self.fallback_source.close()
            except Exception as e:
                logger.warning(f"Error closing fallback data source: {e}")

        # Clear caches
        self.cached_config = None
        self.cached_experiments = None
        self.ffi_cache = None
        self.config_checked_at = None
        self.experiments_checked_at = None

        self.status = ProviderStatus.NOT_READY
        logger.info("LocalResolutionProvider shutdown completed")

    def get_metadata(self) -> Metadata:
        """Get provider metadata."""
        return self.metadata

    def get_status(self) -> ProviderStatus:
        """Get provider status."""
        return self.status

    # --- AllFeatureProvider implementation ---
    def resolve_all_features_with_filter(
        self,
        context: Optional[EvaluationContext],
        prefix_filter: Optional[List[str]] = None,
    ) -> Dict[str, Any]:
        """Resolve all features with optional filtering.

        Args:
            context: Evaluation context.
            prefix_filter: Optional list of key prefixes.

        Returns:
            Dictionary of filtered resolved flags.
        """
        match self.refresh_strategy:
            case OnDemandStrategy():
                logger.debug("ON_DEMAND strategy: data might be stale, use async resolve to ensure freshness")
            case _: ()

        if not self.ffi_cache or not self.cached_config:
            raise SuperpositionError.provider_error("Provider not initialized: no cached config available")

        # Merge contexts
        targeting_key, query_data = self._merge_contexts(context)

        try:
            # Use FFI for local evaluation
            result = self.ffi_cache.eval_config(
                query_data,
                MergeStrategy.MERGE,
                prefix_filter,
                targeting_key,
            )

            return self._decode_flags(result)
        except Exception as e:
            logger.error(f"Error resolving features: {e}")
            raise

    @staticmethod
    def _decode_flags(result: Dict[str, str]) -> Dict[str, Any]:
        """Decode the JSON-encoded values the FFI cache hands back.

        Decoded per key so a single malformed value names the flag it came from, rather than
        failing the whole evaluation with a bare JSONDecodeError.
        """
        decoded: Dict[str, Any] = {}
        for key, value in result.items():
            try:
                decoded[key] = json.loads(value)
            except json.JSONDecodeError as e:
                raise SuperpositionError.serialization_error(
                    f"Flag '{key}' does not hold well-formed JSON: {value}", e
                ) from e
        return decoded

    async def resolve_all_features_with_filter_async(
        self,
        context: Optional[EvaluationContext],
        prefix_filter: Optional[List[str]] = None,
    ) -> Dict[str, Any]:
        """Resolve all features with optional filtering.

        Args:
            context: Evaluation context.
            prefix_filter: Optional list of key prefixes.

        Returns:
            Dictionary of filtered resolved flags.
        """
        # Ensure fresh data (for ON_DEMAND strategy)
        await self._ensure_fresh_data()

        if not self.ffi_cache or not self.cached_config:
            raise SuperpositionError.provider_error("Provider not initialized: no cached config available")

        # Merge contexts
        targeting_key, query_data = self._merge_contexts(context)

        try:
            # Use FFI for local evaluation
            result = self.ffi_cache.eval_config(
                query_data,
                MergeStrategy.MERGE,
                prefix_filter,
                targeting_key,
            )

            return self._decode_flags(result)
        except Exception as e:
            logger.error(f"Error resolving features: {e}")
            raise

    # --- FeatureExperimentMeta implementation ---

    async def get_applicable_variants(
        self,
        context: Optional[EvaluationContext],
        prefix_filter: Optional[List[str]] = None,
    ) -> List[str]:
        """Get applicable experiment variants.

        Args:
            context: Evaluation context with targeting key.
            prefix_filter: Optional list of variant ID prefixes.

        Returns:
            List of applicable variant IDs.
        """
        await self._ensure_fresh_data()

        if not self.ffi_cache or not self.cached_experiments:
            # No experiments cached means nothing can apply — not an error.
            return []

        # Merge contexts
        targeting_key, query_data = self._merge_contexts(context)

        if targeting_key is None:
            return []

        try:
            # Use FFI for local evaluation
            return self.ffi_cache.get_applicable_variants(
                query_data,
                prefix_filter,
                targeting_key,
            )

        except Exception as e:
            logger.error(f"Error getting applicable variants: {e}")
            raise

    # --- OpenFeature FeatureProvider methods ---

    def resolve_boolean_details(
        self,
        flag_key: str,
        default_value: bool,
        evaluation_context: Optional[EvaluationContext] = None,
    ) -> FlagResolutionDetails[bool]:
        """Resolve a boolean flag."""
        return self.resolve_bool(flag_key, default_value, evaluation_context)

    def resolve_string_details(
        self,
        flag_key: str,
        default_value: str,
        evaluation_context: Optional[EvaluationContext] = None,
    ) -> FlagResolutionDetails[str]:
        """Resolve a string flag."""
        return self.resolve_string(flag_key, default_value, evaluation_context)

    def resolve_integer_details(
        self,
        flag_key: str,
        default_value: int,
        evaluation_context: Optional[EvaluationContext] = None,
    ) -> FlagResolutionDetails[int]:
        """Resolve an integer flag."""
        return self.resolve_int(flag_key, default_value, evaluation_context)

    def resolve_float_details(
        self,
        flag_key: str,
        default_value: float,
        evaluation_context: Optional[EvaluationContext] = None,
    ) -> FlagResolutionDetails[float]:
        """Resolve a float flag."""
        return self.resolve_float(flag_key, default_value, evaluation_context)

    def resolve_object_details(
        self,
        flag_key: str,
        default_value: Union[Mapping, Sequence],
        evaluation_context: Optional[EvaluationContext] = None,
    ) -> FlagResolutionDetails[Union[Mapping, Sequence]]:
        """Resolve an object/struct flag."""
        return self.resolve_object(flag_key, default_value, evaluation_context)

    async def resolve_boolean_details_async(
        self,
        flag_key: str,
        default_value: bool,
        evaluation_context: Optional[EvaluationContext] = None,
    ) -> FlagResolutionDetails[bool]:
        """Resolve a boolean flag."""
        return await self.resolve_bool_async(flag_key, default_value, evaluation_context)

    async def resolve_string_details_async(
        self,
        flag_key: str,
        default_value: str,
        evaluation_context: Optional[EvaluationContext] = None,
    ) -> FlagResolutionDetails[str]:
        """Resolve a string flag."""
        return await self.resolve_string_async(flag_key, default_value, evaluation_context)

    async def resolve_integer_details_async(
        self,
        flag_key: str,
        default_value: int,
        evaluation_context: Optional[EvaluationContext] = None,
    ) -> FlagResolutionDetails[int]:
        """Resolve an integer flag."""
        return await self.resolve_int_async(flag_key, default_value, evaluation_context)

    async def resolve_float_details_async(
        self,
        flag_key: str,
        default_value: float,
        evaluation_context: Optional[EvaluationContext] = None,
    ) -> FlagResolutionDetails[float]:
        """Resolve a float flag."""
        return await self.resolve_float_async(flag_key, default_value, evaluation_context)

    async def resolve_object_details_async(
        self,
        flag_key: str,
        default_value: Union[Mapping, Sequence],
        evaluation_context: Optional[EvaluationContext] = None,
    ) -> FlagResolutionDetails[Union[Mapping, Sequence]]:
        """Resolve an object/struct flag."""
        return await self.resolve_object_async(flag_key, default_value, evaluation_context)

    # --- Public refresh methods ---

    async def refresh(self) -> None:
        """Manually refresh both config and experiments.

        Useful for MANUAL refresh strategy.

        Bounded by the refresh strategy's timeout, if it sets one: without it a data source that
        never answers would stall the polling loop, or the caller under ON_DEMAND, indefinitely.

        Every refresh path — polling, watch, on-demand and manual — funnels through here, so this
        is also where staleness is recorded.

        Raises:
            SuperpositionError: if the refresh outlives the strategy's timeout.
        """
        succeeded = False
        try:
            timeout = self._refresh_timeout()
            if timeout is None:
                await self._refresh_once()
            else:
                try:
                    await asyncio.wait_for(self._refresh_once(), timeout=timeout / 1000)
                except asyncio.TimeoutError as e:
                    logger.warning(
                        f"Refresh timed out after {timeout}ms, keeping last known good data"
                    )
                    raise SuperpositionError.refresh_error(
                        f"Refresh timed out after {timeout}ms", e
                    ) from e
            succeeded = True
        finally:
            self._record_refresh_outcome(succeeded)

    def _record_refresh_outcome(self, succeeded: bool) -> None:
        """Mark the provider STALE while a failed refresh leaves the cache frozen.

        The flags keep resolving to their last known good values, and this is the only signal a
        consumer has that they stopped tracking the source of truth. The next successful refresh
        clears it.

        Only meaningful from READY: a failure during init is an ERROR (there is no good data to be
        stale), and a provider that has been shut down stays NOT_READY.

        The event matters as much as the attribute: the SDK keeps its own copy of provider status
        in `provider_registry._provider_status` and never reads ours, so without emitting, nothing
        going through the OpenFeature client — `get_provider_status()`, an `on_provider_stale`
        handler — would ever see this. Evaluation is unaffected either way: the client only
        short-circuits on NOT_READY and FATAL.
        """
        if succeeded:
            if self.status == ProviderStatus.STALE:
                logger.info("LocalResolutionProvider: refresh recovered, no longer stale")
                self.status = ProviderStatus.READY
                self.emit_provider_ready(
                    ProviderEventDetails(message="Refresh recovered; flags are current again")
                )
        elif self.status == ProviderStatus.READY:
            logger.warning("LocalResolutionProvider: refresh failed, serving stale data")
            self.status = ProviderStatus.STALE
            self.emit_provider_stale(
                ProviderEventDetails(
                    message="Refresh failed; serving the last known good config"
                )
            )

    def _refresh_timeout(self) -> Optional[int]:
        """The timeout the configured strategy puts on a single refresh, if any."""
        match self.refresh_strategy:
            case PollingStrategy() | OnDemandStrategy():
                return self.refresh_strategy.timeout_ms()
            case _:
                return None

    async def _refresh_once(self) -> None:
        """Fetch config and experiments concurrently."""
        await asyncio.gather(self._fetch_and_cache_config(), self._fetch_and_cache_experiments())

    # --- Private helpers ---

    async def _handle_fetch_config_from_fallback(self):
        """Fetch config from fallback source, if available."""
        try:
            logger.info("Attempting to fetch config from fallback source...")
            if self.fallback_source:
                logger.info("Fetching config from fallback source...")
                response = await self.fallback_source.fetch_config(None)
                if response.get_data():
                    self.cached_config = response.get_data()
                    self._update_config_ffi_cache()
                    logger.info("Config updated from fallback source")
        except Exception as e:
            logger.error(f"Error fetching fallback config: {e}")
            raise e

    async def _fetch_and_cache_config(self, init: bool = False) -> None:
        """Fetch and cache configuration from primary/fallback sources."""
        try:
            logger.info(f"Fetching config (init={init})...")
            # Try primary source. A refresh after a failed init has no cached copy to date
            # from, so it must ask for everything.
            if_modified_since = (
                None if init or self.cached_config is None
                else self.cached_config.fetched_at
            )
            response = await self.primary_source.fetch_config(if_modified_since)
            logger.debug(f"Primary source fetch_config response: {response}")
            # The fetch returned, so the cache is confirmed current — whether it came back with
            # new data or a 304. Either way the TTL clock restarts.
            self.config_checked_at = datetime.now(timezone.utc)
            if response.get_data():
                self.cached_config = response.get_data()
                self._update_config_ffi_cache()
                logger.info("Config updated from primary source")

            if not self.cached_config:
                raise SuperpositionError.config_error("Failed to fetch config from both primary and fallback sources")
        except Exception as e:
            logger.error(f"Error fetching config from primary source: {e}, init={init}")
            # Try fallback source if available
            if init:
                logger.info("Attempting to fetch config from fallback source due to primary source error during initialization")
                await self._handle_fetch_config_from_fallback()
            else:
                logger.error(f"Error fetching fallback config: {e}")

            if not self.cached_config:
                raise e

    async def _handle_fetch_experiments_from_fallback(self):
        """Fetch experiments from fallback source, if available."""
        if not self.fallback_source or not self.fallback_source.supports_experiments():
            return
        try:
            response = await self.fallback_source.fetch_active_experiments(None)
            if response.get_data():
                self.cached_experiments = response.get_data()
                self._update_exp_ffi_cache()
                logger.info("Experiments updated from fallback source")
        except Exception as e:
            logger.error(f"Error fetching fallback experiments: {e}")
            raise e

    async def _fetch_and_cache_experiments(self, init: bool = False) -> None:
        """Fetch and cache experiments from primary/fallback sources."""
        if not self.primary_source.supports_experiments():
            return
        try:
            # Try primary source
            if_modified_since = (
                None if init or self.cached_experiments is None
                else self.cached_experiments.fetched_at
            )
            response = await self.primary_source.fetch_active_experiments(if_modified_since)
            # See _fetch_and_cache_config: new data or a 304, both are a successful check.
            self.experiments_checked_at = datetime.now(timezone.utc)
            if response.get_data():
                self.cached_experiments = response.get_data()
                self._update_exp_ffi_cache()
                logger.info("Experiments updated from primary source")

            if (not self.fallback_source or self.fallback_source.supports_experiments()) and not self.cached_experiments:
                raise SuperpositionError.config_error("Failed to fetch experiments from both primary and fallback sources")
        except Exception as e:
            logger.error(f"Error fetching experiments from primary source: {e}")

            # Try fallback source if available
            if init:
                await self._handle_fetch_experiments_from_fallback()
            else:
                logger.error(f"Error fetching fallback experiments: {e}")

            if init and self.fallback_source and self.fallback_source.supports_experiments() and not self.cached_experiments:
                raise e

    async def _ensure_fresh_data(self) -> None:
        """Check if data needs refresh (for ON_DEMAND strategy)."""
        match self.refresh_strategy:
            case OnDemandStrategy():
                ttl = self.refresh_strategy.ttl_ms()
                use_stale_on_error = self.refresh_strategy.use_stale_on_error

                def is_elapsed(cached_at: datetime) -> bool:
                    elapsed_ms = (datetime.now(timezone.utc) - cached_at).total_seconds() * 1000
                    return elapsed_ms > ttl

                # Never checked, or last checked before the TTL window opened. Note this also
                # removes an AttributeError: the old form read self.cached_config.fetched_at
                # without guarding against cached_config being None after a failed init.
                should_refresh_config = (
                    self.config_checked_at is None or is_elapsed(self.config_checked_at)
                )
                should_refresh_experiments = self.primary_source.supports_experiments() and (
                    self.experiments_checked_at is None
                    or is_elapsed(self.experiments_checked_at)
                )

                if should_refresh_config or should_refresh_experiments:
                    try:
                        await self.refresh()
                    except Exception as e:
                        if not use_stale_on_error:
                            raise e
                        logger.error(f"Error refreshing: {e}")

            case _:
                logger.debug("Do nothing - fresh data check not required")

    async def _start_refresh_strategy(self) -> None:
        """Start the configured refresh strategy."""
        weak_self = weakref.ref(self)

        async def _polling_loop() -> None:
            """Polling refresh loop with weakref to avoid reference cycle."""
            self_ref = weak_self()
            if self_ref is None:
                return
            interval = self_ref.refresh_strategy.interval_ms()
            del self_ref

            logger.info(f"Starting polling with interval {interval}ms")
            try:
                while True:
                    await asyncio.sleep(interval / 1000)

                    self_ref = weak_self()
                    if self_ref is None:
                        logger.info("LocalResolutionProvider has been garbage collected, stopping polling loop.")
                        return

                    try:
                        await self_ref.refresh()
                    except asyncio.CancelledError:
                        raise
                    except Exception as e:
                        # Keep polling on failure; the last known good data stays in place.
                        logger.warning(f"Polling refresh failed: {e}")
                    del self_ref
            except asyncio.CancelledError:
                logger.info("Polling loop cancelled")

        async def _watch_loop() -> None:
            """File watching refresh loop with weakref to avoid reference cycle."""
            self_ref = weak_self()
            if self_ref is None:
                return
            debounce_interval = self_ref.refresh_strategy.debounce_ms / 1000
            primary_source = self_ref.primary_source
            del self_ref

            logger.info("Starting watch-based refresh")
            watch_iter = None
            next_event = None
            try:
                watch_iter = primary_source.watch()
                next_event = asyncio.ensure_future(anext(watch_iter))

                while True:
                    done, _ = await asyncio.wait([next_event], timeout=5.0)

                    if not done:
                        self_ref = weak_self()
                        if self_ref is None:
                            logger.info("LocalResolutionProvider has been garbage collected, stopping watch loop.")
                            return
                        del self_ref
                        continue

                    logger.debug("File change detected, starting debounce...")
                    while True:
                        try:
                            next_event = asyncio.ensure_future(anext(watch_iter))
                        except StopAsyncIteration:
                            logger.info("Primary source watch stream ended, stopping watch loop.")
                            return

                        done, _ = await asyncio.wait([next_event], timeout=debounce_interval)
                        if done:
                            logger.debug("Another change during debounce window, resetting timer...")
                            continue
                        break

                    self_ref = weak_self()
                    if self_ref is None:
                        logger.info("LocalResolutionProvider has been garbage collected, stopping watch loop.")
                        return

                    logger.debug("Debounce settled, refreshing...")
                    try:
                        await self_ref.refresh()
                    except asyncio.CancelledError:
                        raise
                    except Exception as e:
                        # Keep watching on failure; the last known good data stays in place.
                        logger.warning(f"Watch refresh failed: {e}")
                    del self_ref
            except asyncio.CancelledError:
                logger.info("Watch loop cancelled")
            finally:
                if next_event and not next_event.done():
                    next_event.cancel()
                    try:
                        await next_event
                    except (asyncio.CancelledError, StopAsyncIteration):
                        pass
                if watch_iter is not None:
                    await watch_iter.aclose()

        match self.refresh_strategy:
            case WatchStrategy():
                self._background_task = asyncio.create_task(_watch_loop())
            case PollingStrategy():
                self._background_task = asyncio.create_task(_polling_loop())
            case ManualStrategy():
                logger.debug("MANUAL strategy: caller must invoke refresh()")
            case OnDemandStrategy():
                logger.debug("ON_DEMAND strategy: refresh on first stale access")

    def _merge_contexts(self, context: Optional[EvaluationContext]) -> Tuple[Optional[str], dict[str, str]]:
        """Merge global and evaluation contexts."""
        eval_ctx = self.global_context.merge(context) if context else self.global_context
        query_data = { k: json.dumps(v) for k, v in eval_ctx.attributes.items() }
        return eval_ctx.targeting_key, query_data

    def _update_config_ffi_cache(self) -> None:
        """Update ffi config cache with new values."""
        config = self.cached_config.data
        self.ffi_cache.init_config(config.default_configs, config.contexts, config.overrides, config.dimensions)

    def _update_exp_ffi_cache(self) -> None:
        """Update ffi exp config cache with new values."""
        exp = self.cached_experiments.data
        self.ffi_cache.init_experiments(exp.experiments, exp.experiment_groups)


    async def fetch_filtered_config(
        self,
        context: Optional[Dict[str, Any]] = None,
        prefix_filter: Optional[List[str]] = None,
        if_modified_since: Optional[datetime] = None,
    ) -> FetchResponse[ConfigData]:
        """Fetch configuration, optionally filtered.

        Note: File-based filtering is not efficient; consider using HttpDataSource
        for production configurations that need filtering.

        Args:
            context: Optional context for filtering (ignored).
            prefix_filter: Optional key prefixes to include.
            if_modified_since: Timestamp for 304 Not Modified check.

        Returns:
            FetchResponse with ConfigData or NotModified status.
        """
        if not self.ffi_cache or not self.cached_config:
            raise SuperpositionError.data_source_error("No cached config available")

        if if_modified_since is not None:
            logger.debug("LocalResolutionProvider: ignoring if_modified_since, always reading fresh from file")

        return FetchResponse.data(ConfigData(
            data=self.ffi_cache.filter_config(context, prefix_filter),
            fetched_at=self.cached_config.fetched_at,
        ))

    async def fetch_active_experiments(
        self,
        if_modified_since: Optional[datetime] = None,
    ) -> FetchResponse[ExperimentData]:
        """Fetch experiments from file.

        Args:
            if_modified_since: Timestamp for 304 Not Modified check.

        Returns:
            FetchResponse with ExperimentData or NotModified status.
        """
        if not self.supports_experiments():
            raise SuperpositionError.data_source_error("Experiments not supported by this provider")

        if not self.cached_experiments:
            raise SuperpositionError.data_source_error("No cached experiments available")

        if if_modified_since is not None:
            logger.debug("LocalResolutionProvider: ignoring if_modified_since for experiments, always returning cached data")

        return FetchResponse.data(self.cached_experiments)

    async def fetch_candidate_active_experiments(
        self,
        context: Optional[Dict[str, Any]] = None,
        prefix_filter: Optional[List[str]] = None,
        if_modified_since: Optional[datetime] = None,
    ) -> FetchResponse[ExperimentData]:
        """Fetch candidate active experiments."""
        if not self.supports_experiments():
            raise SuperpositionError.data_source_error("Experiments not supported by this provider")

        if not self.ffi_cache or not self.cached_experiments:
            raise SuperpositionError.data_source_error("No cached experiments available")

        if if_modified_since is not None:
            logger.debug("LocalResolutionProvider: ignoring if_modified_since for experiments, always returning cached data")

        return FetchResponse.data(ExperimentData(
            data=self.ffi_cache.filter_experiment(context, prefix_filter, False),
            fetched_at=self.cached_experiments.fetched_at,
        ))

    async def fetch_matching_active_experiments(
        self,
        context: Optional[Dict[str, Any]] = None,
        prefix_filter: Optional[List[str]] = None,
        if_modified_since: Optional[datetime] = None,
    ) -> FetchResponse[ExperimentData]:
        """Fetch matching active experiments."""
        if not self.supports_experiments():
            raise SuperpositionError.data_source_error("Experiments not supported by this provider")

        if not self.ffi_cache or not self.cached_experiments:
            raise SuperpositionError.data_source_error("No cached experiments available")

        if if_modified_since is not None:
            logger.debug("LocalResolutionProvider: ignoring if_modified_since for experiments, always returning cached data")

        return FetchResponse.data(ExperimentData(
            data=self.ffi_cache.filter_experiment(context, prefix_filter, True),
            fetched_at=self.cached_experiments.fetched_at,
        ))

    def supports_experiments(self) -> bool:
        """File source supports experiments if path is configured."""
        return self.primary_source.supports_experiments()

    async def close(self) -> None:
        """Stop watching and clean up resources."""
        return await self.shutdown()
