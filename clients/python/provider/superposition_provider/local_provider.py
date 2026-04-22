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
from typing import Callable, Dict, List, Optional, Any, Tuple, Union, Sequence, Mapping

from openfeature.provider import (
    AbstractProvider,
    Metadata,
    ProviderStatus,
)
from openfeature.evaluation_context import EvaluationContext
from openfeature.flag_evaluation import FlagResolutionDetails

from superposition_bindings.superposition_client import ProviderCache
from superposition_bindings.superposition_types import MergeStrategy

from . import FetchResponse
from .data_source import SuperpositionDataSource, ConfigData, ExperimentData
from .interfaces import AllFeatureProvider, FeatureExperimentMeta
from .types import RefreshStrategy, OnDemandStrategy, WatchStrategy, PollingStrategy, ManualStrategy, SseStrategy, default_on_demand_strategy

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
        refresh_strategy: RefreshStrategy = default_on_demand_strategy(),
        on_config_change: Optional[Callable[[Dict[str, Any], Dict[str, Any]], None]] = None,
    ):
        """Initialize local resolution provider.

        Args:
            primary_source: Primary data source for config/experiments.
            fallback_source: Optional fallback data source.
            refresh_strategy: How often to refresh data.
        """
        self.primary_source = primary_source
        self.fallback_source = fallback_source
        self.refresh_strategy = refresh_strategy
        self.on_config_change = on_config_change

        self.metadata = Metadata(name="LocalResolutionProvider")
        self.status = ProviderStatus.NOT_READY
        self.global_context = EvaluationContext()

        # Caches (atomic updates via simple assignments)
        self.cached_config: Optional[ConfigData] = None
        self.cached_experiments: Optional[ExperimentData] = None
        self.ffi_cache: Optional[ProviderCache] = None

        # Background task for refresh strategy
        self._background_task: Optional[asyncio.Task] = None
        # Set once the SSE connection is established (SseStrategy only)
        self._sse_connected_event: Optional[asyncio.Event] = None

    async def initialize(self, context: EvaluationContext):
        """Initialize the provider.

        Fetches initial config and experiments, starts refresh strategy.

        Args:
            context: Global evaluation context.
        """
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
            self.status = ProviderStatus.FATAL
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
            raise RuntimeError("Provider not properly initialized")

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

            # Convert from JSON strings to Python values
            return { k: json.loads(v) for k, v in result.items() }
        except Exception as e:
            logger.error(f"Error resolving features: {e}")
            raise

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
            raise RuntimeError("Provider not properly initialized")

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

            # Convert from JSON strings to Python values
            return { k: json.loads(v) for k, v in result.items() }
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

        if not self.cached_experiments or not self.ffi_cache:
            raise RuntimeError("Provider not properly initialized")

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
        """
        before = self.resolve_all_features(EvaluationContext()) if self.on_config_change and self.ffi_cache else None

        results = await asyncio.gather(
            self._fetch_and_cache_config(),
            self._fetch_and_cache_experiments(),
            return_exceptions=True,
        )
        for result in results:
            if isinstance(result, Exception):
                logger.warning(f"Error during refresh: {result}")

        if self.on_config_change and before is not None:
            after = self.resolve_all_features(EvaluationContext())
            if before != after:
                self.on_config_change(before, after)

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
            # Try primary source
            if_modified_since = None if init else self.cached_config.fetched_at
            response = await self.primary_source.fetch_config(if_modified_since)
            logger.debug(f"Primary source fetch_config response: {response}")
            if response.get_data():
                self.cached_config = response.get_data()
                self._update_config_ffi_cache()
                logger.info("Config updated from primary source")
            elif init:
                # need to counter the hack present in HttpDataSource
                await self._handle_fetch_config_from_fallback()

            if not self.cached_config:
                raise RuntimeError("Failed to fetch config from both primary and fallback sources")
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
            if response.get_data():
                self.cached_experiments = response.get_data()
                self._update_exp_ffi_cache()
                logger.info("Experiments updated from primary source")
            elif init:
                # need to counter the hack present in HttpDataSource
                await self._handle_fetch_experiments_from_fallback()

            if (not self.fallback_source or self.fallback_source.supports_experiments()) and not self.cached_experiments:
                raise RuntimeError("Failed to fetch experiments from both primary and fallback sources")
        except Exception as e:
            logger.error(f"Error fetching experiments from primary source: {e}")

            # Try fallback source if available and no 304
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
                ttl = self.refresh_strategy.ttl
                use_stale_on_error = self.refresh_strategy.use_stale_on_error

                def is_elapsed(cached_at: datetime) -> bool:
                    return (datetime.now(timezone.utc) - cached_at).total_seconds() > ttl

                should_refresh_config = self.cached_config.fetched_at is None or is_elapsed(self.cached_config.fetched_at)
                should_refresh_experiments = (
                    self.cached_experiments is None
                    or self.cached_experiments.fetched_at is None
                    or is_elapsed(self.cached_experiments.fetched_at)
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
            interval = self_ref.refresh_strategy.interval
            del self_ref

            logger.info(f"Starting polling with interval {interval}s")
            try:
                while True:
                    await asyncio.sleep(interval)

                    self_ref = weak_self()
                    if self_ref is None:
                        logger.info("LocalResolutionProvider has been garbage collected, stopping polling loop.")
                        return

                    await self_ref.refresh()
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
                    await self_ref.refresh()
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
                self._background_task = asyncio.create_task(self._polling_loop())
            case SseStrategy():
                import weakref
                strategy: SseStrategy = self.refresh_strategy
                options = strategy.superposition_options
                debounce_s = strategy.debounce_ms / 1000
                reconnect_delay = max(strategy.reconnect_delay, 1)
                sse_url = f"{options.endpoint.rstrip('/')}/{options.org_id}/{options.workspace_id}/stream"
                sse_headers = {"Authorization": f"Bearer {options.token}"}
                if options.org_id:
                    sse_headers["x-org-id"] = options.org_id
                weak_self = weakref.ref(self)
                connected_event = asyncio.Event()
                self._sse_connected_event = connected_event

                async def _sse_loop():
                    import aiohttp
                    logger.info(f"Starting SSE refresh (url={sse_url})")
                    try:
                        async with aiohttp.ClientSession() as session:
                            while True:
                                if weak_self() is None:
                                    logger.info("Provider garbage collected, stopping SSE loop.")
                                    return
                                try:
                                    async with session.get(
                                        sse_url,
                                        headers=sse_headers,
                                        timeout=aiohttp.ClientTimeout(total=None, sock_read=30),
                                    ) as resp:
                                        if resp.status != 200:
                                            logger.warning(f"SSE endpoint returned {resp.status}, retrying in {reconnect_delay}s")
                                            await asyncio.sleep(reconnect_delay)
                                            continue
                                        logger.info("SSE connection established")
                                        connected_event.set()
                                        self_ref = weak_self()
                                        if self_ref is not None:
                                            try:
                                                await self_ref.refresh()
                                            except Exception as e:
                                                logger.warning(f"Reconnect refresh failed: {e}")
                                        debounce_task = None
                                        async for line_bytes in resp.content:
                                            self_ref = weak_self()
                                            if self_ref is None:
                                                logger.info("Provider garbage collected, stopping SSE loop.")
                                                return
                                            line = line_bytes.decode("utf-8", errors="replace").strip()
                                            logger.debug(f"SSE raw line: {line!r}")
                                            if not line or line.startswith(":"):
                                                continue
                                            if line.startswith("event:") or line.startswith("data:"):
                                                async def _do_refresh(ref=self_ref):
                                                    await asyncio.sleep(debounce_s)
                                                    try:
                                                        await ref.refresh()
                                                    except Exception as e:
                                                        logger.warning(f"SSE-triggered refresh failed: {e}")
                                                if debounce_task and not debounce_task.done():
                                                    debounce_task.cancel()
                                                debounce_task = asyncio.create_task(_do_refresh())
                                except (aiohttp.ClientError, asyncio.TimeoutError) as e:
                                    logger.warning(f"SSE connection error: {e}, retrying in {reconnect_delay}s")
                                    await asyncio.sleep(reconnect_delay)
                    except asyncio.CancelledError:
                        logger.info("SSE loop cancelled")

                self._background_task = asyncio.create_task(_sse_loop())
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
            raise RuntimeError("Provider not properly initialized or no config available")

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
            raise NotImplementedError("Experiments not supported by this provider")

        if not self.cached_experiments:
            raise RuntimeError("Provider not properly initialized or no experiments available")

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
            raise NotImplementedError("Experiments not supported by this provider")

        if not self.ffi_cache or not self.cached_experiments:
            raise RuntimeError("Provider not properly initialized or no experiments available")

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
            raise NotImplementedError("Experiments not supported by this provider")

        if not self.ffi_cache or not self.cached_experiments:
            raise RuntimeError("Provider not properly initialized or no experiments available")

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
