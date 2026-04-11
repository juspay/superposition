"""
LocalResolutionProvider - Local in-process evaluation with caching.

Implements feature resolution with configurable refresh strategies and
support for primary + fallback data sources.
"""

import logging
import asyncio
import json
from datetime import datetime, timezone
from typing import Dict, List, Optional, Any, Tuple, Union, Sequence, Mapping

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
        refresh_strategy: RefreshStrategy = default_on_demand_strategy(),
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

        self.metadata = Metadata(name="LocalResolutionProvider")
        self.status = ProviderStatus.NOT_READY
        self.global_context = EvaluationContext()

        # Caches (atomic updates via simple assignments)
        self.cached_config: Optional[ConfigData] = None
        self.cached_experiments: Optional[ExperimentData] = None
        self.ffi_cache: Optional[ProviderCache] = None

        # Background task for refresh strategy
        self._background_task: Optional[asyncio.Task] = None

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
        match self.refresh_strategy:
            case WatchStrategy():
                self._background_task = asyncio.create_task(self._watch_loop())
            case PollingStrategy():
                self._background_task = asyncio.create_task(self._polling_loop())
            case ManualStrategy():
                logger.debug("MANUAL strategy: caller must invoke refresh()")
            case OnDemandStrategy():
                logger.debug("ON_DEMAND strategy: refresh on first stale access")

    async def _polling_loop(self) -> None:
        """Polling refresh loop."""
        logger.info(f"Starting polling with interval {self.refresh_strategy.interval}s")
        try:
            while True:
                await asyncio.sleep(self.refresh_strategy.interval)
                await self.refresh()
        except asyncio.CancelledError:
            logger.info("Polling loop cancelled")

    async def _watch_loop(self) -> None:
        """File watching refresh loop."""
        logger.info("Starting watch-based refresh")
        try:
            # use self.refresh_strategy.debounce for debounce interval
            debounce_interval = self.refresh_strategy.debounce_ms / 1000
            # watch() is an async generator, iterate directly with async for
            async for _ in self.primary_source.watch():
                logger.debug("File change detected, refreshing...")
                await asyncio.sleep(debounce_interval)
                await self.refresh()
        except asyncio.CancelledError:
            logger.info("Watch loop cancelled")

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
