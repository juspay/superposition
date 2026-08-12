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
from typing import AsyncGenerator, Dict, List, Optional, Any, Tuple, Union, Sequence, Mapping

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

        # Single-flight: concurrent refresh() callers (e.g. a burst of on-demand evaluations after the
        # TTL expires) share ONE in-flight refresh instead of each launching its own. Without this, N
        # callers cause N redundant re-fetches — an N× load spike on the service.
        self._in_flight_refresh: Optional[asyncio.Future] = None

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

            # Fetch initial config from primary, fall back if needed. A fetch *error* triggers the
            # fallback; init fails if the fallback errors too — same shape as Rust.
            try:
                response = await self.primary_source.fetch_config(None)
                config_data = response.get_data()
                logger.info("LocalResolutionProvider: fetched config from primary source")
            except Exception as primary_err:
                logger.warning(f"LocalResolutionProvider: primary config fetch failed: {primary_err}")
                if self.fallback_source is None:
                    raise SuperpositionError.config_error(
                        f"Primary config fetch failed and no fallback configured: {primary_err}"
                    ) from primary_err
                try:
                    response = await self.fallback_source.fetch_config(None)
                    config_data = response.get_data()
                    logger.info("LocalResolutionProvider: fetched config from fallback source")
                except Exception as fallback_err:
                    raise SuperpositionError.config_error(
                        f"Both primary and fallback config fetch failed. "
                        f"Primary: {primary_err}. Fallback: {fallback_err}"
                    ) from fallback_err
            if config_data is not None:
                self.cached_config = config_data
                self._update_config_ffi_cache()
            self.config_checked_at = datetime.now(timezone.utc)

            # Fetch initial experiments. A source that doesn't support experiments simply yields none
            # (non-fatal). If the primary *does* support them, a fetch error requires an experiment-
            # capable fallback (or init fails) — same shape as Rust.
            if self.primary_source.supports_experiments():
                try:
                    response = await self.primary_source.fetch_active_experiments(None)
                    experiment_data = response.get_data()
                    logger.info("LocalResolutionProvider: fetched experiments from primary source")
                except Exception as primary_err:
                    logger.warning(f"LocalResolutionProvider: primary experiment fetch failed: {primary_err}")
                    if self.fallback_source is None or not self.fallback_source.supports_experiments():
                        raise SuperpositionError.config_error(
                            f"Primary experiment fetch failed and no experiment-capable fallback configured: {primary_err}"
                        ) from primary_err
                    try:
                        response = await self.fallback_source.fetch_active_experiments(None)
                        experiment_data = response.get_data()
                        logger.info("LocalResolutionProvider: fetched experiments from fallback source")
                    except Exception as fallback_err:
                        raise SuperpositionError.config_error(
                            f"Both primary and fallback experiment fetch failed. "
                            f"Primary: {primary_err}. Fallback: {fallback_err}"
                        ) from fallback_err
                if experiment_data is not None:
                    self.cached_experiments = experiment_data
                    self._update_exp_ffi_cache()
                self.experiments_checked_at = datetime.now(timezone.utc)

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
        self.global_context = EvaluationContext()

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
        exclude_prefix_filter: Optional[List[str]] = None,
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
                exclude_prefix_filter,
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
        exclude_prefix_filter: Optional[List[str]] = None,
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
                exclude_prefix_filter,
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
        exclude_prefix_filter: Optional[List[str]] = None,
    ) -> List[str]:
        """Get applicable experiment variants.

        Args:
            context: Evaluation context with targeting key.
            prefix_filter: Optional list of variant ID prefixes to include.
            exclude_prefix_filter: Optional list of variant ID prefixes to exclude.

        Returns:
            List of applicable variant IDs.
        """
        await self._ensure_fresh_data()

        if not self.ffi_cache or not self.cached_experiments:
            # No experiments cached means nothing can apply — not an error.
            return []

        # Merge contexts
        targeting_key, query_data = self._merge_contexts(context)

        try:
            return self.ffi_cache.get_applicable_variants(
                query_data,
                prefix_filter,
                exclude_prefix_filter,
                targeting_key or "",
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
        # Single-flight: start a refresh only if none is running; otherwise join the in-flight one so a
        # burst of callers collapses to a single fetch. The event loop is single threaded, so this
        # check-and-set is atomic — there is no await between them. The shared task owns the timeout and
        # records the outcome once (see _run_one_refresh); shield() keeps one caller's own cancellation
        # from tearing down a refresh the others are still awaiting.
        if self._in_flight_refresh is None or self._in_flight_refresh.done():
            self._in_flight_refresh = asyncio.ensure_future(self._run_one_refresh())
            # Retrieve the result even if every awaiter goes away, to avoid an "exception never
            # retrieved" warning on a background (polling/watch) refresh nobody is awaiting.
            self._in_flight_refresh.add_done_callback(
                lambda t: t.cancelled() or t.exception()
            )
        await asyncio.shield(self._in_flight_refresh)

    async def _run_one_refresh(self) -> None:
        """The single coalesced refresh: enforces the strategy timeout and records the outcome once.

        Runs as one shared task so concurrent :meth:`refresh` callers observe the same result rather
        than each launching a redundant fetch. A timed-out refresh is abandoned here, so the task
        completes and the next caller starts a fresh one instead of joining a stuck refresh.
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
        """Refresh config and experiments concurrently, keeping last known good on failure.

        Mirrors Rust's ``refresh_once`` (inline, no per-source helpers): each source refreshes in its
        own coroutine; a 304 just restarts the TTL clock, a failure keeps the cache but propagates so
        refresh() marks the provider STALE. Config failure takes priority in what is surfaced.
        """
        async def refresh_config() -> None:
            if_modified_since = None if self.cached_config is None else self.cached_config.fetched_at
            response = await self.primary_source.fetch_config(if_modified_since)
            # New data or a 304 — both confirm the cache is current, so the TTL clock restarts.
            self.config_checked_at = datetime.now(timezone.utc)
            if response.get_data():
                self.cached_config = response.get_data()
                self._update_config_ffi_cache()
                logger.debug("LocalResolutionProvider: config refreshed from primary")

        async def refresh_experiments() -> None:
            if not self.primary_source.supports_experiments():
                return
            if_modified_since = None if self.cached_experiments is None else self.cached_experiments.fetched_at
            response = await self.primary_source.fetch_active_experiments(if_modified_since)
            self.experiments_checked_at = datetime.now(timezone.utc)
            if response.get_data():
                self.cached_experiments = response.get_data()
                self._update_exp_ffi_cache()
                logger.debug("LocalResolutionProvider: experiments refreshed from primary")

        config_result, exp_result = await asyncio.gather(
            refresh_config(), refresh_experiments(), return_exceptions=True
        )
        # Both coroutines run to completion; the cache is never overwritten on failure. Surface the
        # config error first (like Rust), else the experiment error — either marks the provider STALE.
        if isinstance(config_result, BaseException):
            logger.warning(
                f"LocalResolutionProvider: config refresh failed, keeping last known good: {config_result}"
            )
            raise config_result
        if isinstance(exp_result, BaseException):
            logger.warning(
                f"LocalResolutionProvider: experiment refresh failed, keeping last known good: {exp_result}"
            )
            raise exp_result

    # --- Private helpers ---

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

        async def _watch_loop(watch_iter: AsyncGenerator[str, None]) -> None:
            """File watching refresh loop with weakref to avoid reference cycle.

            The stream is obtained (and validated non-None) before this task is created, so the Watch
            strategy fails init rather than silently never refreshing when the source can't watch.
            """
            self_ref = weak_self()
            if self_ref is None:
                return
            debounce_interval = self_ref.refresh_strategy.debounce_ms / 1000
            del self_ref

            logger.info("Starting watch-based refresh")
            next_event = None
            try:
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
                watch_iter = self.primary_source.watch()
                if watch_iter is None:
                    raise SuperpositionError.config_error(
                        "Watch strategy selected but data source does not support watching"
                    )
                self._background_task = asyncio.create_task(_watch_loop(watch_iter))
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
        exclude_prefix_filter: Optional[List[str]] = None,
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
            data=self.ffi_cache.filter_config(context, prefix_filter, exclude_prefix_filter),
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
        exclude_prefix_filter: Optional[List[str]] = None,
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
            data=self.ffi_cache.filter_experiment(context, prefix_filter, exclude_prefix_filter, False),
            fetched_at=self.cached_experiments.fetched_at,
        ))

    async def fetch_matching_active_experiments(
        self,
        context: Optional[Dict[str, Any]] = None,
        prefix_filter: Optional[List[str]] = None,
        exclude_prefix_filter: Optional[List[str]] = None,
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
            data=self.ffi_cache.filter_experiment(context, prefix_filter, exclude_prefix_filter, True),
            fetched_at=self.cached_experiments.fetched_at,
        ))

    def supports_experiments(self) -> bool:
        """File source supports experiments if path is configured."""
        return self.primary_source.supports_experiments()

    async def close(self) -> None:
        """Stop watching and clean up resources."""
        return await self.shutdown()
