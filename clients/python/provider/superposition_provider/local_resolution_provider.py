import asyncio
import json
import logging
from datetime import datetime
from typing import Any, Dict, List, Optional, Tuple, Type, TypeVar

from openfeature.evaluation_context import EvaluationContext
from openfeature.flag_evaluation import FlagResolutionDetails
from openfeature.hook import Hook
from openfeature.provider import AbstractProvider, Metadata as ProviderMetadata
from openfeature.provider import ProviderStatus

from .interfaces import (
    AllFeatureProvider,
    FeatureExperimentMeta,
    SuperpositionDataSource,
)
from .types import (
    CacheOptions,
    ConfigData,
    ExperimentData,
    ManualStrategy,
    OnDemandStrategy,
    PollingStrategy,
    RefreshStrategy,
)

logger = logging.getLogger(__name__)

T = TypeVar("T")


class LocalResolutionProvider(
    AbstractProvider, AllFeatureProvider, FeatureExperimentMeta
):
    def __init__(
        self,
        primary: SuperpositionDataSource,
        fallback: Optional[SuperpositionDataSource] = None,
        refresh_strategy: RefreshStrategy = None,
        cache_options: Optional[CacheOptions] = None,
    ) -> None:
        if refresh_strategy is None:
            refresh_strategy = ManualStrategy()

        self._primary = primary
        self._fallback = fallback
        self._refresh_strategy = refresh_strategy
        self._cache_options = cache_options

        self.metadata = ProviderMetadata(name="LocalResolutionProvider")
        self.status = ProviderStatus.NOT_READY
        self.hooks: List[Hook] = []

        self._cached_config: Optional[ConfigData] = None
        self._cached_experiments: Optional[ExperimentData] = None
        self._polling_task: Optional[asyncio.Task] = None
        self._lock = asyncio.Lock()
        self._last_refresh_time: Optional[datetime] = None

    async def initialize(self, context: Optional[EvaluationContext] = None) -> None:
        self.status = ProviderStatus.NOT_READY

        await self._do_refresh()

        if isinstance(self._refresh_strategy, PollingStrategy):
            self._start_polling(self._refresh_strategy.interval)

        self.status = ProviderStatus.READY

    async def shutdown(self) -> None:
        if self._polling_task is not None:
            self._polling_task.cancel()
            try:
                await self._polling_task
            except asyncio.CancelledError:
                pass
            self._polling_task = None

        await self._primary.close()
        if self._fallback is not None:
            await self._fallback.close()

        self._cached_config = None
        self._cached_experiments = None
        self._last_refresh_time = None

        self.status = ProviderStatus.NOT_READY

    async def refresh(self) -> None:
        await self._do_refresh()

    async def resolve_all_features(self, context: EvaluationContext) -> Dict[str, Any]:
        return await self._eval_with_context(context)

    async def resolve_all_features_with_filter(
        self, context: EvaluationContext, prefix_filter: Optional[List[str]] = None
    ) -> Dict[str, Any]:
        return await self._eval_with_context(context, prefix_filter)

    async def get_applicable_variants(self, context: EvaluationContext) -> List[str]:
        if self._cached_experiments is None:
            return []

        return []

    def resolve_boolean_details(
        self,
        flag_key: str,
        default_value: bool,
        evaluation_context: Optional[EvaluationContext] = None,
    ) -> FlagResolutionDetails[bool]:
        return self._resolve_value(flag_key, default_value, evaluation_context, bool)

    def resolve_string_details(
        self,
        flag_key: str,
        default_value: str,
        evaluation_context: Optional[EvaluationContext] = None,
    ) -> FlagResolutionDetails[str]:
        return self._resolve_value(flag_key, default_value, evaluation_context, str)

    def resolve_integer_details(
        self,
        flag_key: str,
        default_value: int,
        evaluation_context: Optional[EvaluationContext] = None,
    ) -> FlagResolutionDetails[int]:
        return self._resolve_value(flag_key, default_value, evaluation_context, int)

    def resolve_float_details(
        self,
        flag_key: str,
        default_value: float,
        evaluation_context: Optional[EvaluationContext] = None,
    ) -> FlagResolutionDetails[float]:
        return self._resolve_value(flag_key, default_value, evaluation_context, float)

    def resolve_object_details(
        self,
        flag_key: str,
        default_value: Any,
        evaluation_context: Optional[EvaluationContext] = None,
    ) -> FlagResolutionDetails[Any]:
        return self._resolve_value(flag_key, default_value, evaluation_context, object)

    def _resolve_value(
        self,
        flag_key: str,
        default_value: T,
        evaluation_context: Optional[EvaluationContext],
        value_type: Type[T],
    ) -> FlagResolutionDetails[T]:
        self._ensure_fresh_data()

        if self._cached_config is None:
            return FlagResolutionDetails(
                value=default_value,
                reason="DEFAULT",
                error_code="PROVIDER_NOT_READY",
                error_message="Configuration not available",
            )

        try:
            evaluated = asyncio.run(
                self._eval_with_context(evaluation_context or EvaluationContext())
            )
            value = evaluated.get(flag_key)

            if value is None:
                return FlagResolutionDetails(
                    value=default_value,
                    reason="DEFAULT",
                    error_code="FLAG_NOT_FOUND",
                    error_message=f"Key not found: {flag_key}",
                )

            converted_value = self._convert_value(value, value_type, default_value)
            return FlagResolutionDetails(
                value=converted_value, reason="TARGETING_MATCH"
            )
        except Exception as e:
            return FlagResolutionDetails(
                value=default_value,
                reason="ERROR",
                error_code="GENERAL",
                error_message=str(e),
            )

    async def _eval_with_context(
        self,
        context: EvaluationContext,
        prefix_filter: Optional[List[str]] = None,
    ) -> Dict[str, Any]:
        self._ensure_fresh_data()

        if self._cached_config is None:
            return {}

        data = self._cached_config
        result: Dict[str, Any] = {}

        if data.default_configs is not None:
            result.update(data.default_configs)

        if data.contexts is not None:
            sorted_contexts = sorted(
                data.contexts,
                key=lambda x: x.get("priority", 0),
                reverse=True,
            )

            extracted_context, _ = self._extract_context(context)

            for ctx in sorted_contexts:
                if self._matches_context(ctx, extracted_context):
                    override_keys = ctx.get("override_with_keys", [])
                    if override_keys and data.overrides is not None:
                        for key in override_keys:
                            override = data.overrides.get(key)
                            if override is not None:
                                result.update(override)

        if prefix_filter is not None:
            result = {
                k: v
                for k, v in result.items()
                if any(k.startswith(prefix) for prefix in prefix_filter)
            }

        return result

    async def _do_refresh(self) -> None:
        async with self._lock:
            try:
                config = await self._primary.fetch_config()
            except Exception as ex:
                if self._fallback is not None:
                    try:
                        config = await self._fallback.fetch_config()
                    except Exception as fallback_ex:
                        raise RuntimeError(
                            "Primary failed and no fallback available"
                        ) from fallback_ex
                else:
                    raise RuntimeError(
                        "Primary failed and no fallback available"
                    ) from ex

            try:
                if self._primary.supports_experiments():
                    experiments = await self._primary.fetch_active_experiments()
                else:
                    experiments = None
            except Exception as ex:
                if self._fallback is not None and self._fallback.supports_experiments():
                    try:
                        experiments = await self._fallback.fetch_active_experiments()
                    except Exception:
                        experiments = None
                else:
                    experiments = None

            self._cached_config = config
            self._cached_experiments = experiments
            self._last_refresh_time = datetime.now()

    def _ensure_fresh_data(self) -> None:
        if isinstance(self._refresh_strategy, OnDemandStrategy):
            ttl_seconds = self._refresh_strategy.ttl
            if self._last_refresh_time is not None:
                elapsed = (datetime.now() - self._last_refresh_time).total_seconds()
                if elapsed > ttl_seconds:
                    try:
                        asyncio.create_task(self._do_refresh())
                    except Exception as e:
                        if not self._refresh_strategy.use_stale_on_error:
                            raise

    def _start_polling(self, interval: int) -> None:
        async def poll() -> None:
            while True:
                try:
                    await asyncio.sleep(interval)
                    await self._do_refresh()
                except asyncio.CancelledError:
                    break
                except Exception as e:
                    logger.error(f"Polling refresh failed: {e}")

        self._polling_task = asyncio.create_task(poll())

    def _extract_context(
        self, context: EvaluationContext
    ) -> Tuple[Dict[str, Any], Optional[str]]:
        if context is None:
            return {}, None

        result: Dict[str, Any] = {}
        if context.attributes is not None:
            for key, value in context.attributes.items():
                result[key] = value

        return result, context.targeting_key

    def _matches_context(
        self, ctx: Dict[str, Any], extracted_context: Dict[str, Any]
    ) -> bool:
        condition = ctx.get("condition")
        if condition is None or not isinstance(condition, dict):
            return True

        for key, value in condition.items():
            extracted_value = extracted_context.get(key)
            if extracted_value is None:
                return False
            if str(extracted_value) != str(value):
                return False

        return True

    def _get_nested_value(self, obj: Dict[str, Any], path: str) -> Any:
        keys = path.split(".")
        current = obj
        for key in keys:
            if not isinstance(current, dict):
                return None
            current = current.get(key)
            if current is None:
                return None
        return current

    def _convert_value(self, value: Any, target_type: Type[T], default: T) -> T:
        if value is None:
            return default

        if target_type == object:
            return value

        try:
            if target_type == bool:
                return bool(value)
            elif target_type == str:
                return str(value)
            elif target_type == int:
                if isinstance(value, (int, float)):
                    return int(value)
                return int(str(value))
            elif target_type == float:
                if isinstance(value, (int, float)):
                    return float(value)
                return float(str(value))
        except (ValueError, TypeError):
            return default

        return default

    def get_metadata(self) -> ProviderMetadata:
        return self.metadata

    def get_status(self) -> ProviderStatus:
        return self.status
