import json
import logging
import weakref
from decimal import Decimal
from typing import Any, Dict, Optional, TypeVar

from .conversions import to_dimension_type, document_to_python_value
from .types import OnDemandStrategy, PollingStrategy, SuperpositionOptions, ConfigurationOptions
from superposition_sdk.client import Superposition, GetConfigInput
from superposition_sdk.config import Config
from superposition_sdk.auth_helpers import bearer_auth_config
import asyncio
from datetime import datetime, timedelta
from superposition_bindings.superposition_types import Context, DimensionInfo

T = TypeVar("T")
logger = logging.getLogger(__name__)

from smithy_core.documents import Document
from smithy_core.shapes import ShapeType


class DecimalEncoder(json.JSONEncoder):
    """Custom JSON encoder that handles Decimal types"""
    def default(self, obj):
        if isinstance(obj, Decimal):
            return float(obj)  # Convert Decimal to float for JSON serialization
        return super().default(obj)

def safe_json_dumps(obj: Any) -> str:
    """Safely serialize object to JSON, handling Decimal types"""
    return json.dumps(obj, cls=DecimalEncoder)

def convert_fallback_config(fallback_config: Optional[Dict[str, Any]]) -> Optional[Dict[str, Any]]:
    """Convert fallback config to the expected format."""
    if not fallback_config:
        return None

    converted = {}
    if 'default_configs' in fallback_config:
        converted['default_configs'] = {
            key: json.dumps(value)
            for key, value in fallback_config['default_configs'].items()
        }
    if 'overrides' in fallback_config:
        converted['overrides'] = {
            key: {k: json.dumps(v) for k, v in value.items()}
            for key, value in fallback_config['overrides'].items()
        }
    if 'contexts' in fallback_config:
        converted['contexts'] = [
            Context(
                id=ctx['id'],
                priority=ctx['priority'],
                weight=ctx['weight'],
                override_with_keys=ctx.get('override_with_keys', []),
                condition={k: json.dumps(v) for k, v in ctx.get('condition', {}).items()}
            )
            for ctx in fallback_config['contexts']
        ]
    if 'dimensions' in fallback_config:
        converted['dimensions'] = {
            key: DimensionInfo(
                schema={k: json.dumps(v) for k, v in dimension.get('schema', {}).items()},
                position=dimension['position'],
                dimension_type=dimension['dimension_type'],
                dependency_graph=dimension['dependency_graph'],
                value_compute_function_name=dimension.get('value_compute_function_name'),
            )
            for key, dimension in fallback_config['dimensions'].items()
        }

    return converted

class CacConfig:
    def __init__(self, superposition_options: SuperpositionOptions, options: ConfigurationOptions, on_config_change=None):
        self.superposition_options = superposition_options
        self.options = options
        self.on_config_change = on_config_change  # Callback when config changes
        self.fallback_config = None
        if options.fallback_config:
            self.fallback_config = convert_fallback_config(options.fallback_config)

        self.cached_config = None
        self.last_updated = None
        self.evaluation_cache: Dict[str, Dict[str, Any]] = {}
        self._polling_task = None

    async def create_config(self) -> None:
        weak_self = weakref.ref(self)

        async def poll_config(interval: int, timeout: int) -> None:
            while True:
                self_ref = weak_self()
                if self_ref is None:
                    logger.info("CacConfig has been garbage collected, stopping polling task.")
                    return

                try:
                    latest_config = await self_ref._get_config(self_ref.superposition_options)
                    if latest_config is None and self_ref.cached_config is None:
                        logger.warning("No config found, using fallback config.")
                        self_ref.cached_config = self_ref.fallback_config
                        if self_ref.on_config_change:
                            self_ref.on_config_change()
                    elif latest_config is not None:
                        # Only trigger callback if config actually changed
                        if self_ref.cached_config != latest_config:
                            self_ref.cached_config = latest_config
                            self_ref.last_updated = datetime.utcnow()
                            logger.info("Config fetched and updated.")
                            # Trigger callback for cache reinitialization
                            if self_ref.on_config_change:
                                self_ref.on_config_change()
                        else:
                            logger.info("Config unchanged (skipping callback)")

                except Exception as e:
                    logger.error(f"Polling error: {e}")
                finally:
                    del self_ref

                await asyncio.sleep(interval)

        latest_config = await self._get_config(self.superposition_options)
        if latest_config is None and self.cached_config is None:
            if not self.fallback_config:
                logger.error("No config found and no fallback config provided.")
                raise ValueError("No configuration available and no fallback config provided.")
            logger.warning("No config found, using fallback config.")
            self.cached_config = self.fallback_config
            if self.on_config_change:
                self.on_config_change()
        elif latest_config is not None:
            self.cached_config = latest_config
            self.last_updated = datetime.utcnow()
            logger.info("Config fetched successfully.")
            if self.on_config_change:
                self.on_config_change()

        match self.options.refresh_strategy:
            case PollingStrategy(interval=interval, timeout=timeout):
                logger.info(f"Using PollingStrategy: interval={interval}, timeout={timeout}")
                if self._polling_task is None:
                    self._polling_task = asyncio.create_task(poll_config(interval, timeout))

            case OnDemandStrategy(ttl=ttl, use_stale_on_error=use_stale, timeout=timeout):
                logger.info(f"Using OnDemandStrategy: ttl={ttl}, use_stale_on_error={use_stale}, timeout={timeout}")



    @staticmethod
    async def _get_config(superposition_options: SuperpositionOptions) -> Optional[Dict[str, Any]]:
        """
        Fetch configuration from Superposition service using the generated Python SDK.

        Args:
            superposition_options: Options containing endpoint, token, org_id, workspace_id

        Returns:
            Dict containing the configuration data
        """
        try:
            # Create SDK config with bearer token authentication
            (resolver, schemes) = bearer_auth_config(
                token=superposition_options.token
            )
            sdk_config = Config(
                endpoint_uri=superposition_options.endpoint,
                http_auth_scheme_resolver=resolver,
                http_auth_schemes=schemes
            )

            # Create Superposition client
            client = Superposition(config=sdk_config)

            # Prepare input for get_config API call
            get_config_input = GetConfigInput(
                workspace_id=superposition_options.workspace_id,
                org_id=superposition_options.org_id,
                context={},  # No specific context filtering for now
                prefix=None,   # No prefix filtering for now
                version=None   # Get latest version
            )

            # Call the get_config API
            response = await client.get_config(get_config_input)

            # Convert the response to a dictionary format
            config_data = {}

            # Add default configs
            if response.default_configs:
                default_configs: dict[str, str] = {}
                for key, value in response.default_configs.items():
                    default_configs[key] = json.dumps(document_to_python_value(value))
                config_data['default_configs'] = default_configs
            if response.overrides:
                overrides = {}
                for (key, value) in response.overrides.items():
                    override = {}
                    for (key1,value1) in value.items():
                        override[key1] = json.dumps(document_to_python_value(value1))
                    overrides[key] = override
                config_data['overrides'] = overrides
            if response.contexts:
                context = []
                for ele in response.contexts:
                    condition = {}
                    for key, value in ele.condition.items():
                        condition[key] = json.dumps(document_to_python_value(value))
                    cv = Context(
                        id=ele.id,
                        priority=ele.priority,
                        weight=ele.weight,
                        override_with_keys=ele.override_with_keys,
                        condition=condition
                    )
                    context.append(cv)
                config_data['contexts'] = context
            if response.dimensions:
                dimensions = {}
                for (key, dim_info) in response.dimensions.items():
                    dimension = DimensionInfo(
                        schema={k: json.dumps(document_to_python_value(v)) for k, v in dim_info.schema.items()},
                        position=dim_info.position,
                        dimension_type=to_dimension_type(dim_info.dimension_type),
                        dependency_graph=dim_info.dependency_graph,
                        value_compute_function_name=dim_info.value_compute_function_name,
                    )
                    dimensions[key] = dimension
                config_data['dimensions'] = dimensions

            return config_data

        except Exception as e:
            # Log the error and return empty config as fallback
            logger.error(f"Error fetching config from Superposition: {e}")
            return None

    async def on_demand_config(self, ttl, use_stale) -> dict:
        """Get config on-demand based on TTL or fall back to stale if needed."""
        now = datetime.utcnow()
        should_refresh = (
            not self.last_updated or
            (now - self.last_updated) > timedelta(seconds=ttl)
        )

        if should_refresh:
            try:
                logger.debug("TTL expired. Fetching config on-demand.")
                latest_config = await self._get_config(self.superposition_options)

                if latest_config is None and self.cached_config is None:
                    logger.warning("No config found, using fallback config.")
                    self.cached_config = self.fallback_config
                    if self.on_config_change:
                        self.on_config_change()
                elif latest_config is not None:
                    if self.cached_config != latest_config:
                        logger.info("Config fetched and updated successfully.")
                        self.cached_config = latest_config
                        self.last_updated = datetime.utcnow()
                        # Trigger callback for cache reinitialization
                        if self.on_config_change:
                            self.on_config_change()

            except Exception as e:
                logger.warning(f"On-demand fetch failed: {e}")
                if not use_stale or self.cached_config is None:
                    raise e
                else:
                    logger.info("Using stale config due to error.")

        return self.cached_config


    def _generate_cache_key(self, query_data: dict) -> str:
        return json.dumps(query_data, sort_keys=True)

    def _get_from_eval_cache(self, key: str) -> Optional[Any]:
        self.evaluation_cache.get(key)

    def _set_eval_cache(self, key: str, value: Any) -> None:
        self.evaluation_cache[key] = value

    def _clear_eval_cache(self) -> None:
        self.evaluation_cache.clear()

    async def close(self):
        """
        Close the configuration client and clean up resources.
        Stops polling tasks and clears caches.
        """
        try:
            # Cancel polling task if running
            if self._polling_task and not self._polling_task.done():
                logger.info("Stopping polling task...")
                self._polling_task.cancel()
                try:
                    await self._polling_task
                except asyncio.CancelledError:
                    logger.debug("Polling task cancelled successfully")

            # Clear caches
            self._clear_eval_cache()
            self.cached_config = None
            self.last_updated = None

            logger.info("ConfigurationClient closed successfully")

        except Exception as e:
            logger.error(f"Error during ConfigurationClient cleanup: {e}")
            raise
