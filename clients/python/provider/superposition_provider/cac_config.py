import json
import logging
from decimal import Decimal
from typing import Any, Dict, Optional, TypeVar
from .types import OnDemandStrategy, PollingStrategy, SuperpositionOptions, ConfigurationOptions
from superposition_sdk.client import Superposition, Config, GetConfigInput
import asyncio
from datetime import datetime, timedelta
from superposition_bindings.superposition_types import Context

T = TypeVar("T")
logger = logging.getLogger(__name__)

from smithy_core.documents import Document
from smithy_core.shapes import ShapeType
from typing import Any

class DecimalEncoder(json.JSONEncoder):
    """Custom JSON encoder that handles Decimal types"""
    def default(self, obj):
        if isinstance(obj, Decimal):
            return float(obj)  # Convert Decimal to float for JSON serialization
        return super().default(obj)

def safe_json_dumps(obj: Any) -> str:
    """Safely serialize object to JSON, handling Decimal types"""
    return json.dumps(obj, cls=DecimalEncoder)

def document_to_python_value(doc: Document) -> Any:
    """Recursively unwrap smithy_core.Document into plain Python values."""
    if doc.is_none():
        return None

    match doc.shape_type:
        case ShapeType.BOOLEAN:
            return doc.as_boolean()
        case ShapeType.STRING:
            return doc.as_string()
        case ShapeType.BLOB:
            return doc.as_blob()
        case ShapeType.TIMESTAMP:
            return doc.as_timestamp()
        case ShapeType.BYTE | ShapeType.SHORT | ShapeType.INTEGER | ShapeType.LONG | ShapeType.BIG_INTEGER:
            return doc.as_integer()
        case ShapeType.FLOAT | ShapeType.DOUBLE:
            return doc.as_float()
        case ShapeType.BIG_DECIMAL:
            # Convert Decimal to float for JSON compatibility
            decimal_val = doc.as_decimal()
            return float(decimal_val) if decimal_val is not None else None
        case ShapeType.LIST:
            return [document_to_python_value(e) for e in doc.as_list()]
        case ShapeType.STRUCTURE | ShapeType.UNION | ShapeType.MAP:
            return {
                key: document_to_python_value(value)
                for key, value in doc.as_map().items()
            }
        case _:
            # Fallback to doc.as_value() if unknown shape or primitive
            val = doc.as_value()
            # Handle Decimal in fallback case too
            if isinstance(val, Decimal):
                return float(val)
            return val

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
    
    return converted

class CacConfig:
    def __init__(self, superposition_options: SuperpositionOptions, options: ConfigurationOptions):
        self.superposition_options = superposition_options
        self.options = options
        self.fallback_config = None
        if options.fallback_config:
            self.fallback_config = convert_fallback_config(options.fallback_config)
        
        self.cached_config = None
        self.last_updated = None
        self.evaluation_cache: Dict[str, Dict[str, Any]] = {}
        self._polling_task = None

    async def create_config(self) -> None:
        async def poll_config(interval: int, timeout: int) -> None:
            while True:
                try:
                    latest_config = await self._get_config(self.superposition_options)
                    if latest_config is None and self.cached_config is None:
                        logger.warning("No config found, using fallback config.")
                        self.cached_config = self.fallback_config
                    elif latest_config is not None:
                        self.cached_config = latest_config
                        self.last_updated = datetime.utcnow()
                        logger.info("Config fetched successfully.")

                except Exception as e:
                    logger.error(f"Polling error: {e}")

                await asyncio.sleep(interval)

        latest_config = await self._get_config(self.superposition_options)
        if latest_config is None and self.cached_config is None:
            if not self.fallback_config:
                logger.error("No config found and no fallback config provided.")
                raise ValueError("No configuration available and no fallback config provided.")
            logger.warning("No config found, using fallback config.")
            self.cached_config = self.fallback_config
        elif latest_config is not None:
            self.cached_config = latest_config
            self.last_updated = datetime.utcnow()
            logger.info("Config fetched successfully.")
        
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
            # Create SDK config
            sdk_config = Config(
                endpoint_uri=superposition_options.endpoint
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
                elif latest_config is not None:
                    logger.info("Config fetched successfully.")
                    self.cached_config = latest_config
                    self.last_updated = datetime.utcnow()
                
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
