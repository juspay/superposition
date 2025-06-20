import json
from typing import Any, Dict, Optional, TypeVar, Generic
from .superposition_client import ffi_eval_config
from .types import OnDemandStrategy, PollingStrategy, SuperpositionOptions, ConfigurationOptions
from clients.generated.smithy.python.superposition_python_sdk.client import Superposition, Config, GetConfigInput
import asyncio
from .superposition_client import MergeStrategy
from datetime import datetime, timedelta
from .superposition_types import Context

T = TypeVar("T")

from smithy_core.documents import Document
from smithy_core.shapes import ShapeType
from typing import Any

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
            return doc.as_decimal()
        case ShapeType.LIST:
            return [document_to_python_value(e) for e in doc.as_list()]
        case ShapeType.STRUCTURE | ShapeType.UNION | ShapeType.MAP:
            return {
                key: document_to_python_value(value)
                for key, value in doc.as_map().items()
            }
        case _:
            # Fallback to doc.as_value() if unknown shape or primitive
            return doc.as_value()

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

class ConfigurationClient:
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
                        print("⚠️ No config found, using fallback config.")
                        self.cached_config = self.fallback_config
                    elif latest_config is not None:
                        self.cached_config = latest_config
                        self.last_updated = datetime.utcnow()
                        print("✅ Config fetched successfully.")

                except Exception as e:
                    print(f"Polling error: {e}")

                await asyncio.sleep(interval)

        latest_config = await self._get_config(self.superposition_options)
        if latest_config is None and self.cached_config is None:
            print("⚠️ No config found, using fallback config.")
            self.cached_config = self.fallback_config
        elif latest_config is not None:
            self.cached_config = latest_config
            self.last_updated = datetime.utcnow()
            print("✅ Config fetched successfully.")
        
        match self.options.refresh_strategy:
            case PollingStrategy(interval=interval, timeout=timeout):
                print(f"Using PollingStrategy: interval={interval}, timeout={timeout}")
                if self._polling_task is None:
                    self._polling_task = asyncio.create_task(poll_config(interval, timeout))

            case OnDemandStrategy(ttl=ttl, use_stale_on_error=use_stale, timeout=timeout):
                print(f"Using OnDemandStrategy: ttl={ttl}, use_stale_on_error={use_stale}, timeout={timeout}")

        

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
            print(f"Error fetching config from Superposition: {e}")
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
                print("🧠 TTL expired. Fetching config on-demand.")
                latest_config = await self._get_config(self.superposition_options)

                if latest_config is None and self.cached_config is None:
                    print("⚠️ No config found, using fallback config.")
                    self.cached_config = self.fall_back_config
                elif latest_config is not None:
                    print("✅ Config fetched successfully.")
                    self.cached_config = latest_config
                    self.last_updated = datetime.utcnow()
                
            except Exception as e:
                print(f"⚠️ On-demand fetch failed: {e}")
                if not use_stale or self.cached_config is None:
                    raise e
                else:
                    print("🕒 Using stale config due to error.")

        return self.cached_config
    
    async def eval_async(self, query_data: dict) -> dict[str, Any]:
        try:
            cache_key = self._generate_cache_key(query_data)
            cached = self._get_from_eval_cache(cache_key)

            if cached:
                print("Using cached evaluation result")
                return cached

            print("Fetching fresh configuration data")
            match self.options.refresh_strategy:
                case OnDemandStrategy(ttl=ttl, use_stale_on_error=use_stale, timeout=timeout):
                    await self.on_demand_config(ttl, use_stale)
            

            result = ffi_eval_config(
                self.cached_config.get('default_configs', {}),
                self.cached_config.get('contexts', []),
                self.cached_config.get('overrides', {}),
                query_data,
                MergeStrategy.MERGE,
                filter_prefixes=None
            )
            eval_result = {}
            for (key, value) in result.items():
                eval_result[key] = json.loads(value)
            print("Resolution result:", eval_result)

            return eval_result

        except Exception as e:
            print("Evaluation failed:", e)
            raise
    def eval(self, query_data: dict) -> dict[str, Any]:
        try:
            cache_key = self._generate_cache_key(query_data)
            cached = self._get_from_eval_cache(cache_key)

            if cached:
                print("Using cached evaluation result")
                return cached

            print("Fetching fresh configuration data")

            result = ffi_eval_config(
                self.cached_config.get('default_configs', {}),
                self.cached_config.get('contexts', []),
                self.cached_config.get('overrides', {}),
                query_data,
                MergeStrategy.MERGE,
                filter_prefixes=None
            )
            
            eval_result = {}
            for (key, value) in result.items():
                eval_result[key] = json.loads(value)
            print("Resolution result:", eval_result)

            return eval_result

        except Exception as e:
            print("Evaluation failed:", e)
            raise

    def get_boolean_value(self, key: str, default_value: bool, query_data: dict) -> bool:
        config = self.eval(query_data)
        value = self._get_nested(config, key)
        return self._to_boolean(value, default_value)

    def get_string_value(self, key: str, default_value: str, query_data: dict) -> str:
        config = self.eval(query_data)
        value = self._get_nested(config, key)
        return self._to_string(value, default_value)

    def get_integer_value(self, key: str, default_value: int, query_data: dict) -> int:
        config = self.eval(query_data)
        value = self._get_nested(config, key)
        return self._to_integer(value, default_value)

    def get_float_value(self, key: str, default_value: int, query_data: dict) -> int:
        config = self.eval(query_data)
        value = self._get_nested(config, key)
        return self._to_float(value, default_value)

    def get_object_value(self, key: str, default_value: T, query_data: dict) -> T:
        config = self.eval(query_data)
        value = self._get_nested(config, key)
        return self._to_object(value, default_value)

    def get_all(self, query_data: dict) -> dict:
        return self.eval(query_data)


    # Internal helpers

    def _get_nested(self, obj: Any, key: str) -> Any:
        keys = key.split('.')
        for k in keys:
            if not isinstance(obj, dict):
                return None
            obj = obj.get(k)
        return obj

    def _to_boolean(self, value: Any, default: bool) -> bool:
        if isinstance(value, bool): return value
        if isinstance(value, str):
            if value.lower() == "true": return True
            if value.lower() == "false": return False
        if isinstance(value, (int, float)): return value != 0
        return default

    def _to_string(self, value: Any, default: str) -> str:
        if isinstance(value, str): return value
        if value is not None: return str(value)
        return default

    def _to_integer(self, value: Any, default: int) -> int:
        try:
            return int(value)
        except (TypeError, ValueError):
            return default
    def _to_integer(self, value: Any, default: int) -> int:
        try:
            return float(value)
        except (TypeError, ValueError):
            return default

    def _to_object(self, value: Any, default: T) -> T:
        if isinstance(value, dict): return value
        return default

    def _generate_cache_key(self, query_data: dict) -> str:
        return json.dumps(query_data, sort_keys=True)

    def _get_from_eval_cache(self, key: str) -> Optional[Any]:
        self.evaluation_cache.get(key)

    def _set_eval_cache(self, key: str, value: Any) -> None:
        self.evaluation_cache[key] = value

    def _clear_eval_cache(self) -> None:
        self.evaluation_cache.clear()
