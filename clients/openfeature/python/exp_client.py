import json
import logging
from decimal import Decimal
from typing import Any, Dict, Optional, TypeVar, Generic
from .uniffi_client import ffi_get_applicable_variants
from .types import OnDemandStrategy, PollingStrategy, SuperpositionOptions, ConfigurationOptions, ExperimentationOptions
from clients.generated.smithy.python.superposition_python_sdk.client import Superposition, Config, ListExperimentInput
import asyncio
from .uniffi_client import MergeStrategy
from datetime import datetime, timedelta
from .uniffi_types import Context

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


class ExperimentationClient():
    def __init__(self, superposition_options: SuperpositionOptions, experiment_options: ExperimentationOptions):
        self.superposition_options = superposition_options
        self.options = experiment_options
        self.cached_experiments = None
        self.last_updated = None
        self.evaluation_cache: Dict[str, Dict[str, Any]] = {}
        self._polling_task = None

    async def create_config(self) -> None:
        async def poll_config(interval: int, timeout: int) -> None:
            while True:
                try:
                    latest_exp_list = await self._get_experiments(self.superposition_options)
                    if latest_exp_list is not None:
                        self.cached_experiments = latest_exp_list
                        self.last_updated = datetime.utcnow()
                        logger.info("Experiment List fetched successfully.")

                except Exception as e:
                    logger.error(f"Polling error: {e}")

                await asyncio.sleep(interval)

        latest_exp_list = await self._get_config(self.superposition_options)
        if latest_exp_list is not None:
            self.cached_experiments = latest_exp_list
            self.last_updated = datetime.utcnow()
            logger.info("Experiment List fetched successfully.")
        
        match self.options.refresh_strategy:
            case PollingStrategy(interval=interval, timeout=timeout):
                logger.info(f"Using PollingStrategy: interval={interval}, timeout={timeout}")
                if self._polling_task is None:
                    self._polling_task = asyncio.create_task(poll_config(interval, timeout))

            case OnDemandStrategy(ttl=ttl, use_stale_on_error=use_stale, timeout=timeout):
                logger.info(f"Using OnDemandStrategy: ttl={ttl}, use_stale_on_error={use_stale}, timeout={timeout}")

        

    @staticmethod
    async def _get_experiments(superposition_options: SuperpositionOptions) -> Optional[Dict[str, Any]]:
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
            # @dataclass(kw_only=True)
            # class ListExperimentInput:

            #     workspace_id: str | None = None
            #     org_id: str = "juspay"
            #     page: int | None = None
            #     count: int | None = None
            #     all: bool | None = None
            #     status: str | None = None
            #     from_date: datetime | None = None
            #     to_date: datetime | None = None
            #     experiment_name: str | None = None
            #     experiment_ids: str | None = None
            #     created_by: str | None = None
            #     context_query: str | None = None
            #     sort_on: str | None = None
            #     sort_by: str | None = None

            list_exp_input = ListExperimentInput(
                workspace_id=superposition_options.workspace_id,
                org_id=superposition_options.org_id,
                all=True
            )
            
            # Call the get_config API
            response = await client.list_experiment(list_exp_input)
            
            # Convert the response to a dictionary format
            exp_list = response.get("data", [])
            trimmed_exp_list = []
            for exp in exp_list:
                trimmed_exp = {
                    "id": exp.id,
                    "context": exp.context,
                    "variants": exp.variants,
                    "traffic_percentage": exp.traffic_percentage,
                }
                trimmed_exp_list.append(trimmed_exp)
            
            # Add default config
            
            return trimmed_exp_list
            
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
                latest_exp_list = await self._get_experiments(self.superposition_options)

                if latest_exp_list is not None:
                    logger.info("Experiment List fetched successfully.")
                    self.cached_experiments = latest_exp_list
                    self.last_updated = datetime.utcnow()
                
            except Exception as e:
                logger.warning(f"On-demand fetch failed: {e}")
                if not use_stale or self.cached_experiments is None:
                    raise e
                else:
                    logger.info("Using stale config due to error.")

        return self.cached_experiments
    
    # async def eval_async(self, query_data: dict) -> dict[str, Any]:
    #     try:
    #         cache_key = self._generate_cache_key(query_data)
    #         cached = self._get_from_eval_cache(cache_key)

    #         if cached:
    #             logger.debug("Using cached evaluation result")
    #             return cached

    #         logger.debug("Fetching fresh configuration data")
    #         match self.options.refresh_strategy:
    #             case OnDemandStrategy(ttl=ttl, use_stale_on_error=use_stale, timeout=timeout):
    #                 await self.on_demand_config(ttl, use_stale)
            

    #         result = ffi_eval_config(
    #             self.cached_config.get('default_configs', {}),
    #             self.cached_config.get('contexts', []),
    #             self.cached_config.get('overrides', {}),
    #             query_data,
    #             MergeStrategy.MERGE,
    #             filter_prefixes=None
    #         )
    #         eval_result = {}
    #         for (key, value) in result.items():
    #             eval_result[key] = json.loads(value)
    #         logger.debug(f"Resolution result: {eval_result}")

    #         return eval_result

    #     except Exception as e:
    #         logger.error(f"Evaluation failed: {e}")
    #         raise
            
    # def eval(self, query_data: dict) -> dict[str, Any]:
    #     try:
    #         cache_key = self._generate_cache_key(query_data)
    #         cached = self._get_from_eval_cache(cache_key)

    #         if cached:
    #             logger.debug("Using cached evaluation result")
    #             return cached

    #         logger.debug("Fetching fresh configuration data")
            
    #         result = ffi_eval_config(
    #             self.cached_config.get('default_configs', {}),
    #             self.cached_config.get('contexts', []),
    #             self.cached_config.get('overrides', {}),
    #             query_data,
    #             MergeStrategy.MERGE,
    #             filter_prefixes=None
    #         )
            
    #         eval_result = {}
    #         for (key, value) in result.items():
    #             eval_result[key] = json.loads(value)
    #         logger.debug(f"Resolution result: {eval_result}")

    #         return eval_result

    #     except Exception as e:
    #         logger.error(f"Evaluation failed: {e}")
    #         raise

    # def get_boolean_value(self, key: str, default_value: bool, query_data: dict) -> bool:
    #     config = self.eval(query_data)
    #     value = self._get_nested(config, key)
    #     return self._to_boolean(value, default_value)

    # def get_string_value(self, key: str, default_value: str, query_data: dict) -> str:
    #     config = self.eval(query_data)
    #     value = self._get_nested(config, key)
    #     return self._to_string(value, default_value)

    # def get_integer_value(self, key: str, default_value: int, query_data: dict) -> int:
    #     config = self.eval(query_data)
    #     value = self._get_nested(config, key)
    #     return self._to_integer(value, default_value)

    # def get_float_value(self, key: str, default_value: int, query_data: dict) -> int:
    #     config = self.eval(query_data)
    #     value = self._get_nested(config, key)
    #     return self._to_float(value, default_value)

    # def get_object_value(self, key: str, default_value: T, query_data: dict) -> T:
    #     config = self.eval(query_data)
    #     value = self._get_nested(config, key)
    #     return self._to_object(value, default_value)

    # def get_all(self, query_data: dict) -> dict:
    #     return self.eval(query_data)


    # Internal helpers
    def get_applicable_variant(self, context: Dict[str, Any], toss: Optional[int] = None) -> list[str]:
        """
        Get applicable variant for the given context.
        
        Args:
            context: Dictionary containing context data
            toss: Optional integer to determine variant selection
            
        Returns:
            List of applicable variants
        """
        if self.cached_experiments is None:
            raise ValueError("No experiments available. Please fetch experiments first.")
        
        # Filter experiments based on context and toss
        
        applicable_variants = ffi_get_applicable_variants(self.cached_experiments, context, toss)

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
    def _to_float(self, value: Any, default: int) -> int:
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
            self.cached_experiments = None
            self.last_updated = None
            
            logger.info("ConfigurationClient closed successfully")
            
        except Exception as e:
            logger.error(f"Error during ConfigurationClient cleanup: {e}")
            raise
