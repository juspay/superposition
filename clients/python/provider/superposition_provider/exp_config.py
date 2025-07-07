import json
import logging
from decimal import Decimal
from typing import Any, Dict, Optional, TypeVar, Generic
from superposition_bindings.superposition_client import FfiExperiment
from .types import OnDemandStrategy, PollingStrategy, SuperpositionOptions, ConfigurationOptions, ExperimentationOptions
from superposition_sdk.client import Superposition, Config, ListExperimentInput
import asyncio
from datetime import datetime, timedelta
from superposition_bindings.superposition_types import Variant, VariantType

T = TypeVar("T")
logger = logging.getLogger(__name__)
for name in logging.root.manager.loggerDict:
    if not name.startswith("python"):  # e.g., "my_project"
        logging.getLogger(name).setLevel(logging.INFO)

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


class ExperimentationConfig():
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

        latest_exp_list = await self._get_experiments(self.superposition_options)
        logger.info("response from experimentation: " + str(latest_exp_list))
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
            
           

            list_exp_input = ListExperimentInput(
                workspace_id=superposition_options.workspace_id,
                org_id=superposition_options.org_id,
                all=True
            )
            

            response = await client.list_experiment(list_exp_input)
           
            exp_list = response.data
            logger.info(f"Fetched {len(exp_list)} experiments from Superposition")
            trimmed_exp_list = []
            for exp in exp_list:
                condition = {}
                for key, value in exp.context.items():
                    condition[key] = json.dumps(document_to_python_value(value))
                
                variants = []
                
                for variant in exp.variants:
                    variant_type = VariantType.CONTROL if variant.variant_type == "CONTROL" else VariantType.EXPERIMENTAL
                    override =  document_to_python_value(variant.overrides)
                    overrides = {}
                    if isinstance(override, dict):
                        overrides = {k: json.dumps(v) for k, v in override.items()}
                    variants.append(
                        Variant(
                            id=variant.id,
                            variant_type=variant_type,
                            context_id=variant.context_id,
                            override_id=variant.override_id,
                            overrides = overrides
                        )
                    )
                 
                    
                trimmed_exp = FfiExperiment(
                    id=exp.id,
                    context=condition,
                    variants=variants,
                    traffic_percentage=exp.traffic_percentage,
                )


                trimmed_exp_list.append(trimmed_exp)
                
            
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


