import asyncio
import logging
import json
from typing import Dict, Any, Optional
from .cac_config import CacConfig
from .exp_config import ExperimentationConfig
from .types import SuperpositionOptions, ConfigurationOptions, ExperimentationOptions
from superposition_bindings.superposition_client import ExperimentationArgs, ffi_eval_config, ffi_get_applicable_variants
from superposition_bindings.superposition_client import MergeStrategy
logger = logging.getLogger(__name__)


class ConfigurationClient:
    def __init__(self, superposition_options: SuperpositionOptions, 
                 cac_options: ConfigurationOptions, 
                 exp_options: Optional[ExperimentationOptions] = None):
        self.superposition_options = superposition_options
        self.cac_options = cac_options
        self.exp_options = exp_options
        
        # Initialize the CAC client
        self.cac_config = CacConfig(
            superposition_options=superposition_options,
            options=cac_options
        )
        
        # Initialize the experimentation client if options are provided
        self.exp_config = None
        if exp_options:
            self.exp_config = ExperimentationConfig(
                superposition_options=superposition_options,
                experiment_options=exp_options
            )
        
        self._polling_task = None
        self.cached_config = None
        self.last_updated = None

    async def create_config(self):
        """Initialize both CAC and experimentation clients"""
        logger.info("Creating SuperpositionClient configuration...")
        
        # Initialize CAC client
        await self.cac_config.create_config()
        
        # Initialize experimentation client if available
        if self.exp_config:
            logger.info("Creating ExperimentationClient configuration...")
            await self.exp_config.create_config()
            
        logger.info("SuperpositionClient configuration created successfully")

    def start_polling(self):
        """Start polling for both CAC and experimentation updates"""
        logger.info("Starting polling for SuperpositionClient...")
        
        # Start CAC polling
        if hasattr(self.cac_config, 'start_polling_update'):
            self.cac_config.start_polling_update()
        
        # Start experimentation polling
        if self.exp_config and hasattr(self.exp_config, 'start_polling_update'):
            self.exp_config.start_polling_update()

    def eval(self, query_data: dict, targeting_key: Optional[str]) -> dict[str, Any]:
        
        experimentdata = None
        if self.exp_config:
            experimentdata = ExperimentationArgs(
                experiments=self.exp_config.cached_experiments,
                targeting_key= targeting_key if targeting_key else "",
            )
        try:
            cache_key = self.cac_config._generate_cache_key(query_data)
            cached = self.cac_config._get_from_eval_cache(cache_key)

            if cached:
                logger.debug("Using cached evaluation result")
                return cached

            print(f"Evaluating configuration with query data: {query_data}")
            result = ffi_eval_config(
                self.cac_config.cached_config.get('default_configs', {}),
                self.cac_config.cached_config.get('contexts', []),
                self.cac_config.cached_config.get('overrides', {}),
                query_data,
                MergeStrategy.MERGE,
                filter_prefixes=None,
                experimentation=experimentdata
            )
            print(f"Evaluation result: {result}")
            eval_result = {}
            for (key, value) in result.items():
                eval_result[key] = json.loads(value)
            logger.info(f"Resolution result: {eval_result}")

            return eval_result

        except Exception as e:
            logger.error(f"Evaluation failed: {e}")
            raise
    

    def get_applicable_variants(self, context: Dict[str, str], toss: Optional[int] = None) -> list:
        if self.exp_config:
            return self.exp_config.get_applicable_variants(context, toss)
        else:
            return []

    def get_all_config_value(self,  default_value: Any, context: Dict[str, str],  targeting_key: Optional[str]) -> Any:
        """Get all configuration details for a flag"""
        try:
            return  self.eval(context, targeting_key)
        except Exception as e:
            logger.error(f"Error getting full config: {e}")
            return default_value

    def get_boolean_value(self, flag_key: str, default_value: bool, context: Dict[str, str], targeting_key: Optional[str]) -> bool:
        """Get boolean value from CAC client"""

        try:
            config = self.eval(context, targeting_key)
            value = self._get_nested(config, flag_key)
            return self._to_boolean(value, default_value)
        except Exception as e:
            logger.error(f"Error getting boolean value for {flag_key}: {e}")
            return default_value
       

    def get_string_value(self, flag_key: str, default_value: str, context: Dict[str, str], targeting_key: Optional[str]) -> bool:
        """Get string value from CAC client"""

        try:
            config = self.eval(context, targeting_key)
            value = self._get_nested(config, flag_key)
            return self._to_string(value, default_value)
        except Exception as e:
            logger.error(f"Error getting string value for {flag_key}: {e}")
            return default_value
        

    def get_integer_value(self, flag_key: str, default_value: int, context: Dict[str, str], targeting_key: Optional[str]) -> bool:
        """Get integer value from CAC client"""
        try:
            config = self.eval(context, targeting_key)
            value = self._get_nested(config, flag_key)
            return self._to_integer(value, default_value)
        except Exception as e:
            logger.error(f"Error getting integer value for {flag_key}: {e}")
            return default_value
    
    def get_float_value(self, flag_key: str, default_value: float, context: Dict[str, str], targeting_key: Optional[str]) -> bool:
        """Get float value from CAC client"""
        try:
            config = self.eval(context, targeting_key)
            value = self._get_nested(config, flag_key)
            return self._to_float(value, default_value)
        except Exception as e:
            logger.error(f"Error getting float value for {flag_key}: {e}")
            return default_value



    def get_object_value(self, flag_key: str, default_value: object, context: Dict[str, str], targeting_key: Optional[str]) -> bool:
        """Get object value from CAC client"""
        try:
            config = self.eval(context, targeting_key)
            value = self._get_nested(config, flag_key)
            return self._to_object(value, default_value)
        except Exception as e:
            logger.error(f"Error getting object value for {flag_key}: {e}")
            return default_value

    # Experimentation Client methods

    def get_applicable_variant(self, context: Dict[str, Any], targeting_key: Optional[str] = None) -> list:
        """Get applicable variant from experimentation client"""
        if self.exp_config:
            experimentdata = ExperimentationArgs(
                experiments=self.exp_config.cached_experiments(),
                targeting_key= targeting_key if targeting_key else "",
            )
            return ffi_get_applicable_variants(experimentdata,context, prefix=None)
        return []

    def _clear_eval_cache(self):
        """Clear evaluation cache"""
        if hasattr(self.cac_config, '_clear_eval_cache'):
            self.cac_config._clear_eval_cache()
        self.cached_config = None
        self.last_updated = None

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
    def _to_float(self, value: Any, default: int) -> int:
        try:
            return float(value)
        except (TypeError, ValueError):
            return default
    def _to_object(self, value: Any, default: Any) -> Any:
        if isinstance(value, dict): return value
        return default
        
    async def close(self):
        """Close the SuperpositionClient and clean up resources"""
        logger.info("Closing SuperpositionClient...")
        
        try:
            # Cancel polling task if running
            if self._polling_task and not self._polling_task.done():
                logger.debug("Cancelling polling task...")
                self._polling_task.cancel()
                try:
                    await self._polling_task
                except asyncio.CancelledError:
                    logger.debug("Polling task cancelled successfully")

            # Close CAC client
            if self.cac_config and hasattr(self.cac_config, 'close'):
                if asyncio.iscoroutinefunction(self.cac_config.close):
                    await self.cac_config.close()
                else:
                    self.cac_config.close()

            # Close experimentation client
            if self.exp_config and hasattr(self.exp_config, 'free_client'):
                self.exp_config.free_client()

            # Clear cached data
            self._clear_eval_cache()
            
            logger.info("SuperpositionClient closed successfully")
            
        except Exception as e:
            logger.error(f"Error while closing SuperpositionClient: {e}")
            raise