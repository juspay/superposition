import asyncio
import logging
import json
import time
import weakref
from typing import Dict, Any, Optional
from .cac_config import CacConfig
from .exp_config import ExperimentationConfig
from .types import SuperpositionOptions, ConfigurationOptions, ExperimentationOptions
from superposition_bindings.superposition_client import (
    ExperimentationArgs, 
    ffi_get_applicable_variants,
    ProviderCache,
    MergeStrategy,
)
logger = logging.getLogger(__name__)


class ConfigurationClient:
    def __init__(self, superposition_options: SuperpositionOptions,
                 cac_options: ConfigurationOptions,
                 exp_options: Optional[ExperimentationOptions] = None):
        self.superposition_options = superposition_options
        self.cac_options = cac_options
        self.exp_options = exp_options

        try:
            self.cache = ProviderCache()
            logger.info("Created ProviderCache instance")
        except Exception as e:
            logger.error(f"Failed to create cache: {e}")
            self.cache = None

        weak_self = weakref.ref(self)

        def _weak_reinit_config_cache():
            s = weak_self()
            if s is not None:
                s._reinit_config_cache()

        def _weak_reinit_experiments_cache():
            s = weak_self()
            if s is not None:
                s._reinit_experiments_cache()

        self.cac_config = CacConfig(
            superposition_options=superposition_options,
            options=cac_options,
            on_config_change=_weak_reinit_config_cache
        )

        # Initialize the experimentation client if options are provided
        self.exp_config = None
        if exp_options:
            self.exp_config = ExperimentationConfig(
                superposition_options=superposition_options,
                experiment_options=exp_options,
                on_config_change=_weak_reinit_experiments_cache
            )

        self._polling_task = None
        self.cached_config = None
        self.last_updated = None
        self._cache_initialized = False  # Track if Rust cache has been initialized

    async def create_config(self):
        logger.info("Creating SuperpositionClient configuration...")

        # Initialize CAC client
        await self.cac_config.create_config()

        # Initialize experimentation client if available
        if self.exp_config:
            logger.info("Creating ExperimentationClient configuration...")
            await self.exp_config.create_config()

        self._cache_initialized = True

        logger.info("SuperpositionClient configuration created successfully")

    def _reinit_config_cache(self):
        """Callback when config data changes - reinitialize config cache only"""
        if not self.cache:
            logger.error("Cache instance not initialized, cannot reinitialize config")
            return

        config = self.cac_config.cached_config
        if config:
            try:
                self.cache.init_config(
                    config.get('default_configs', {}),
                    config.get('contexts', []),
                    config.get('overrides', {}),
                    config.get('dimensions', {})
                )
                logger.debug("Config cache initialized")
            except Exception as e:
                logger.error(f"Failed to initialize config cache: {e}", exc_info=True)

    def _reinit_experiments_cache(self):
        """Callback when experiments data changes - reinitialize experiments cache only"""
        if not self.cache:
            logger.error("Cache instance not initialized, cannot reinitialize experiments")
            return

        if self.exp_config:
            try:
                experimentdata = ExperimentationArgs(
                    experiments=self.exp_config.cached_experiments or [],
                    experiment_groups=self.exp_config.cached_experiment_groups or [],
                    targeting_key="",
                )
                self.cache.init_experiments(experimentdata)
                logger.debug("Experiments cache initialized")
            except Exception as e:
                logger.error(f"Failed to initialize experiments cache: {e}", exc_info=True)


    def start_polling(self):
        """Start polling for both CAC and experimentation updates"""
        logger.info("Starting polling for SuperpositionClient...")

        # Start CAC polling
        if hasattr(self.cac_config, 'start_polling_update'):
            self.cac_config.start_polling_update()

        # Start experimentation polling
        if self.exp_config and hasattr(self.exp_config, 'start_polling_update'):
            self.exp_config.start_polling_update()


    def eval(self, query_data: dict, targeting_key: Optional[str] = None) -> dict[str, Any]:
        if not self._cache_initialized:
            logger.warning("Attempted to evaluate config before cache initialization. Returning empty config.")
            return {}

        if not self.cache:
            logger.error("Provider cache not initialized")
            return {}
        
        try:
            cache_ref = id(self.cache)
            logger.debug(f"Evaluating config with cache {cache_ref}")
            
            cache_key = self.cac_config._generate_cache_key(query_data)
            cached = self.cac_config._get_from_eval_cache(cache_key)
            if cached:
                logger.debug(f"Using cached evaluation result (cache {cache_ref})")
                return cached

            # Use instance method
            result = self.cache.eval_config(
                query_data,
                MergeStrategy.MERGE,
                None, 
                targeting_key
            )

            eval_result = {}
            for (key, value) in result.items():
                eval_result[key] = json.loads(value)
            logger.debug(f"Resolution result for cache {cache_ref}: {list(eval_result.keys())[:3]}...")

            return eval_result

        except Exception as e:
            logger.error(f"Evaluation failed: {e}")
            raise


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


    def get_string_value(self, flag_key: str, default_value: str, context: Dict[str, str], targeting_key: Optional[str]) -> str:
        """Get string value from CAC client"""

        try:
            config = self.eval(context, targeting_key)
            value = self._get_nested(config, flag_key)
            return self._to_string(value, default_value)
        except Exception as e:
            logger.error(f"Error getting string value for {flag_key}: {e}")
            return default_value


    def get_integer_value(self, flag_key: str, default_value: int, context: Dict[str, str], targeting_key: Optional[str]) -> int:
        """Get integer value from CAC client"""
        try:
            config = self.eval(context, targeting_key)
            value = self._get_nested(config, flag_key)
            return self._to_integer(value, default_value)
        except Exception as e:
            logger.error(f"Error getting integer value for {flag_key}: {e}")
            return default_value

    def get_float_value(self, flag_key: str, default_value: float, context: Dict[str, str], targeting_key: Optional[str]) -> float:
        """Get float value from CAC client"""
        try:
            config = self.eval(context, targeting_key)
            value = self._get_nested(config, flag_key)
            return self._to_float(value, default_value)
        except Exception as e:
            logger.error(f"Error getting float value for {flag_key}: {e}")
            return default_value



    def get_object_value(self, flag_key: str, default_value: object, context: Dict[str, str], targeting_key: Optional[str]) -> object:
        """Get object value from CAC client"""
        try:
            config = self.eval(context, targeting_key)
            value = self._get_nested(config, flag_key)
            return self._to_object(value, default_value)
        except Exception as e:
            logger.error(f"Error getting object value for {flag_key}: {e}")
            return default_value

    # Experimentation Client methods

    def get_applicable_variants(self, context: Dict[str, Any], targeting_key: Optional[str] = None) -> list:
        """Get applicable variant from experimentation client"""
        if self.exp_config:
            experimentdata = ExperimentationArgs(
                experiments=self.exp_config.cached_experiments or [],
                experiment_groups=self.exp_config.cached_experiment_groups or [],
                targeting_key=targeting_key if targeting_key else "",
            )
            return ffi_get_applicable_variants(experimentdata, self.cac_config.cached_config.get('dimensions', {}), context, prefix=None)
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
    def _to_float(self, value: Any, default: float) -> float:
        try:
            return float(value)
        except (TypeError, ValueError):
            return default
    def _to_object(self, value: Any, default: object) -> object:
        if value is not None:
            return value
        return default

    async def close(self):
        """Close the SuperpositionClient and clean up resources"""
        logger.info("Closing SuperpositionClient...")

        try:
            if self.cac_config:
                await self.cac_config.close()

            if self.exp_config:
                await self.exp_config.close()

            self._clear_eval_cache()
            self.cache = None

            logger.info("SuperpositionClient closed")

        except Exception as e:
            logger.error(f"Error while closing SuperpositionClient: {e}")
            raise
