import asyncio
import logging
from typing import Dict, Any, Optional
from .cac_client import ConfigurationClient
from .exp_client import ExperimentationClient
from .types import SuperpositionOptions, SuperpositionProviderOptions, ConfigurationOptions, ExperimentationOptions

logger = logging.getLogger(__name__)


class SuperpositionClient:
    def __init__(self, superposition_options: SuperpositionOptions, 
                 cac_options: ConfigurationOptions, 
                 exp_options: Optional[ExperimentationOptions] = None):
        self.superposition_options = superposition_options
        self.cac_options = cac_options
        self.exp_options = exp_options
        
        # Initialize the CAC client
        self.cac_client = ConfigurationClient(
            superposition_options=superposition_options,
            options=cac_options
        )
        
        # Initialize the experimentation client if options are provided
        self.exp_client = None
        if exp_options:
            self.exp_client = ExperimentationClient(
                superposition_options=superposition_options,
                options=exp_options
            )
        
        self._polling_task = None
        self.cached_config = None
        self.last_updated = None

    async def create_config(self):
        """Initialize both CAC and experimentation clients"""
        logger.info("Creating SuperpositionClient configuration...")
        
        # Initialize CAC client
        await self.cac_client.create_config()
        
        # Initialize experimentation client if available
        if self.exp_client:
            await self.exp_client.create_client()
            
        logger.info("SuperpositionClient configuration created successfully")

    def start_polling(self):
        """Start polling for both CAC and experimentation updates"""
        logger.info("Starting polling for SuperpositionClient...")
        
        # Start CAC polling
        if hasattr(self.cac_client, 'start_polling_update'):
            self.cac_client.start_polling_update()
        
        # Start experimentation polling
        if self.exp_client and hasattr(self.exp_client, 'start_polling_update'):
            self.exp_client.start_polling_update()

    def eval(self, context: Dict[str, Any]) -> Dict[str, Any]:
        """Evaluate configuration with the given context"""
        if self.cac_client:
            return self.cac_client.eval(context)
        return {}

    # CAC Client methods
    def get_boolean_value(self, flag_key: str, default_value: bool, context: Dict[str, str]) -> bool:
        """Get boolean value from CAC client"""
        if self.cac_client:
            return self.cac_client.get_boolean_value(flag_key, default_value, context)
        return default_value

    def get_string_value(self, flag_key: str, default_value: str, context: Dict[str, str]) -> str:
        """Get string value from CAC client"""
        if self.cac_client:
            return self.cac_client.get_string_value(flag_key, default_value, context)
        return default_value

    def get_integer_value(self, flag_key: str, default_value: int, context: Dict[str, str]) -> int:
        """Get integer value from CAC client"""
        if self.cac_client:
            return self.cac_client.get_integer_value(flag_key, default_value, context)
        return default_value

    def get_float_value(self, flag_key: str, default_value: float, context: Dict[str, str]) -> float:
        """Get float value from CAC client"""
        if self.cac_client:
            return self.cac_client.get_float_value(flag_key, default_value, context)
        return default_value

    def get_object_value(self, flag_key: str, default_value: Any, context: Dict[str, str]) -> Any:
        """Get object value from CAC client"""
        if self.cac_client:
            return self.cac_client.get_object_value(flag_key, default_value, context)
        return default_value

    # Experimentation Client methods
    def get_running_experiments(self) -> list:
        """Get running experiments from experimentation client"""
        if self.exp_client:
            return self.exp_client.get_running_experiments()
        return []

    def get_satisfied_experiments(self, context: Dict[str, Any], filter_prefix: Optional[list] = None) -> list:
        """Get satisfied experiments from experimentation client"""
        if self.exp_client:
            return self.exp_client.get_satisfied_experiments(context, filter_prefix)
        return []

    def get_applicable_variant(self, context: Dict[str, Any], toss: Optional[int] = None) -> list:
        """Get applicable variant from experimentation client"""
        if self.exp_client:
            return self.exp_client.get_applicable_variant(context, toss)
        return []

    def _clear_eval_cache(self):
        """Clear evaluation cache"""
        if hasattr(self.cac_client, '_clear_eval_cache'):
            self.cac_client._clear_eval_cache()
        self.cached_config = None
        self.last_updated = None

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
            if self.cac_client and hasattr(self.cac_client, 'close'):
                if asyncio.iscoroutinefunction(self.cac_client.close):
                    await self.cac_client.close()
                else:
                    self.cac_client.close()

            # Close experimentation client
            if self.exp_client and hasattr(self.exp_client, 'free_client'):
                self.exp_client.free_client()

            # Clear cached data
            self._clear_eval_cache()
            
            logger.info("SuperpositionClient closed successfully")
            
        except Exception as e:
            logger.error(f"Error while closing SuperpositionClient: {e}")
            raise