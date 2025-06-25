from typing import Optional, Any, Dict, List
from openfeature.provider import AbstractProvider, Metadata as ProviderMetadata, ProviderStatus
from openfeature.evaluation_context import EvaluationContext
from openfeature.flag_evaluation import FlagResolutionDetails
from openfeature.hook import Hook
# from openfeature.eventing import EventHandler, ProviderEvent
# from openfeature import ProviderEvent, EventType
import asyncio
import json
import logging

# Placeholder imports (implement these yourself)
from .cac_client import ConfigurationClient
from .types import SuperpositionOptions, SuperpositionProviderOptions, ConfigurationOptions

logger = logging.getLogger(__name__)



class SuperpositionProvider(AbstractProvider):
    def __init__(self, provider_options: SuperpositionProviderOptions):
        self.metadata = ProviderMetadata(name="SuperpositionProvider")
        self.status = ProviderStatus.NOT_READY
        self.hooks: List[Hook] = []
        # self.events = EventHandler()
        self.options = provider_options
        self.client = None
            
        # )
    async def initialize(self, context: Optional[EvaluationContext] = None):
        try:
            self.status = ProviderStatus.NOT_READY

            # Timeout handling
            try:
                self.client = ConfigurationClient(
                    superposition_options=SuperpositionOptions(
                        endpoint=self.options.endpoint,
                        token=self.options.token,
                        org_id=self.options.org_id,
                        workspace_id=self.options.workspace_id
                    ),
                    options = ConfigurationOptions(
                        refresh_strategy=self.options.refresh_strategy,
                        fallback_config=self.options.fallback_config,
                        evaluation_cache_options=self.options.evaluation_cache_options
                    ),
                )
                await self.client.create_config()
                # asyncio.wait_for(self._do_initialize(context), timeout=self.config["initOpts"]["timeout"] / 1000)
            except asyncio.TimeoutError:
                raise RuntimeError("Provider initialization timed out")

            self.status = ProviderStatus.READY
        except Exception as e:
            self.status = ProviderStatus.ERROR
            raise
    def _do_initialize(self, context: Optional[EvaluationContext]):
        self.cached_config = self.client.eval(context or EvaluationContext({}))
    
    async def shutdown(self):
        """
        Shutdown the provider and clean up all resources.
        This includes stopping polling tasks, clearing caches, and closing connections.
        """
        logger.info("Shutting down SuperpositionProvider...")
        
        try:
            # Stop polling tasks if running
            if self.client and hasattr(self.client, '_polling_task') and self.client._polling_task:
                logger.debug("Cancelling polling task...")
                self.client._polling_task.cancel()
                try:
                    await self.client._polling_task
                except asyncio.CancelledError:
                    logger.debug("Polling task cancelled successfully")
                except Exception as e:
                    logger.warning(f"Error while cancelling polling task: {e}")
            
            # Clear evaluation cache if it exists
            if self.client and hasattr(self.client, '_clear_eval_cache'):
                logger.debug("Clearing evaluation cache...")
                self.client._clear_eval_cache()
            
            # Reset cached configuration
            if self.client and hasattr(self.client, 'cached_config'):
                self.client.cached_config = None
                self.client.last_updated = None
            
            # Close the client if it has a close method
            if self.client and hasattr(self.client, 'close'):
                logger.debug("Closing client connection...")
                if asyncio.iscoroutinefunction(self.client.close):
                    await self.client.close()
                else:
                    self.client.close()
            
            # Reset client reference
            self.client = None
            
            # Update provider status
            self.status = ProviderStatus.NOT_READY
            
            # Clear hooks if any
            self.hooks.clear()
            
            logger.info("SuperpositionProvider shutdown completed successfully")
            
        except Exception as e:
            logger.error(f"Error during provider shutdown: {e}")
            # Even if there's an error, ensure we're in a clean state
            self.client = None
            self.status = ProviderStatus.FATAL
            raise

    def resolve_boolean_details(self, flag_key: str, default_value: bool, evaluation_context: EvaluationContext) -> FlagResolutionDetails[bool]:
        data = (evaluation_context.attributes or {}) if evaluation_context else {}
        val = self.client.get_boolean_value(flag_key, default_value, data)
        return FlagResolutionDetails(val)

    def resolve_string_details(self, flag_key: str, default_value: str, evaluation_context: EvaluationContext) -> FlagResolutionDetails[str]:
        data = (evaluation_context.attributes or {}) if evaluation_context else {}
        val = self.client.get_string_value(flag_key, default_value, data)
        return FlagResolutionDetails(val)

    def resolve_integer_details(self, flag_key: str, default_value: int, evaluation_context: EvaluationContext) -> FlagResolutionDetails[int]:
        data = (evaluation_context.attributes or {}) if evaluation_context else {}
        val = self.client.get_integer_value(flag_key, default_value, data)
        return FlagResolutionDetails(val)

    def resolve_float_details(self, flag_key: str, default_value: float, evaluation_context: EvaluationContext) -> FlagResolutionDetails[float]:
        data = (evaluation_context.attributes or {}) if evaluation_context else {}
        val = self.client.get_float_value(flag_key, default_value, data)
        return FlagResolutionDetails(val)

    def resolve_object_details(self, flag_key: str, default_value: Any, evaluation_context: EvaluationContext) -> FlagResolutionDetails[Any]:
        data = (evaluation_context.attributes or {}) if evaluation_context else {}
        val = self.client.get_object_value(flag_key, default_value, data)
        return FlagResolutionDetails(val)

    def get_metadata(self) -> ProviderMetadata:
        return self.metadata

    def get_status(self) -> ProviderStatus:
        return self.status
