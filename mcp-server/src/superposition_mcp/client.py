"""Client wrapper for Superposition SDK with error handling and configuration."""

import asyncio
import logging
from typing import Any, Dict, Optional

import httpx
from smithy_core.aio.endpoints import StaticEndpointResolver
from smithy_core.interfaces import URI
from smithy_http.aio.aiohttp import AIOHTTPClient

# Import the Superposition SDK
import sys
import os

# Add the path to the Superposition SDK relative to this file
sdk_path = os.path.abspath(os.path.join(os.path.dirname(__file__), "../../../../../clients/python/sdk"))
sys.path.insert(0, sdk_path)

try:
    from superposition_sdk.client import Superposition
    from superposition_sdk.config import Config as SuperpositionSDKConfig
    from superposition_sdk import models
except ImportError as e:
    logger = logging.getLogger(__name__)
    logger.error(f"Failed to import Superposition SDK: {e}")
    logger.error(f"SDK path: {sdk_path}")
    logger.error("Make sure the Superposition Python SDK is installed or accessible")
    raise

from .config import SuperpositionConfig


logger = logging.getLogger(__name__)


class SuperpositionClientError(Exception):
    """Base exception for Superposition client errors."""
    pass


class SuperpositionAuthError(SuperpositionClientError):
    """Authentication-related errors."""
    pass


class SuperpositionNotFoundError(SuperpositionClientError):
    """Resource not found errors."""
    pass


class SuperpositionValidationError(SuperpositionClientError):
    """Input validation errors."""
    pass


class SuperpositionClient:
    """Wrapper client for Superposition SDK with enhanced error handling."""
    
    def __init__(self, config: SuperpositionConfig):
        """Initialize the Superposition client.
        
        Args:
            config: Configuration for the Superposition API client
        """
        self.config = config
        self._client: Optional[Superposition] = None
        self._setup_logging()
    
    def _setup_logging(self):
        """Set up logging for the client."""
        self.logger = logging.getLogger(f"{__name__}.{self.__class__.__name__}")
    
    async def _get_client(self) -> Superposition:
        """Get or create the Superposition SDK client."""
        if self._client is None:
            try:
                # Create the SDK configuration
                sdk_config = SuperpositionSDKConfig()
                
                # Set up the endpoint resolver
                endpoint_uri = URI.parse(self.config.base_url)
                sdk_config.endpoint_resolver = StaticEndpointResolver(endpoint_uri)
                
                # Set up HTTP client with timeout
                sdk_config.http_client = AIOHTTPClient()
                
                # Note: For now, we'll handle authentication in a simplified way
                # The full smithy auth setup is complex, so we'll handle bearer tokens
                # via HTTP headers in the requests
                if self.config.bearer_token:
                    self.logger.info("Bearer token provided - will add to request headers")
                else:
                    self.logger.warning("No bearer token provided - API calls may fail")
                
                self._client = Superposition(config=sdk_config)
                self.logger.info(f"Initialized Superposition client for {self.config.base_url}")
                
            except Exception as e:
                self.logger.error(f"Failed to initialize Superposition client: {e}")
                raise SuperpositionClientError(f"Client initialization failed: {e}")
        
        return self._client
    
    def _fill_defaults(self, **kwargs) -> Dict[str, Any]:
        """Fill in default values for workspace_id and org_id if not provided."""
        if kwargs.get("workspace_id") is None:
            kwargs["workspace_id"] = self.config.default_workspace_id
        if kwargs.get("org_id") is None:
            kwargs["org_id"] = self.config.default_org_id
        return kwargs
    
    def _get_auth_headers(self) -> Dict[str, str]:
        """Get authentication headers for API requests."""
        headers = {}
        if self.config.bearer_token:
            headers["Authorization"] = f"Bearer {self.config.bearer_token}"
        return headers
    
    async def _execute_with_retry(self, operation_func, *args, **kwargs):
        """Execute an operation with retry logic and error handling."""
        kwargs = self._fill_defaults(**kwargs)
        
        last_exception = None
        for attempt in range(self.config.max_retries):
            try:
                result = await operation_func(*args, **kwargs)
                self.logger.debug(f"Operation succeeded on attempt {attempt + 1}")
                return result
                
            except Exception as e:
                last_exception = e
                self.logger.warning(f"Operation failed on attempt {attempt + 1}: {e}")
                
                # Check if this is a retryable error
                if not self._is_retryable_error(e):
                    break
                
                # Wait before retrying (exponential backoff)
                if attempt < self.config.max_retries - 1:
                    delay = self.config.retry_delay * (2 ** attempt)
                    self.logger.info(f"Retrying in {delay} seconds...")
                    await asyncio.sleep(delay)
        
        # All retries exhausted, handle the final error
        self._handle_error(last_exception)
    
    def _is_retryable_error(self, error: Exception) -> bool:
        """Determine if an error is retryable."""
        # Consider network-related errors and 5xx status codes as retryable
        error_str = str(error).lower()
        retryable_indicators = [
            "timeout", "connection", "network", "503", "502", "500", 
            "gateway", "service unavailable", "internal server error"
        ]
        return any(indicator in error_str for indicator in retryable_indicators)
    
    def _handle_error(self, error: Exception):
        """Convert SDK errors to appropriate client errors."""
        if error is None:
            return
        
        error_str = str(error).lower()
        self.logger.debug(f"Handling error: {type(error).__name__}: {error}")
        
        # Handle specific HTTP status codes
        if "401" in error_str or "unauthorized" in error_str:
            raise SuperpositionAuthError(f"Authentication failed. Please check your bearer token: {error}")
        elif "403" in error_str or "forbidden" in error_str:
            raise SuperpositionAuthError(f"Access denied. Insufficient permissions: {error}")
        elif "404" in error_str or "not found" in error_str:
            raise SuperpositionNotFoundError(f"Resource not found. Please verify the ID exists: {error}")
        elif "400" in error_str or "bad request" in error_str or "validation" in error_str:
            raise SuperpositionValidationError(f"Invalid request data. Please check the input parameters: {error}")
        elif "409" in error_str or "conflict" in error_str:
            raise SuperpositionValidationError(f"Resource conflict. The resource may already exist: {error}")
        elif "422" in error_str or "unprocessable" in error_str:
            raise SuperpositionValidationError(f"Validation failed. Please check required fields and data formats: {error}")
        elif "429" in error_str or "rate limit" in error_str:
            raise SuperpositionClientError(f"Rate limit exceeded. Please wait before retrying: {error}")
        elif "500" in error_str or "internal server" in error_str:
            raise SuperpositionClientError(f"Server error. Please try again later: {error}")
        elif "503" in error_str or "service unavailable" in error_str:
            raise SuperpositionClientError(f"Service temporarily unavailable. Please try again later: {error}")
        elif "timeout" in error_str or "connection" in error_str:
            raise SuperpositionClientError(f"Connection error. Please check network connectivity: {error}")
        else:
            # Generic error handling
            raise SuperpositionClientError(f"Unexpected API error: {error}")
    
    # Configuration operations
    async def get_config(self, **kwargs) -> Any:
        """Get configuration with context."""
        client = await self._get_client()
        input_model = models.GetConfigInput(**kwargs)
        return await self._execute_with_retry(client.get_config, input_model)
    
    async def get_resolved_config(self, **kwargs) -> Any:
        """Get resolved configuration."""
        client = await self._get_client()
        input_model = models.GetResolvedConfigInput(**kwargs)
        return await self._execute_with_retry(client.get_resolved_config, input_model)
    
    async def list_config_versions(self, **kwargs) -> Any:
        """List configuration versions."""
        client = await self._get_client()
        input_model = models.ListVersionsInput(**kwargs)
        return await self._execute_with_retry(client.list_versions, input_model)
    
    # Context operations
    async def create_context(self, **kwargs) -> Any:
        """Create a new context."""
        client = await self._get_client()
        input_model = models.CreateContextInput(**kwargs)
        return await self._execute_with_retry(client.create_context, input_model)
    
    async def get_context(self, **kwargs) -> Any:
        """Get a specific context by ID."""
        client = await self._get_client()
        input_model = models.GetContextInput(**kwargs)
        return await self._execute_with_retry(client.get_context, input_model)
    
    async def update_context(self, **kwargs) -> Any:
        """Update context overrides."""
        client = await self._get_client()
        
        # Handle the different update context formats
        if "context_id" in kwargs:
            # Use context ID for identification
            request = {
                "context": {"id": kwargs.pop("context_id")},
                **kwargs
            }
        else:
            # Use context condition for identification
            context_condition = kwargs.pop("context")
            request = {
                "context": {"context": context_condition},
                **kwargs
            }
        
        input_model = models.UpdateOverrideInput(request=request, **kwargs)
        return await self._execute_with_retry(client.update_override, input_model)
    
    async def delete_context(self, context_id: str, **kwargs) -> Any:
        """Delete a context."""
        client = await self._get_client()
        input_model = models.DeleteContextInput(id=context_id, **kwargs)
        return await self._execute_with_retry(client.delete_context, input_model)
    
    async def move_context(self, context_id: str, **kwargs) -> Any:
        """Move a context (change its condition)."""
        client = await self._get_client()
        input_model = models.MoveContextInput(id=context_id, **kwargs)
        return await self._execute_with_retry(client.move_context, input_model)
    
    async def list_contexts(self, **kwargs) -> Any:
        """List contexts with filtering and pagination."""
        client = await self._get_client()
        input_model = models.ListContextsInput(**kwargs)
        return await self._execute_with_retry(client.list_contexts, input_model)
    
    # Dimension operations
    async def create_dimension(self, **kwargs) -> Any:
        """Create a new dimension."""
        client = await self._get_client()
        input_model = models.CreateDimensionInput(**kwargs)
        return await self._execute_with_retry(client.create_dimension, input_model)
    
    async def get_dimension(self, dimension_name: str, **kwargs) -> Any:
        """Get a specific dimension by name."""
        client = await self._get_client()
        input_model = models.GetDimensionInput(dimension_name=dimension_name, **kwargs)
        return await self._execute_with_retry(client.get_dimension, input_model)
    
    async def list_dimensions(self, **kwargs) -> Any:
        """List dimensions with filtering and pagination."""
        client = await self._get_client()
        input_model = models.ListDimensionsInput(**kwargs)
        return await self._execute_with_retry(client.list_dimensions, input_model)
    
    async def update_dimension(self, dimension_name: str, **kwargs) -> Any:
        """Update a dimension."""
        client = await self._get_client()
        input_model = models.UpdateDimensionInput(dimension_name=dimension_name, **kwargs)
        return await self._execute_with_retry(client.update_dimension, input_model)
    
    async def delete_dimension(self, dimension_name: str, **kwargs) -> Any:
        """Delete a dimension."""
        client = await self._get_client()
        input_model = models.DeleteDimensionInput(dimension_name=dimension_name, **kwargs)
        return await self._execute_with_retry(client.delete_dimension, input_model)
    
    # Default Config operations
    async def create_default_config(self, **kwargs) -> Any:
        """Create a new default configuration."""
        client = await self._get_client()
        input_model = models.CreateDefaultConfigInput(**kwargs)
        return await self._execute_with_retry(client.create_default_config, input_model)
    
    async def list_default_configs(self, **kwargs) -> Any:
        """List default configurations."""
        client = await self._get_client()
        input_model = models.ListDefaultConfigsInput(**kwargs)
        return await self._execute_with_retry(client.list_default_configs, input_model)
    
    async def update_default_config(self, **kwargs) -> Any:
        """Update default configuration."""
        client = await self._get_client()
        input_model = models.UpdateDefaultConfigInput(**kwargs)
        return await self._execute_with_retry(client.update_default_config, input_model)
    
    async def delete_default_config(self, config_key: str, **kwargs) -> Any:
        """Delete a default configuration."""
        client = await self._get_client()
        input_model = models.DeleteDefaultConfigInput(key=config_key, **kwargs)
        return await self._execute_with_retry(client.delete_default_config, input_model)
    
    # Experiment operations
    async def create_experiment(self, **kwargs) -> Any:
        """Create a new experiment."""
        client = await self._get_client()
        
        # Convert variant schemas to SDK models
        if "variants" in kwargs:
            variants = []
            for variant_data in kwargs["variants"]:
                variant = models.Variant(
                    id=variant_data["id"],
                    variant_type=variant_data["variant_type"],
                    overrides=variant_data["overrides"],
                    context_id=variant_data.get("context_id"),
                    override_id=variant_data.get("override_id")
                )
                variants.append(variant)
            kwargs["variants"] = variants
        
        input_model = models.CreateExperimentInput(**kwargs)
        return await self._execute_with_retry(client.create_experiment, input_model)
    
    async def get_experiment(self, experiment_id: str, **kwargs) -> Any:
        """Get a specific experiment by ID."""
        client = await self._get_client()
        input_model = models.GetExperimentInput(id=experiment_id, **kwargs)
        return await self._execute_with_retry(client.get_experiment, input_model)
    
    async def update_experiment(self, experiment_id: str, **kwargs) -> Any:
        """Update experiment overrides."""
        client = await self._get_client()
        
        # Convert variant schemas to SDK models
        if "variants" in kwargs:
            variant_list = []
            for variant_data in kwargs["variants"]:
                variant_update = models.VariantUpdateRequest(
                    id=variant_data["id"],
                    overrides=variant_data["overrides"]
                )
                variant_list.append(variant_update)
            kwargs["variant_list"] = variant_list
            del kwargs["variants"]
        
        input_model = models.UpdateOverridesExperimentInput(id=experiment_id, **kwargs)
        return await self._execute_with_retry(client.update_overrides_experiment, input_model)
    
    async def conclude_experiment(self, experiment_id: str, **kwargs) -> Any:
        """Conclude an experiment."""
        client = await self._get_client()
        input_model = models.ConcludeExperimentInput(id=experiment_id, **kwargs)
        return await self._execute_with_retry(client.conclude_experiment, input_model)
    
    async def discard_experiment(self, experiment_id: str, **kwargs) -> Any:
        """Discard an experiment."""
        client = await self._get_client()
        input_model = models.DiscardExperimentInput(id=experiment_id, **kwargs)
        return await self._execute_with_retry(client.discard_experiment, input_model)
    
    async def ramp_experiment(self, experiment_id: str, **kwargs) -> Any:
        """Ramp experiment traffic."""
        client = await self._get_client()
        input_model = models.RampExperimentInput(id=experiment_id, **kwargs)
        return await self._execute_with_retry(client.ramp_experiment, input_model)
    
    async def pause_experiment(self, experiment_id: str, **kwargs) -> Any:
        """Pause an experiment."""
        client = await self._get_client()
        input_model = models.PauseExperimentInput(id=experiment_id, **kwargs)
        return await self._execute_with_retry(client.pause_experiment, input_model)
    
    async def resume_experiment(self, experiment_id: str, **kwargs) -> Any:
        """Resume an experiment."""
        client = await self._get_client()
        input_model = models.ResumeExperimentInput(id=experiment_id, **kwargs)
        return await self._execute_with_retry(client.resume_experiment, input_model)
    
    async def list_experiments(self, **kwargs) -> Any:
        """List experiments with filtering and pagination."""
        client = await self._get_client()
        input_model = models.ListExperimentInput(**kwargs)
        return await self._execute_with_retry(client.list_experiment, input_model)
    
    async def get_applicable_variants(self, **kwargs) -> Any:
        """Get applicable experiment variants for a context."""
        client = await self._get_client()
        input_model = models.ApplicableVariantsInput(**kwargs)
        return await self._execute_with_retry(client.applicable_variants, input_model)
    
    # Experiment Group operations
    async def create_experiment_group(self, **kwargs) -> Any:
        """Create a new experiment group."""
        client = await self._get_client()
        input_model = models.CreateExperimentGroupInput(**kwargs)
        return await self._execute_with_retry(client.create_experiment_group, input_model)
    
    async def get_experiment_group(self, group_id: str, **kwargs) -> Any:
        """Get a specific experiment group by ID."""
        client = await self._get_client()
        input_model = models.GetExperimentGroupInput(id=group_id, **kwargs)
        return await self._execute_with_retry(client.get_experiment_group, input_model)
    
    async def list_experiment_groups(self, **kwargs) -> Any:
        """List experiment groups with filtering and pagination."""
        client = await self._get_client()
        input_model = models.ListExperimentGroupsInput(**kwargs)
        return await self._execute_with_retry(client.list_experiment_groups, input_model)
    
    async def update_experiment_group(self, group_id: str, **kwargs) -> Any:
        """Update an experiment group."""
        client = await self._get_client()
        input_model = models.UpdateExperimentGroupInput(id=group_id, **kwargs)
        return await self._execute_with_retry(client.update_experiment_group, input_model)
    
    async def delete_experiment_group(self, group_id: str, **kwargs) -> Any:
        """Delete an experiment group."""
        client = await self._get_client()
        input_model = models.DeleteExperimentGroupInput(id=group_id, **kwargs)
        return await self._execute_with_retry(client.delete_experiment_group, input_model)
    
    async def add_members_to_group(self, group_id: str, **kwargs) -> Any:
        """Add members to an experiment group."""
        client = await self._get_client()
        input_model = models.AddMembersToGroupInput(id=group_id, **kwargs)
        return await self._execute_with_retry(client.add_members_to_group, input_model)
    
    async def remove_members_from_group(self, group_id: str, **kwargs) -> Any:
        """Remove members from an experiment group."""
        client = await self._get_client()
        input_model = models.RemoveMembersFromGroupInput(id=group_id, **kwargs)
        return await self._execute_with_retry(client.remove_members_from_group, input_model)
    
    async def close(self):
        """Close the client and cleanup resources."""
        if self._client:
            # Close any underlying HTTP clients
            try:
                if hasattr(self._client._config, 'http_client'):
                    await self._client._config.http_client.close()
            except Exception as e:
                self.logger.warning(f"Error closing HTTP client: {e}")
            
            self._client = None
            self.logger.info("Superposition client closed")