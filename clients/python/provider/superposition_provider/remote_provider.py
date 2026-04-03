"""
SuperpositionAPIProvider - Direct remote evaluation without caching.

Uses the Superposition SDK directly for every flag evaluation,
with no local state or caching.
"""

import logging
from typing import Dict, List, Optional, Any, Tuple, Union, Sequence, Mapping

from openfeature.provider import (
    AbstractProvider,
    Metadata,
    ProviderStatus,
)
from openfeature.evaluation_context import EvaluationContext
from openfeature.flag_evaluation import FlagResolutionDetails

from smithy_core.documents import Document

from superposition_sdk.client import Superposition
from superposition_sdk.config import Config as SdkConfig
from superposition_sdk.auth_helpers import bearer_auth_config
from superposition_sdk.models import ApplicableVariantsInput, GetResolvedConfigWithIdentifierInput

from .conversions import document_to_python_value
from .interfaces import AllFeatureProvider, FeatureExperimentMeta
from .types import SuperpositionOptions

logger = logging.getLogger(__name__)


class SuperpositionAPIProvider(AbstractProvider, AllFeatureProvider, FeatureExperimentMeta):
    """Direct remote OpenFeature provider with no local caching.

    Every flag evaluation goes directly to the Superposition API.
    This provider is suitable for serverless and stateless deployments.
    """

    def __init__(self, options: SuperpositionOptions):
        """Initialize the remote provider.

        Args:
            options: Superposition configuration options.
        """
        self.options = options
        self.metadata = Metadata(name="SuperpositionAPIProvider")
        self.status = ProviderStatus.NOT_READY
        self.global_context = EvaluationContext()
        self.client = self._create_client()

    def _create_client(self) -> Superposition:
        """Create and configure the SDK client."""
        (resolver, schemes) = bearer_auth_config(
            token=self.options.token
        )
        sdk_config = SdkConfig(
            endpoint_uri=self.options.endpoint,
            http_auth_scheme_resolver=resolver,
            http_auth_schemes=schemes
        )

        # Create Superposition client
        return Superposition(config=sdk_config)

    def initialize(self, evaluation_context: EvaluationContext):
        """Initialize the provider.

        Args:
            evaluation_context: Global evaluation context to apply to all resolutions.
        """
        try:
            logger.info("Initializing SuperpositionAPIProvider...")
            self.global_context = evaluation_context
            self.status = ProviderStatus.READY
            logger.info("SuperpositionAPIProvider initialized successfully")
        except Exception as e:
            logger.error(f"Failed to initialize SuperpositionAPIProvider: {e}")
            self.status = ProviderStatus.FATAL
            raise

    def shutdown(self):
        """Shutdown the provider."""
        logger.info("Shutting down SuperpositionAPIProvider...")
        if self.client:
            self.client = None
        self.status = ProviderStatus.NOT_READY
        logger.info("SuperpositionAPIProvider shutdown completed")

    def get_metadata(self) -> Metadata:
        """Get provider metadata."""
        return self.metadata

    def get_status(self) -> ProviderStatus:
        """Get provider status."""
        return self.status

    def resolve_all_features_with_filter(
        self,
        context: Optional[EvaluationContext] = None,
        prefix_filter: Optional[List[str]] = None,
    ) -> Dict[str, Any]:
        """Resolve all features with optional prefix filtering.

        Args:
            context: Evaluation context (optional, uses global context if not provided).
            prefix_filter: Optional list of key prefixes to include.

        Returns:
            Dictionary of filtered resolved flags.
        """
        raise NotImplementedError("Synchronous resolution is not implemented in SuperpositionAPIProvider. Use async functions instead.")

    # --- AllFeatureProvider implementation ---
    async def resolve_all_features_with_filter_async(
        self,
        context: Optional[EvaluationContext] = None,
        prefix_filter: Optional[List[str]] = None,
    ) -> Dict[str, Any]:
        """Resolve all features with optional prefix filtering.

        Args:
            context: Evaluation context (optional, uses global context if not provided).
            prefix_filter: Optional list of key prefixes to include.

        Returns:
            Dictionary of filtered resolved flags.
        """
        return await self._resolve_remote(context, prefix_filter)

    # --- FeatureExperimentMeta implementation ---

    async def get_applicable_variants(
        self,
        context: Optional[EvaluationContext] = None,
        prefix_filter: Optional[List[str]] = None,
    ) -> List[str]:
        """Get applicable experiment variants via remote API.

        Args:
            context: Evaluation context with targeting key (optional).
            prefix_filter: Optional list of variant ID prefixes.

        Returns:
            List of applicable variant IDs.
        """
        targeting_key, merged_context = self._get_merged_context(context)

        if not targeting_key:
            logger.warning("Missing targeting key for variant resolution")
            return []

        try:
            response = await self.client.applicable_variants(
                input=ApplicableVariantsInput(
                    workspace_id=self.options.workspace_id,
                    org_id=self.options.org_id,
                    identifier=targeting_key,
                    context=merged_context,
                    prefix=prefix_filter,
                )
            )

            # Extract variant IDs from response
            return [v.id for v in response.data] if response.data else []
        except Exception as e:
            logger.error(f"Failed to get applicable variants: {e}")
            return []

    # --- OpenFeature FeatureProvider methods ---

    def resolve_boolean_details(
        self,
        flag_key: str,
        default_value: bool,
        evaluation_context: Optional[EvaluationContext] = None,
    ) -> FlagResolutionDetails[bool]:
        """Resolve a boolean flag."""
        return self.resolve_bool(flag_key, default_value, evaluation_context)

    def resolve_string_details(
        self,
        flag_key: str,
        default_value: str,
        evaluation_context: Optional[EvaluationContext] = None,
    ) -> FlagResolutionDetails[str]:
        """Resolve a string flag."""
        return self.resolve_string(flag_key, default_value, evaluation_context)

    def resolve_integer_details(
        self,
        flag_key: str,
        default_value: int,
        evaluation_context: Optional[EvaluationContext] = None,
    ) -> FlagResolutionDetails[int]:
        """Resolve an integer flag."""
        return self.resolve_int(flag_key, default_value, evaluation_context)

    def resolve_float_details(
        self,
        flag_key: str,
        default_value: float,
        evaluation_context: Optional[EvaluationContext] = None,
    ) -> FlagResolutionDetails[float]:
        """Resolve a float flag."""
        return self.resolve_float(flag_key, default_value, evaluation_context)

    def resolve_object_details(
        self,
        flag_key: str,
        default_value: Union[Mapping, Sequence],
        evaluation_context: Optional[EvaluationContext] = None,
    ) -> FlagResolutionDetails[Union[Mapping, Sequence]]:
        """Resolve an object/struct flag."""
        return self.resolve_object(flag_key, default_value, evaluation_context)

    async def resolve_boolean_details_async(
        self,
        flag_key: str,
        default_value: bool,
        evaluation_context: Optional[EvaluationContext] = None,
    ) -> FlagResolutionDetails[bool]:
        """Resolve a boolean flag."""
        return await self.resolve_bool_async(flag_key, default_value, evaluation_context)

    async def resolve_string_details_async(
        self,
        flag_key: str,
        default_value: str,
        evaluation_context: Optional[EvaluationContext] = None,
    ) -> FlagResolutionDetails[str]:
        """Resolve a string flag."""
        return await self.resolve_string_async(flag_key, default_value, evaluation_context)

    async def resolve_integer_details_async(
        self,
        flag_key: str,
        default_value: int,
        evaluation_context: Optional[EvaluationContext] = None,
    ) -> FlagResolutionDetails[int]:
        """Resolve an integer flag."""
        return await self.resolve_int_async(flag_key, default_value, evaluation_context)

    async def resolve_float_details_async(
        self,
        flag_key: str,
        default_value: float,
        evaluation_context: Optional[EvaluationContext] = None,
    ) -> FlagResolutionDetails[float]:
        """Resolve a float flag."""
        return await self.resolve_float_async(flag_key, default_value, evaluation_context)

    async def resolve_object_details_async(
        self,
        flag_key: str,
        default_value: Union[Mapping, Sequence],
        evaluation_context: Optional[EvaluationContext] = None,
    ) -> FlagResolutionDetails[Union[Mapping, Sequence]]:
        """Resolve an object/struct flag."""
        return await self.resolve_object_async(flag_key, default_value, evaluation_context)

    # --- Private helpers ---

    async def _resolve_remote(
        self,
        context: Optional[EvaluationContext] = None,
        prefix_filter: Optional[List[str]] = None,
    ) -> Dict[str, Any]:
        """Resolve configuration via remote API.

        Args:
            context: Merged context dictionary with string values.
            prefix_filter: Optional list of key prefixes.

        Returns:
            Dictionary of resolved flags.
        """
        try:
            targeting_key, merged_context = self._get_merged_context(context)
            response = await self.client.get_resolved_config_with_identifier(
                input=GetResolvedConfigWithIdentifierInput(
                    workspace_id=self.options.workspace_id,
                    org_id=self.options.org_id,
                    context=merged_context,
                    prefix=prefix_filter,
                    identifier=targeting_key,
                )
            )

            # Convert response to plain dict
            config_value = document_to_python_value(response.config)

            if isinstance(config_value, dict):
                return config_value
            else:
                # Wrap non-dict responses
                return {"_value": config_value}
        except Exception as e:
            logger.error(f"Failed to resolve config: {e}")
            raise

    def _get_merged_context(
        self,
        context: Optional[EvaluationContext],
    ) -> Tuple[Optional[str], dict[str, Document]]:
        """Merge global and evaluation contexts.

        Returns:
            Tuple of (merged_query_data, targeting_key)
        """
        eval_ctx = self.global_context if context is None else self.global_context.merge(context)
        query_data = { k: Document(v) for k, v in eval_ctx.attributes.items() }
        return eval_ctx.targeting_key, query_data
