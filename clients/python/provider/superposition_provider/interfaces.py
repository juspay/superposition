"""
Interface definitions for feature resolution and experiment metadata.

These interfaces define the core capabilities for flag evaluation and variant resolution.
"""

import logging
from abc import ABC, abstractmethod
from typing import Dict, List, Optional, Any, Union, Mapping, Sequence

from openfeature.evaluation_context import EvaluationContext
from openfeature.exception import ErrorCode
from openfeature.flag_evaluation import FlagResolutionDetails

logger = logging.getLogger(__name__)


class FeatureExperimentMeta(ABC):
    """Trait for experiment variant resolution.

    Implementors provide the ability to determine which experiment variants
    are applicable for a given evaluation context.
    """

    @abstractmethod
    async def get_applicable_variants(
        self,
        context: Optional[EvaluationContext],
        prefix_filter: Optional[List[str]] = None,
    ) -> List[str]:
        """Get the list of applicable experiment variant IDs for the given context.

        Args:
            context: Evaluation context with targeting key and attributes.
            prefix_filter: Optional list of variant ID prefixes to filter by.

        Returns:
            List of variant IDs that apply to this context.
        """
        pass


class AllFeatureProvider(ABC):
    """Trait for bulk configuration resolution.

    Implementors provide the ability to resolve all feature flags at once,
    optionally filtered by key prefixes.
    """

    def resolve_all_features(
        self,
        context: Optional[EvaluationContext],
    ) -> Dict[str, Any]:
        """Resolve all features for the given evaluation context.

        Args:
            context: Evaluation context with targeting key and attributes.

        Returns:
            Map of all resolved feature flags and their values.
        """
        return self.resolve_all_features_with_filter(context, None)

    @abstractmethod
    def resolve_all_features_with_filter(
        self,
        context: Optional[EvaluationContext],
        prefix_filter: Optional[List[str]] = None,
    ) -> Dict[str, Any]:
        """Resolve all features, optionally filtered by key prefixes.

        Args:
            context: Evaluation context with targeting key and attributes.
            prefix_filter: Optional list of key prefixes to include.

        Returns:
            Map of filtered resolved feature flags and their values.
        """
        pass

    def resolve_typed(
        self,
        flag_key: str,
        evaluation_context: Optional[EvaluationContext],
        type_name: str,
        extractor,
        default
    ) -> FlagResolutionDetails:
        """Generic typed resolution using an extractor function.

        Resolves all features and extracts a specific flag, applying type conversion.

        Args:
            flag_key: The flag key to resolve.
            evaluation_context: Evaluation context.
            type_name: Type name for error messages.
            extractor: Function to extract the correct type from the value.
            default: The default value to use if the flag key is not found.

        Returns:
            FlagResolutionDetails with the typed value or error.
        """
        try:
            config = self.resolve_all_features(evaluation_context)
            if flag_key not in config:
                return FlagResolutionDetails(default)

            value = config[flag_key]
            extracted = extractor(value)
            if extracted is None:
                return FlagResolutionDetails(default)
            return FlagResolutionDetails(extracted)
        except Exception as e:
            logger.error(f"Error evaluating {type_name} flag {flag_key}: {e}")
            return FlagResolutionDetails(
                default,
                error_code=ErrorCode.GENERAL,
                error_message=f"Error evaluating flag '{flag_key}': {e}"
            )

    def resolve_bool(
        self,
        flag_key: str,
        default_value: bool,
        evaluation_context: Optional[EvaluationContext],
    ) -> FlagResolutionDetails[bool]:
        """Resolve a boolean flag."""
        return self.resolve_typed(
            flag_key,
            evaluation_context,
            "boolean",
            lambda v: _to_bool(v),
            default_value
        )

    def resolve_string(
        self,
        flag_key: str,
        default_value: str,
        evaluation_context: Optional[EvaluationContext],
    ) -> FlagResolutionDetails[str]:
        """Resolve a string flag."""
        return self.resolve_typed(
            flag_key,
            evaluation_context,
            "string",
            lambda v: v if isinstance(v, str) else None,
            default_value
        )

    def resolve_int(
        self,
        flag_key: str,
        default_value: int,
        evaluation_context: Optional[EvaluationContext],
    ) -> FlagResolutionDetails[int]:
        """Resolve an integer flag."""
        return self.resolve_typed(
            flag_key,
            evaluation_context,
            "integer",
            lambda v: v if isinstance(v, int) and not isinstance(v, bool) else None,
            default_value
        )

    def resolve_float(
        self,
        flag_key: str,
        default_value: float,
        evaluation_context: Optional[EvaluationContext],
    ) -> FlagResolutionDetails[float]:
        """Resolve a float flag."""
        return self.resolve_typed(
            flag_key,
            evaluation_context,
            "float",
            lambda v: v if isinstance(v, (int, float)) and not isinstance(v, bool) else None,
            default_value
        )

    def resolve_object(
        self,
        flag_key: str,
        default_value: Union[Mapping, Sequence],
        evaluation_context: Optional[EvaluationContext],
    ) -> FlagResolutionDetails[Union[Mapping, Sequence]]:
        """Resolve a struct/object flag."""
        return self.resolve_typed(
            flag_key,
            evaluation_context,
            "struct",
            lambda v: v if isinstance(v, dict) or isinstance(v, list) else None,
            default_value
        )

    async def resolve_all_features_async(
        self,
        context: Optional[EvaluationContext],
    ) -> Dict[str, Any]:
        """Resolve all features for the given evaluation context.

        Args:
            context: Evaluation context with targeting key and attributes.

        Returns:
            Map of all resolved feature flags and their values.
        """
        return await self.resolve_all_features_with_filter_async(context, None)

    @abstractmethod
    async def resolve_all_features_with_filter_async(
        self,
        context: Optional[EvaluationContext],
        prefix_filter: Optional[List[str]] = None,
    ) -> Dict[str, Any]:
        """Resolve all features, optionally filtered by key prefixes.

        Args:
            context: Evaluation context with targeting key and attributes.
            prefix_filter: Optional list of key prefixes to include.

        Returns:
            Map of filtered resolved feature flags and their values.
        """
        pass

    async def resolve_typed_async(
        self,
        flag_key: str,
        evaluation_context: Optional[EvaluationContext],
        type_name: str,
        extractor,
        default
    ) -> FlagResolutionDetails:
        """Generic typed resolution using an extractor function.

        Resolves all features and extracts a specific flag, applying type conversion.

        Args:
            flag_key: The flag key to resolve.
            evaluation_context: Evaluation context.
            type_name: Type name for error messages.
            extractor: Function to extract the correct type from the value.
            default: The default value to return if the type cannot be extracted.

        Returns:
            FlagResolutionDetails with the typed value or error.
        """
        try:
            config = await self.resolve_all_features_async(evaluation_context)
            if flag_key not in config:
                return FlagResolutionDetails(default)

            value = config[flag_key]
            extracted = extractor(value)
            if extracted is None:
                return FlagResolutionDetails(default)
            return FlagResolutionDetails(extracted)
        except Exception as e:
            logger.error(f"Error evaluating {type_name} flag {flag_key}: {e}")
            return FlagResolutionDetails(
                default,
                error_code=ErrorCode.GENERAL,
                error_message=f"Error evaluating flag '{flag_key}': {e}"
            )

    async def resolve_bool_async(
        self,
        flag_key: str,
        default_value: bool,
        evaluation_context: Optional[EvaluationContext],
    ) -> FlagResolutionDetails[bool]:
        """Resolve a boolean flag."""
        return await self.resolve_typed_async(
            flag_key,
            evaluation_context,
            "boolean",
            lambda v: _to_bool(v),
            default_value
        )

    async def resolve_string_async(
        self,
        flag_key: str,
        default_value: str,
        evaluation_context: Optional[EvaluationContext],
    ) -> FlagResolutionDetails[str]:
        """Resolve a string flag."""
        return await self.resolve_typed_async(
            flag_key,
            evaluation_context,
            "string",
            lambda v: v if isinstance(v, str) else None,
            default_value
        )

    async def resolve_int_async(
        self,
        flag_key: str,
        default_value: int,
        evaluation_context: Optional[EvaluationContext],
    ) -> FlagResolutionDetails[int]:
        """Resolve an integer flag."""
        return await self.resolve_typed_async(
            flag_key,
            evaluation_context,
            "integer",
            lambda v: v if isinstance(v, int) and not isinstance(v, bool) else None,
            default_value
        )

    async def resolve_float_async(
        self,
        flag_key: str,
        default_value: float,
        evaluation_context: Optional[EvaluationContext],
    ) -> FlagResolutionDetails[float]:
        """Resolve a float flag."""
        return await self.resolve_typed_async(
            flag_key,
            evaluation_context,
            "float",
            lambda v: v if isinstance(v, (int, float)) and not isinstance(v, bool) else None,
            default_value
        )

    async def resolve_object_async(
        self,
        flag_key: str,
        default_value: Union[Mapping, Sequence],
        evaluation_context: Optional[EvaluationContext],
    ) -> FlagResolutionDetails[Union[Mapping, Sequence]]:
        """Resolve a struct/object flag."""
        return await self.resolve_typed_async(
            flag_key,
            evaluation_context,
            "struct",
            lambda v: v if isinstance(v, dict) or isinstance(v, list) else None,
            default_value
        )

def _to_bool(value: Any) -> Optional[bool]:
    if isinstance(value, bool): return value
    if isinstance(value, str):
        if value.lower() == "true": return True
        if value.lower() == "false": return False
    if isinstance(value, (int, float)): return value != 0
    return None
