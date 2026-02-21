from abc import ABC, abstractmethod
from typing import Dict, Any, List, Optional

from openfeature.evaluation_context import EvaluationContext

from .types import ConfigData, ExperimentData


class AllFeatureProvider(ABC):
    @abstractmethod
    async def resolve_all_features(self, context: EvaluationContext) -> Dict[str, Any]:
        pass

    @abstractmethod
    async def resolve_all_features_with_filter(
        self, context: EvaluationContext, prefix_filter: Optional[List[str]] = None
    ) -> Dict[str, Any]:
        pass


class FeatureExperimentMeta(ABC):
    @abstractmethod
    async def get_applicable_variants(self, context: EvaluationContext) -> List[str]:
        pass


class SuperpositionDataSource(ABC):
    @abstractmethod
    async def fetch_config(self) -> ConfigData:
        pass

    @abstractmethod
    async def fetch_filtered_config(
        self,
        context: Optional[Dict[str, Any]] = None,
        prefix_filter: Optional[List[str]] = None,
    ) -> ConfigData:
        pass

    @abstractmethod
    async def fetch_active_experiments(self) -> Optional[ExperimentData]:
        pass

    @abstractmethod
    async def fetch_candidate_active_experiments(
        self,
        context: Optional[Dict[str, Any]] = None,
        prefix_filter: Optional[List[str]] = None,
    ) -> Optional[ExperimentData]:
        pass

    @abstractmethod
    async def fetch_matching_active_experiments(
        self,
        context: Optional[Dict[str, Any]] = None,
        prefix_filter: Optional[List[str]] = None,
    ) -> Optional[ExperimentData]:
        pass

    @abstractmethod
    def supports_experiments(self) -> bool:
        pass

    @abstractmethod
    async def close(self) -> None:
        pass
