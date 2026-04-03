"""
Data source abstraction for fetching configuration and experiment data.

Provides a unified interface for different transport mechanisms (HTTP, file-based)
to fetch configuration and experiment data from a Superposition backend.
"""

import logging
from abc import ABC, abstractmethod
from dataclasses import dataclass
from datetime import datetime
from typing import Dict, List, Optional, Any, TypeVar, Generic

from superposition_bindings.superposition_types import Config
from superposition_bindings.superposition_client import ExperimentConfig

logger = logging.getLogger(__name__)

T = TypeVar('T')

class FetchResponse(Generic[T]):
    """Represents a fetch response with optional data, supporting 304 Not Modified."""

    def __init__(self, data: Optional[T] = None):
        self._data = data

    @staticmethod
    def not_modified():
        """Create a 304 Not Modified response."""
        return FetchResponse(data=None)

    @staticmethod
    def data(data: T):
        """Create a successful response with data."""
        return FetchResponse(data=data)

    def is_not_modified(self) -> bool:
        """Check if this is a 304 Not Modified response."""
        return self._data is None

    def get_data(self) -> Optional[T]:
        """Get the response data, or None if not modified."""
        return self._data


@dataclass
class ConfigData:
    """Configuration data with fetch metadata."""
    data: Config
    fetched_at: datetime

    def __str__(self):
        return (f"ConfigData(fetched_at: {self.fetched_at}, "
                f"contexts: {len(self.data.contexts)}, "
                f"overrides: {len(self.data.overrides)}, "
                f"default_configs: {len(self.data.default_configs)}, "
                f"dimensions: {len(self.data.dimensions)})")


@dataclass
class ExperimentData:
    """Experiment data with fetch metadata."""
    data: ExperimentConfig
    fetched_at: datetime

    def __str__(self):
        return (f"ExperimentData(experiments: {len(self.data.experiments)}, "
                f"experiment_groups: {len(self.data.experiment_groups)}, "
                f"fetched_at: {self.fetched_at})")


class SuperpositionDataSource(ABC):
    """
    Abstract interface for fetching configuration and experiment data.

    Implementors provide the transport mechanism (HTTP, file-based, etc.)
    while consumers interact with this unified interface.
    """

    @abstractmethod
    async def fetch_config(
        self,
        if_modified_since: Optional[datetime] = None,
    ) -> FetchResponse[ConfigData]:
        """Fetch the full resolved configuration.

        Args:
            if_modified_since: Optional timestamp for 304 Not Modified check.

        Returns:
            FetchResponse with ConfigData or NotModified status.
        """
        pass

    @abstractmethod
    async def fetch_filtered_config(
        self,
        context: Optional[Dict[str, Any]] = None,
        prefix_filter: Optional[List[str]] = None,
        if_modified_since: Optional[datetime] = None,
    ) -> FetchResponse[ConfigData]:
        """Fetch resolved configuration filtered by context and prefixes.

        Args:
            context: Optional context for filtering.
            prefix_filter: Optional list of key prefixes to include.
            if_modified_since: Optional timestamp for 304 Not Modified check.

        Returns:
            FetchResponse with ConfigData or NotModified status.
        """
        pass

    @abstractmethod
    async def fetch_active_experiments(
        self,
        if_modified_since: Optional[datetime] = None,
    ) -> FetchResponse[ExperimentData]:
        """Fetch all active experiments.

        Args:
            if_modified_since: Optional timestamp for 304 Not Modified check.

        Returns:
            FetchResponse with ExperimentData or NotModified status.
        """
        pass

    @abstractmethod
    async def fetch_candidate_active_experiments(
        self,
        context: Optional[Dict[str, Any]] = None,
        prefix_filter: Optional[List[str]] = None,
        if_modified_since: Optional[datetime] = None,
    ) -> FetchResponse[ExperimentData]:
        """Fetch active experiments with conditions matching the context.

        Args:
            context: Optional context for filtering.
            prefix_filter: Optional list of key prefixes to include.
            if_modified_since: Optional timestamp for 304 Not Modified check.

        Returns:
            FetchResponse with ExperimentData or NotModified status.
        """
        pass

    @abstractmethod
    async def fetch_matching_active_experiments(
        self,
        context: Optional[Dict[str, Any]] = None,
        prefix_filter: Optional[List[str]] = None,
        if_modified_since: Optional[datetime] = None,
    ) -> FetchResponse[ExperimentData]:
        """Fetch active experiments that match the context.

        Args:
            context: Optional context for filtering.
            prefix_filter: Optional list of key prefixes to include.
            if_modified_since: Optional timestamp for 304 Not Modified check.

        Returns:
            FetchResponse with ExperimentData or NotModified status.
        """
        pass

    def supports_experiments(self) -> bool:
        """Whether this data source supports experiments."""
        return False

    async def watch(self):
        """Set up file watching for changes.

        Returns:
            Optional watch stream for change notifications, or None if not supported.
        """
        return None

    @abstractmethod
    async def close(self) -> None:
        """Clean up any resources held by this data source."""
        pass
