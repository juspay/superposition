"""
Data source abstraction for fetching configuration and experiment data.

Provides a unified interface for different transport mechanisms (HTTP, file-based)
to fetch configuration and experiment data from a Superposition backend.
"""

import logging
from abc import ABC, abstractmethod
from dataclasses import dataclass
from datetime import datetime
from typing import Dict, List, Optional, Any, TypeVar, Generic, AsyncGenerator

from superposition_bindings.superposition_types import Config
from superposition_bindings.superposition_client import ExperimentConfig

logger = logging.getLogger(__name__)

T = TypeVar('T')

class FetchResponse(Generic[T]):
    """Either fetched data or a 304 Not Modified marker.

    A true sum type: `NotModified` is a distinct variant rather than "data is None", so a data
    source that legitimately returns nothing cannot be mistaken for an unchanged one.
    """

    __slots__ = ("_data", "_not_modified")

    def __init__(self, data: Optional[T] = None, not_modified: bool = False):
        self._data = data
        self._not_modified = not_modified

    @staticmethod
    def not_modified() -> "FetchResponse[T]":
        """Create a 304 Not Modified response."""
        return FetchResponse(data=None, not_modified=True)

    @staticmethod
    def data(data: T) -> "FetchResponse[T]":
        """Create a successful response with data."""
        return FetchResponse(data=data, not_modified=False)

    def is_not_modified(self) -> bool:
        """Check if this is a 304 Not Modified response."""
        return self._not_modified

    def get_data(self) -> Optional[T]:
        """Get the response data, or None if not modified."""
        return self._data

    def map_data(self, mapper):
        """Transform the data if present, preserving a NotModified response."""
        if self._not_modified:
            return FetchResponse.not_modified()
        return FetchResponse.data(mapper(self._data))

    def __repr__(self) -> str:
        return "FetchResponse.NotModified" if self._not_modified else f"FetchResponse.Data({self._data})"


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
        return await self.fetch_filtered_config(if_modified_since=if_modified_since)

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

    async def watch(self) -> Optional[AsyncGenerator[str, None]]:
        """Set up file watching for changes.

        Returns:
            Optional watch stream for change notifications, or None if not supported.
        """
        return None

    @abstractmethod
    async def close(self) -> None:
        """Clean up any resources held by this data source."""
        pass
