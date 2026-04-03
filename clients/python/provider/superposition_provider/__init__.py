"""
Superposition OpenFeature Provider.

Provides OpenFeature-compliant feature flag providers with support for:
- Local resolution with caching (LocalResolutionProvider)
- Remote evaluation without caching (SuperpositionAPIProvider)
- Configurable refresh strategies (Polling, OnDemand, Watch, Manual)
- File-based and HTTP-based data sources
- Full FFI integration for performance
"""

from .data_source import (
    SuperpositionDataSource,
    FetchResponse,
    ConfigData,
    ExperimentData,
)
from .file_data_source import FileDataSource
from .http_data_source import HttpDataSource

from .interfaces import AllFeatureProvider, FeatureExperimentMeta

from .local_provider import LocalResolutionProvider, RefreshStrategy
from .remote_provider import SuperpositionAPIProvider

from .types import (
    SuperpositionOptions,
    EvaluationCacheOptions,
    PollingStrategy,
    OnDemandStrategy,
    WatchStrategy,
    ManualStrategy,
    RefreshStrategy as RefreshStrategyType,
    ConfigurationOptions,
    ExperimentationOptions,
    LocalProviderOptions,
    RemoteProviderOptions,
    SuperpositionProviderOptions,
)

__all__ = [
    # Data sources
    "SuperpositionDataSource",
    "FetchResponse",
    "ConfigData",
    "ExperimentData",
    "FileDataSource",
    "HttpDataSource",
    # Traits
    "AllFeatureProvider",
    "FeatureExperimentMeta",
    # Providers
    "LocalResolutionProvider",
    "SuperpositionAPIProvider",
    "RefreshStrategy",
    # Types
    "SuperpositionOptions",
    "EvaluationCacheOptions",
    "PollingStrategy",
    "OnDemandStrategy",
    "WatchStrategy",
    "ManualStrategy",
    "RefreshStrategyType",
    "ConfigurationOptions",
    "ExperimentationOptions",
    "LocalProviderOptions",
    "RemoteProviderOptions",
    "SuperpositionProviderOptions",
]
