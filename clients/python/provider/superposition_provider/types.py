"""
Type definitions for Superposition provider configuration.
"""

from dataclasses import dataclass, field
from typing import Optional, Dict, Any, Union
from .data_source import SuperpositionDataSource


# ============================================================================
# Basic Configuration Types
# ============================================================================

@dataclass
class SuperpositionOptions:
    """Core Superposition API configuration."""
    endpoint: str
    token: str
    org_id: str
    workspace_id: str


# ============================================================================
# Cache Configuration
# ============================================================================

@dataclass
class EvaluationCacheOptions:
    """Options for evaluation result caching."""
    ttl: Optional[int] = None
    size: Optional[int] = None


# ============================================================================
# Refresh Strategy Types
# ============================================================================

@dataclass
class PollingStrategy:
    """Polling-based refresh strategy.

    Fetches configuration at regular intervals.
    """
    interval: int  # seconds
    timeout: Optional[int] = None

def default_polling_strategy():
    return PollingStrategy(60, 30)

@dataclass
class OnDemandStrategy:
    """On-demand refresh strategy.

    Refreshes only when data becomes stale.
    """
    ttl: int  # time-to-live in seconds
    use_stale_on_error: bool = False
    timeout: Optional[int] = None

def default_on_demand_strategy():
    return OnDemandStrategy(300, True, 30)

@dataclass
class WatchStrategy:
    """File watch-based refresh strategy.

    Refreshes when local files change.
    """
    debounce_ms: int = 500

def default_watch_strategy():
    return WatchStrategy(500)

@dataclass
class SseStrategy:
    """SSE-based refresh strategy.

    Connects to the server's SSE endpoint and refreshes when a change event
    is received. Reconnects automatically on connection failure.

    Requires SuperpositionOptions to build the SSE endpoint URL and authenticate.
    """
    superposition_options: SuperpositionOptions
    reconnect_delay: int = 5  # seconds between reconnect attempts
    debounce_ms: int = 500  # debounce rapid successive events

@dataclass
class ManualStrategy:
    """Manual refresh strategy.

    Caller explicitly triggers refresh via refresh() method.
    """
    pass


# Union type for all refresh strategies
RefreshStrategy = Union[PollingStrategy, OnDemandStrategy, WatchStrategy, SseStrategy, ManualStrategy]


# ============================================================================
# Provider-Specific Options
# ============================================================================

@dataclass
class ExperimentationOptions:
    """Configuration for experimentation client."""
    refresh_strategy: RefreshStrategy
    evaluation_cache_options: Optional[EvaluationCacheOptions] = None
    default_toss: int = -1

@dataclass
class ConfigurationOptions:
    """Configuration for config/CAC client."""
    refresh_strategy: RefreshStrategy
    fallback_config: Optional[Dict[str, Any]] = None
    evaluation_cache_options: Optional[EvaluationCacheOptions] = None

# ============================================================================
# Provider Initialization Options
# ============================================================================

@dataclass
class LocalProviderOptions:
    """Options for LocalResolutionProvider."""
    primary_source: SuperpositionDataSource
    fallback_source: Optional[SuperpositionDataSource] = None
    refresh_strategy: RefreshStrategy = field(default_factory=default_polling_strategy)


@dataclass
class RemoteProviderOptions:
    """Options for SuperpositionAPIProvider."""
    superposition_options: SuperpositionOptions


@dataclass
class SuperpositionProviderOptions:
    """Universal provider options (backward compatibility)."""
    refresh_strategy: RefreshStrategy
    endpoint: str
    token: str
    org_id: str
    workspace_id: str

    fallback_config: Optional[Dict[str, Any]] = None
    evaluation_cache_options: Optional[EvaluationCacheOptions] = None
    experimentation_options: Optional[ExperimentationOptions] = None
