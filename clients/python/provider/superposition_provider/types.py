"""
Type definitions for Superposition provider configuration.
"""

from dataclasses import dataclass
from typing import Optional, Dict, Any, Union


# ============================================================================
# Basic Configuration Types
# ============================================================================

@dataclass
class TokenAuth:
    """Bearer-token authentication."""
    token: str


@dataclass
class BasicAuth:
    """HTTP basic authentication."""
    username: str
    password: str


# How to authenticate with the Superposition backend.
AuthMethod = Union[TokenAuth, BasicAuth]


def auth_scheme_config(auth: AuthMethod):
    """Build the SDK ``(resolver, schemes)`` pair for the given auth method.

    One place that maps an :data:`AuthMethod` to the SDK's auth scheme, mirroring the Rust
    client's ``From<&SuperpositionOptions> for Config`` — every client-creation site goes
    through here rather than hard-coding bearer auth.
    """
    from superposition_sdk.auth_helpers import bearer_auth_config, basic_auth_config

    if isinstance(auth, TokenAuth):
        return bearer_auth_config(token=auth.token)
    if isinstance(auth, BasicAuth):
        return basic_auth_config(username=auth.username, password=auth.password)
    raise TypeError(f"Unsupported auth method: {type(auth).__name__}")


@dataclass
class SuperpositionOptions:
    """Core Superposition API configuration."""
    endpoint: str
    auth: AuthMethod
    org_id: str
    workspace_id: str

    def __post_init__(self):
        """Reject a blank endpoint, org, workspace, or auth credential at construction, so
        misconfiguration fails here rather than as an opaque error on the first request."""
        def blank(s):
            return not s or not s.strip()

        if blank(self.endpoint):
            raise ValueError("endpoint is required")
        if isinstance(self.auth, TokenAuth):
            if blank(self.auth.token):
                raise ValueError("token is required")
        elif isinstance(self.auth, BasicAuth):
            if blank(self.auth.username):
                raise ValueError("username is required")
            if blank(self.auth.password):
                raise ValueError("password is required")
        if blank(self.org_id):
            raise ValueError("org_id is required")
        if blank(self.workspace_id):
            raise ValueError("workspace_id is required")


# ============================================================================
# Refresh Strategy Types — all durations are MILLISECONDS.
# ============================================================================

@dataclass
class PollingStrategy:
    """Polling-based refresh strategy.

    Fetches configuration at regular intervals.
    """
    interval_milliseconds: int
    timeout_milliseconds: Optional[int] = None

    def interval_ms(self) -> int:
        """The refresh interval in milliseconds."""
        return self.interval_milliseconds

    def timeout_ms(self) -> Optional[int]:
        """The refresh timeout in milliseconds, if one is set."""
        return self.timeout_milliseconds

def default_polling_strategy():
    return PollingStrategy(interval_milliseconds=60_000, timeout_milliseconds=30_000)

@dataclass
class OnDemandStrategy:
    """On-demand refresh strategy.

    Refreshes only when data becomes stale.
    """
    ttl_milliseconds: int
    use_stale_on_error: bool = True
    timeout_milliseconds: Optional[int] = None

    def ttl_ms(self) -> int:
        """How long cached data stays fresh, in milliseconds."""
        return self.ttl_milliseconds

    def timeout_ms(self) -> Optional[int]:
        """The refresh timeout in milliseconds, if one is set."""
        return self.timeout_milliseconds

def default_on_demand_strategy():
    return OnDemandStrategy(
        ttl_milliseconds=300_000, use_stale_on_error=True, timeout_milliseconds=30_000
    )

@dataclass
class WatchStrategy:
    """File watch-based refresh strategy.

    Refreshes when local files change.
    """
    debounce_ms: int = 500

def default_watch_strategy():
    return WatchStrategy(500)

@dataclass
class ManualStrategy:
    """Manual refresh strategy.

    Caller explicitly triggers refresh via refresh() method.
    """
    pass


# Union type for all refresh strategies
RefreshStrategy = Union[PollingStrategy, OnDemandStrategy, WatchStrategy, ManualStrategy]


# ============================================================================
# Provider-Specific Options
# ============================================================================

@dataclass
class ExperimentationOptions:
    """Configuration for experimentation client."""
    refresh_strategy: RefreshStrategy

@dataclass
class ConfigurationOptions:
    """Configuration for config/CAC client."""
    refresh_strategy: RefreshStrategy
    fallback_config: Optional[Dict[str, Any]] = None


# ============================================================================
# Provider Initialization Options
# ============================================================================

@dataclass
class SuperpositionProviderOptions:
    """Universal provider options (backward compatibility)."""
    refresh_strategy: RefreshStrategy
    endpoint: str
    token: str
    org_id: str
    workspace_id: str

    fallback_config: Optional[Dict[str, Any]] = None
    experimentation_options: Optional[ExperimentationOptions] = None
