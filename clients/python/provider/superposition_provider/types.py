"""
Type definitions for Superposition provider configuration.
"""

import warnings
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


# ============================================================================
# Refresh Strategy Types
#
# Durations are MILLISECONDS, matching the other Superposition clients. The seconds-based
# `interval` / `ttl` / `timeout` fields are DEPRECATED: set the `_milliseconds` field instead.
# Setting both units for the same duration raises ValueError — see `_reject_both`.
# ============================================================================

def _resolve_ms(milliseconds: Optional[int], seconds: Optional[int], name: str) -> Optional[int]:
    """Prefer the millisecond field; fall back to the deprecated seconds one."""
    if milliseconds is not None:
        return milliseconds
    if seconds is None:
        return None
    warnings.warn(
        f"'{name}' is deprecated and is read as SECONDS; use '{name}_milliseconds' instead.",
        DeprecationWarning,
        stacklevel=3,
    )
    return seconds * 1000


def _reject_both(milliseconds: Optional[int], seconds: Optional[int], name: str) -> None:
    """Setting both units is ambiguous, so refuse it rather than silently picking one.

    Without this, starting from a default and overriding only the deprecated field —
    `dataclasses.replace(default_polling_strategy(), interval=30)` — would leave the default's
    `interval_milliseconds` in place, and it would win: the caller asks for 30s and gets 60s.
    """
    if milliseconds is not None and seconds is not None:
        raise ValueError(
            f"set either '{name}' (deprecated, seconds) or '{name}_milliseconds', not both; "
            f"'{name}_milliseconds' would win and '{name}' would be silently ignored"
        )


@dataclass
class PollingStrategy:
    """Polling-based refresh strategy.

    Fetches configuration at regular intervals.
    """
    interval: Optional[int] = None  # DEPRECATED: seconds
    timeout: Optional[int] = None  # DEPRECATED: seconds
    interval_milliseconds: Optional[int] = None
    timeout_milliseconds: Optional[int] = None

    def __post_init__(self):
        _reject_both(self.interval_milliseconds, self.interval, "interval")
        _reject_both(self.timeout_milliseconds, self.timeout, "timeout")

    def interval_ms(self) -> int:
        """The refresh interval in milliseconds."""
        resolved = _resolve_ms(self.interval_milliseconds, self.interval, "interval")
        if resolved is None:
            raise ValueError("PollingStrategy needs interval_milliseconds")
        return resolved

    def timeout_ms(self) -> Optional[int]:
        """The refresh timeout in milliseconds, if one is set."""
        return _resolve_ms(self.timeout_milliseconds, self.timeout, "timeout")

def default_polling_strategy():
    return PollingStrategy(interval_milliseconds=60_000, timeout_milliseconds=30_000)

@dataclass
class OnDemandStrategy:
    """On-demand refresh strategy.

    Refreshes only when data becomes stale.
    """
    ttl: Optional[int] = None  # DEPRECATED: seconds
    use_stale_on_error: bool = True
    timeout: Optional[int] = None  # DEPRECATED: seconds
    ttl_milliseconds: Optional[int] = None
    timeout_milliseconds: Optional[int] = None

    def __post_init__(self):
        _reject_both(self.ttl_milliseconds, self.ttl, "ttl")
        _reject_both(self.timeout_milliseconds, self.timeout, "timeout")

    def ttl_ms(self) -> int:
        """How long cached data stays fresh, in milliseconds."""
        resolved = _resolve_ms(self.ttl_milliseconds, self.ttl, "ttl")
        if resolved is None:
            raise ValueError("OnDemandStrategy needs ttl_milliseconds")
        return resolved

    def timeout_ms(self) -> Optional[int]:
        """The refresh timeout in milliseconds, if one is set."""
        return _resolve_ms(self.timeout_milliseconds, self.timeout, "timeout")

def default_on_demand_strategy():
    return OnDemandStrategy(
        use_stale_on_error=True, ttl_milliseconds=300_000, timeout_milliseconds=30_000
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
