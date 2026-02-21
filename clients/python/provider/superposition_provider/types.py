from dataclasses import dataclass, field
from datetime import datetime
from typing import Dict, Any, List, Optional


@dataclass
class ConfigData:
    default_configs: Dict[str, Any]
    contexts: List[Dict[str, Any]]
    overrides: Dict[str, Any]
    dimensions: Dict[str, Any]
    fetched_at: datetime = field(default_factory=datetime.now)


@dataclass
class ExperimentData:
    experiments: List[Dict[str, Any]]
    experiment_groups: List[Dict[str, Any]]
    fetched_at: datetime = field(default_factory=datetime.now)


@dataclass
class RefreshStrategy:
    pass


@dataclass
class PollingStrategy(RefreshStrategy):
    interval: int
    timeout: Optional[int] = None


@dataclass
class OnDemandStrategy(RefreshStrategy):
    ttl: int
    use_stale_on_error: bool = False
    timeout: Optional[int] = None


@dataclass
class ManualStrategy(RefreshStrategy):
    pass


@dataclass
class CacheOptions:
    size: int = 1000
    ttl: int = 300


@dataclass
class SuperpositionConfig:
    endpoint: str
    token: str
    org_id: str
    workspace_id: str


@dataclass
class EvaluationCacheOptions:
    ttl: Optional[int] = None
    size: Optional[int] = None


@dataclass
class ExperimentationOptions:
    refresh_strategy: RefreshStrategy
    evaluation_cache_options: Optional[EvaluationCacheOptions] = None
    default_toss: int = -1


@dataclass
class SuperpositionOptions:
    endpoint: str
    token: str
    org_id: str
    workspace_id: str


@dataclass
class ConfigurationOptions:
    refresh_strategy: RefreshStrategy
    fallback_config: Optional[Dict[str, Any]] = None
    evaluation_cache_options: Optional[EvaluationCacheOptions] = None


@dataclass
class SuperpositionProviderOptions:
    refresh_strategy: RefreshStrategy
    endpoint: str
    token: str
    org_id: str
    workspace_id: str

    fallback_config: Optional[Dict[str, Any]] = None
    evaluation_cache_options: Optional[EvaluationCacheOptions] = None
    experimentation_options: Optional[ExperimentationOptions] = None
