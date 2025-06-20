from dataclasses import dataclass
from typing import Optional, Dict, Any


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
class PollingStrategy:
    interval: int
    timeout: int = None

@dataclass
class OnDemandStrategy:
    ttl: int
    use_stale_on_error: bool = False
    timeout: int = None

RefreshStrategy = PollingStrategy | OnDemandStrategy

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
   

