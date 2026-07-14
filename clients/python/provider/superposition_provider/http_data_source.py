"""
HTTP data source for fetching configuration and experiment data from Superposition API.

Communicates with the Superposition service via HTTP using the SDK client.
"""

import logging
from datetime import datetime
from typing import Dict, List, Optional, Any

from smithy_core.documents import Document
from smithy_core.interceptors import Interceptor, ResponseContext
from superposition_bindings.superposition_client import ExperimentConfig

from superposition_sdk.client import Superposition
from superposition_sdk.config import Config as SdkConfig
from superposition_sdk.auth_helpers import bearer_auth_config
from superposition_sdk.models import GetConfigInput, DimensionMatchStrategy, \
    GetExperimentConfigInput, ServiceError
from .conversions import experiments_to_ffi_experiments, exp_grps_to_ffi_exp_grps, config_response_to_ffi_config

from .errors import SuperpositionError
from .types import SuperpositionOptions
from .data_source import (
    SuperpositionDataSource,
    FetchResponse,
    ConfigData,
    ExperimentData,
)

logger = logging.getLogger(__name__)


class _NotModified(ServiceError):
    """The service answered 304: the caller's cached copy is still current.

    Deliberately a `ServiceError`: the client wraps anything else in one on its way out
    (`raise ServiceError(e) from e`), which would bury this behind a cause chain.
    """


class _NotModifiedInterceptor(Interceptor):
    """Turns an HTTP 304 into a distinguishable signal.

    The generated SDK models no shape for 304, and its deserializer treats every status
    >= 300 as a failure, collapsing it into an `UnknownApiError` that carries only a
    message string. Reading the raw status before deserialization is the only way to tell
    "your cached copy is still current" from a genuine failure — the Java client solves
    this the same way.

    This replaces an `except UnknownApiError: return not_modified()` catch-all, which
    reported expired credentials and 500s as a successful "nothing changed" and silently
    froze the config.
    """

    def read_after_transmit(self, context: ResponseContext) -> None:
        response = context.transport_response
        if response is not None and response.status == 304:
            raise _NotModified()


class HttpDataSource(SuperpositionDataSource):
    """HTTP-based data source for Superposition API.

    Fetches configuration and experiment data from the Superposition HTTP API
    using the SDK client. Supports conditional requests via Last-Modified timestamps.
    """

    def __init__(
        self,
        options: SuperpositionOptions,
    ):
        """Initialize HTTP data source.

        Args:
            options: Superposition options.
        """
        self.options = options
        self.client: Superposition = self._create_client()

    def _create_client(self) -> Superposition:
        """Create and configure the SDK client."""
        (resolver, schemes) = bearer_auth_config(
            token=self.options.token
        )
        sdk_config = SdkConfig(
            endpoint_uri=self.options.endpoint,
            http_auth_scheme_resolver=resolver,
            http_auth_schemes=schemes,
            interceptors=[_NotModifiedInterceptor()],
        )

        # Create Superposition client
        return Superposition(config=sdk_config)

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
        try:
            context = {k: Document(v) for k, v in context.items()} if context else None
            response = await self.client.get_config(
                input=GetConfigInput(
                    workspace_id=self.options.workspace_id,
                    org_id=self.options.org_id,
                    context=context,
                    prefix=prefix_filter,
                    if_modified_since=if_modified_since,
                )
            )
            return FetchResponse.data(ConfigData(
                fetched_at=response.last_modified,
                data=config_response_to_ffi_config(response),
            ))
        except _NotModified:
            return FetchResponse.not_modified()
        except Exception as e:
            raise SuperpositionError.network_error(f"Failed to fetch config: {e}", e) from e

    async def _fetch_filtered_experiment(
        self,
        context: Optional[Dict[str, Any]] = None,
        prefix_filter: Optional[List[str]] = None,
        if_modified_since: Optional[datetime] = None,
        dimension_match_strategy: Optional[DimensionMatchStrategy] = None,
    ) -> FetchResponse[ExperimentData]:
        """Fetch resolved experiment filtered by context and prefixes."""
        try:
            context = {k: Document(v) for k, v in context.items()} if context else None
            response = await self.client.get_experiment_config(
                input=GetExperimentConfigInput(
                    workspace_id=self.options.workspace_id,
                    org_id=self.options.org_id,
                    context=context,
                    prefix=prefix_filter,
                    if_modified_since=if_modified_since,
                    dimension_match_strategy=dimension_match_strategy,
                )
            )
            return FetchResponse.data(ExperimentData(
                fetched_at=response.last_modified,
                data=ExperimentConfig(
                    experiments=experiments_to_ffi_experiments(response.experiments),
                    experiment_groups=exp_grps_to_ffi_exp_grps(response.experiment_groups),
                )
            ))
        except _NotModified:
            return FetchResponse.not_modified()
        except Exception as e:
            raise SuperpositionError.network_error(
                f"Failed to fetch experiments: {e}", e
            ) from e

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
        return await self._fetch_filtered_experiment(if_modified_since=if_modified_since)

    async def fetch_candidate_active_experiments(
        self,
        context: Optional[Dict[str, Any]] = None,
        prefix_filter: Optional[List[str]] = None,
        if_modified_since: Optional[datetime] = None,
    ) -> FetchResponse[ExperimentData]:
        """Fetch active experiments with candidate conditions.

        Args:
            context: Optional context for filtering.
            prefix_filter: Optional list of key prefixes to include.
            if_modified_since: Optional timestamp for 304 Not Modified check.

        Returns:
            FetchResponse with ExperimentData or NotModified status.
        """
        return await self._fetch_filtered_experiment(
            context,
            prefix_filter,
            if_modified_since,
            DimensionMatchStrategy.EXACT
        )

    async def fetch_matching_active_experiments(
        self,
        context: Optional[Dict[str, Any]] = None,
        prefix_filter: Optional[List[str]] = None,
        if_modified_since: Optional[datetime] = None,
    ) -> FetchResponse[ExperimentData]:
        """Fetch active experiments matching the context.

        Args:
            context: Optional context for filtering.
            prefix_filter: Optional list of key prefixes to include.
            if_modified_since: Optional timestamp for 304 Not Modified check.

        Returns:
            FetchResponse with ExperimentData or NotModified status.
        """
        return await self._fetch_filtered_experiment(
            context,
            prefix_filter,
            if_modified_since,
            DimensionMatchStrategy.SUBSET
        )

    def supports_experiments(self) -> bool:
        """HTTP data source supports experiments."""
        return True

    async def close(self) -> None:
        """Close the HTTP client."""
        if self.client:
            self.client = None
