from datetime import datetime
from typing import Any, Dict, List, Optional

import aiohttp

from ..interfaces import SuperpositionDataSource
from ..types import ConfigData, ExperimentData


class HttpDataSource(SuperpositionDataSource):
    def __init__(
        self,
        endpoint: str,
        token: str,
        org_id: str,
        workspace_id: str,
        http_client: Optional[aiohttp.ClientSession] = None,
    ) -> None:
        self.endpoint = endpoint.rstrip("/")
        self.token = token
        self.org_id = org_id
        self.workspace_id = workspace_id
        self._http_client = http_client
        self._owned_session = http_client is None

    def _get_session(self) -> aiohttp.ClientSession:
        if self._http_client is None:
            self._http_client = aiohttp.ClientSession()
        return self._http_client

    def _headers(self) -> Dict[str, str]:
        return {
            "Authorization": f"Bearer {self.token}",
            "Content-Type": "application/json",
            "x-org-id": self.org_id,
            "x-workspace": self.workspace_id,
        }

    async def fetch_config(self) -> ConfigData:
        session = self._get_session()
        url = f"{self.endpoint}/config"

        async with session.get(url, headers=self._headers()) as response:
            response.raise_for_status()
            data = await response.json()

        return ConfigData(
            default_configs=data.get("default_configs", {}),
            contexts=data.get("contexts", []),
            overrides=data.get("overrides", {}),
            dimensions=data.get("dimensions", {}),
            fetched_at=datetime.now(),
        )

    async def fetch_filtered_config(
        self,
        context: Optional[Dict[str, Any]] = None,
        prefix_filter: Optional[List[str]] = None,
    ) -> ConfigData:
        session = self._get_session()
        url = f"{self.endpoint}/config"

        params: Dict[str, Any] = {}
        if context:
            params["context"] = context
        if prefix_filter:
            params["prefix"] = prefix_filter

        async with session.get(url, headers=self._headers(), params=params) as response:
            response.raise_for_status()
            data = await response.json()

        return ConfigData(
            default_configs=data.get("default_configs", {}),
            contexts=data.get("contexts", []),
            overrides=data.get("overrides", {}),
            dimensions=data.get("dimensions", {}),
            fetched_at=datetime.now(),
        )

    async def fetch_active_experiments(self) -> Optional[ExperimentData]:
        session = self._get_session()
        experiments_url = f"{self.endpoint}/experiments"
        groups_url = f"{self.endpoint}/experiment-groups"

        headers = self._headers()

        try:
            async with session.get(experiments_url, headers=headers) as exp_response:
                exp_response.raise_for_status()
                experiments_data = await exp_response.json()

            async with session.get(groups_url, headers=headers) as groups_response:
                groups_response.raise_for_status()
                groups_data = await groups_response.json()

            return ExperimentData(
                experiments=experiments_data
                if isinstance(experiments_data, list)
                else [],
                experiment_groups=groups_data if isinstance(groups_data, list) else [],
                fetched_at=datetime.now(),
            )
        except aiohttp.ClientError:
            return None

    async def fetch_candidate_active_experiments(
        self,
        context: Optional[Dict[str, Any]] = None,
        prefix_filter: Optional[List[str]] = None,
    ) -> Optional[ExperimentData]:
        experiment_data = await self.fetch_active_experiments()
        if experiment_data is None:
            return None

        filtered_experiments = experiment_data.experiments

        if prefix_filter:
            filtered_experiments = [
                exp
                for exp in filtered_experiments
                if self._matches_prefix(exp, prefix_filter)
            ]

        if context:
            filtered_experiments = [
                exp
                for exp in filtered_experiments
                if self._partial_match(exp.get("context", {}), context)
            ]

        if not filtered_experiments:
            return None

        return ExperimentData(
            experiments=filtered_experiments,
            experiment_groups=experiment_data.experiment_groups,
            fetched_at=datetime.now(),
        )

    async def fetch_matching_active_experiments(
        self,
        context: Optional[Dict[str, Any]] = None,
        prefix_filter: Optional[List[str]] = None,
    ) -> Optional[ExperimentData]:
        experiment_data = await self.fetch_active_experiments()
        if experiment_data is None:
            return None

        filtered_experiments = experiment_data.experiments

        if prefix_filter:
            filtered_experiments = [
                exp
                for exp in filtered_experiments
                if self._matches_prefix(exp, prefix_filter)
            ]

        if context:
            filtered_experiments = [
                exp
                for exp in filtered_experiments
                if self._exact_match(exp.get("context", {}), context)
            ]

        if not filtered_experiments:
            return None

        return ExperimentData(
            experiments=filtered_experiments,
            experiment_groups=experiment_data.experiment_groups,
            fetched_at=datetime.now(),
        )

    def supports_experiments(self) -> bool:
        return True

    async def close(self) -> None:
        if self._owned_session and self._http_client is not None:
            await self._http_client.close()
            self._http_client = None

    def _partial_match(
        self, exp_context: Dict[str, Any], filter_context: Dict[str, Any]
    ) -> bool:
        for key, value in filter_context.items():
            if key not in exp_context:
                return False
            if exp_context[key] != value:
                return False
        return True

    def _exact_match(
        self, exp_context: Dict[str, Any], filter_context: Dict[str, Any]
    ) -> bool:
        return exp_context == filter_context

    def _matches_prefix(self, experiment: Dict[str, Any], prefixes: List[str]) -> bool:
        exp_id = experiment.get("id", "")
        exp_name = experiment.get("name", "")

        for prefix in prefixes:
            if exp_id.startswith(prefix) or exp_name.startswith(prefix):
                return True
        return False
