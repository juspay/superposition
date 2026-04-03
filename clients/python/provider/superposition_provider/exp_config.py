import json
import logging
import weakref
from decimal import Decimal
from typing import Any, Dict, Optional, TypeVar

from superposition_bindings.superposition_client import FfiExperiment, FfiExperimentGroup
from superposition_sdk.models import ExperimentStatusType as SDKExperimentStatusType
from .types import OnDemandStrategy, PollingStrategy, SuperpositionOptions, ExperimentationOptions
from superposition_sdk.client import Superposition, ListExperimentInput, ListExperimentGroupsInput
from superposition_sdk.config import Config
from superposition_sdk.auth_helpers import bearer_auth_config
import asyncio
from datetime import datetime, timedelta
from .conversions import exp_grps_to_ffi_exp_grps, experiments_to_ffi_experiments

T = TypeVar("T")
logger = logging.getLogger(__name__)
for name in logging.root.manager.loggerDict:
    if not name.startswith("python"):  # e.g., "my_project"
        logging.getLogger(name).setLevel(logging.INFO)


class DecimalEncoder(json.JSONEncoder):
    """Custom JSON encoder that handles Decimal types"""

    def default(self, obj):
        if isinstance(obj, Decimal):
            return float(obj)  # Convert Decimal to float for JSON serialization
        return super().default(obj)


def safe_json_dumps(obj: Any) -> str:
    """Safely serialize object to JSON, handling Decimal types"""
    return json.dumps(obj, cls=DecimalEncoder)

class ExperimentationConfig():
    def __init__(self, superposition_options: SuperpositionOptions, experiment_options: ExperimentationOptions, on_config_change=None):
        self.superposition_options = superposition_options
        self.options = experiment_options
        self.on_config_change = on_config_change  # Callback when experiments change
        self.cached_experiments = None
        self.cached_experiment_groups = None
        self.last_updated = None
        self.evaluation_cache: Dict[str, Dict[str, Any]] = {}
        self._polling_task = None

    async def create_config(self) -> None:
        weak_self = weakref.ref(self)

        async def poll_config(interval: int, timeout: int) -> None:
            while True:
                self_ref = weak_self()
                if self_ref is None:
                    logger.info("ExperimentationConfig has been garbage collected, stopping polling task.")
                    return

                try:
                    latest_exp_list = await self_ref._get_experiments(self_ref.superposition_options)
                    latest_exp_grp_list = await self_ref._get_experiment_groups(self_ref.superposition_options)
                    if latest_exp_list is not None and latest_exp_grp_list is not None:
                        # Only trigger callback if experiments or groups actually changed
                        if self_ref.cached_experiments != latest_exp_list or self_ref.cached_experiment_groups != latest_exp_grp_list:
                            self_ref.cached_experiments = latest_exp_list
                            self_ref.cached_experiment_groups = latest_exp_grp_list
                            self_ref.last_updated = datetime.utcnow()
                            logger.info("Experiment List and Experiment Group List updated successfully.")
                            # Trigger callback for cache reinitialization
                            if self_ref.on_config_change:
                                self_ref.on_config_change()
                        else:
                            logger.info("Experiments/groups unchanged (skipping callback)")

                except Exception as e:
                    logger.error(f"Polling error: {e}")
                finally:
                    del self_ref

                await asyncio.sleep(interval)

        latest_exp_list = await self._get_experiments(self.superposition_options)
        latest_exp_grp_list = await self._get_experiment_groups(self.superposition_options)
        logger.info("response from experimentation: " + str(latest_exp_list))
        logger.info("response from experimentation groups: " + str(latest_exp_grp_list))
        if latest_exp_list is not None and latest_exp_grp_list is not None:
            self.cached_experiments = latest_exp_list
            self.cached_experiment_groups = latest_exp_grp_list
            self.last_updated = datetime.utcnow()
            logger.info("Experiment List and Experiment Group List fetched successfully.")
            # Trigger callback on initial fetch
            if self.on_config_change:
                self.on_config_change()

        match self.options.refresh_strategy:
            case PollingStrategy(interval=interval, timeout=timeout):
                logger.info(f"Using PollingStrategy: interval={interval}, timeout={timeout}")
                if self._polling_task is None:
                    self._polling_task = asyncio.create_task(poll_config(interval, timeout))

            case OnDemandStrategy(ttl=ttl, use_stale_on_error=use_stale, timeout=timeout):
                logger.info(f"Using OnDemandStrategy: ttl={ttl}, use_stale_on_error={use_stale}, timeout={timeout}")

    @staticmethod
    async def _get_experiments(superposition_options: SuperpositionOptions) -> Optional[list[FfiExperiment]]:
        """
        Fetch configuration from Superposition service using the generated Python SDK.

        Args:
            superposition_options: Options containing endpoint, token, org_id, workspace_id

        Returns:
            Dict containing the configuration data
        """
        try:
            # Create SDK config with bearer token authentication
            (resolver, schemes) = bearer_auth_config(
                token=superposition_options.token
            )
            sdk_config = Config(
                endpoint_uri=superposition_options.endpoint,
                http_auth_scheme_resolver=resolver,
                http_auth_schemes=schemes
            )

            # Create Superposition client
            client = Superposition(config=sdk_config)

            list_exp_input = ListExperimentInput(
                workspace_id=superposition_options.workspace_id,
                org_id=superposition_options.org_id,
                all=True,
                status=[SDKExperimentStatusType.CREATED, SDKExperimentStatusType.INPROGRESS]
            )

            response = await client.list_experiment(list_exp_input)

            exp_list = response.data
            logger.info(f"Fetched {len(exp_list)} experiments from Superposition")
            return experiments_to_ffi_experiments(exp_list)

        except Exception as e:
            # Log the error and return empty config as fallback
            logger.error(f"Error fetching config from Superposition: {e}")
            return None

    @staticmethod
    async def _get_experiment_groups(superposition_options: SuperpositionOptions) -> Optional[list[FfiExperimentGroup]]:
        """
        Fetch configuration from Superposition service using the generated Python SDK.

        Args:
            superposition_options: Options containing endpoint, token, org_id, workspace_id

        Returns:
            Dict containing the configuration data
        """
        try:
            # Create SDK config with bearer token authentication
            (resolver, schemes) = bearer_auth_config(
                token=superposition_options.token
            )
            sdk_config = Config(
                endpoint_uri=superposition_options.endpoint,
                http_auth_scheme_resolver=resolver,
                http_auth_schemes=schemes
            )

            # Create Superposition client
            client = Superposition(config=sdk_config)

            list_exp_grp_input = ListExperimentGroupsInput(
                workspace_id=superposition_options.workspace_id,
                org_id=superposition_options.org_id,
                all=True
            )

            response = await client.list_experiment_groups(list_exp_grp_input)

            exp_grp_list = response.data
            logger.info(f"Fetched {len(exp_grp_list)} experiment groups from Superposition")
            return exp_grps_to_ffi_exp_grps(exp_grp_list)

        except Exception as e:
            # Log the error and return empty config as fallback
            logger.error(f"Error fetching config from Superposition: {e}")
            return None

    async def on_demand_config(self, ttl, use_stale) -> list[FfiExperiment]:
        """Get config on-demand based on TTL or fall back to stale if needed."""
        now = datetime.utcnow()
        should_refresh = (
            not self.last_updated or
            (now - self.last_updated) > timedelta(seconds=ttl)
        )

        if should_refresh:
            try:
                logger.debug("TTL expired. Fetching config on-demand.")
                latest_exp_list = await self._get_experiments(self.superposition_options)
                latest_exp_grp_list = await self._get_experiment_groups(self.superposition_options)

                if latest_exp_list is not None and latest_exp_grp_list is not None:
                    # Only trigger callback if experiments or groups actually changed
                    if self.cached_experiments != latest_exp_list or self.cached_experiment_groups != latest_exp_grp_list:
                        logger.info("Experiment List and Experiment Group List updated successfully on-demand.")
                        self.cached_experiments = latest_exp_list
                        self.cached_experiment_groups = latest_exp_grp_list
                        self.last_updated = datetime.utcnow()
                        # Trigger callback for cache reinitialization
                        if self.on_config_change:
                            self.on_config_change()
                    else:
                        logger.info("Experiments/groups unchanged on-demand (skipping callback)")

            except Exception as e:
                logger.warning(f"On-demand fetch failed: {e}")
                if not use_stale or self.cached_experiments is None:
                    raise e
                else:
                    logger.info("Using stale config due to error.")

        return self.cached_experiments

    def _generate_cache_key(self, query_data: dict) -> str:
        return json.dumps(query_data, sort_keys=True)

    def _get_from_eval_cache(self, key: str) -> Optional[Any]:
        self.evaluation_cache.get(key)

    def _set_eval_cache(self, key: str, value: Any) -> None:
        self.evaluation_cache[key] = value

    def _clear_eval_cache(self) -> None:
        self.evaluation_cache.clear()

    async def close(self):
        """
        Close the configuration client and clean up resources.
        Stops polling tasks and clears caches.
        """
        try:
            # Cancel polling task if running
            if self._polling_task and not self._polling_task.done():
                logger.info("Stopping polling task...")
                self._polling_task.cancel()
                try:
                    await self._polling_task
                except asyncio.CancelledError:
                    logger.debug("Polling task cancelled successfully")

            # Clear caches
            self._clear_eval_cache()
            self.cached_experiments = None
            self.last_updated = None

            logger.info("ConfigurationClient closed successfully")

        except Exception as e:
            logger.error(f"Error during ConfigurationClient cleanup: {e}")
            raise
