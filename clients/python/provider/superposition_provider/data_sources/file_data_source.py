"""File-based implementation of SuperpositionDataSource."""

import os
from pathlib import Path
from typing import Dict, List, Optional, Any

from superposition_bindings.superposition_client import ffi_parse_toml_config

from ..interfaces import SuperpositionDataSource
from ..types import ConfigData, ExperimentData


class FileDataSource(SuperpositionDataSource):
    """File-based data source for Superposition configuration.

    Reads configuration from a local file (TOML or JSON format).
    Uses native bindings for TOML parsing.
    """

    def __init__(self, file_path: str):
        """Initialize FileDataSource.

        Args:
            file_path: Path to the configuration file
        """
        self.file_path = Path(file_path)

    async def fetch_config(self) -> ConfigData:
        """Fetch the latest configuration from the data source."""
        try:
            content = self.file_path.read_text(encoding="utf-8")
            ext = self.file_path.suffix.lower()

            if ext == ".toml":
                # Use native TOML parser from bindings
                parsed = ffi_parse_toml_config(content)
                return ConfigData(
                    default_configs=parsed.default_configs or {},
                    contexts=parsed.contexts or [],
                    overrides=parsed.overrides or {},
                    dimensions=parsed.dimensions or {},
                )
            elif ext == ".json":
                # Use JSON parser for JSON files
                import json

                data = json.loads(content)
                return ConfigData(
                    default_configs=data.get("default_configs", {}),
                    contexts=data.get("contexts", []),
                    overrides=data.get("overrides", {}),
                    dimensions=data.get("dimensions", {}),
                )
            else:
                # Try TOML first, then JSON
                try:
                    parsed = ffi_parse_toml_config(content)
                    return ConfigData(
                        default_configs=parsed.default_configs or {},
                        contexts=parsed.contexts or [],
                        overrides=parsed.overrides or {},
                        dimensions=parsed.dimensions or {},
                    )
                except Exception:
                    # Try JSON if TOML fails
                    import json

                    data = json.loads(content)
                    return ConfigData(
                        default_configs=data.get("default_configs", {}),
                        contexts=data.get("contexts", []),
                        overrides=data.get("overrides", {}),
                        dimensions=data.get("dimensions", {}),
                    )
        except Exception as e:
            raise RuntimeError(
                f"Failed to read or parse config file at {self.file_path}: {e}"
            )

    async def fetch_filtered_config(
        self,
        context: Optional[Dict[str, Any]] = None,
        prefix_filter: Optional[List[str]] = None,
    ) -> ConfigData:
        """Fetch configuration with context/prefix filters."""
        config = await self.fetch_config()

        filtered_contexts = config.contexts
        if context:
            filtered_contexts = [
                ctx for ctx in config.contexts if self._matches_context(ctx, context)
            ]

        filtered_configs = config.default_configs
        if prefix_filter:
            filtered_configs = self._filter_by_prefix(
                config.default_configs, prefix_filter
            )

        return ConfigData(
            default_configs=filtered_configs,
            contexts=filtered_contexts,
            overrides=config.overrides,
            dimensions=config.dimensions,
        )

    async def fetch_active_experiments(self) -> Optional[ExperimentData]:
        """Fetch all active experiment data.

        File data source doesn't support experiments.
        """
        return None

    async def fetch_candidate_active_experiments(
        self,
        context: Optional[Dict[str, Any]] = None,
        prefix_filter: Optional[List[str]] = None,
    ) -> Optional[ExperimentData]:
        """Fetch active experiments filtered with partial context matching.

        File data source doesn't support experiments.
        """
        return None

    async def fetch_matching_active_experiments(
        self,
        context: Optional[Dict[str, Any]] = None,
        prefix_filter: Optional[List[str]] = None,
    ) -> Optional[ExperimentData]:
        """Fetch active experiments filtered with exact context matching.

        File data source doesn't support experiments.
        """
        return None

    def supports_experiments(self) -> bool:
        """Check if this data source supports experiments."""
        return False

    async def close(self) -> None:
        """Close and cleanup resources.

        No cleanup needed for file data source.
        """
        pass

    def _matches_context(
        self,
        context: Dict[str, Any],
        filter_dict: Dict[str, Any],
    ) -> bool:
        """Check if context matches filter criteria."""
        for key, value in filter_dict.items():
            if context.get(key) != value:
                return False
        return True

    def _filter_by_prefix(
        self,
        configs: Dict[str, Any],
        prefixes: List[str],
    ) -> Dict[str, Any]:
        """Filter config keys by prefixes."""
        filtered: Dict[str, Any] = {}
        for key, value in configs.items():
            if any(key.startswith(prefix) for prefix in prefixes):
                filtered[key] = value
        return filtered
