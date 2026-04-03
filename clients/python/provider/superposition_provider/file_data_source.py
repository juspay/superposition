"""
File-based data source for loading configuration from local TOML/JSON files.

Supports watching files for changes and 304 Not Modified via file modification times.
Uses native OS file system notifications (inotify on Linux, FSEvents on macOS, etc.)
via the watchdog library when available, with polling fallback.
"""

import logging
import os
import json
from datetime import datetime, timezone
from typing import Dict, List, Optional, Any, AsyncGenerator
import asyncio

from superposition_bindings.superposition_client import ffi_parse_config_file_with_filters
from superposition_bindings.superposition_types import Config

from watchdog.observers import Observer
from watchdog.events import FileSystemEventHandler

from .data_source import (
    SuperpositionDataSource,
    FetchResponse,
    ConfigData,
    ExperimentData,
)

logger = logging.getLogger(__name__)

class _FileEventHandler(FileSystemEventHandler):
    """Handler for file system events from watchdog."""

    def __init__(self, file_path: str, loop: asyncio.AbstractEventLoop, queue: asyncio.Queue):
        """Initialize handler.

        Args:
            file_path: Path to the file to watch.
            loop: The asyncio event loop to schedule callbacks on.
            queue: The asyncio queue to put change events into.
        """
        super().__init__()
        self.file_path = file_path
        self._loop = loop
        self._queue = queue

    def on_modified(self, event):
        """Called when a file is modified (from watchdog's background thread)."""
        if event.is_directory:
            return

        # Check if the modified file matches our watched file
        if os.path.abspath(event.src_path) == os.path.abspath(self.file_path):
            logger.info(f"File changed (watchdog event): {self.file_path}")
            # Thread-safe: schedule the queue put from the watchdog thread
            self._loop.call_soon_threadsafe(self._queue.put_nowait, self.file_path)

def _parse_config_file(
    content: str,
    file_format: str,
    context: Optional[Dict[str, Any]] = None,
    prefix_filter: Optional[List[str]] = None
) -> Config:
    """Parse TOML or JSON configuration file.

    Uses FFI functions ffi_parse_config_file_with_filters
    from superposition_core for proper parsing.
    """
    try:
        query_data = {k: json.dumps(v) for k, v in context.items()} if context else None
        return ffi_parse_config_file_with_filters(content, file_format, query_data, prefix_filter)
    except Exception as e:
        logger.error(f"Failed to parse config file: {e}")
        raise

class FileDataSource(SuperpositionDataSource):
    """File-based data source for configuration and experiment data.

    Loads configuration from local TOML or JSON files. Supports:
    - File modification time-based caching (304 Not Modified)
    - File watching for automatic reload
    - Fallback configurations
    """

    def __init__(
        self,
        file_path: Optional[str] = None,
    ):
        """Initialize file data source.

        Args:
            file_path: Path to configuration TOML/JSON file.
        """
        self._watch_task = None
        self.file_path = file_path
        if file_path.endswith('.toml'):
            self.file_format = "toml"
        elif file_path.endswith('.json'):
            self.file_format = "json"
        else:
            raise ValueError(f"Unsupported file format: {file_path}")

    async def _fetch_config_with_filters(
        self,
        context: Optional[Dict[str, Any]] = None,
        prefix_filter: Optional[List[str]] = None,
        if_modified_since: Optional[datetime] = None,
    ) -> FetchResponse[ConfigData]:
        """Fetch configuration from file, applying filters and 304 Not Modified logic.
        Args:
            context: Optional context for filtering (ignored).
            prefix_filter: Optional key prefixes to include.
            if_modified_since: Timestamp for 304 Not Modified check.
        """
        if if_modified_since is not None:
            logger.debug("FileDataSource: ignoring if_modified_since, always reading fresh from file")

        try:
            now = datetime.now(timezone.utc)
            # Read and parse file
            with open(self.file_path, 'r') as f:
                content = f.read()

            config = _parse_config_file(content, self.file_format, context, prefix_filter)

            return FetchResponse.data(ConfigData(
                data=config,
                fetched_at=now,
            ))
        except Exception as e:
            logger.error(f"Failed to fetch config from {self.file_path}: {e}")
            raise

    async def fetch_config(
        self,
        if_modified_since: Optional[datetime] = None,
    ) -> FetchResponse[ConfigData]:
        """Fetch configuration from file.

        Args:
            if_modified_since: Timestamp for 304 Not Modified check.

        Returns:
            FetchResponse with ConfigData or NotModified status.
        """
        return await self._fetch_config_with_filters(if_modified_since=if_modified_since)

    async def fetch_filtered_config(
        self,
        context: Optional[Dict[str, Any]] = None,
        prefix_filter: Optional[List[str]] = None,
        if_modified_since: Optional[datetime] = None,
    ) -> FetchResponse[ConfigData]:
        """Fetch configuration, optionally filtered.

        Note: File-based filtering is not efficient; consider using HttpDataSource
        for production configurations that need filtering.

        Args:
            context: Optional context for filtering (ignored).
            prefix_filter: Optional key prefixes to include.
            if_modified_since: Timestamp for 304 Not Modified check.

        Returns:
            FetchResponse with ConfigData or NotModified status.
        """
        return await self._fetch_config_with_filters(context, prefix_filter, if_modified_since)

    async def fetch_active_experiments(
        self,
        if_modified_since: Optional[datetime] = None,
    ) -> FetchResponse[ExperimentData]:
        """Fetch experiments from file.

        Args:
            if_modified_since: Timestamp for 304 Not Modified check.

        Returns:
            FetchResponse with ExperimentData or NotModified status.
        """
        raise NotImplementedError("Experiments not supported by FileDataSource")

    async def fetch_candidate_active_experiments(
        self,
        context: Optional[Dict[str, Any]] = None,
        prefix_filter: Optional[List[str]] = None,
        if_modified_since: Optional[datetime] = None,
    ) -> FetchResponse[ExperimentData]:
        """Fetch candidate active experiments."""
        raise NotImplementedError("Experiments not supported by FileDataSource")

    async def fetch_matching_active_experiments(
        self,
        context: Optional[Dict[str, Any]] = None,
        prefix_filter: Optional[List[str]] = None,
        if_modified_since: Optional[datetime] = None,
    ) -> FetchResponse[ExperimentData]:
        """Fetch matching active experiments."""
        raise NotImplementedError("Experiments not supported by FileDataSource")

    def supports_experiments(self) -> bool:
        """File source supports experiments if path is configured."""
        return False

    async def watch(self) -> Optional[AsyncGenerator[str, None]]:
        """Set up file watching for changes using native file system notifications.

        Uses watchdog library which provides:
        - inotify on Linux
        - FSEvents on macOS
        - ReadDirectoryChangesW on Windows

        Returns:
            Async generator yielding changed file paths.
        """
        if not self.file_path:
            return

        try:
            # Create a queue to bridge sync events to async
            event_queue: asyncio.Queue = asyncio.Queue()
            loop = asyncio.get_running_loop()

            # Set up watchdog observer
            self._watch_task = Observer()
            event_handler = _FileEventHandler(self.file_path, loop, event_queue)

            # Watch the directory containing the file
            watch_dir = os.path.dirname(os.path.abspath(self.file_path))
            self._watch_task.schedule(event_handler, watch_dir, recursive=False)
            self._watch_task.start()

            logger.info(f"Watching file: {self.file_path} using inotify (watchdog)")

            # Yield events as they arrive
            while True:
                try:
                    # Wait for file change event (with timeout to allow cancellation)
                    changed_path = await asyncio.wait_for(
                        event_queue.get(),
                        timeout=5.0
                    )
                    yield changed_path
                except asyncio.TimeoutError:
                    # No events, continue watching
                    continue
                except asyncio.CancelledError:
                    logger.debug("File watch cancelled")
                    break
        finally:
            # Clean up observer
            if self._watch_task:
                self._watch_task.stop()
                self._watch_task.join(timeout=1.0)
            logger.debug("File watcher stopped")

    async def close(self) -> None:
        """Stop watching and clean up resources."""
        if self._watch_task:
            self._watch_task.stop()
            self._watch_task.join(timeout=1.0)
            self._watch_task = None
