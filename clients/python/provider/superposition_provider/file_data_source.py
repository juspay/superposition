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
from watchdog.observers.api import BaseObserver
from watchdog.events import FileSystemEventHandler

from .data_source import (
    SuperpositionDataSource,
    FetchResponse,
    ConfigData,
    ExperimentData,
)
from .errors import SuperpositionError

logger = logging.getLogger(__name__)


def _stop_observer(observer: Optional[BaseObserver]) -> None:
    """Release a watchdog Observer's OS handle, whether or not it ever started.

    The join is guarded. If setup failed before ``start()`` — ``schedule()`` raises OSError on
    inotify when the directory is missing — then joining a thread that was never started raises
    ``RuntimeError: cannot join thread before it is started``. Raised from a ``finally``, that
    would *replace* the real error, and the caller would be told their thread is unstartable
    rather than that their directory does not exist.
    """
    if observer is None:
        return
    try:
        observer.stop()
        if observer.is_alive():
            observer.join(timeout=1.0)
    except Exception as e:  # cleanup must never mask the failure that triggered it
        logger.warning(f"Error stopping file watcher: {e}")


class _FileEventHandler(FileSystemEventHandler):
    """Fans every change to the watched file out to all live subscribers.

    One handler serves every subscriber: a single OS watcher, N queues.
    An Observer per subscriber would mean several watchers registered on the same
    directory, which is both wasteful and unreliable.
    """

    def __init__(
        self,
        file_path: str,
        loop: asyncio.AbstractEventLoop,
        subscribers: List[asyncio.Queue],
    ):
        """Initialize handler.

        Args:
            file_path: Path to the file to watch.
            loop: The asyncio event loop to schedule callbacks on.
            subscribers: Live subscriber queues; read on each event, so late joiners are served.
        """
        super().__init__()
        self.file_path = file_path
        self._loop = loop
        self._subscribers = subscribers

    def on_modified(self, event):
        """Called when a file is modified (from watchdog's background thread)."""
        if event.is_directory:
            return

        if os.path.realpath(event.src_path) != os.path.realpath(self.file_path):
            return

        logger.info(f"File changed (watchdog event): {self.file_path}")
        # Thread-safe: schedule the queue puts from the watchdog thread.
        for queue in list(self._subscribers):
            self._loop.call_soon_threadsafe(queue.put_nowait, self.file_path)

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
        self._observer: Optional[BaseObserver] = None
        self._subscribers: List[asyncio.Queue] = []
        self.file_path = file_path
        if file_path.endswith('.toml'):
            self.file_format = "toml"
        elif file_path.endswith('.json'):
            self.file_format = "json"
        else:
            raise SuperpositionError.data_source_error(
                f"Unsupported file extension: {file_path}. Supported formats are 'json' and 'toml'."
            )

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
        if if_modified_since is not None and self._is_not_modified(if_modified_since):
            logger.debug(f"FileDataSource: config file not modified since {if_modified_since}")
            return FetchResponse.not_modified()

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
        except SuperpositionError:
            raise
        except Exception as e:
            logger.error(f"Failed to fetch config from {self.file_path}: {e}")
            raise SuperpositionError.data_source_error(
                f"Failed to read config file {self.file_path}: {e}", e
            ) from e

    def _last_modified_at(self) -> datetime:
        """The file's last-modified time."""
        try:
            mtime = os.path.getmtime(self.file_path)
        except OSError as e:
            raise SuperpositionError.data_source_error(
                f"Failed to read modified time for config file {self.file_path}: {e}", e
            ) from e
        return datetime.fromtimestamp(mtime, tz=timezone.utc)

    def _is_not_modified(self, if_modified_since: datetime) -> bool:
        """Whether the file is unchanged since ``if_modified_since`` (mtime at or before it)."""
        return self._last_modified_at() <= if_modified_since

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
        raise SuperpositionError.data_source_error("Experiments not supported by FileDataSource")

    async def fetch_candidate_active_experiments(
        self,
        context: Optional[Dict[str, Any]] = None,
        prefix_filter: Optional[List[str]] = None,
        if_modified_since: Optional[datetime] = None,
    ) -> FetchResponse[ExperimentData]:
        """Fetch candidate active experiments."""
        raise SuperpositionError.data_source_error("Experiments not supported by FileDataSource")

    async def fetch_matching_active_experiments(
        self,
        context: Optional[Dict[str, Any]] = None,
        prefix_filter: Optional[List[str]] = None,
        if_modified_since: Optional[datetime] = None,
    ) -> FetchResponse[ExperimentData]:
        """Fetch matching active experiments."""
        raise SuperpositionError.data_source_error("Experiments not supported by FileDataSource")

    def supports_experiments(self) -> bool:
        """File source supports experiments if path is configured."""
        return False

    async def watch(self) -> Optional[AsyncGenerator[str, None]]:
        """Watch the file for changes, yielding its path on every change.

        Every caller gets its own stream of events. They share one OS watcher, which starts with
        the first subscriber and stops when the last one leaves.

        Returns:
            Async generator yielding changed file paths.
        """
        if not self.file_path:
            return

        event_queue: asyncio.Queue = asyncio.Queue()
        self._subscribers.append(event_queue)
        try:
            self._ensure_observer()
            logger.info(f"Watching file: {self.file_path} (subscribers: {len(self._subscribers)})")

            while True:
                try:
                    # Time-limited so a cancelled consumer is noticed promptly.
                    changed_path = await asyncio.wait_for(event_queue.get(), timeout=5.0)
                    yield changed_path
                except asyncio.TimeoutError:
                    continue
                except asyncio.CancelledError:
                    logger.debug("File watch cancelled")
                    break
        finally:
            if event_queue in self._subscribers:
                self._subscribers.remove(event_queue)
            # The watcher exists for the subscribers; with none left it is just an open handle.
            if not self._subscribers:
                observer, self._observer = self._observer, None
                _stop_observer(observer)
                logger.debug("Last subscriber left, file watcher stopped")

    def _ensure_observer(self) -> None:
        """Start the shared watcher, unless a previous subscriber already did.

        The field is published only once ``start()`` has succeeded, so a failure here leaves
        nothing half-registered for ``close()`` — or the next subscriber — to trip over.
        """
        if self._observer is not None:
            return

        loop = asyncio.get_running_loop()
        handler = _FileEventHandler(self.file_path, loop, self._subscribers)
        # realpath, so the directory we register matches the paths the OS reports back.
        watch_dir = os.path.dirname(os.path.realpath(self.file_path))

        observer = Observer()
        try:
            observer.schedule(handler, watch_dir, recursive=False)
            observer.start()
        except Exception as e:
            _stop_observer(observer)
            raise SuperpositionError.data_source_error(
                f"Failed to watch {self.file_path}: {e}", e
            ) from e

        self._observer = observer

    async def close(self) -> None:
        """Stop watching and clean up resources."""
        observer, self._observer = self._observer, None
        self._subscribers.clear()
        _stop_observer(observer)
