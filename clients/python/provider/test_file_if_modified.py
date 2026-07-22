"""FileDataSource honors if_modified_since via the file's last-modified time.

Previously the file source ignored if_modified_since and always re-read/re-parsed. It now returns
NotModified when the file's mtime is at or before the given timestamp, matching the Rust and Java
clients and letting the provider skip re-parsing an unchanged file on every refresh.

Run standalone: python test_file_if_modified.py
"""

import asyncio
import os
import tempfile
from datetime import timedelta
from pathlib import Path

from superposition_provider.file_data_source import FileDataSource

TOML = """
[default-configs]
currency = { value = "Rupee", schema = { type = "string" } }
[dimensions]
city = { position = 1, schema = { type = "string" }, type = "REGULAR" }
[[overrides]]
_context_ = { city = "Boston" }
currency = "Dollar"
"""


def test_unchanged_file_is_not_modified():
    async def run():
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "config.toml"
            path.write_text(TOML)
            source = FileDataSource(str(path))

            fetched_at = (await source.fetch_config(None)).get_data().fetched_at

            # Nothing changed since the read, so a conditional fetch is NotModified.
            response = await source.fetch_config(fetched_at)
            assert response.is_not_modified(), "unchanged file should be NotModified"
            assert response.get_data() is None

    asyncio.run(run())


def test_modified_file_returns_fresh_data():
    async def run():
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "config.toml"
            path.write_text(TOML)
            source = FileDataSource(str(path))

            fetched_at = (await source.fetch_config(None)).get_data().fetched_at

            # Rewrite with a strictly later mtime; the next conditional fetch must re-read.
            path.write_text(TOML.replace('"Rupee"', '"Yen"'))
            later = (fetched_at + timedelta(seconds=2)).timestamp()
            os.utime(path, (later, later))

            response = await source.fetch_config(fetched_at)
            assert not response.is_not_modified(), "an edited file must return fresh data"
            assert response.get_data().data.default_configs["currency"] == '"Yen"'

    asyncio.run(run())


if __name__ == "__main__":
    cases = [v for k, v in sorted(globals().items()) if k.startswith("test_")]
    for case in cases:
        case()
        print(f"ok  {case.__name__}")
    print(f"\n{len(cases)} passed")
