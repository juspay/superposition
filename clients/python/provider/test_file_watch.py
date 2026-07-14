"""FileDataSource supports several concurrent watchers, like the Java and Rust clients.

One OS watcher, N subscriber queues. watch() used to build a new Observer per call and keep it in
a single field, so a second caller overwrote the first: when either generator exited, its `finally`
stopped whichever observer happened to be in the field, killing the other subscriber's watcher and
orphaning its own — a leaked OS handle and a subscriber that silently went deaf.

Run standalone: python test_file_watch.py
"""

import asyncio
import tempfile
from pathlib import Path

from superposition_provider.file_data_source import FileDataSource

TOML = """
[default-configs]
currency = { value = "Rupee", schema = { type = "string" } }
[dimensions]
city = { position = 1, schema = { type = "string" }, type = "REGULAR" }
"""


async def _drive(generators, path: Path, body: str, timeout: float = 20.0):
    """Advance each generator by one event, by making a change they will all see."""
    pending = [asyncio.ensure_future(g.__anext__()) for g in generators]
    await asyncio.sleep(0.4)  # let the watcher settle before touching the file
    path.write_text(body)
    return [await asyncio.wait_for(p, timeout=timeout) for p in pending]


def test_every_subscriber_sees_every_change():
    async def run():
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "config.toml"
            path.write_text(TOML)
            source = FileDataSource(str(path))

            first = source.watch()
            second = source.watch()

            seen = await _drive([first, second], path, TOML + "\n# one\n")
            assert all(str(path) in s for s in seen), "a subscriber missed the change"

            # One OS watcher shared by both, not one each.
            assert len(source._subscribers) == 2
            assert source._observer is not None and source._observer.is_alive()

            await first.aclose()
            await second.aclose()
            await source.close()

    asyncio.run(run())


def test_closing_one_subscriber_leaves_the_other_working():
    async def run():
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "config.toml"
            path.write_text(TOML)
            source = FileDataSource(str(path))

            first = source.watch()
            second = source.watch()
            await _drive([first, second], path, TOML + "\n# one\n")

            observer = source._observer
            await first.aclose()

            # The shared watcher must survive while a subscriber remains.
            assert len(source._subscribers) == 1, "closing one subscriber dropped the other"
            assert source._observer is observer and observer.is_alive(), (
                "closing one subscriber stopped the shared watcher"
            )

            # ...and the survivor still gets events.
            seen = await _drive([second], path, TOML + "\n# two\n")
            assert str(path) in seen[0]

            await second.aclose()
            await source.close()

    asyncio.run(run())


def test_the_watcher_stops_once_the_last_subscriber_leaves():
    async def run():
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "config.toml"
            path.write_text(TOML)
            source = FileDataSource(str(path))

            first = source.watch()
            second = source.watch()
            await _drive([first, second], path, TOML + "\n# one\n")
            observer = source._observer

            await first.aclose()
            assert observer.is_alive()

            await second.aclose()
            assert source._observer is None, "the watcher outlived its last subscriber"
            assert not observer.is_alive(), "the OS handle was leaked"

            await source.close()

    asyncio.run(run())


if __name__ == "__main__":
    cases = [v for k, v in sorted(globals().items()) if k.startswith("test_")]
    for case in cases:
        case()
        print(f"ok  {case.__name__}")
    print(f"\n{len(cases)} passed")
