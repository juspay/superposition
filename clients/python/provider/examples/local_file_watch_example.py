"""
Local File Watch Example - Watch a local config file for changes.

Demonstrates file watching capability with automatic reload when the file changes.
Edit the config file in another terminal to see the changes reflected in the running example.
"""

import asyncio
import logging
from pathlib import Path

from openfeature.evaluation_context import EvaluationContext

from superposition_provider import LocalResolutionProvider, FileDataSource
from superposition_provider.types import WatchStrategy

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


async def main():
    """Run the file watch example."""
    # Get the path to the example config file
    example_dir = Path(__file__).parent
    config_path = example_dir / "config.toml"

    logger.info(f"Watching config file: {config_path}")
    logger.info("Edit the file in another terminal to see changes.\n")

    # Create a provider using FileDataSource with WatchStrategy
    provider = LocalResolutionProvider(
        primary_source=FileDataSource(config_path.__str__()),
        refresh_strategy=WatchStrategy(debounce_ms=1000),
    )

    # Initialize the provider
    await provider.initialize(EvaluationContext())

    # Create an evaluation context
    context = EvaluationContext(
        targeting_key=None,
        attributes={
            "os": "linux",
            "city": "Boston",
        },
    )

    logger.info("Polling config every 2 seconds. Press Ctrl-C to stop.\n")

    try:
        # Poll in a loop to show updated values after file changes
        while True:
            config = provider.resolve_all_features(context)
            logger.info(f"Config: {config}")
            await asyncio.sleep(2)
    except KeyboardInterrupt:
        logger.info("\nShutting down...")
    finally:
        await provider.shutdown()
        logger.info("Provider shutdown complete")


if __name__ == "__main__":
    asyncio.run(main())
