"""
Local with Fallback Example - Use HTTP with local file as fallback.

Demonstrates resilience with primary + fallback data sources.
If the HTTP server fails, the provider automatically falls back to local file config.

Prerequisites:
  - Superposition server (optional) at http://localhost:8080
  - config.toml file for fallback
"""

import asyncio
import logging
from pathlib import Path

from openfeature.evaluation_context import EvaluationContext

from superposition_provider import LocalResolutionProvider, HttpDataSource, FileDataSource
from superposition_provider.types import SuperpositionOptions, PollingStrategy

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


async def main():
    """Run the local with fallback example."""
    # Get the path to the example config file
    example_dir = Path(__file__).parent
    config_path = example_dir / "config.toml"

    logger.info("=== Superposition Fallback + Polling Example ===")
    logger.info(f"Primary: HTTP (localhost:8080)")
    logger.info(f"Fallback: {config_path}")
    logger.info("Polling every 10s.\n")

    # Configure Superposition API connection
    options = SuperpositionOptions(
        endpoint="http://localhost:8080",
        token="token",
        org_id="localorg",
        workspace_id="dev",
    )

    # Create a provider with both HTTP (primary) and file (fallback) sources
    provider = LocalResolutionProvider(
        primary_source=HttpDataSource(options),
        refresh_strategy=PollingStrategy(interval=10, timeout=10),
        fallback_source=FileDataSource(config_path.__str__()),
    )

    # Initialize the provider
    await provider.initialize(EvaluationContext())

    # Create an evaluation context
    context = EvaluationContext(
        targeting_key="user-456",
        attributes={
            "os": "linux",
            "city": "Berlin",
        },
    )

    logger.info("Polling config every 5s. Press Ctrl-C to stop.\n")

    try:
        # Poll in a loop to show resolved config
        while True:
            logger.info("Fetching config...")
            config = provider.resolve_all_features(context)
            logger.info(f"Resolved config: {config}")

            # Also show a specific value
            currency = provider.resolve_string_details("currency", "No value", context)
            logger.info(f"Currency: {currency.value}\n")

            await asyncio.sleep(5)
    except KeyboardInterrupt:
        logger.info("\nShutting down...")
    finally:
        await provider.shutdown()
        logger.info("Provider shutdown complete")


if __name__ == "__main__":
    asyncio.run(main())
