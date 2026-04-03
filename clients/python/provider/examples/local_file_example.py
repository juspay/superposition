"""
Local File Example - Load configuration from a local TOML file.

Demonstrates basic file-based configuration loading with on-demand refresh strategy.
The provider loads the config once and refreshes only when data becomes stale (TTL).
"""

import asyncio
import logging
from pathlib import Path

from openfeature.evaluation_context import EvaluationContext

from superposition_provider import FileDataSource, LocalResolutionProvider
from superposition_provider.types import OnDemandStrategy

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


async def main():
    """Run the local file example."""
    # Get the path to the example config file
    example_dir = Path(__file__).parent
    config_path = example_dir / "config.toml"

    logger.info(f"Loading config from: {config_path}")

    # Create a provider using FileDataSource with OnDemandStrategy
    provider = LocalResolutionProvider(
        primary_source=FileDataSource(config_path.__str__()),
        refresh_strategy=OnDemandStrategy(ttl=60),
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

    # Resolve all features
    config = provider.resolve_all_features(context)
    logger.info(f"Config: {config}")

    # Resolve a specific feature
    timeout = provider.resolve_integer_details("timeout", 0, context)
    currency = provider.resolve_string_details("currency", "No Value", context)
    logger.info(f"Timeout: {timeout.value}, Currency: {currency.value}")

    # Shutdown
    await provider.shutdown()
    logger.info("Provider shutdown complete")


if __name__ == "__main__":
    asyncio.run(main())
