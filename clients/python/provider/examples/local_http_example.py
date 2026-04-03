"""
Local HTTP Example - Load configuration from Superposition API with polling.

Demonstrates HTTP-based configuration fetching with periodic polling.
The provider polls the Superposition server for config updates every 30 seconds.

Prerequisites:
  - Superposition server running at http://localhost:8080
"""

import asyncio
import logging

from openfeature.evaluation_context import EvaluationContext

from superposition_provider import LocalResolutionProvider, HttpDataSource
from superposition_provider.types import SuperpositionOptions, PollingStrategy

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


async def main():
    """Run the HTTP example."""
    # Configure Superposition API connection
    options = SuperpositionOptions(
        endpoint="http://localhost:8080",
        token="token",
        org_id="localorg",
        workspace_id="dev",
    )

    logger.info("Creating HTTP-based provider with polling strategy...")

    # Create a provider using HttpDataSource with PollingStrategy
    provider = LocalResolutionProvider(
        primary_source=HttpDataSource(options),
        refresh_strategy=PollingStrategy(interval=30, timeout=10),
    )

    # Initialize the provider
    await provider.initialize(EvaluationContext())

    # Create an evaluation context with targeting key and dimensions
    context = EvaluationContext(
        targeting_key="user-1234",
        attributes={
            "dimension": "d2",
        },
    )

    # Resolve all features
    logger.info("Resolving all features...")
    all_config = provider.resolve_all_features(context)
    logger.info(f"All config: {all_config}")

    # Resolve applicable experiment variants
    logger.info("Getting applicable variants...")
    variants = await provider.get_applicable_variants(context)
    logger.info(f"Applicable variants: {variants}")

    # Shutdown
    await provider.shutdown()
    logger.info("Provider shutdown complete")


if __name__ == "__main__":
    asyncio.run(main())
