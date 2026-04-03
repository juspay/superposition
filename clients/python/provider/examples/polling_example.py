"""
Polling Example - HTTP polling with environment variable configuration.

Demonstrates the Polling refresh strategy with LocalResolutionProvider
using environment variables for flexible configuration.

Change the config on the server and watch printed values update automatically.

Usage:
  python polling_example.py

Environment variables (all optional, with defaults shown):
  SUPERPOSITION_ENDPOINT      http://localhost:8080
  SUPERPOSITION_TOKEN         token
  SUPERPOSITION_ORG_ID        localorg
  SUPERPOSITION_WORKSPACE     dev
  POLL_INTERVAL               10        (seconds between server polls)
  PRINT_INTERVAL              5         (seconds between printing the value)
  CONFIG_KEY                  max_connections   (the config key to watch)
"""

import asyncio
import logging
import os
from typing import Optional

from openfeature.evaluation_context import EvaluationContext

from superposition_provider import LocalResolutionProvider, HttpDataSource
from superposition_provider.types import SuperpositionOptions, PollingStrategy

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
)
logger = logging.getLogger(__name__)


def env_or(key: str, default: str) -> str:
    """Get environment variable or return default."""
    return os.environ.get(key, default)


async def main():
    """Run the polling example."""
    # Read configuration from environment variables
    endpoint = env_or("SUPERPOSITION_ENDPOINT", "http://localhost:8080")
    token = env_or("SUPERPOSITION_TOKEN", "token")
    org_id = env_or("SUPERPOSITION_ORG_ID", "localorg")
    workspace = env_or("SUPERPOSITION_WORKSPACE", "dev")
    poll_interval = int(env_or("POLL_INTERVAL", "10"))
    print_interval = int(env_or("PRINT_INTERVAL", "5"))
    config_key = env_or("CONFIG_KEY", "max_connections")

    logger.info("=== Superposition Polling Example ===")
    logger.info(f"Endpoint:        {endpoint}")
    logger.info(f"Org / Workspace: {org_id} / {workspace}")
    logger.info(f"Poll interval:   {poll_interval}s")
    logger.info(f"Print interval:  {print_interval}s")
    logger.info(f"Watching key:    {config_key}")
    logger.info("")

    # Create Superposition API options
    options = SuperpositionOptions(
        endpoint=endpoint,
        token=token,
        org_id=org_id,
        workspace_id=workspace,
    )

    # Create provider with polling strategy
    logger.info("Creating provider with polling strategy...")
    provider = LocalResolutionProvider(
        primary_source=HttpDataSource(options),
        refresh_strategy=PollingStrategy(
            interval=poll_interval,
            timeout=10,
        ),
    )

    # Initialize
    await provider.initialize(EvaluationContext())
    logger.info("Provider initialized.\n")

    # Create evaluation context
    context = EvaluationContext(
        targeting_key="polling-example-user",
        attributes={
            "source": "polling_example",
        },
    )

    logger.info("Polling config. Press Ctrl-C to stop.\n")

    try:
        last_value: Optional[str] = None
        iteration = 0

        while True:
            iteration += 1

            # Every print_interval seconds, read and print the watched key
            if iteration % max(1, (print_interval // poll_interval)) == 0:
                try:
                    # Resolve the watched config key
                    result = provider.resolve_all_features(context)

                    if config_key in result:
                        current_value = str(result[config_key])
                        if current_value != last_value:
                            logger.info(f"[UPDATE] {config_key}: {current_value}")
                            last_value = current_value
                        else:
                            logger.info(f"[UNCHANGED] {config_key}: {current_value}")
                    else:
                        logger.warning(f"Key '{config_key}' not found in config")
                        logger.info(f"Available keys: {list(result.keys())}")
                except Exception as e:
                    logger.error(f"Error resolving config: {e}")

            await asyncio.sleep(poll_interval)

    except KeyboardInterrupt:
        logger.info("\n\nShutting down as requested...")
    finally:
        await provider.shutdown()
        logger.info("Cleanup complete. Goodbye!")


if __name__ == "__main__":
    asyncio.run(main())
