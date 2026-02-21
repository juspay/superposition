"""
Polling Example

Demonstrates the polling refresh strategy with LocalResolutionProvider.

This example connects to a Superposition server via HTTP, polls for config
changes every 10 seconds, and prints a config value in a loop.

Usage:
    python examples/polling_example.py

Environment variables (all optional, with defaults shown):
    SUPERPOSITION_ENDPOINT  http://localhost:8080
    SUPERPOSITION_TOKEN     token
    SUPERPOSITION_ORG_ID    localorg
    SUPERPOSITION_WORKSPACE dev
    POLL_INTERVAL           10        (seconds between server polls)
    PRINT_INTERVAL          5         (seconds between printing the value)
    CONFIG_KEY              max_connections   (the config key to watch)
"""

import asyncio
import os
from datetime import datetime
from openfeature.evaluation_context import EvaluationContext

from superposition_provider import (
    HttpDataSource,
    LocalResolutionProvider,
    PollingStrategy,
)


async def sleep(seconds: float):
    """Async sleep helper."""
    await asyncio.sleep(seconds)


async def main():
    # Get configuration from environment variables
    endpoint = os.environ.get("SUPERPOSITION_ENDPOINT", "http://localhost:8080")
    token = os.environ.get("SUPERPOSITION_TOKEN", "token")
    org_id = os.environ.get("SUPERPOSITION_ORG_ID", "localorg")
    workspace_id = os.environ.get("SUPERPOSITION_WORKSPACE", "dev")
    poll_interval = int(os.environ.get("POLL_INTERVAL", "10"))
    print_interval = int(os.environ.get("PRINT_INTERVAL", "5"))
    config_key = os.environ.get("CONFIG_KEY", "max_connections")

    print("=== Superposition Polling Example ===")
    print(f"Endpoint:        {endpoint}")
    print(f"Org / Workspace: {org_id} / {workspace_id}")
    print(f"Poll interval:   {poll_interval}s")
    print(f"Print interval:  {print_interval}s")
    print(f"Watching key:    {config_key}")
    print()

    # Create HTTP data source
    http_source = HttpDataSource(
        endpoint=endpoint,
        token=token,
        org_id=org_id,
        workspace_id=workspace_id,
    )

    # Create provider with polling strategy
    provider = LocalResolutionProvider(
        primary=http_source,
        fallback=None,
        refresh_strategy=PollingStrategy(interval=poll_interval),
    )

    try:
        # Initialize provider
        print("Initializing provider...")
        await provider.initialize()
        print(
            f"Provider ready. Printing config every {print_interval}s (Ctrl-C to stop).\n"
        )

        context = EvaluationContext(
            attributes={},
            targeting_key="user-123",
        )

        # Print config value in a loop
        iteration = 0
        while iteration < 20:  # Run for ~2 minutes max (20 * 5s)
            timestamp = datetime.now().strftime("%H:%M:%S")

            try:
                result = provider.resolve_integer_details(config_key, 100, context)
                print(f"[{timestamp}] {config_key} = {result.value}")
            except Exception as e:
                print(f"[{timestamp}] Error resolving {config_key}: {e}")

            await sleep(print_interval)
            iteration += 1

        print("\nExample completed!")

    except Exception as e:
        print(f"Error: {e}")
    finally:
        # Cleanup
        await provider.shutdown()


if __name__ == "__main__":
    asyncio.run(main())
