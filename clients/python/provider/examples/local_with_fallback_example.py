"""
Local with Fallback Example

Demonstrates using LocalResolutionProvider with HTTP primary source
and File fallback source. If the HTTP server is unavailable, the
provider falls back to reading from a local config file.

Usage:
    python examples/local_with_fallback_example.py

Environment variables (all optional, with defaults shown):
    SUPERPOSITION_ENDPOINT  http://localhost:8080
    SUPERPOSITION_TOKEN     token
    SUPERPOSITION_ORG_ID    localorg
    SUPERPOSITION_WORKSPACE dev
"""

import asyncio
import os
from pathlib import Path
from openfeature.evaluation_context import EvaluationContext

from superposition_provider import (
    HttpDataSource,
    FileDataSource,
    LocalResolutionProvider,
    PollingStrategy,
)


async def sleep(seconds: float):
    """Async sleep helper."""
    await asyncio.sleep(seconds)


async def main():
    print("=== Superposition Fallback + Polling Example ===\n")

    # Get configuration from environment variables
    endpoint = os.environ.get("SUPERPOSITION_ENDPOINT", "http://localhost:8080")
    token = os.environ.get("SUPERPOSITION_TOKEN", "token")
    org_id = os.environ.get("SUPERPOSITION_ORG_ID", "localorg")
    workspace_id = os.environ.get("SUPERPOSITION_WORKSPACE", "dev")

    # Create HTTP data source (primary)
    http_source = HttpDataSource(
        endpoint=endpoint,
        token=token,
        org_id=org_id,
        workspace_id=workspace_id,
    )

    # Create File data source (fallback)
    config_path = Path(__file__).parent / "config.toml"
    print(f"Primary: HTTP ({endpoint})")
    print(f"Fallback: {config_path}")
    print(
        f"Polling every 10s. Printing currency and timeout values (Ctrl-C to stop).\n"
    )

    file_source = FileDataSource(str(config_path))

    # Create provider with both sources and polling strategy
    provider = LocalResolutionProvider(
        primary=http_source,
        fallback=file_source,
        refresh_strategy=PollingStrategy(interval=10),
    )

    try:
        # Initialize provider
        print("Initializing provider...")
        await provider.initialize()
        print("Provider initialized successfully!\n")

        context = EvaluationContext(
            attributes={
                "os": "linux",
                "city": "Berlin",
            },
            targeting_key="user-456",
        )

        print("Starting polling loop...\n")

        # Poll for config changes and print values in a loop
        iteration = 0
        while iteration < 12:  # Run for 1 minute (12 * 5s)
            from datetime import datetime

            timestamp = datetime.now().strftime("%H:%M:%S")

            try:
                currency_result = provider.resolve_string_details(
                    "currency", "USD", context
                )
                print(f"[{timestamp}] currency = {currency_result.value}", end="")
            except Exception as e:
                print(f"[{timestamp}] currency error: {e}", end="")

            try:
                timeout_result = provider.resolve_integer_details(
                    "timeout", 30, context
                )
                print(f"  |  timeout = {timeout_result.value}")
            except Exception as e:
                print(f"  |  timeout error: {e}")

            await sleep(5)
            iteration += 1

        print("\nExample completed!")

    except Exception as e:
        print(f"Error: {e}")
    finally:
        # Cleanup
        await provider.shutdown()


if __name__ == "__main__":
    asyncio.run(main())
