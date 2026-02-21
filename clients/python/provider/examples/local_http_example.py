"""
Local HTTP Example

Demonstrates using LocalResolutionProvider with HttpDataSource
to fetch configuration from a Superposition server.

Usage:
    python examples/local_http_example.py

Environment variables (all optional, with defaults shown):
    SUPERPOSITION_ENDPOINT  http://localhost:8080
    SUPERPOSITION_TOKEN     token
    SUPERPOSITION_ORG_ID    localorg
    SUPERPOSITION_WORKSPACE dev
"""

import asyncio
import os
from openfeature.evaluation_context import EvaluationContext

from superposition_provider import (
    HttpDataSource,
    LocalResolutionProvider,
    ManualStrategy,
)


async def main():
    print("=== Superposition Local HTTP Example ===\n")

    # Get configuration from environment variables
    endpoint = os.environ.get("SUPERPOSITION_ENDPOINT", "http://localhost:8080")
    token = os.environ.get("SUPERPOSITION_TOKEN", "token")
    org_id = os.environ.get("SUPERPOSITION_ORG_ID", "localorg")
    workspace_id = os.environ.get("SUPERPOSITION_WORKSPACE", "dev")

    # Create HTTP data source
    http_source = HttpDataSource(
        endpoint=endpoint,
        token=token,
        org_id=org_id,
        workspace_id=workspace_id,
    )

    # Create provider with manual refresh strategy
    provider = LocalResolutionProvider(
        primary=http_source,
        fallback=None,
        refresh_strategy=ManualStrategy(),
    )

    try:
        # Initialize the provider
        print("Initializing provider...")
        await provider.initialize()
        print("Provider initialized successfully!\n")

        # Create evaluation context
        context = EvaluationContext(
            attributes={
                "userId": "1234",
                "region": "us-east-1",
            },
            targeting_key="user-1234",
        )

        # Resolve all features
        print("Resolving all features...")
        all_config = await provider.resolve_all_features(context)
        print(f"All config: {all_config}")
        print()

        # Get applicable variants for experiments
        print("Getting applicable variants...")
        variants = await provider.get_applicable_variants(context)
        print(f"Variants: {variants}")
        print()

        # Demonstrate single flag resolution
        print("Resolving specific flags:")
        bool_result = provider.resolve_boolean_details(
            "feature_enabled", False, context
        )
        print(f"  feature_enabled: {bool_result.value}")

        string_result = provider.resolve_string_details("theme", "default", context)
        print(f"  theme: {string_result.value}")

        int_result = provider.resolve_integer_details("timeout", 30, context)
        print(f"  timeout: {int_result.value}")

    except Exception as e:
        print(f"Error: {e}")
    finally:
        # Cleanup
        print("\nClosing provider...")
        await provider.shutdown()
        print("Done!")


if __name__ == "__main__":
    asyncio.run(main())
