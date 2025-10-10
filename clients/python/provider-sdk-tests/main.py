import asyncio
import platform
import traceback

from openfeature import api
from openfeature.evaluation_context import EvaluationContext
from smithy_core.documents import Document
from superposition_provider.provider import SuperpositionProvider
from superposition_provider.types import (
    ExperimentationOptions,
    PollingStrategy,
    SuperpositionProviderOptions,
)
from superposition_sdk.client import (
    Config,
    Superposition,
)
from superposition_sdk.models import (
    CreateContextInput,
    CreateDefaultConfigInput,
    CreateDimensionInput,
    CreateWorkspaceInput,
    DimensionTypeLOCAL_COHORT,
)

ORG_ID = "localorg"
WORKSPACE_ID = "pyprovidertest"

SUPERPOSITION_SDK_CONFIG = {
    "endpoint": "http://localhost:8080",
    "token": {
        "token": "12131",
    },
}


async def create_workspace(client, org_id: str, workspace_name: str):
    input_data = CreateWorkspaceInput(
        org_id=org_id,
        workspace_name=workspace_name,
        workspace_admin_email="test@tests.com",
        workspace_status="ENABLED",
        strict_mode=True,
        allow_experiment_self_approval=True,
        auto_populate_control=True,
    )
    try:
        response = await client.create_workspace(input_data)
        print("Workspace created!\n", response)
    except Exception as e:
        print("An exception occurred while creating a workspace: ", e)
        raise e


async def create_dimensions(client, org_id: str, workspace_id: str):
    dimensions = [
        CreateDimensionInput(
            workspace_id=workspace_id,
            org_id=org_id,
            dimension="name",
            position=1,
            schema={
                "type": Document("string"),
            },
            description="customer name dimension",
            change_reason="adding name dimension",
        ),
        CreateDimensionInput(
            workspace_id=workspace_id,
            org_id=org_id,
            dimension="city",
            position=2,
            schema={
                "type": Document("string"),
            },
            description="city dimension",
            change_reason="adding city dimension",
        ),
        CreateDimensionInput(
            workspace_id=workspace_id,
            org_id=org_id,
            dimension="customers",
            position=3,
            schema={
                "type": Document("string"),
                "enum": Document(["platinum", "gold", "otherwise"]),
                "definitions": Document(
                    {
                        "platinum": {"in": [{"var": "name"}, ["Agush", "Sauyav"]]},
                        "gold": {"in": [{"var": "name"}, ["Angit", "Bhrey"]]},
                    }
                ),
            },
            description="customers dimension",
            change_reason="adding customers dimension",
            dimension_type=DimensionTypeLOCAL_COHORT("name"),
        ),
    ]

    print("Creating dimensions:")
    for dimension in dimensions:
        print(f"dimension request: {dimension}")
        try:
            response = await client.create_dimension(dimension)
            print(f"  - Created dimension: {response.dimension}")
        except Exception as e:
            print(f"Error occurred while creating dimension: {dimension}", e)
            traceback.print_exc()
            raise e


async def create_default_configs(client, org_id: str, workspace_id: str):
    configs = [
        {
            "key": "price",
            "value": Document(10000),
            "schema": {
                "type": Document("number"),
                "minimum": Document(0),
            },
            "description": "price as a positive number",
            "change_reason": "adding price config",
            "workspace_id": workspace_id,
            "org_id": org_id,
        },
        {
            "key": "currency",
            "value": Document("Rupee"),
            "schema": {
                "type": Document("string"),
                "enum": Document(["Rupee", "Dollar", "Euro"]),
            },
            "description": "currency as an enum",
            "change_reason": "adding currency config",
            "workspace_id": workspace_id,
            "org_id": org_id,
        },
    ]

    print("Creating default configs:")
    for config_data in configs:
        input_data = CreateDefaultConfigInput(**config_data)
        try:
            response = await client.create_default_config(input_data)
            print(f"  - Created config: {response.key}")
        except Exception as e:
            print(f"Error occurred while creating config: {config_data['key']}", e)
            raise e


async def create_overrides(client, org_id: str, workspace_id: str):
    overrides = [
        {
            "workspace_id": workspace_id,
            "org_id": org_id,
            "context": {
                "city": Document("Boston"),
            },
            "override": {
                "currency": Document("Dollar"),
            },
            "description": "Bostonian",
            "change_reason": "testing",
        },
        {
            "workspace_id": workspace_id,
            "org_id": org_id,
            "context": {
                "city": Document("Berlin"),
            },
            "override": {
                "currency": Document("Euro"),
            },
            "description": "Berlin",
            "change_reason": "testing",
        },
        {
            "workspace_id": workspace_id,
            "org_id": org_id,
            "context": {"customers": Document("platinum")},
            "override": {
                "price": Document(5000),
            },
            "description": "platinum customer",
            "change_reason": "testing",
        },
        {
            "workspace_id": workspace_id,
            "org_id": org_id,
            "context": {
                "customers": Document("gold"),
            },
            "override": {
                "price": Document(8000),
            },
            "description": "gold customers",
            "change_reason": "testing",
        },
        {
            "workspace_id": workspace_id,
            "org_id": org_id,
            "context": {
                "name": Document("karbik"),
                "customers": Document("otherwise"),
            },
            "override": {
                "price": Document(1),
            },
            "description": "edge case customer karbik",
            "change_reason": "testing",
        },
    ]

    print("Creating overrides:")
    for override in overrides:
        input_data = CreateContextInput(**override)
        try:
            response = await client.create_context(input_data)
            print(f"Created override: {response}")
        except Exception as e:
            print(f"Error occurred while creating override: {override}", e)
            raise e


async def setup_with_sdk(client, org_id: str, workspace_id: str):
    print("\n=== Setting up test environment ===\n")
    await create_workspace(client, org_id, workspace_id)
    await create_dimensions(client, org_id, workspace_id)
    await create_default_configs(client, org_id, workspace_id)
    await create_overrides(client, org_id, workspace_id)
    print("\n=== Setup complete ===\n")


async def run_demo(org_id: str, workspace_id: str):
    provider_options = SuperpositionProviderOptions(
        refresh_strategy=PollingStrategy(
            interval=5,  # Poll every 5 seconds
            timeout=3,
        ),
        experimentation_options=ExperimentationOptions(
            refresh_strategy=PollingStrategy(
                interval=5,  # Poll every 5 seconds
                timeout=3,  # Timeout after 3 seconds
            )
        ),
        fallback_config=None,
        evaluation_cache_options=None,
        endpoint="http://localhost:8080",
        token="12345678",
        org_id=org_id,
        workspace_id=workspace_id,
    )

    try:
        print("\n=== Starting OpenFeature tests ===\n")
        print(f"Running on CPU architecture: {platform.machine()}")

        provider = SuperpositionProvider(provider_options)
        print("Provider created successfully")

        # Initialize the provider
        await provider.initialize()
        api.set_provider(provider)
        print("Provider initialized successfully\n")

        client = api.get_client()

        # Test 1: Default values (no context)
        print("Test 1: Default values (no context)")
        evaluation_context = EvaluationContext(attributes={})
        price = client.get_integer_value("price", 0, evaluation_context)
        currency = client.get_string_value("currency", "", evaluation_context)

        assert price == 10000, "Default price should be 10000"
        assert currency == "Rupee", "Default currency should be Rupee"
        print("  ✓ Test passed\n")

        # Test 2: Platinum customer - Agush, no city
        print("Test 2: Platinum customer - Agush (no city)")
        evaluation_context = EvaluationContext(attributes={"name": "Agush"})
        price = client.get_integer_value("price", 0, evaluation_context)
        currency = client.get_string_value("currency", "", evaluation_context)

        assert price == 5000, "Price should be default 5000 (platinum customer)"
        assert currency == "Rupee", "Currency should be default Rupee"
        print("  ✓ Test passed\n")

        # Test 3: Platinum customer - Sauyav, no city
        print("Test 3: Platinum customer - Sauyav (no city)")
        evaluation_context = EvaluationContext(attributes={"name": "Sauyav", "city": "Boston"})
        price = client.get_integer_value("price", 0, evaluation_context)
        currency = client.get_string_value("currency", "", evaluation_context)

        assert price == 5000, "Price should be 5000"
        assert currency == "Dollar", "Currency should be dollar"
        print("  ✓ Test passed\n")

        print("Test 4: Regular customer - John (no city)")
        evaluation_context = EvaluationContext(attributes={"name": "John"})
        price = client.get_integer_value("price", 0, evaluation_context)
        currency = client.get_string_value("currency", "", evaluation_context)

        assert price == 10000, "Price should be default 10000"
        assert currency == "Rupee", "Currency should be default Rupee"
        print("  ✓ Test passed\n")

        print("Test 5: Platinum customer - Sauyav with city Berlin")
        evaluation_context = EvaluationContext(attributes={"name": "Sauyav", "city": "Berlin"})
        price = client.get_integer_value("price", 0, evaluation_context)
        currency = client.get_string_value("currency", "", evaluation_context)

        assert price == 5000, "Price should be 5000"
        assert currency == "Euro", "Currency should be Euro in Berlin"
        print("  ✓ Test passed\n")

        print("Test 6: Regular customer - John with city Boston")
        evaluation_context = EvaluationContext(attributes={"name": "John", "city": "Boston"})
        price = client.get_integer_value("price", 0, evaluation_context)
        currency = client.get_string_value("currency", "", evaluation_context)

        assert price == 10000, "Price should be default 10000"
        assert currency == "Dollar", "Currency should be Dollar in Boston"
        print("  ✓ Test passed\n")

        print("Test 7: Edge case customer - karbik (specific override)")
        evaluation_context = EvaluationContext(attributes={"name": "karbik"})
        price = client.get_integer_value("price", 0, evaluation_context)
        currency = client.get_string_value("currency", "", evaluation_context)

        assert price == 1, "Price should be 1 for karbik"
        assert currency == "Rupee", "Currency should be default Rupee"
        print("  ✓ Test passed\n")

        print("Test 8: Edge case customer - karbik with city Boston")
        evaluation_context = EvaluationContext(attributes={"name": "karbik", "city": "Boston"})
        price = client.get_integer_value("price", 0, evaluation_context)
        currency = client.get_string_value("currency", "", evaluation_context)

        assert price == 1, "Price should be 1 for karbik"
        assert currency == "Dollar", "Currency should be Dollar in Boston"
        print("  ✓ Test passed\n")

        print("\n=== All tests passed! ===\n")
    except Exception as error:
        print(f"\n❌ Error running tests: {error}")
        raise error
    finally:
        print("OpenFeature closed successfully")


async def main():
    print("Starting Superposition OpenFeature demo and tests (Python)...")
    try:
        config = Config(endpoint_uri=SUPERPOSITION_SDK_CONFIG["endpoint"])
        client = Superposition(config)
        await setup_with_sdk(client, ORG_ID, WORKSPACE_ID)
        await run_demo(ORG_ID, WORKSPACE_ID)
    except Exception as error:
        print(f"\n❌ Test suite failed: {error}")
        exit(1)


if __name__ == "__main__":
    asyncio.run(main())
