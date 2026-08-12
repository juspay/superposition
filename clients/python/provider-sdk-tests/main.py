import asyncio
import inspect
import platform
import traceback

from openfeature import api
from openfeature.evaluation_context import EvaluationContext
from smithy_core.documents import Document

from superposition_provider import LocalResolutionProvider, HttpDataSource, SuperpositionOptions, PollingStrategy, \
    OnDemandStrategy, SuperpositionAPIProvider, FileDataSource, TokenAuth
from superposition_sdk.client import (
    Config,
    Superposition,
)
from superposition_sdk.models import (
    ContextPut,
    CreateContextInput,
    CreateDefaultConfigInput,
    CreateDimensionInput,
    CreateExperimentInput,
    CreateOrganisationInput,
    CreateWorkspaceInput,
    DimensionTypeLOCAL_COHORT,
    Variant,
    RampExperimentInput,
)

WORKSPACE_ID = "pyprovidertest"

SUPERPOSITION_SDK_CONFIG = {
    "endpoint": "http://localhost:8080",
    "token": {
        "token": "12131",
    },
}


async def create_organisation(client) -> str:
    input_data = CreateOrganisationInput(
        name="pytestorg", admin_email="admin@pytestorg.com"
    )
    try:
        response = await client.create_organisation(input_data)
        print(
            f"Organisation created successfully: {response.name} with ID: {response.id}"
        )
        return response.id
    except Exception as e:
        print("An exception occurred while creating organization:", e)
        raise e


async def create_workspace(client, org_id: str, workspace_name: str):
    input_data = CreateWorkspaceInput(
        org_id=org_id,
        workspace_name=workspace_name,
        workspace_admin_email="test@tests.com",
        workspace_status="ENABLED",
        allow_experiment_self_approval=True,
        auto_populate_control=False, # disable auto populate control for testing experiment
        enable_context_validation=True,
        enable_change_reason_validation=True,
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
            position=1,
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
            "request": ContextPut(
                context={
                    "city": Document("Boston"),
                },
                override={
                    "currency": Document("Dollar"),
                },
                description="Bostonian",
                change_reason="testing",
            )
        },
        {
            "workspace_id": workspace_id,
            "org_id": org_id,
            "request": ContextPut(
                context={
                    "city": Document("Berlin"),
                },
                override={
                    "currency": Document("Euro"),
                },
                description="Berlin",
                change_reason="testing",
            )
        },
        {
            "workspace_id": workspace_id,
            "org_id": org_id,
            "request": ContextPut(
                context={
                    "customers": Document("platinum")
                },
                override={
                    "price": Document(5000),
                },
                description="platinum customer",
                change_reason="testing",
            )
        },
        {
            "workspace_id": workspace_id,
            "org_id": org_id,
            "request": ContextPut(
                context={
                    "customers": Document("gold"),
                },
                override={
                    "price": Document(8000),
                },
                description="gold customers",
                change_reason="testing",
            )
        },
        {
            "workspace_id": workspace_id,
            "org_id": org_id,
            "request": ContextPut(
                context={
                    "name": Document("karbik"),
                },
                override={
                    "price": Document(1),
                },
                description="edge case customer karbik",
                change_reason="testing",
            )
        },
    ]

    print("Creating overrides:")
    for override in overrides:
        input_data = CreateContextInput(**override)
        try:
            response = await client.create_context(input_data)
            print(f"Created override: {response.value}")
        except Exception as e:
            print(f"Error occurred while creating override: {override}", e)
            raise e

async def create_experiments(client, org_id: str, workspace_id: str):
    experiments = [
        {
            "workspace_id": workspace_id,
            "org_id": org_id,
            "name": "Test Experiment",
            "context": {
                "city": Document("Kolkata"),
            },
            "variants": [
                Variant(
                    id="test-control",
                    variant_type="CONTROL",
                    overrides={
                        "price": Document(8000) # Note: Using a different price to distinguish from default
                    }
                ),
                Variant(
                    id="test-experimental",
                    variant_type="EXPERIMENTAL",
                    overrides={
                        "price": Document(7000)
                    }
                )
            ],
            "description": "A test experiment for Kolkata customers",
            "change_reason": "adding test experiment",
        }
    ]

    print("Creating experiments:")
    for experiment in experiments:
        input_data = CreateExperimentInput(**experiment)
        try:
            response = await client.create_experiment(input_data)
            print(f"  - Created experiment: {response.id}")
            ramp_input = {
                "workspace_id": workspace_id,
                "org_id": org_id,
                "id": response.id,
                "change_reason": "ramp the experiment",
                "traffic_percentage": 50,
            }
            ramp_input_ = RampExperimentInput(**ramp_input)
            await client.ramp_experiment(ramp_input_)
            print(f"  - Ramped experiment to 50% traffic: {response.id}")

        except Exception as e:
            print(f"Error occurred while creating experiment: {experiment}", e)
            raise e



async def setup_with_sdk(client, org_id: str, workspace_id: str):
    print("\n=== Setting up test environment ===\n")
    await create_workspace(client, org_id, workspace_id)
    await create_dimensions(client, org_id, workspace_id)
    await create_default_configs(client, org_id, workspace_id)
    await create_overrides(client, org_id, workspace_id)
    await create_experiments(client, org_id, workspace_id)
    print("\n=== Setup complete ===\n")


async def run_provider_tests(label, provider, test_experiments=True, is_async=False):
    """Runs the shared scenario suite against a provider.

    ``is_async`` selects the async OpenFeature client methods (needed for the
    remote provider, which resolves over the network); ``test_experiments`` gates
    the experiment case (a file-backed source carries no experiments).
    """
    print(f"\n=== Starting OpenFeature tests: {label} ===\n")
    print(f"Running on CPU architecture: {platform.machine()}")

    try:
        # LocalResolutionProvider.initialize is async and must be awaited before
        # resolving; SuperpositionAPIProvider.initialize is synchronous. Await only
        # when the provider hands back an awaitable.
        maybe = provider.initialize(EvaluationContext())
        if inspect.isawaitable(maybe):
            await maybe
        api.set_provider(provider)
        print("Provider initialized successfully\n")

        client = api.get_client()

        async def price_for(ctx):
            if is_async:
                return await client.get_integer_value_async("price", 0, ctx)
            return client.get_integer_value("price", 0, ctx)

        async def currency_for(ctx):
            if is_async:
                return await client.get_string_value_async("currency", "", ctx)
            return client.get_string_value("currency", "", ctx)

        # Test 1: Default values (no context)
        print("Test 1: Default values (no context)")
        ctx = EvaluationContext()
        assert await price_for(ctx) == 10000, "Default price should be 10000"
        assert await currency_for(ctx) == "Rupee", "Default currency should be Rupee"
        print("  ✓ Test passed\n")

        # Test 2: Platinum customer - Agush (no city)
        print("Test 2: Platinum customer - Agush (no city)")
        ctx = EvaluationContext(attributes={"name": "Agush"})
        assert await price_for(ctx) == 5000, "Price should be 5000 (platinum customer)"
        assert await currency_for(ctx) == "Rupee", "Currency should be default Rupee"
        print("  ✓ Test passed\n")

        # Test 3: Platinum customer - Sauyav with city Boston
        print("Test 3: Platinum customer - Sauyav with city Boston")
        ctx = EvaluationContext(attributes={"name": "Sauyav", "city": "Boston"})
        assert await price_for(ctx) == 5000, "Price should be 5000"
        assert await currency_for(ctx) == "Dollar", "Currency should be Dollar"
        print("  ✓ Test passed\n")

        # Test 4: Regular customer - John (no city)
        print("Test 4: Regular customer - John (no city)")
        ctx = EvaluationContext(attributes={"name": "John"})
        assert await price_for(ctx) == 10000, "Price should be default 10000"
        assert await currency_for(ctx) == "Rupee", "Currency should be default Rupee"
        print("  ✓ Test passed\n")

        # Test 5: Platinum customer - Sauyav with city Berlin
        print("Test 5: Platinum customer - Sauyav with city Berlin")
        ctx = EvaluationContext(attributes={"name": "Sauyav", "city": "Berlin"})
        assert await price_for(ctx) == 5000, "Price should be 5000"
        assert await currency_for(ctx) == "Euro", "Currency should be Euro in Berlin"
        print("  ✓ Test passed\n")

        # Test 6: Regular customer - John with city Boston
        print("Test 6: Regular customer - John with city Boston")
        ctx = EvaluationContext(attributes={"name": "John", "city": "Boston"})
        assert await price_for(ctx) == 10000, "Price should be default 10000"
        assert await currency_for(ctx) == "Dollar", "Currency should be Dollar in Boston"
        print("  ✓ Test passed\n")

        # Test 7: Edge case customer - karbik (specific override)
        print("Test 7: Edge case customer - karbik (specific override)")
        ctx = EvaluationContext(attributes={"name": "karbik"})
        assert await price_for(ctx) == 1, "Price should be 1 for karbik"
        assert await currency_for(ctx) == "Rupee", "Currency should be default Rupee"
        print("  ✓ Test passed\n")

        # Test 8: Edge case customer - karbik with city Boston
        print("Test 8: Edge case customer - karbik with city Boston")
        ctx = EvaluationContext(attributes={"name": "karbik", "city": "Boston"})
        assert await price_for(ctx) == 1, "Price should be 1 for karbik"
        assert await currency_for(ctx) == "Dollar", "Currency should be Dollar in Boston"
        print("  ✓ Test passed\n")

        if test_experiments:
            # Test 9: Experiment case - Kolkata pricing
            print("Test 9: Experiment case: Kolkata pricing")
            ctx = EvaluationContext(targeting_key="test", attributes={"city": "Kolkata"})
            price = await price_for(ctx)
            currency = await currency_for(ctx)
            print(f"  - Retrieved price: {price}, currency: {currency}")
            assert price in (8000, 7000), "Price should be 8000 (control) or 7000 (experimental)"
            assert currency == "Rupee", "Currency should be Rupee in Kolkata"
            print("  ✓ Experiment Test passed\n")

        print(f"\n=== All tests passed: {label} ===\n")
    except Exception as error:
        print(f"\n❌ Error running tests ({label}): {error}")
        raise error
    finally:
        api.shutdown()
        print("OpenFeature closed successfully")


async def run_demo(org_id: str, workspace_id: str):
    http_options = SuperpositionOptions(
        endpoint="http://localhost:8080",
        auth=TokenAuth("12345678"),
        org_id=org_id,
        workspace_id=workspace_id,
    )
    # Wrong workspace: every call to the primary fails, forcing the file fallback.
    wrong_http_options = SuperpositionOptions(
        endpoint="http://localhost:8080",
        auth=TokenAuth("12345678"),
        org_id=org_id,
        workspace_id="workspace_id",
    )
    polling = PollingStrategy(interval_milliseconds=5_000, timeout_milliseconds=3_000)

    # Flow A: LocalResolutionProvider over HTTP, polling refresh (experiments supported).
    await run_provider_tests(
        "LocalResolutionProvider with HTTP data source (polling)",
        LocalResolutionProvider(
            primary_source=HttpDataSource(http_options), refresh_strategy=polling
        ),
        test_experiments=True,
        is_async=False,
    )

    # Flow B: SuperpositionAPIProvider — server-side resolution, resolved via the async client.
    await run_provider_tests(
        "SuperpositionAPIProvider",
        SuperpositionAPIProvider(http_options),
        test_experiments=True,
        is_async=True,
    )

    # Flow C: LocalResolutionProvider whose HTTP primary fails, falling back to a file.
    await run_provider_tests(
        "LocalResolutionProvider with failing HTTP primary and file fallback",
        LocalResolutionProvider(
            primary_source=HttpDataSource(wrong_http_options),
            fallback_source=FileDataSource("config.toml"),
            refresh_strategy=polling,
        ),
        test_experiments=False,
        is_async=False,
    )

    # Flow D: LocalResolutionProvider over HTTP with the OnDemand refresh strategy,
    # exercising the lazy TTL refresh path (experiments supported).
    await run_provider_tests(
        "LocalResolutionProvider with HTTP data source (on-demand refresh)",
        LocalResolutionProvider(
            primary_source=HttpDataSource(http_options),
            refresh_strategy=OnDemandStrategy(
                ttl_milliseconds=300_000, timeout_milliseconds=3_000
            ),
        ),
        test_experiments=True,
        is_async=False,
    )


async def main():
    print("Starting Superposition OpenFeature demo and tests (Python)...")
    try:
        config = Config(endpoint_uri=SUPERPOSITION_SDK_CONFIG["endpoint"])
        client = Superposition(config)
        org_id = await create_organisation(client)
        await setup_with_sdk(client, org_id, WORKSPACE_ID)
        await run_demo(org_id, WORKSPACE_ID)
    except Exception as error:
        print(f"\n❌ Test suite failed: {error}")
        exit(1)


if __name__ == "__main__":
    asyncio.run(main())
