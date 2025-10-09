import { OpenFeature } from '@openfeature/server-sdk';
import { SuperpositionProvider } from 'superposition-provider';
import {
  SuperpositionClient,
  CreateWorkspaceCommand,
  CreateDimensionCommand,
  CreateDefaultConfigCommand,
  CreateContextCommand,
  CreateExperimentCommand,
  RampExperimentCommand,
  CreateOrganisationCommand
} from "superposition-sdk";
import assert from "assert";

const WORKSPACE_ID = "jsprovidertest";

const SuperpositionSDKConfig = {
  endpoint: "http://localhost:8080",
  token: {
    token: "12131",
  },
};

const client = new SuperpositionClient(SuperpositionSDKConfig);

async function createOrganisation() {
  const command = new CreateOrganisationCommand({
    name: "jstestorg",
    admin_email: "admin@jstestorg.com"
  });

  try {
    const response = await client.send(command);
    console.log("Organisation created successfully:", response.name, "with ID:", response.id);
    return response.id;
  } catch (e) {
    console.error("An exception occurred while creating organization:", e);
    throw e;
  }
}

async function createWorkspace(org_id, workspace_name) {
  let command = new CreateWorkspaceCommand({
    org_id,
    workspace_name,
    workspace_admin_email: "test@tests.com",
    workspace_status: "ENABLED",
    strict_mode: true,
    allow_experiment_self_approval: true,
    auto_populate_control: true,
  });
  try {
    const response = await client.send(command);
    console.log("Workspace created!\n", response);
  } catch (e) {
    console.error("An exception occurred while creating a workspace: ", e);
    throw e;
  }
}

async function create_dimensions(org_id, workspace_id) {
  let dimensions = [
    {
      workspace_id,
      org_id,
      dimension: "name",
      position: 1,
      schema: {
        type: "string",
      },
      description: "customer name dimension",
      change_reason: "adding name dimension",
      dimension_type: {
        REGULAR: {},
      },
    },
    {
      workspace_id,
      org_id,
      dimension: "city",
      position: 2,
      schema: {
        type: "string",
      },
      description: "city dimension",
      change_reason: "adding city dimension",
      dimension_type: {
        REGULAR: {},
      },
    },
    {
      workspace_id,
      org_id,
      dimension: "customers",
      position: 3,
      schema: {
          "type": "string",
          "enum": ["platinum", "gold", "otherwise"],
          "definitions": {
              "platinum": {
                  "in": [
                      {"var": "name"},
                      ["Agush", "Sauyav"]
                  ]
              },
              "gold": {
                  "in": [
                      {"var": "name"},
                      ["Angit", "Bhrey"]
                  ]
              }
          }
      },
      description: "customers dimension",
      change_reason: "adding customers dimension",
      dimension_type: {
        LOCAL_COHORT: "name",
      },
    },
  ];

  console.log("Creating dimensions:");
  for (const dimension of dimensions) {
    let command = new CreateDimensionCommand(dimension);
    try {
      let response = await client.send(command);
      console.log("  - Created dimension:", response.dimension);
    } catch (e) {
      console.error(
        "Error occurred while creating dimension:",
        dimension.dimension,
        e,
      );
      throw e;
    }
  }
}

async function create_default_configs(org_id, workspace_id) {
  let configs = [
    {
      key: "price",
      value: 10000,
      schema: {
        type: "number",
        minimum: 0,
      },
      description: "price as a positive number",
      change_reason: "adding price config",
      workspace_id,
      org_id,
    },
    {
      key: "currency",
      value: "Rupee",
      schema: {
        type: "string",
        enum: ["Rupee", "Dollar", "Euro"],
      },
      description: "currency as an enum",
      change_reason: "adding currency config",
      workspace_id,
      org_id,
    },
  ];

  console.log("Creating default configs:");
  for (const config of configs) {
    let command = new CreateDefaultConfigCommand(config);
    try {
      let response = await client.send(command);
      console.log("  - Created config:", response.key);
    } catch (e) {
      console.error("Error occurred while creating config:", config.key, e);
      throw e;
    }
  }
}

async function create_overrides(org_id, workspace_id) {
  let overrides = [
    {
      workspace_id,
      org_id,
      context: {
        city: "Boston",
      },
      override: {
        currency: "Dollar",
      },
      description: "Bostonian",
      change_reason: "testing",
    },
    {
      workspace_id,
      org_id,
      context: {
        city: "Berlin",
      },
      override: {
        currency: "Euro",
      },
      description: "Berlin",
      change_reason: "testing",
    },
    {
      workspace_id,
      org_id,
      context: {
          customers: "platinum"
      },
      override: {
        price: 5000,
      },
      description: "platinum customer",
      change_reason: "testing",
    },
    {
      workspace_id,
      org_id,
      context: {
        customers: "gold",
      },
      override: {
        price: 8000,
      },
      description: "gold customers",
      change_reason: "testing",
    },
    {
      workspace_id,
      org_id,
      context: {
        name: "karbik",
        customers: "otherwise",
      },
      override: {
        price: 1,
      },
      description: "edge case customer karbik",
      change_reason: "testing",
    },
  ];

  console.log("Creating overrides:");
  for (const override of overrides) {
    let command = new CreateContextCommand(override);
    try {
      let response = await client.send(command);
      console.log("Created override:", JSON.stringify(response.context));
    } catch (e) {
      console.error("Error occurred while creating override:", override, e);
      throw e;
    }
  }
}

async function create_experiments(org_id, workspace_id) {
    let experiments = [
        {
              workspace_id,
              org_id,
              name: "testexperiment",
              context: {
                city: "Bangalore",
              },
              variants: [
                {
                  id: "testexperiment-control",
                  variant_type: "CONTROL",
                  overrides: {
                      "price": 10000
                  },
                },
                {
                  id: "testexperiment-experimental",
                  variant_type: "EXPERIMENTAL",
                  overrides: {
                      "price": 8800
                  }
                }
            ],
            description: "test experimentation",
            change_reason: "a reason",
        }
    ]
    for (const experiment of experiments) {
        const command = new CreateExperimentCommand(experiment);
        try {
          let response = await client.send(command);
          console.log("Created experiment:", response);
          const command_two = new RampExperimentCommand({
            workspace_id,
            org_id,
            id: response.id,
            change_reason: "ramp the experiment",
            traffic_percentage: 25,
            });
          let response_two = await client.send(command_two);
          console.log("Ramped experiment:", response_two);
        } catch (e) {
          console.error("Error occurred while creating experiment:", experiment, e);
          throw e;
        }
    }
}

async function setupWithSDK(org_id, workspace_id) {
  console.log("\n=== Setting up test environment ===\n");
  await createWorkspace(org_id, workspace_id);
  await create_dimensions(org_id, workspace_id);
  await create_default_configs(org_id, workspace_id);
  await create_overrides(org_id, workspace_id);
  await create_experiments(org_id, workspace_id);
  console.log("\n=== Setup complete ===\n");
}

async function runDemo(org_id, workspace_id) {
  const config = {
    endpoint: "http://localhost:8080",
    token: "12345678",
    org_id,
    workspace_id,
  };

  try {
    console.log("\n=== Starting OpenFeature tests ===\n");
    console.log("Running on CPU architecture:", process.arch);

    const provider = new SuperpositionProvider(config);
    console.log("Provider created successfully");

    // Initialize the provider
    await OpenFeature.setProviderAndWait(provider);
    console.log("Provider initialized successfully\n");

    const ofClient = OpenFeature.getClient();

    // Test 1: Default values (no context)
    console.log("Test 1: Default values (no context)");
    {
      const context = {};
      const price = await ofClient.getNumberValue("price", 0, context);
      const currency = await ofClient.getStringValue("currency", "", context);

      assert.strictEqual(price, 10000, "Default price should be 10000");
      assert.strictEqual(currency, "Rupee", "Default currency should be Rupee");
      console.log("  ✓ Test passed\n");
    }

    // Test 2: Platinum customer - Agush, no city
    console.log("Test 2: Platinum customer - Agush (no city)");
    {
      const context = { name: "Agush" };
      const price = await ofClient.getNumberValue("price", 0, context);
      const currency = await ofClient.getStringValue("currency", "", context);

      assert.strictEqual(price, 5000, "Price should be default 5000 (platinum customer)");
      assert.strictEqual(currency, "Rupee", "Currency should be default Rupee");
      console.log("  ✓ Test passed\n");
    }

    // Test 3: Platinum customer - Sauyav, no city
    console.log("Test 3: Platinum customer - Sauyav (no city)");
    {
      const context = { name: "Sauyav", city: "Boston" };
      const price = await ofClient.getNumberValue("price", 0, context);
      const currency = await ofClient.getStringValue("currency", "", context);

      assert.strictEqual(price, 5000, "Price should be 5000");
      assert.strictEqual(currency, "Dollar", "Currency should be dollar");
      console.log("  ✓ Test passed\n");
    }

    console.log("Test 4: Regular customer - John (no city)");
    {
      const context = { name: "John" };
      const price = await ofClient.getNumberValue("price", 0, context);
      const currency = await ofClient.getStringValue("currency", "", context);

      assert.strictEqual(price, 10000, "Price should be default 10000");
      assert.strictEqual(currency, "Rupee", "Currency should be default Rupee");
      console.log("  ✓ Test passed\n");
    }

    console.log("Test 5: Platinum customer - Sauyav with city Berlin");
    {
      const context = { name: "Sauyav", city: "Berlin" };
      const price = await ofClient.getNumberValue("price", 0, context);
      const currency = await ofClient.getStringValue("currency", "", context);

      assert.strictEqual(price, 5000, "Price should be 5000");
      assert.strictEqual(currency, "Euro", "Currency should be Euro in Berlin");
      console.log("  ✓ Test passed\n");
    }

    console.log("Test 6: Regular customer - John with city Boston");
    {
      const context = { name: "John", city: "Boston" };
      const price = await ofClient.getNumberValue("price", 0, context);
      const currency = await ofClient.getStringValue("currency", "", context);

      assert.strictEqual(price, 10000, "Price should be default 10000");
      assert.strictEqual(currency, "Dollar", "Currency should be Dollar in Boston");
      console.log("  ✓ Test passed\n");
    }

    console.log("Test 7: Edge case customer - karbik (specific override)");
    {
      const context = { name: "karbik" };
      const price = await ofClient.getNumberValue("price", 0, context);
      const currency = await ofClient.getStringValue("currency", "", context);

      assert.strictEqual(price, 1, "Price should be 1 for karbik");
      assert.strictEqual(currency, "Rupee", "Currency should be default Rupee");
      console.log("  ✓ Test passed\n");
    }

    console.log("Test 8: Edge case customer - karbik with city Boston");
    {
      const context = { name: "karbik", city: "Boston" };
      const price = await ofClient.getNumberValue("price", 0, context);
      const currency = await ofClient.getStringValue("currency", "", context);

      assert.strictEqual(price, 1, "Price should be 1 for karbik");
      assert.strictEqual(currency, "Dollar", "Currency should be Dollar in Boston");
      console.log("  ✓ Test passed\n");
    }
    // turn this on when bucketing is done for FFI legacy and uniffi
    // console.log("Test 9: Experiment case: Bangalore pricing");
    // {
    //   const context_control = { city: "Bangalore", toss: 30 };
    //   const price_control = await ofClient.getNumberValue("price", 0, context_control);
    //   const currency_control = await ofClient.getStringValue("currency", "", context_control);

    //   assert.strictEqual(price_control, 10000, "Price should be 10000 for Bangalore Control Variant");
    //   assert.strictEqual(currency_control, "Rupee", "Currency should be Rupee in Bangalore");
    //   console.log("  ✓ Control Test passed\n");

    //   const context_variant = { city: "Bangalore", toss: 1 };
    //   const price_variant = await ofClient.getNumberValue("price", 0, context_variant);
    //   const currency_variant = await ofClient.getStringValue("currency", "", context_variant);

    //   assert.strictEqual(price_variant, 8800, "Price should be 8800 for Bangalore Experimental Variant");
    //   assert.strictEqual(currency_variant, "Rupee", "Currency should be Rupee in Bangalore");
    //   console.log("  ✓ Experimental Test passed\n");
    // }

    console.log("\n=== All tests passed! ===\n");
  } catch (error) {
    console.error("\n❌ Error running tests:", error);
    throw error;
  } finally {
      await OpenFeature.close();
      console.log("OpenFeature closed successfully");
  }
}

console.log(
"Starting Superposition OpenFeature demo and tests (JavaScript)...",
);

try {
    const ORG_ID = await createOrganisation();
    await setupWithSDK(ORG_ID, WORKSPACE_ID);
    await runDemo(ORG_ID, WORKSPACE_ID);
} catch (error) {
    console.error("\n❌ Test suite failed:", error);
    process.exit(1);
}
