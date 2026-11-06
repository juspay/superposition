import { Given, When, Then } from "@cucumber/cucumber";
import {
  CreateExperimentCommand,
  GetExperimentCommand,
  ListExperimentCommand,
  RampExperimentCommand,
  ConcludeExperimentCommand,
  DiscardExperimentCommand,
  PauseExperimentCommand,
  ResumeExperimentCommand,
  UpdateOverridesExperimentCommand,
  CreateDimensionCommand,
  CreateDefaultConfigCommand,
  UpdateWorkspaceCommand,
  VariantType,
  ExperimentStatusType,
} from "@juspay/superposition-sdk";
import { SuperpositionWorld } from "../support/world.ts";
import * as assert from "node:assert";

// ── Given ───────────────────────────────────────────────────────────

Given(
  "dimensions and default configs are set up for experiment tests",
  async function (this: SuperpositionWorld) {
    // Create "os" dimension
    try {
      await this.client.send(
        new CreateDimensionCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          dimension: "os",
          position: 1,
          schema: { type: "string", enum: ["android", "ios", "web"] },
          description: "OS dimension",
          change_reason: "Cucumber experiment setup",
        })
      );
      this.createdDimensions.push("os");
    } catch {
      // Already exists
    }

    // Create config key
    const configKey = "exp-config-key";
    try {
      await this.client.send(
        new CreateDefaultConfigCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          key: configKey,
          value: "default-value",
          schema: { type: "string" },
          description: "Config for experiment tests",
          change_reason: "Cucumber setup",
        })
      );
      this.createdConfigs.push(configKey);
    } catch {
      // Already exists
    }
  }
);

async function createExperiment(
  world: SuperpositionWorld,
  name: string,
  dimName: string,
  dimValue: string,
  configKey: string = "exp-config-key"
) {
  const uniqueName = world.uniqueName(name);
  const response = await world.client.send(
    new CreateExperimentCommand({
      workspace_id: world.workspaceId,
      org_id: world.orgId,
      name: uniqueName,
      context: { [dimName]: dimValue },
      variants: [
        {
          variant_type: VariantType.CONTROL,
          id: "control",
          overrides: { [configKey]: "control-val" },
        },
        {
          variant_type: VariantType.EXPERIMENTAL,
          id: "experimental",
          overrides: { [configKey]: "experimental-val" },
        },
      ],
      description: `Test experiment ${uniqueName}`,
      change_reason: "Cucumber test",
    })
  );
  world.experimentId = response.id ?? "";
  world.experimentVariants = response.variants ?? [];
  world.createdExperimentIds.push(world.experimentId);
  return response;
}

Given(
  "an experiment {string} exists with context {string} equals {string}",
  async function (this: SuperpositionWorld, name: string, dim: string, val: string) {
    await createExperiment(this, name, dim, val);
  }
);

Given(
  "an experiment {string} exists and is ramped to {int} percent",
  async function (this: SuperpositionWorld, name: string, traffic: number) {
    await createExperiment(this, name, "os", "android");
    await this.client.send(
      new RampExperimentCommand({
        workspace_id: this.workspaceId,
        org_id: this.orgId,
        id: this.experimentId,
        traffic_percentage: traffic,
        change_reason: "Cucumber ramp",
      })
    );
  }
);

Given(
  "an experiment exists and is ramped",
  async function (this: SuperpositionWorld) {
    await createExperiment(this, "exp-ramped", "os", "android");
    await this.client.send(
      new RampExperimentCommand({
        workspace_id: this.workspaceId,
        org_id: this.orgId,
        id: this.experimentId,
        traffic_percentage: 50,
        change_reason: "Cucumber ramp for pause test",
      })
    );
  }
);

Given(
  "an experiment exists and is paused",
  async function (this: SuperpositionWorld) {
    await createExperiment(this, "exp-paused", "os", "android");
    await this.client.send(
      new RampExperimentCommand({
        workspace_id: this.workspaceId,
        org_id: this.orgId,
        id: this.experimentId,
        traffic_percentage: 50,
        change_reason: "Cucumber ramp for pause test",
      })
    );
    await this.client.send(
      new PauseExperimentCommand({
        workspace_id: this.workspaceId,
        org_id: this.orgId,
        id: this.experimentId,
        change_reason: "Cucumber pause setup",
      })
    );
  }
);

// ── Given: Metrics ────────────────────────────────────────────────

Given(
  "the workspace has metric definitions {string}",
  async function (this: SuperpositionWorld, spec: string) {
    const definitions = this.parseMetricDefinitions(spec);
    this.metricDefinitions = definitions;
    await this.client.send(
      new UpdateWorkspaceCommand({
        org_id: this.orgId,
        workspace_name: this.workspaceId,
        metrics: { enabled: true, definitions },
      })
    );
  }
);

Given(
  "the workspace has no metric definitions",
  async function (this: SuperpositionWorld) {
    this.metricDefinitions = [];
    await this.client.send(
      new UpdateWorkspaceCommand({
        org_id: this.orgId,
        workspace_name: this.workspaceId,
        metrics: { enabled: false },
      })
    );
  }
);

// ── When ────────────────────────────────────────────────────────────

When(
  "I create an experiment with name {string} and context {string} equals {string}",
  async function (this: SuperpositionWorld, name: string, dim: string, val: string) {
    // Store for the multi-step creation
    this.experimentId = ""; // Will be set in the final step
    this._pendingExpName = this.uniqueName(name);
    this._pendingExpContext = { [dim]: val };
    this._pendingExpVariants = [];
  }
);

When(
  "the experiment has a control variant with override {string} = {string}",
  function (this: SuperpositionWorld, key: string, value: string) {
    this._pendingExpVariants.push({
      variant_type: VariantType.CONTROL,
      id: "control",
      overrides: { [key]: value },
    });
  }
);

When(
  "the experiment has an experimental variant with override {string} = {string}",
  async function (this: SuperpositionWorld, key: string, value: string) {
    this._pendingExpVariants.push({
      variant_type: VariantType.EXPERIMENTAL,
      id: "experimental",
      overrides: { [key]: value },
    });

    // Now create the experiment
    try {
      this.lastResponse = await this.client.send(
        new CreateExperimentCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          name: this._pendingExpName,
          context: this._pendingExpContext,
          variants: this._pendingExpVariants,
          description: "Cucumber test experiment",
          change_reason: "Cucumber test",
        })
      );
      this.experimentId = this.lastResponse.id ?? "";
      this.experimentVariants = this.lastResponse.variants ?? [];
      this.createdExperimentIds.push(this.experimentId);
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

When(
  "I get the experiment by its ID",
  async function (this: SuperpositionWorld) {
    try {
      this.lastResponse = await this.client.send(
        new GetExperimentCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          id: this.experimentId,
        })
      );
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

When("I list experiments", async function (this: SuperpositionWorld) {
  try {
    this.lastResponse = await this.client.send(
      new ListExperimentCommand({
        workspace_id: this.workspaceId,
        org_id: this.orgId,
      })
    );
    this.lastError = undefined;
  } catch (e: any) {
    this.lastError = e;
    this.lastResponse = undefined;
  }
});

When(
  "I ramp the experiment to {int} percent traffic",
  async function (this: SuperpositionWorld, traffic: number) {
    try {
      this.lastResponse = await this.client.send(
        new RampExperimentCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          id: this.experimentId,
          traffic_percentage: traffic,
          change_reason: "Cucumber ramp test",
        })
      );
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

When(
  "I update the experimental variant override for {string} to {string}",
  async function (this: SuperpositionWorld, key: string, value: string) {
    const experimentalVariant = this.experimentVariants.find(
      (v: any) => v.variant_type === VariantType.EXPERIMENTAL
    );
    // Server requires all variants in the update request
    const variantList = this.experimentVariants.map((v: any) => ({
      id: v.id ?? v.variant_type,
      overrides: v.id === experimentalVariant?.id
        ? { ...v.overrides, [key]: value }
        : v.overrides ?? {},
    }));
    try {
      this.lastResponse = await this.client.send(
        new UpdateOverridesExperimentCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          id: this.experimentId,
          variant_list: variantList,
          change_reason: "Cucumber override update",
        })
      );
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

When(
  "I conclude the experiment with the experimental variant",
  async function (this: SuperpositionWorld) {
    const experimentalVariant = this.experimentVariants.find(
      (v: any) => v.variant_type === VariantType.EXPERIMENTAL
    );
    try {
      this.lastResponse = await this.client.send(
        new ConcludeExperimentCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          id: this.experimentId,
          chosen_variant: experimentalVariant?.id ?? "experimental",
          change_reason: "Cucumber conclude test",
        })
      );
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

When("I pause the experiment", async function (this: SuperpositionWorld) {
  try {
    this.lastResponse = await this.client.send(
      new PauseExperimentCommand({
        workspace_id: this.workspaceId,
        org_id: this.orgId,
        id: this.experimentId,
        change_reason: "Cucumber pause test",
      })
    );
    this.lastError = undefined;
  } catch (e: any) {
    this.lastError = e;
    this.lastResponse = undefined;
  }
});

When("I resume the experiment", async function (this: SuperpositionWorld) {
  try {
    this.lastResponse = await this.client.send(
      new ResumeExperimentCommand({
        workspace_id: this.workspaceId,
        org_id: this.orgId,
        id: this.experimentId,
        change_reason: "Cucumber resume test",
      })
    );
    this.lastError = undefined;
  } catch (e: any) {
    this.lastError = e;
    this.lastResponse = undefined;
  }
});

When("I discard the experiment", async function (this: SuperpositionWorld) {
  try {
    this.lastResponse = await this.client.send(
      new DiscardExperimentCommand({
        workspace_id: this.workspaceId,
        org_id: this.orgId,
        id: this.experimentId,
        change_reason: "Cucumber discard test",
      })
    );
    this.lastError = undefined;
  } catch (e: any) {
    this.lastError = e;
    this.lastResponse = undefined;
  }
});

// ── When: Metrics ─────────────────────────────────────────────────

/** Build a metric selection, taking each metric's direction from the workspace definitions. */
function buildMetricSelection(
  world: SuperpositionWorld,
  primary: string,
  secondary: string | undefined,
  guardrail: string
): Record<string, any> {
  const directionOf = (name: string) =>
    world.metricDefinitions.find((d) => d.name === name)?.direction ?? "maximize";
  return {
    primary: { name: primary, direction: directionOf(primary) },
    ...(secondary
      ? { secondary: { name: secondary, direction: directionOf(secondary) } }
      : {}),
    guardrail,
  };
}

async function createExperimentWithMetrics(
  world: SuperpositionWorld,
  name: string,
  selection: Record<string, any>
): Promise<void> {
  const uniqueName = world.uniqueName(name);
  try {
    world.lastResponse = await world.client.send(
      new CreateExperimentCommand({
        workspace_id: world.workspaceId,
        org_id: world.orgId,
        name: uniqueName,
        context: { os: "android" },
        variants: [
          {
            variant_type: VariantType.CONTROL,
            id: "control",
            overrides: { "exp-config-key": "control-val" },
          },
          {
            variant_type: VariantType.EXPERIMENTAL,
            id: "experimental",
            overrides: { "exp-config-key": "experimental-val" },
          },
        ],
        description: `Metrics experiment ${uniqueName}`,
        change_reason: "Cucumber metrics test",
        metrics: { enabled: true, selection },
      })
    );
    world.experimentId = world.lastResponse.id ?? "";
    world.experimentVariants = world.lastResponse.variants ?? [];
    world.createdExperimentIds.push(world.experimentId);
    world.lastError = undefined;
  } catch (e: any) {
    world.lastError = e;
    world.lastResponse = undefined;
  }
}

When(
  "I create an experiment {string} with primary metric {string}, secondary metric {string} and guardrail {string}",
  async function (
    this: SuperpositionWorld,
    name: string,
    primary: string,
    secondary: string,
    guardrail: string
  ) {
    await createExperimentWithMetrics(
      this,
      name,
      buildMetricSelection(this, primary, secondary, guardrail)
    );
  }
);

When(
  "I create an experiment {string} with primary metric {string} and guardrail {string}",
  async function (this: SuperpositionWorld, name: string, primary: string, guardrail: string) {
    await createExperimentWithMetrics(
      this,
      name,
      buildMetricSelection(this, primary, undefined, guardrail)
    );
  }
);

When(
  "I update the experiment metric selection to primary {string} and guardrail {string}",
  async function (this: SuperpositionWorld, primary: string, guardrail: string) {
    // Server requires all variants in the update request
    const variantList = this.experimentVariants.map((v: any) => ({
      id: v.id ?? v.variant_type,
      overrides: v.overrides ?? {},
    }));
    try {
      this.lastResponse = await this.client.send(
        new UpdateOverridesExperimentCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          id: this.experimentId,
          variant_list: variantList,
          change_reason: "Cucumber metric selection update",
          metrics: {
            enabled: true,
            selection: buildMetricSelection(this, primary, undefined, guardrail),
          },
        })
      );
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

// ── Then ────────────────────────────────────────────────────────────

Then(
  "the experiment metrics should be enabled",
  function (this: SuperpositionWorld) {
    assert.ok(this.lastResponse, "No response");
    assert.strictEqual(this.lastResponse.metrics?.enabled, true, "Experiment metrics are not enabled");
  }
);

Then(
  "the experiment metrics should be disabled",
  function (this: SuperpositionWorld) {
    assert.ok(this.lastResponse, "No response");
    assert.ok(
      this.lastResponse.metrics?.enabled !== true,
      `Expected disabled metrics, got ${JSON.stringify(this.lastResponse.metrics)}`
    );
  }
);

Then(
  "the experiment primary metric should be {string} with direction {string}",
  function (this: SuperpositionWorld, name: string, direction: string) {
    assert.ok(this.lastResponse, "No response");
    const selection = this.lastResponse.metrics?.selection;
    assert.ok(selection, "No metric selection in response");
    assert.strictEqual(selection.primary?.name, name);
    assert.strictEqual(selection.primary?.direction, direction);
  }
);

Then(
  "the experiment secondary metric should be {string}",
  function (this: SuperpositionWorld, name: string) {
    assert.ok(this.lastResponse, "No response");
    const selection = this.lastResponse.metrics?.selection;
    assert.ok(selection, "No metric selection in response");
    assert.strictEqual(selection.secondary?.name, name);
  }
);

Then(
  "the experiment should have no secondary metric",
  function (this: SuperpositionWorld) {
    assert.ok(this.lastResponse, "No response");
    const selection = this.lastResponse.metrics?.selection;
    assert.ok(selection, "No metric selection in response");
    assert.ok(
      selection.secondary === undefined || selection.secondary === null,
      `Expected no secondary metric, got ${JSON.stringify(selection.secondary)}`
    );
  }
);

Then(
  "the experiment guardrail metric should be {string}",
  function (this: SuperpositionWorld, name: string) {
    assert.ok(this.lastResponse, "No response");
    const selection = this.lastResponse.metrics?.selection;
    assert.ok(selection, "No metric selection in response");
    assert.strictEqual(selection.guardrail, name);
  }
);

Then(
  "the response should have experiment status {string}",
  function (this: SuperpositionWorld, status: string) {
    assert.ok(this.lastResponse, "No response");
    assert.strictEqual(this.lastResponse.status, status);
  }
);

Then(
  "the experiment status should be {string}",
  function (this: SuperpositionWorld, status: string) {
    assert.ok(this.lastResponse, "No response");
    assert.strictEqual(this.lastResponse.status, status);
  }
);

Then(
  "the response should have {int} variants",
  function (this: SuperpositionWorld, count: number) {
    assert.ok(this.lastResponse, "No response");
    assert.strictEqual(this.lastResponse.variants?.length, count);
  }
);

Then(
  "the response should have the experiment name",
  function (this: SuperpositionWorld) {
    assert.ok(this.lastResponse, "No response");
    assert.ok(this.lastResponse.name, "No experiment name in response");
  }
);

Then(
  "the list should contain the created experiment",
  function (this: SuperpositionWorld) {
    const data = this.lastResponse?.data ?? this.lastResponse;
    assert.ok(Array.isArray(data), "Response is not a list");
    const found = data.find((e: any) => e.id === this.experimentId);
    assert.ok(found, `Experiment ${this.experimentId} not found in list`);
  }
);
