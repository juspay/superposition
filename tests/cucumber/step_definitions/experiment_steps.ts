import { Given, When, Then } from "@cucumber/cucumber";
import {
  CreateExperimentCommand,
  GetExperimentCommand,
  ListExperimentCommand,
  RampExperimentCommand,
  ConcludeExperimentCommand,
  DiscardExperimentCommand,
  UpdateOverridesExperimentCommand,
  CreateDimensionCommand,
  CreateDefaultConfigCommand,
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

// ── When ────────────────────────────────────────────────────────────

When(
  "I create an experiment with name {string} and context {string} equals {string}",
  async function (this: SuperpositionWorld, name: string, dim: string, val: string) {
    // Store for the multi-step creation
    this.experimentId = ""; // Will be set in the final step
    (this as any)._pendingExpName = this.uniqueName(name);
    (this as any)._pendingExpContext = { [dim]: val };
    (this as any)._pendingExpVariants = [];
  }
);

When(
  "the experiment has a control variant with override {string} = {string}",
  function (this: SuperpositionWorld, key: string, value: string) {
    (this as any)._pendingExpVariants.push({
      variant_type: VariantType.CONTROL,
      id: "control",
      overrides: { [key]: value },
    });
  }
);

When(
  "the experiment has an experimental variant with override {string} = {string}",
  async function (this: SuperpositionWorld, key: string, value: string) {
    (this as any)._pendingExpVariants.push({
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
          name: (this as any)._pendingExpName,
          context: (this as any)._pendingExpContext,
          variants: (this as any)._pendingExpVariants,
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

// ── Then ────────────────────────────────────────────────────────────

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
