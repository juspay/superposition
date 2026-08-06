import { Given, When, Then } from "@cucumber/cucumber";
import {
  GetResolvedConfigWithIdentifierCommand,
  CreateDimensionCommand,
  CreateDefaultConfigCommand,
  CreateExperimentCommand,
  RampExperimentCommand,
  DiscardExperimentCommand,
  DeleteDefaultConfigCommand,
  DeleteDimensionCommand,
  VariantType,
} from "@juspay/superposition-sdk";
import { SuperpositionWorld } from "../support/world.ts";
import * as assert from "node:assert";

const BUCKETING_DIM = "clientId";
const BUCKETING_CONFIG_KEY = "testKey_resolve";
const BUCKETING_CLIENT_ID = "test-client-bucketing-123";
const BUCKETING_IDENTIFIER = "test-identifier-bucketing-456";
const DEFAULT_VALUE = "default-bucketing-value";
const EXPERIMENTAL_VALUE = "experimental-bucketing-value";

// ── Given ───────────────────────────────────────────────────────────

Given(
  "a dimension, default config, and experiment are set up for bucketing tests",
  async function (this: SuperpositionWorld) {
    // Create dimension
    try {
      await this.client.send(
        new CreateDimensionCommand({
          dimension: BUCKETING_DIM,
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          schema: { type: "string" },
          position: 1,
          change_reason: "Cucumber bucketing setup",
          description: "Client ID dimension",
        })
      );
      this.createdDimensions.push(BUCKETING_DIM);
    } catch {
      // Already exists
    }

    // Create default config
    const configKey = this.uniqueName(BUCKETING_CONFIG_KEY);
    this.configKey = configKey;
    try {
      await this.client.send(
        new CreateDefaultConfigCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          key: configKey,
          value: DEFAULT_VALUE,
          schema: { type: "string" },
          description: "Bucketing test config",
          change_reason: "Cucumber setup",
        })
      );
      this.createdConfigs.push(configKey);
    } catch {
      // Already exists
    }

    // Create experiment
    const expResp = await this.client.send(
      new CreateExperimentCommand({
        workspace_id: this.workspaceId,
        org_id: this.orgId,
        name: this.uniqueName("bucketing-exp"),
        context: { [BUCKETING_DIM]: BUCKETING_CLIENT_ID },
        variants: [
          {
            variant_type: VariantType.CONTROL,
            id: "control",
            overrides: { [configKey]: DEFAULT_VALUE },
          },
          {
            variant_type: VariantType.EXPERIMENTAL,
            id: "test1",
            overrides: { [configKey]: EXPERIMENTAL_VALUE },
          },
        ],
        description: "Bucketing test experiment",
        change_reason: "Cucumber setup",
      })
    );
    this.experimentId = expResp.id ?? "";
    this.createdExperimentIds.push(this.experimentId);

    // Ramp to 50%
    await this.client.send(
      new RampExperimentCommand({
        workspace_id: this.workspaceId,
        org_id: this.orgId,
        id: this.experimentId,
        traffic_percentage: 50,
        change_reason: "Cucumber ramp",
      })
    );
  }
);

// ── When ────────────────────────────────────────────────────────────

When(
  "I resolve the config with the test identifier and matching context",
  async function (this: SuperpositionWorld) {
    try {
      this.lastResponse = await this.client.send(
        new GetResolvedConfigWithIdentifierCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          prefix: [this.configKey],
          identifier: BUCKETING_IDENTIFIER,
          context: { [BUCKETING_DIM]: BUCKETING_CLIENT_ID },
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
  "the config value should be either the default or experimental value",
  function (this: SuperpositionWorld) {
    assert.ok(this.lastResponse, "No response");
    assert.ok(this.lastResponse.config, "No config in response");
    const value = (this.lastResponse.config as any)[this.configKey];
    assert.ok(
      value === DEFAULT_VALUE || value === EXPERIMENTAL_VALUE,
      `Expected "${DEFAULT_VALUE}" or "${EXPERIMENTAL_VALUE}", got "${value}"`
    );
  }
);
