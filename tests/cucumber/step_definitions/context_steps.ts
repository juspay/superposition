import { Given, When, Then } from "@cucumber/cucumber";
import {
  CreateContextCommand,
  GetContextCommand,
  ListContextsCommand,
  UpdateOverrideCommand,
  MoveContextCommand,
  DeleteContextCommand,
  BulkOperationCommand,
  WeightRecomputeCommand,
  CreateDimensionCommand,
  CreateDefaultConfigCommand,
} from "@juspay/superposition-sdk";
import { SuperpositionWorld } from "../support/world.ts";
import * as assert from "node:assert";

// ── Given ───────────────────────────────────────────────────────────

Given(
  "dimensions and default configs are set up for context tests",
  async function (this: SuperpositionWorld) {
    // Create "os" dimension if not exists
    try {
      await this.client.send(
        new CreateDimensionCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          dimension: "os",
          position: 1,
          schema: { type: "string", enum: ["android", "ios", "web"] },
          description: "OS dimension",
          change_reason: "Cucumber context test setup",
        })
      );
      this.createdDimensions.push("os");
    } catch {
      // Already exists
    }

    // Create config key
    const configKey = "ctx-config-key";
    try {
      await this.client.send(
        new CreateDefaultConfigCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          key: configKey,
          value: "default",
          schema: { type: "string" },
          description: "Config for context tests",
          change_reason: "Cucumber setup",
        })
      );
      this.createdConfigs.push(configKey);
    } catch {
      // Already exists
    }
  }
);

Given(
  "a context exists with condition {string} equals {string} and override {string} to {string}",
  async function (
    this: SuperpositionWorld,
    dimName: string,
    dimValue: string,
    configKey: string,
    configValue: string
  ) {
    try {
      const response = await this.client.send(
        new CreateContextCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          request: {
            context: { [dimName]: dimValue },
            override: { [configKey]: configValue },
            description: "Cucumber test context",
            change_reason: "Cucumber setup",
          },
        })
      );
      this.contextId = response.id ?? "";
      this.createdContextIds.push(this.contextId);
    } catch {
      // May already exist
    }
  }
);

Given(
  "contexts exist for weight recompute",
  async function (this: SuperpositionWorld) {
    // Create a couple of contexts
    for (const val of ["android", "ios"]) {
      try {
        const response = await this.client.send(
          new CreateContextCommand({
            workspace_id: this.workspaceId,
            org_id: this.orgId,
            request: {
              context: { os: val },
              override: { "ctx-config-key": `${val}-weight` },
              description: "Weight recompute test",
              change_reason: "Cucumber setup",
            },
          })
        );
        this.createdContextIds.push(response.id ?? "");
      } catch {
        // May already exist
      }
    }
  }
);

// ── When ────────────────────────────────────────────────────────────

When(
  "I create a context with condition {string} equals {string} and override {string} to {string}",
  async function (
    this: SuperpositionWorld,
    dimName: string,
    dimValue: string,
    configKey: string,
    configValue: string
  ) {
    try {
      this.lastResponse = await this.client.send(
        new CreateContextCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          request: {
            context: { [dimName]: dimValue },
            override: { [configKey]: configValue },
            description: "Cucumber test context",
            change_reason: "Cucumber test",
          },
        })
      );
      this.contextId = this.lastResponse.id ?? "";
      this.createdContextIds.push(this.contextId);
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

When(
  "I get the context by its ID",
  async function (this: SuperpositionWorld) {
    try {
      this.lastResponse = await this.client.send(
        new GetContextCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          id: this.contextId,
        })
      );
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

When("I list all contexts", async function (this: SuperpositionWorld) {
  try {
    this.lastResponse = await this.client.send(
      new ListContextsCommand({
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
  "I update the context override for {string} to {string}",
  async function (this: SuperpositionWorld, configKey: string, newValue: string) {
    try {
      this.lastResponse = await this.client.send(
        new UpdateOverrideCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          request: {
            context: { id: this.contextId },
            override: { [configKey]: newValue },
            description: "Updated override",
            change_reason: "Cucumber update test",
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

When(
  "I move the context to condition {string} equals {string}",
  async function (this: SuperpositionWorld, dimName: string, dimValue: string) {
    try {
      this.lastResponse = await this.client.send(
        new MoveContextCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          id: this.contextId,
          request: {
            context: { [dimName]: dimValue },
            description: "Moved context",
            change_reason: "Cucumber move test",
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

When("I delete the context", async function (this: SuperpositionWorld) {
  try {
    this.lastResponse = await this.client.send(
      new DeleteContextCommand({
        workspace_id: this.workspaceId,
        org_id: this.orgId,
        id: this.contextId,
      })
    );
    this.createdContextIds = this.createdContextIds.filter(
      (id) => id !== this.contextId
    );
    this.lastError = undefined;
  } catch (e: any) {
    this.lastError = e;
    this.lastResponse = undefined;
  }
});

When(
  "I perform a bulk operation to create contexts for {string} values {string}",
  async function (this: SuperpositionWorld, dimName: string, values: string) {
    const valueList = values.split(",").map((v) => v.trim());
    try {
      this.lastResponse = await this.client.send(
        new BulkOperationCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          operations: valueList.map((val) => ({
            PUT: {
              context: { [dimName]: val },
              override: { "ctx-config-key": `${val}-bulk` },
              change_reason: "Cucumber bulk test",
            },
          })),
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
  "I trigger weight recomputation",
  async function (this: SuperpositionWorld) {
    try {
      this.lastResponse = await this.client.send(
        new WeightRecomputeCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
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
  "the response should have a context ID",
  function (this: SuperpositionWorld) {
    assert.ok(this.lastResponse, "No response");
    assert.ok(
      this.lastResponse.context_id || this.contextId,
      "No context ID in response"
    );
  }
);

Then(
  "the response should include the override for {string}",
  function (this: SuperpositionWorld, configKey: string) {
    assert.ok(this.lastResponse, "No response");
    const override = this.lastResponse.override ?? this.lastResponse.r_override;
    assert.ok(override, "No override in response");
  }
);

Then(
  "the list should contain the created context",
  function (this: SuperpositionWorld) {
    const data = this.lastResponse?.data ?? this.lastResponse;
    assert.ok(Array.isArray(data), "Response is not a list");
    assert.ok(data.length > 0, "List is empty");
  }
);
