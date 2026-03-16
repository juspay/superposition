import { Given, When, Then } from "@cucumber/cucumber";
import {
  CreateDimensionCommand,
  GetDimensionCommand,
  ListDimensionsCommand,
  UpdateDimensionCommand,
  DeleteDimensionCommand,
} from "@juspay/superposition-sdk";
import { PlaywrightWorld } from "../support_ui/world.ts";
import * as assert from "node:assert";

function nextPosition(): number {
  return 1; // Always insert at position 1 (front) to avoid exceeding dimension count
}

// ── Given ───────────────────────────────────────────────────────────

Given(
  "a dimension {string} exists with schema type {string}",
  async function (this: PlaywrightWorld, name: string, schemaType: string) {
    const uniqueName = this.uniqueName(name);
    this.dimensionName = uniqueName;
    try {
      await this.client.send(
        new CreateDimensionCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          dimension: uniqueName,
          position: nextPosition(),
          schema: { type: schemaType },
          description: `Test dimension ${uniqueName}`,
          change_reason: "Cucumber test setup",
        })
      );
      this.createdDimensions.push(uniqueName);
    } catch {
      // Already exists
    }
  }
);

// ── When ────────────────────────────────────────────────────────────

When(
  "I create a dimension with name {string} and schema type {string}",
  async function (this: PlaywrightWorld, name: string, schemaType: string) {
    const uniqueName = this.uniqueName(name);
    this.dimensionName = uniqueName;
    try {
      this.lastResponse = await this.client.send(
        new CreateDimensionCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          dimension: uniqueName,
          position: nextPosition(),
          schema: { type: schemaType },
          description: `Test dimension ${uniqueName}`,
          change_reason: "Cucumber test",
        })
      );
      this.createdDimensions.push(uniqueName);
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

When(
  "I create a dimension with name {string} and enum values {string}",
  async function (this: PlaywrightWorld, name: string, values: string) {
    const uniqueName = this.uniqueName(name);
    this.dimensionName = uniqueName;
    const enumValues = values.split(",").map((v) => v.trim());
    try {
      this.lastResponse = await this.client.send(
        new CreateDimensionCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          dimension: uniqueName,
          position: nextPosition(),
          schema: { type: "string", enum: enumValues },
          description: `Enum dimension ${uniqueName}`,
          change_reason: "Cucumber test",
        })
      );
      this.createdDimensions.push(uniqueName);
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

When(
  "I get dimension {string}",
  async function (this: PlaywrightWorld, name: string) {
    try {
      this.lastResponse = await this.client.send(
        new GetDimensionCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          dimension: this.dimensionName,
        })
      );
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

When("I list all dimensions", async function (this: PlaywrightWorld) {
  try {
    this.lastResponse = await this.client.send(
      new ListDimensionsCommand({
        workspace_id: this.workspaceId,
        org_id: this.orgId,
        count: 200,
      })
    );
    this.lastError = undefined;
  } catch (e: any) {
    this.lastError = e;
    this.lastResponse = undefined;
  }
});

When(
  "I update dimension {string} description to {string}",
  async function (this: PlaywrightWorld, name: string, description: string) {
    try {
      this.lastResponse = await this.client.send(
        new UpdateDimensionCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          dimension: this.dimensionName,
          description,
          change_reason: "Cucumber update test",
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
  "I delete dimension {string}",
  async function (this: PlaywrightWorld, name: string) {
    try {
      this.lastResponse = await this.client.send(
        new DeleteDimensionCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          dimension: this.dimensionName,
        })
      );
      // Remove from cleanup list since we deleted it
      this.createdDimensions = this.createdDimensions.filter(
        (d) => d !== this.dimensionName
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
  "the response should have dimension name {string}",
  function (this: PlaywrightWorld, name: string) {
    assert.ok(this.lastResponse, "No response");
    assert.strictEqual(this.lastResponse.dimension, this.dimensionName);
  }
);

Then(
  "the list should contain dimension {string}",
  function (this: PlaywrightWorld, name: string) {
    const data = this.lastResponse?.data;
    assert.ok(Array.isArray(data), "No list data");
    const found = data.find((d: any) => d.dimension === this.dimensionName);
    assert.ok(found, `Dimension "${this.dimensionName}" not found in list`);
  }
);
