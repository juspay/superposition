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

// Dimension creation uses SDK because the Monaco JSON editor is difficult to automate reliably.
// After creation, we verify the dimension appears in the UI list.
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

// SDK: no detail page easily accessible by name
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

// PLAYWRIGHT: Navigate to dimensions page, read the table data
When("I list all dimensions", async function (this: PlaywrightWorld) {
  try {
    await this.goToWorkspacePage("dimensions");

    // Wait for the table to be present
    await this.page.locator("table").waitFor({ state: "visible", timeout: 10000 });

    // Extract dimension data from the table
    const rows = this.page.locator("table tbody tr");
    const rowCount = await rows.count();
    const dimensions: any[] = [];

    for (let i = 0; i < rowCount; i++) {
      const row = rows.nth(i);
      const cells = row.locator("td");
      const dimensionName = (await cells.nth(0).textContent())?.trim() ?? "";
      dimensions.push({ dimension: dimensionName });
    }

    this.lastResponse = { data: dimensions };
    this.lastError = undefined;
  } catch (e: any) {
    this.lastError = e;
    this.lastResponse = undefined;
  }
});

// SDK: no UI edit form available
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

// SDK: no UI delete button available
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

// PLAYWRIGHT: Verify dimension exists in the table on the dimensions page
Then(
  "the list should contain dimension {string}",
  async function (this: PlaywrightWorld, name: string) {
    // If lastResponse came from the Playwright table read, check it
    if (this.lastResponse?.data && Array.isArray(this.lastResponse.data)) {
      const found = this.lastResponse.data.find(
        (d: any) => d.dimension === this.dimensionName
      );
      if (found) return; // Found in existing response data
    }

    // Fallback: navigate to dimensions page and verify in the table directly
    await this.goToWorkspacePage("dimensions");
    await this.page.locator("table").waitFor({ state: "visible", timeout: 10000 });
    const tableText = await this.page.locator("table").textContent();
    assert.ok(
      tableText?.includes(this.dimensionName),
      `Dimension "${this.dimensionName}" not found in dimensions table`
    );
  }
);
