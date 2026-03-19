import { Given, When, Then } from "@cucumber/cucumber";
import {
  CreateDimensionCommand,
  GetDimensionCommand,
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

// PLAYWRIGHT: Navigate to dimension detail page, then fetch via SDK for assertions
When(
  "I get dimension {string}",
  async function (this: PlaywrightWorld, name: string) {
    try {
      // Navigate to the detail page for UI interaction
      await this.goToDetailPage("dimensions", this.dimensionName);
      await this.page.waitForTimeout(300);

      // Fetch via SDK for response assertions (detail page doesn't expose all fields in DOM)
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

// PLAYWRIGHT: edit dimension description via UI detail page (full-page form)
When(
  "I update dimension {string} description to {string}",
  async function (this: PlaywrightWorld, name: string, description: string) {
    try {
      await this.goToDetailPage("dimensions", this.dimensionName);
      await this.page.waitForTimeout(300);

      // Edit is an <a> link on dimension detail page, not a button
      await this.page.getByRole("link", { name: "Edit" }).click();
      await this.page.waitForLoadState("networkidle");
      await this.page.waitForTimeout(300);

      const descInput = this.page.getByPlaceholder("Enter a description");
      await descInput.clear();
      await descInput.fill(description);

      await this.page
        .getByPlaceholder("Enter a reason for this change")
        .fill("Cucumber update description");

      await this.page.getByRole("button", { name: "Submit" }).last().click();

      await this.page.getByRole("button", { name: "Yes, Update" }).click();

      const toastText = await this.waitForToast();
      if (
        toastText.toLowerCase().includes("error") ||
        toastText.toLowerCase().includes("failed")
      ) {
        this.lastError = { message: toastText };
        this.lastResponse = undefined;
      } else {
        // Fetch updated dimension via SDK for response assertions
        this.lastResponse = await this.client.send(
          new GetDimensionCommand({
            workspace_id: this.workspaceId,
            org_id: this.orgId,
            dimension: this.dimensionName,
          })
        );
        this.lastError = undefined;
      }
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

// PLAYWRIGHT: delete dimension via UI detail page
When(
  "I delete dimension {string}",
  async function (this: PlaywrightWorld, name: string) {
    // Check if dimension exists first; if not, use SDK to get proper error
    try {
      await this.client.send(
        new GetDimensionCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          dimension: this.dimensionName,
        })
      );
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
      return;
    }
    try {
      await this.goToDetailPage("dimensions", this.dimensionName);
      await this.page.waitForTimeout(300);

      await this.page.getByRole("button", { name: "Delete" }).click();
      await this.page.waitForTimeout(300);

      await this.page.getByRole("button", { name: "Yes, Delete" }).click();

      const toastText = await this.waitForToast();
      if (
        toastText.toLowerCase().includes("error") ||
        toastText.toLowerCase().includes("failed")
      ) {
        this.lastError = { message: toastText };
        this.lastResponse = undefined;
      } else {
        // Remove from cleanup list since we deleted it
        this.createdDimensions = this.createdDimensions.filter(
          (d) => d !== this.dimensionName
        );
        this.lastResponse = { deleted: true, toast: toastText };
        this.lastError = undefined;
      }
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
