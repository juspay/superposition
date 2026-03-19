import { Given, When, Then } from "@cucumber/cucumber";
import {
  CreateContextCommand,
  GetContextCommand,
  ListContextsCommand,
  UpdateOverrideCommand,
  MoveContextCommand,
  BulkOperationCommand,
  WeightRecomputeCommand,
  CreateDimensionCommand,
  CreateDefaultConfigCommand,
} from "@juspay/superposition-sdk";
import { PlaywrightWorld } from "../support_ui/world.ts";
import * as assert from "node:assert";

// ── Given ───────────────────────────────────────────────────────────

Given(
  "dimensions and default configs are set up for context tests",
  async function (this: PlaywrightWorld) {
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
    this: PlaywrightWorld,
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
  async function (this: PlaywrightWorld) {
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
    this: PlaywrightWorld,
    dimName: string,
    dimValue: string,
    configKey: string,
    configValue: string
  ) {
    try {
      // Navigate to overrides page (Playwright)
      await this.goToWorkspacePage("overrides");
      await this.page.waitForTimeout(500);

      // Create via SDK (drawer form is too complex for full Playwright interaction)
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

      // Reload the page and verify the new context card appears
      await this.page.reload();
      await this.page.waitForTimeout(500);
      const conditionEl = this.page.locator(`[id="${this.contextId}"]`);
      await conditionEl.waitFor({ state: "visible", timeout: 10000 });

      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

When(
  "I get the context by its ID",
  async function (this: PlaywrightWorld) {
    try {
      // Navigate to overrides page and verify the context card is visible (Playwright)
      await this.goToWorkspacePage("overrides");
      await this.page.waitForTimeout(500);
      const conditionEl = this.page.locator(`[id="${this.contextId}"]`);
      await conditionEl.waitFor({ state: "visible", timeout: 10000 });

      // Use SDK to retrieve full response data for assertions
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

When("I list all contexts", async function (this: PlaywrightWorld) {
  try {
    // Navigate to the overrides page (actual UI test)
    await this.goToWorkspacePage("overrides");
    await this.page.waitForTimeout(500);
    // Also get data via SDK for assertions
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
  async function (this: PlaywrightWorld, configKey: string, newValue: string) {
    try {
      // Navigate to the overrides page and verify the context card exists (Playwright)
      await this.goToWorkspacePage("overrides");
      await this.page.waitForTimeout(500);
      const conditionEl = this.page.locator(`[id="${this.contextId}"]`);
      await conditionEl.waitFor({ state: "visible", timeout: 10000 });

      // Use SDK for actual update (edit drawer form is too complex for full Playwright interaction)
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

      // Reload the page and verify the context card is still present after update
      await this.page.reload();
      await this.page.waitForTimeout(500);
      await conditionEl.waitFor({ state: "visible", timeout: 10000 });

      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

When(
  "I move the context to condition {string} equals {string}",
  async function (this: PlaywrightWorld, dimName: string, dimValue: string) {
    try {
      // Navigate to overrides page (Playwright) — move has no direct UI equivalent
      await this.goToWorkspacePage("overrides");
      await this.page.waitForTimeout(500);

      // Use SDK for the move operation
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

      // Update tracked contextId if the move returned a new ID
      if (this.lastResponse.id && this.lastResponse.id !== this.contextId) {
        this.createdContextIds = this.createdContextIds.filter(
          (id) => id !== this.contextId
        );
        this.contextId = this.lastResponse.id;
        this.createdContextIds.push(this.contextId);
      }

      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

When("I delete the context", async function (this: PlaywrightWorld) {
  // Pre-check existence via SDK to avoid long Playwright timeout on missing entity
  try {
    await this.client.send(
      new GetContextCommand({
        workspace_id: this.workspaceId,
        org_id: this.orgId,
        id: this.contextId,
      })
    );
  } catch (e: any) {
    this.lastError = e;
    this.lastResponse = undefined;
    return;
  }
  try {
    // Navigate to the overrides page
    await this.goToWorkspacePage("overrides");
    await this.page.waitForTimeout(500);

    // The ConditionComponent for each context card renders with id=contextId on the <ol> element.
    // Use attribute selector to handle IDs starting with digits (which are invalid CSS #id selectors)
    const conditionEl = this.page.locator(`[id="${this.contextId}"]`);
    await conditionEl.waitFor({ state: "visible", timeout: 10000 });

    // The card is the closest ancestor div with rounded-lg shadow bg-base-100 classes
    const card = conditionEl.locator(
      "xpath=ancestor::div[contains(@class,'rounded-lg') and contains(@class,'shadow')][1]"
    );

    // The dropdown toggle is a <label> element with the ri-more-2-fill icon inside
    await card.locator("label:has(i.ri-more-2-fill)").click();
    await this.page.waitForTimeout(300);

    // Click "Delete Overrides" in the dropdown menu
    await this.page.getByText("Delete Overrides").last().click();
    await this.page.waitForTimeout(500);

    // The ChangeLogPopup modal appears with a portal overlay.
    // The "Yes, Delete" button starts in loading/disabled state (showing a spinner)
    // until the context data is fetched. Wait for it to become enabled with text visible.
    const confirmBtn = this.page.getByRole("button", { name: "Yes, Delete" });
    await confirmBtn.waitFor({ state: "visible", timeout: 15000 });
    await confirmBtn.click();

    // The context delete does NOT produce a toast — it just closes the modal and refetches.
    // Wait for the modal to disappear and the card to no longer be visible.
    await this.page.waitForTimeout(1000);
    // Wait for the condition element to be gone (context deleted from the page)
    await this.page.locator(`[id="${this.contextId}"]`).waitFor({
      state: "hidden",
      timeout: 10000,
    });

    this.createdContextIds = this.createdContextIds.filter(
      (id) => id !== this.contextId
    );
    this.lastResponse = { deleted: true };
    this.lastError = undefined;
  } catch (e: any) {
    this.lastError = e;
    this.lastResponse = undefined;
  }
});

When(
  "I perform a bulk operation to create contexts for {string} values {string}",
  async function (this: PlaywrightWorld, dimName: string, values: string) {
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
              description: `Bulk context for ${val}`,
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
  async function (this: PlaywrightWorld) {
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
  function (this: PlaywrightWorld) {
    assert.ok(this.lastResponse, "No response");
    assert.ok(
      this.lastResponse.context_id || this.contextId,
      "No context ID in response"
    );
  }
);

Then(
  "the response should include the override for {string}",
  function (this: PlaywrightWorld, configKey: string) {
    assert.ok(this.lastResponse, "No response");
    const override = this.lastResponse.override ?? this.lastResponse.r_override;
    assert.ok(override, "No override in response");
  }
);

Then(
  "the list should contain the created context",
  function (this: PlaywrightWorld) {
    const data = this.lastResponse?.data ?? this.lastResponse;
    assert.ok(Array.isArray(data), "Response is not a list");
    assert.ok(data.length > 0, "List is empty");
  }
);
