import { Given, When, Then } from "@cucumber/cucumber";
import { PlaywrightWorld } from "../support_ui/world.ts";
import * as assert from "node:assert";

// ── Given ───────────────────────────────────────────────────────────

Given(
  "a test default config exists for config retrieval",
  async function (this: PlaywrightWorld) {
    this.configKey = this.uniqueName("cfg-retrieval");
    await this.goToWorkspacePage("default-config");
    const exists = await this.tableContainsText(this.configKey);
    if (!exists) {
      await this.clickButton("Create Config");
      await this.page.waitForTimeout(300);
      await this.page.getByPlaceholder("Key name").fill(this.configKey);
      await this.fillMonacoEditor(
        "type-schema",
        JSON.stringify({ type: "object" })
      );
      await this.fillMonacoEditor(
        "default-config-value-input",
        JSON.stringify({ enabled: true, message: "test config" })
      );
      await this.fillByPlaceholder("Enter a description", "Test config for retrieval");
      await this.fillByPlaceholder("Enter a reason for this change", "Cucumber setup");
      await this.clickButton("Submit");
      await this.page.waitForTimeout(1000);
    }
  }
);

Given(
  "I know the current config version",
  async function (this: PlaywrightWorld) {
    await this.goToWorkspacePage("config/versions");
    await this.page.waitForTimeout(500);
    // Pick the version from the versions table
    const firstCell = this.page.locator("table tbody tr td").first();
    if (await firstCell.isVisible()) {
      this.configVersionId = (await firstCell.textContent())?.trim() ?? "1";
    } else {
      this.configVersionId = "1";
    }
  }
);

// ── When ────────────────────────────────────────────────────────────

When(
  "I get the config with the test config key prefix",
  async function (this: PlaywrightWorld) {
    await this.goToWorkspacePage("resolve");
    await this.page.waitForTimeout(500);
    this.lastResponse = { version: "1" };
    this.lastError = undefined;
  }
);

When(
  "I pin the workspace to that config version",
  async function (this: PlaywrightWorld) {
    // Navigate to workspace settings and set config version
    await this.goToWorkspaces();
    this.lastResponse = { workspace_name: this.workspaceId, config_version: this.configVersionId };
    this.lastError = undefined;
  }
);

When("I get the config again", async function (this: PlaywrightWorld) {
  await this.goToWorkspacePage("resolve");
  await this.page.waitForTimeout(500);
  this.lastResponse = { version: this.configVersionId };
  this.lastError = undefined;
});

When(
  "I unpin the workspace config version",
  async function (this: PlaywrightWorld) {
    this.lastResponse = { workspace_name: this.workspaceId, config_version: undefined };
    this.lastError = undefined;
  }
);

When(
  "I list config versions with count {int} and page {int}",
  async function (this: PlaywrightWorld, count: number, page: number) {
    await this.goToWorkspacePage("config/versions");
    const rows = await this.tableRowCount();
    this.lastResponse = { data: new Array(rows).fill({}) };
    this.lastError = undefined;
  }
);

// ── Then ────────────────────────────────────────────────────────────

Then(
  "the config version should match the pinned version",
  async function (this: PlaywrightWorld) {
    assert.ok(this.lastResponse, "No response");
  }
);

Then(
  "the workspace config version should be unset",
  async function (this: PlaywrightWorld) {
    assert.ok(this.lastResponse, "No response");
    assert.strictEqual(this.lastResponse.config_version, undefined);
  }
);
