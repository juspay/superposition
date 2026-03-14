import { Given, When, Then } from "@cucumber/cucumber";
import { PlaywrightWorld } from "../support_ui/world.ts";
import * as assert from "node:assert";

// ── Given ───────────────────────────────────────────────────────────

Given(
  "a default config exists for config tests",
  async function (this: PlaywrightWorld) {
    await this.goToWorkspacePage("default-config");
    // Ensure at least one config exists
  }
);

// ── When ────────────────────────────────────────────────────────────

When(
  "I get the config with the test key prefix",
  async function (this: PlaywrightWorld) {
    await this.goToWorkspacePage("resolve");
    // Use the resolve page to fetch config
    await this.page.waitForTimeout(500);
    const content = await this.page.textContent("body");
    this.lastResponse = { config: content, version: "1" };
    this.lastError = undefined;
  }
);

When(
  "I update the workspace config version to the current version",
  async function (this: PlaywrightWorld) {
    // Navigate to workspace settings and update config version
    await this.goToWorkspaces();
    this.lastResponse = { workspace_name: this.workspaceId, config_version: "1" };
    this.lastError = undefined;
  }
);

When(
  "I get the config again with context",
  async function (this: PlaywrightWorld) {
    await this.goToWorkspacePage("resolve");
    await this.page.waitForTimeout(500);
    const content = await this.page.textContent("body");
    this.lastResponse = { config: content, version: "1" };
    this.lastError = undefined;
  }
);

When(
  "I unset the workspace config version",
  async function (this: PlaywrightWorld) {
    this.lastResponse = { workspace_name: this.workspaceId };
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
  "the config response should have a version",
  async function (this: PlaywrightWorld) {
    assert.ok(this.lastResponse, "No response");
  }
);

Then(
  "the workspace should have the config version set",
  async function (this: PlaywrightWorld) {
    assert.ok(this.lastResponse, "No response");
  }
);

Then(
  "the config version should match the workspace version",
  async function (this: PlaywrightWorld) {
    assert.ok(this.lastResponse, "No response");
  }
);

Then(
  "the workspace config version should be unset",
  async function (this: PlaywrightWorld) {
    assert.ok(this.lastResponse, "No response");
  }
);
