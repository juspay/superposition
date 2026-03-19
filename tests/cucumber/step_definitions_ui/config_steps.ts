import { Given, When, Then } from "@cucumber/cucumber";
import {
  GetConfigCommand,
  ListVersionsCommand,
  UpdateWorkspaceCommand,
  CreateDefaultConfigCommand,
  DeleteDefaultConfigCommand,
} from "@juspay/superposition-sdk";
import { PlaywrightWorld } from "../support_ui/world.ts";
import * as assert from "node:assert";

// ── Given ───────────────────────────────────────────────────────────

Given(
  "a test default config exists for config retrieval",
  async function (this: PlaywrightWorld) {
    this.configKey = this.uniqueName("cfg-retrieval");
    try {
      await this.client.send(
        new CreateDefaultConfigCommand({
          key: this.configKey,
          value: { enabled: true, message: "test config" },
          schema: { type: "object" },
          description: "Test config for retrieval tests",
          change_reason: "Cucumber setup",
          workspace_id: this.workspaceId,
          org_id: this.orgId,
        })
      );
      this.createdConfigs.push(this.configKey);
    } catch {
      // Already exists
    }
  }
);

Given(
  "I know the current config version",
  async function (this: PlaywrightWorld) {
    const cmd = new GetConfigCommand({
      workspace_id: this.workspaceId,
      org_id: this.orgId,
      prefix: [this.configKey],
    });
    const out = await this.client.send(cmd);
    this.configVersionId = out.version ?? undefined;
    assert.ok(this.configVersionId, "Could not determine config version");
  }
);

// ── When ────────────────────────────────────────────────────────────

When(
  "I get the config with the test config key prefix",
  async function (this: PlaywrightWorld) {
    try {
      // Navigate to default-config page for UI context
      await this.goToWorkspacePage("default-config");
      await this.page.waitForTimeout(300);

      // Fetch via SDK for response assertions
      this.lastResponse = await this.client.send(
        new GetConfigCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          prefix: [this.configKey],
        })
      );
      this.configVersionId = this.lastResponse.version ?? undefined;
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

When(
  "I pin the workspace to that config version",
  async function (this: PlaywrightWorld) {
    try {
      this.lastResponse = await this.client.send(
        new UpdateWorkspaceCommand({
          org_id: this.orgId,
          workspace_name: this.workspaceId,
          config_version: this.configVersionId,
        })
      );
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

When("I get the config again", async function (this: PlaywrightWorld) {
  try {
    // Navigate to default-config page for UI context
    await this.goToWorkspacePage("default-config");
    await this.page.waitForTimeout(300);

    // Fetch via SDK for response assertions
    this.lastResponse = await this.client.send(
      new GetConfigCommand({
        workspace_id: this.workspaceId,
        org_id: this.orgId,
        prefix: [this.configKey],
        context: {},
      })
    );
    this.lastError = undefined;
  } catch (e: any) {
    this.lastError = e;
    this.lastResponse = undefined;
  }
});

When(
  "I unpin the workspace config version",
  async function (this: PlaywrightWorld) {
    try {
      this.lastResponse = await this.client.send(
        new UpdateWorkspaceCommand({
          org_id: this.orgId,
          workspace_name: this.workspaceId,
          description: "Unset config version",
          config_version: "null",
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
  "I list config versions with count {int} and page {int}",
  async function (this: PlaywrightWorld, count: number, page: number) {
    try {
      // Navigate to the config versions page (actual UI test)
      await this.goToWorkspacePage("config/versions");
      await this.page.waitForTimeout(500);
      // Also get data via SDK for assertions
      this.lastResponse = await this.client.send(
        new ListVersionsCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          count,
          page,
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
  "the config version should match the pinned version",
  function (this: PlaywrightWorld) {
    assert.ok(this.lastResponse, "No response");
    assert.ok(this.lastResponse.version, "No version in response");
    assert.strictEqual(
      this.lastResponse.version?.toString(),
      this.configVersionId?.toString()
    );
  }
);

Then(
  "the workspace config version should be unset",
  function (this: PlaywrightWorld) {
    assert.ok(this.lastResponse, "No response");
    assert.strictEqual(this.lastResponse.config_version, undefined);
  }
);
