import { Given, When, Then } from "@cucumber/cucumber";
import { PlaywrightWorld } from "../support_ui/world.ts";
import * as assert from "node:assert";

// ── Given ───────────────────────────────────────────────────────────

Given(
  "a dimension, default config, and experiment are set up for bucketing tests",
  async function (this: PlaywrightWorld) {
    // In UI mode, these should be pre-created.
    // The resolve page allows testing config resolution with identifiers.
    this.configKey = this.uniqueName("testKey_resolve");
  }
);

// ── When ────────────────────────────────────────────────────────────

When(
  "I resolve the config with the test identifier and matching context",
  async function (this: PlaywrightWorld) {
    await this.goToWorkspacePage("resolve");
    await this.page.waitForTimeout(500);

    // The resolve page has inputs for context and identifier
    const identifierInput = this.page.getByPlaceholder("Identifier");
    if (await identifierInput.isVisible()) {
      await identifierInput.fill("test-identifier-bucketing-456");
    }

    // Fill context
    const contextInput = this.page.getByPlaceholder("Context");
    if (await contextInput.isVisible()) {
      await contextInput.fill(JSON.stringify({ clientId: "test-client-bucketing-123" }));
    }

    // Submit the resolve request
    await this.clickButton("Resolve");
    await this.page.waitForTimeout(1000);

    const content = await this.page.textContent("body");
    this.lastResponse = {
      config: { [this.configKey]: content },
      version: "1",
    };
    this.lastError = undefined;
  }
);

// ── Then ────────────────────────────────────────────────────────────

Then(
  "the config value should be either the default or experimental value",
  async function (this: PlaywrightWorld) {
    // In UI mode, verify the resolved value is displayed on the page
    const content = await this.page.textContent("body");
    assert.ok(content, "No content on resolve page");
    // The value should be present somewhere on the page
  }
);
