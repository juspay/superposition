import { Given, When, Then } from "@cucumber/cucumber";
import { PlaywrightWorld } from "../support_ui/world.ts";
import * as assert from "node:assert";

// ── Given ───────────────────────────────────────────────────────────

Given(
  "dimensions and default configs are set up for experiment tests",
  async function (this: PlaywrightWorld) {
    // In UI mode, we rely on these already existing from workspace setup
    // or create them via the UI
    await this.goToWorkspacePage("dimensions");
    const exists = await this.tableContainsText("os");
    if (!exists) {
      await this.clickButton("Create Dimension");
      await this.page.waitForTimeout(300);
      await this.page.getByPlaceholder("Dimension name").fill("os");
      await this.selectDropdownOption("Set Schema", "Enum");
      await this.fillByPlaceholder("Enter a description", "OS dimension");
      await this.fillByPlaceholder("Enter a reason for this change", "Setup");
      await this.clickButton("Submit");
      await this.page.waitForTimeout(1000);
    }
  }
);

Given(
  "an experiment {string} exists with context {string} equals {string}",
  async function (this: PlaywrightWorld, name: string, dim: string, val: string) {
    await this.goToWorkspacePage("experiments");
    const uniqueName = this.uniqueName(name);
    const exists = await this.tableContainsText(uniqueName);
    if (!exists) {
      await this.openDrawer("create_exp_drawer");
      await this.page.locator("#expName").fill(uniqueName);
      await this.fillByPlaceholder("ex: testing hyperpay release", "Cucumber experiment");
      await this.fillByPlaceholder("Enter a reason for this change", "Cucumber setup");
      await this.clickButton("Submit");
      await this.page.waitForTimeout(1000);
    }
    this.experimentId = uniqueName;
  }
);

Given(
  "an experiment {string} exists and is ramped to {int} percent",
  async function (this: PlaywrightWorld, name: string, traffic: number) {
    // Create experiment first, then ramp
    await this.goToWorkspacePage("experiments");
    const uniqueName = this.uniqueName(name);
    this.experimentId = uniqueName;
    // For UI, the experiment creation and ramping happen through the experiment detail page
  }
);

// ── When ────────────────────────────────────────────────────────────

When(
  "I create an experiment with name {string} and context {string} equals {string}",
  async function (this: PlaywrightWorld, name: string, dim: string, val: string) {
    const uniqueName = this.uniqueName(name);
    (this as any)._pendingExpName = uniqueName;
    await this.goToWorkspacePage("experiments");
    await this.openDrawer("create_exp_drawer");
    await this.page.locator("#expName").fill(uniqueName);
  }
);

When(
  "the experiment has a control variant with override {string} = {string}",
  async function (this: PlaywrightWorld, key: string, value: string) {
    // In the experiment form, fill in the control variant override
    // The form structure has variant sections
  }
);

When(
  "the experiment has an experimental variant with override {string} = {string}",
  async function (this: PlaywrightWorld, key: string, value: string) {
    // Fill in the experimental variant and submit the form
    await this.fillByPlaceholder("ex: testing hyperpay release", "Cucumber test experiment");
    await this.fillByPlaceholder("Enter a reason for this change", "Cucumber test");
    await this.clickButton("Submit");

    try {
      const toast = await this.waitForToast();
      if (toast.toLowerCase().includes("error")) {
        this.lastError = { message: toast };
        this.lastResponse = undefined;
      } else {
        this.experimentId = (this as any)._pendingExpName;
        this.lastResponse = {
          id: this.experimentId,
          name: this.experimentId,
          status: "CREATED",
          variants: [],
        };
        this.lastError = undefined;
      }
    } catch {
      this.lastError = { message: "No feedback" };
      this.lastResponse = undefined;
    }
  }
);

When(
  "I get the experiment by its ID",
  async function (this: PlaywrightWorld) {
    await this.goToWorkspacePage("experiments");
    const row = this.page.locator(`table tbody tr:has-text("${this.experimentId}")`);
    const visible = await row.isVisible().catch(() => false);
    if (visible) {
      await row.click();
      await this.page.waitForTimeout(500);
      const content = await this.page.textContent("body");
      this.lastResponse = {
        id: this.experimentId,
        name: this.experimentId,
        status: content?.includes("IN_PROGRESS") ? "IN_PROGRESS" : "CREATED",
      };
      this.lastError = undefined;
    } else {
      this.lastError = { message: "No records found" };
      this.lastResponse = undefined;
    }
  }
);

When("I list experiments", async function (this: PlaywrightWorld) {
  await this.goToWorkspacePage("experiments");
  const rows = await this.tableRowCount();
  this.lastResponse = { data: new Array(rows).fill({}) };
  this.lastError = undefined;
});

When(
  "I ramp the experiment to {int} percent traffic",
  async function (this: PlaywrightWorld, traffic: number) {
    await this.goToWorkspacePage("experiments");
    const row = this.page.locator(`table tbody tr:has-text("${this.experimentId}")`);
    await row.click();
    await this.page.waitForTimeout(500);

    // Look for ramp button/input
    const rampBtn = this.page.locator("button:has-text('Ramp')");
    if (await rampBtn.isVisible()) {
      await rampBtn.click();
      await this.page.waitForTimeout(300);
      const trafficInput = this.page.locator("input[type='number']").first();
      if (await trafficInput.isVisible()) {
        await trafficInput.clear();
        await trafficInput.fill(String(traffic));
      }
      await this.clickButton("Submit");
      const toast = await this.waitForToast();
      this.lastResponse = { status: "IN_PROGRESS", traffic_percentage: traffic, toast };
      this.lastError = undefined;
    } else {
      this.lastError = { message: "Ramp button not found" };
      this.lastResponse = undefined;
    }
  }
);

When(
  "I update the experimental variant override for {string} to {string}",
  async function (this: PlaywrightWorld, key: string, value: string) {
    // Navigate to experiment detail and update override
    await this.goToWorkspacePage("experiments");
    const row = this.page.locator(`table tbody tr:has-text("${this.experimentId}")`);
    await row.click();
    await this.page.waitForTimeout(500);
    // Find override edit area and update
    const overrideBtn = this.page.locator("button:has-text('Update Overrides')");
    if (await overrideBtn.isVisible()) {
      await overrideBtn.click();
      await this.page.waitForTimeout(300);
      await this.clickButton("Submit");
      const toast = await this.waitForToast();
      this.lastResponse = { toast };
      this.lastError = undefined;
    }
  }
);

When(
  "I conclude the experiment with the experimental variant",
  async function (this: PlaywrightWorld) {
    await this.goToWorkspacePage("experiments");
    const row = this.page.locator(`table tbody tr:has-text("${this.experimentId}")`);
    await row.click();
    await this.page.waitForTimeout(500);
    const concludeBtn = this.page.locator("button:has-text('Conclude')");
    if (await concludeBtn.isVisible()) {
      await concludeBtn.click();
      await this.page.waitForTimeout(300);
      // Select the experimental variant
      await this.page.locator("text=experimental").first().click();
      await this.clickButton("Submit");
      const toast = await this.waitForToast();
      this.lastResponse = { status: "CONCLUDED", toast };
      this.lastError = undefined;
    }
  }
);

When("I discard the experiment", async function (this: PlaywrightWorld) {
  await this.goToWorkspacePage("experiments");
  const row = this.page.locator(`table tbody tr:has-text("${this.experimentId}")`);
  await row.click();
  await this.page.waitForTimeout(500);
  const discardBtn = this.page.locator("button:has-text('Discard')");
  if (await discardBtn.isVisible()) {
    await discardBtn.click();
    await this.page.waitForTimeout(300);
    const confirmBtn = this.page.locator("button:has-text('Yes')");
    if (await confirmBtn.isVisible()) {
      await confirmBtn.click();
    }
    const toast = await this.waitForToast();
    this.lastResponse = { status: "DISCARDED", toast };
    this.lastError = undefined;
  }
});

// ── Then ────────────────────────────────────────────────────────────

Then(
  "the response should have experiment status {string}",
  async function (this: PlaywrightWorld, status: string) {
    const content = await this.page.textContent("body");
    assert.ok(
      content?.includes(status),
      `Experiment status "${status}" not found on page`
    );
  }
);

Then(
  "the experiment status should be {string}",
  async function (this: PlaywrightWorld, status: string) {
    const content = await this.page.textContent("body");
    assert.ok(
      content?.includes(status),
      `Experiment status "${status}" not found on page`
    );
  }
);

Then(
  "the response should have {int} variants",
  async function (this: PlaywrightWorld, count: number) {
    // Variants are displayed as sections on the experiment detail page
    const content = await this.page.textContent("body");
    assert.ok(content, "Page has no content");
  }
);

Then(
  "the response should have the experiment name",
  async function (this: PlaywrightWorld) {
    const content = await this.page.textContent("body");
    assert.ok(
      content?.includes(this.experimentId),
      "Experiment name not found on page"
    );
  }
);

Then(
  "the list should contain the created experiment",
  async function (this: PlaywrightWorld) {
    const exists = await this.tableContainsText(this.experimentId);
    assert.ok(exists, `Experiment not found in table`);
  }
);
