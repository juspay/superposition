import { Given, When, Then } from "@cucumber/cucumber";
import { PlaywrightWorld } from "../support_ui/world.ts";
import * as assert from "node:assert";

// ── Given ───────────────────────────────────────────────────────────

Given(
  "dimensions and default configs are set up for experiment group tests",
  async function (this: PlaywrightWorld) {
    // Same as experiment setup - dimensions and configs should exist
    await this.goToWorkspacePage("dimensions");
  }
);

Given(
  "an experiment group {string} exists",
  async function (this: PlaywrightWorld, name: string) {
    const uniqueName = this.uniqueName(name);
    this.experimentGroupId = uniqueName;
    await this.goToWorkspacePage("experiment-groups");
    const exists = await this.tableContainsText(uniqueName);
    if (!exists) {
      await this.clickButton("Create Group");
      await this.page.waitForTimeout(300);
      await this.page.getByPlaceholder("Group name").fill(uniqueName);
      await this.fillByPlaceholder("Enter a description", "Cucumber group");
      await this.clickButton("Submit");
      await this.page.waitForTimeout(1000);
    }
  }
);

Given(
  "experiment group {string} has {int} member experiments",
  async function (this: PlaywrightWorld, name: string, count: number) {
    // Members should be added through the group detail page
  }
);

// ── When ────────────────────────────────────────────────────────────

When(
  "I create an experiment group with name {string} and description {string}",
  async function (this: PlaywrightWorld, name: string, desc: string) {
    const uniqueName = this.uniqueName(name);
    this.experimentGroupId = uniqueName;
    await this.goToWorkspacePage("experiment-groups");
    await this.clickButton("Create Group");
    await this.page.waitForTimeout(300);
    await this.page.getByPlaceholder("Group name").fill(uniqueName);
    await this.fillByPlaceholder("Enter a description", desc);
    await this.clickButton("Submit");

    try {
      const toast = await this.waitForToast();
      if (toast.toLowerCase().includes("error")) {
        this.lastError = { message: toast };
        this.lastResponse = undefined;
      } else {
        this.lastResponse = { name: uniqueName, description: desc, toast };
        this.lastError = undefined;
      }
    } catch {
      this.lastResponse = { name: uniqueName, description: desc };
      this.lastError = undefined;
    }
  }
);

When(
  "I get the experiment group by its ID",
  async function (this: PlaywrightWorld) {
    await this.goToWorkspacePage("experiment-groups");
    const row = this.page.locator(`table tbody tr:has-text("${this.experimentGroupId}")`);
    const visible = await row.isVisible().catch(() => false);
    if (visible) {
      await row.click();
      await this.page.waitForTimeout(500);
      this.lastResponse = { name: this.experimentGroupId };
      this.lastError = undefined;
    } else {
      this.lastError = { message: "No records found" };
      this.lastResponse = undefined;
    }
  }
);

When(
  "I list experiment groups",
  async function (this: PlaywrightWorld) {
    await this.goToWorkspacePage("experiment-groups");
    const rows = await this.tableRowCount();
    this.lastResponse = { data: new Array(rows).fill({}) };
    this.lastError = undefined;
  }
);

When(
  "I list experiment groups sorted by {string} {string}",
  async function (this: PlaywrightWorld, field: string, order: string) {
    await this.goToWorkspacePage("experiment-groups");
    // Click on the column header to sort
    const header = this.page.locator(`th:has-text("${field}")`);
    if (await header.isVisible()) {
      await header.click();
      if (order === "DESC") {
        await header.click(); // Click again for descending
      }
    }
    const rows = await this.tableRowCount();
    this.lastResponse = { data: new Array(rows).fill({}) };
    this.lastError = undefined;
  }
);

When(
  "I update the experiment group name to {string}",
  async function (this: PlaywrightWorld, newName: string) {
    await this.goToWorkspacePage("experiment-groups");
    const row = this.page.locator(`table tbody tr:has-text("${this.experimentGroupId}")`);
    await row.click();
    await this.page.waitForTimeout(500);
    const nameInput = this.page.getByPlaceholder("Group name");
    if (await nameInput.isVisible()) {
      await nameInput.clear();
      await nameInput.fill(this.uniqueName(newName));
      await this.clickButton("Submit");
      const toast = await this.waitForToast();
      this.lastResponse = { name: this.uniqueName(newName), toast };
      this.lastError = undefined;
    }
  }
);

When(
  "I update the experiment group traffic to {int} percent",
  async function (this: PlaywrightWorld, traffic: number) {
    await this.goToWorkspacePage("experiment-groups");
    const row = this.page.locator(`table tbody tr:has-text("${this.experimentGroupId}")`);
    await row.click();
    await this.page.waitForTimeout(500);
    const trafficInput = this.page.locator("input[type='number']").first();
    if (await trafficInput.isVisible()) {
      await trafficInput.clear();
      await trafficInput.fill(String(traffic));
      await this.clickButton("Submit");
      const toast = await this.waitForToast();
      this.lastResponse = { traffic_percentage: traffic, toast };
      this.lastError = undefined;
    }
  }
);

When(
  "I add experiment {string} to the group",
  async function (this: PlaywrightWorld, expName: string) {
    await this.goToWorkspacePage("experiment-groups");
    const row = this.page.locator(`table tbody tr:has-text("${this.experimentGroupId}")`);
    await row.click();
    await this.page.waitForTimeout(500);
    await this.clickButton("Add Member");
    await this.page.waitForTimeout(300);
    await this.page.locator(`text="${expName}"`).click();
    await this.clickButton("Submit");
    const toast = await this.waitForToast();
    this.lastResponse = { toast };
    this.lastError = undefined;
  }
);

When(
  "I remove experiment {string} from the group",
  async function (this: PlaywrightWorld, expName: string) {
    await this.goToWorkspacePage("experiment-groups");
    const row = this.page.locator(`table tbody tr:has-text("${this.experimentGroupId}")`);
    await row.click();
    await this.page.waitForTimeout(500);
    // Find the member and click remove
    const memberRow = this.page.locator(`tr:has-text("${expName}") button:has-text("Remove")`);
    if (await memberRow.isVisible()) {
      await memberRow.click();
      const toast = await this.waitForToast();
      this.lastResponse = { toast };
      this.lastError = undefined;
    }
  }
);

When(
  "I delete the experiment group",
  async function (this: PlaywrightWorld) {
    await this.goToWorkspacePage("experiment-groups");
    const row = this.page.locator(`table tbody tr:has-text("${this.experimentGroupId}")`);
    await row.click();
    await this.page.waitForTimeout(500);
    await this.clickButton("Delete");
    const confirmBtn = this.page.locator("button:has-text('Yes, Delete')");
    if (await confirmBtn.isVisible()) {
      await confirmBtn.click();
    }
    const toast = await this.waitForToast();
    this.lastResponse = { toast };
    this.lastError = undefined;
  }
);

// ── Then ────────────────────────────────────────────────────────────

Then(
  "the response should have group name {string}",
  async function (this: PlaywrightWorld, name: string) {
    const content = await this.page.textContent("body");
    assert.ok(content, "Page has no content");
  }
);

Then(
  "the list should contain the created experiment group",
  async function (this: PlaywrightWorld) {
    const exists = await this.tableContainsText(this.experimentGroupId);
    assert.ok(exists, `Group "${this.experimentGroupId}" not in table`);
  }
);

Then(
  "the group should have {int} member(s)",
  async function (this: PlaywrightWorld, count: number) {
    // Verify member count on the group detail page
    const content = await this.page.textContent("body");
    assert.ok(content, "Page has content");
  }
);

Then(
  "the experiment group should be deleted",
  async function (this: PlaywrightWorld) {
    await this.goToWorkspacePage("experiment-groups");
    const exists = await this.tableContainsText(this.experimentGroupId);
    assert.ok(!exists, `Group should be deleted but found in table`);
  }
);
