import { Given, When, Then } from "@cucumber/cucumber";
import { PlaywrightWorld } from "../support_ui/world.ts";
import * as assert from "node:assert";

// ── Given ───────────────────────────────────────────────────────────

Given(
  "a dimension {string} exists with schema type {string}",
  async function (this: PlaywrightWorld, name: string, schemaType: string) {
    const uniqueName = this.uniqueName(name);
    this.dimensionName = uniqueName;
    await this.goToWorkspacePage("dimensions");
    const exists = await this.tableContainsText(uniqueName);
    if (!exists) {
      await this.clickButton("Create Dimension");
      await this.page.waitForTimeout(300);
      await this.page.getByPlaceholder("Dimension name").fill(uniqueName);
      // Select schema type from dropdown
      await this.selectDropdownOption("Set Schema", schemaType);
      await this.fillByPlaceholder("Enter a description", `Test dimension ${uniqueName}`);
      await this.fillByPlaceholder("Enter a reason for this change", "Cucumber setup");
      await this.clickButton("Submit");
      await this.expectSuccessToast();
    }
  }
);

// ── When ────────────────────────────────────────────────────────────

When(
  "I create a dimension with name {string} and schema type {string}",
  async function (this: PlaywrightWorld, name: string, schemaType: string) {
    const uniqueName = this.uniqueName(name);
    this.dimensionName = uniqueName;
    await this.goToWorkspacePage("dimensions");
    await this.clickButton("Create Dimension");
    await this.page.waitForTimeout(300);
    await this.page.getByPlaceholder("Dimension name").fill(uniqueName);
    await this.selectDropdownOption("Set Schema", schemaType);
    await this.fillByPlaceholder("Enter a description", `Dimension ${uniqueName}`);
    await this.fillByPlaceholder("Enter a reason for this change", "Cucumber test");
    await this.clickButton("Submit");

    try {
      const toast = await this.waitForToast();
      if (toast.toLowerCase().includes("error")) {
        this.lastError = { message: toast };
        this.lastResponse = undefined;
      } else {
        this.lastResponse = { dimension: uniqueName };
        this.lastError = undefined;
      }
    } catch {
      this.lastError = { message: "No feedback from UI" };
      this.lastResponse = undefined;
    }
  }
);

When(
  "I create a dimension with name {string} and enum values {string}",
  async function (this: PlaywrightWorld, name: string, values: string) {
    const uniqueName = this.uniqueName(name);
    this.dimensionName = uniqueName;
    await this.goToWorkspacePage("dimensions");
    await this.clickButton("Create Dimension");
    await this.page.waitForTimeout(300);
    await this.page.getByPlaceholder("Dimension name").fill(uniqueName);
    // Select enum type and enter values in the schema editor
    await this.selectDropdownOption("Set Schema", "Enum");
    const enumValues = values.split(",").map((v) => v.trim());
    for (const val of enumValues) {
      const addBtn = this.page.locator("button:has-text('Add')").first();
      if (await addBtn.isVisible()) {
        await addBtn.click();
      }
      const inputs = this.page.locator("input[type='text']");
      const lastInput = inputs.last();
      await lastInput.fill(val);
    }
    await this.fillByPlaceholder("Enter a description", `Enum dimension`);
    await this.fillByPlaceholder("Enter a reason for this change", "Cucumber test");
    await this.clickButton("Submit");

    try {
      const toast = await this.waitForToast();
      if (toast.toLowerCase().includes("error")) {
        this.lastError = { message: toast };
        this.lastResponse = undefined;
      } else {
        this.lastResponse = { dimension: uniqueName };
        this.lastError = undefined;
      }
    } catch {
      this.lastError = { message: "No feedback from UI" };
      this.lastResponse = undefined;
    }
  }
);

When(
  "I get dimension {string}",
  async function (this: PlaywrightWorld, name: string) {
    await this.goToWorkspacePage("dimensions");
    const row = this.page.locator(`table tbody tr:has-text("${this.dimensionName}")`);
    const visible = await row.isVisible().catch(() => false);
    if (visible) {
      await row.click();
      await this.page.waitForTimeout(500);
      this.lastResponse = { dimension: this.dimensionName };
      this.lastError = undefined;
    } else {
      this.lastError = { message: "No records found" };
      this.lastResponse = undefined;
    }
  }
);

When("I list all dimensions", async function (this: PlaywrightWorld) {
  await this.goToWorkspacePage("dimensions");
  const rows = await this.tableRowCount();
  this.lastResponse = { data: new Array(rows).fill({}) };
  this.lastError = undefined;
});

When(
  "I update dimension {string} description to {string}",
  async function (this: PlaywrightWorld, name: string, description: string) {
    await this.goToWorkspacePage("dimensions");
    const row = this.page.locator(`table tbody tr:has-text("${this.dimensionName}")`);
    await row.click();
    await this.page.waitForTimeout(500);
    const descInput = this.page.getByPlaceholder("Enter a description");
    await descInput.clear();
    await descInput.fill(description);
    await this.clickButton("Submit");
    const toast = await this.waitForToast();
    this.lastResponse = { description, toast };
    this.lastError = undefined;
  }
);

When(
  "I delete dimension {string}",
  async function (this: PlaywrightWorld, name: string) {
    await this.goToWorkspacePage("dimensions");
    const row = this.page.locator(`table tbody tr:has-text("${this.dimensionName}")`);
    const visible = await row.isVisible().catch(() => false);
    if (!visible) {
      this.lastError = { message: "No records found" };
      this.lastResponse = undefined;
      return;
    }
    await row.click();
    await this.page.waitForTimeout(500);
    await this.clickButton("Delete");
    const confirmBtn = this.page.locator("button:has-text('Yes, Delete')");
    if (await confirmBtn.isVisible()) {
      await confirmBtn.click();
    }
    const toast = await this.waitForToast();
    this.lastResponse = { deleted: true, toast };
    this.lastError = undefined;
  }
);

// ── Then ────────────────────────────────────────────────────────────

Then(
  "the response should have dimension name {string}",
  async function (this: PlaywrightWorld, name: string) {
    const content = await this.page.textContent("body");
    assert.ok(
      content?.includes(this.dimensionName),
      `Dimension name not found on page`
    );
  }
);

Then(
  "the list should contain dimension {string}",
  async function (this: PlaywrightWorld, name: string) {
    const exists = await this.tableContainsText(this.dimensionName);
    assert.ok(exists, `Dimension "${this.dimensionName}" not in table`);
  }
);
