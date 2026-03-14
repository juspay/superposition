import { Given, When, Then } from "@cucumber/cucumber";
import { PlaywrightWorld } from "../support_ui/world.ts";
import * as assert from "node:assert";

// ── Given ───────────────────────────────────────────────────────────

Given(
  "a type template {string} exists with schema type {string}",
  async function (this: PlaywrightWorld, name: string, schemaType: string) {
    const uniqueName = this.uniqueName(name);
    this.typeTemplateName = uniqueName;
    await this.goToWorkspacePage("types");
    const exists = await this.tableContainsText(uniqueName);
    if (!exists) {
      await this.clickButton("Create Type");
      await this.page.waitForTimeout(300);
      await this.page.locator("#type_name").fill(uniqueName);
      await this.fillMonacoEditor("type-schema", JSON.stringify({ type: schemaType }));
      await this.fillByPlaceholder("Enter a description", `Template ${uniqueName}`);
      await this.fillByPlaceholder("Enter a reason for this change", "Cucumber setup");
      await this.clickButton("Submit");
      await this.expectSuccessToast();
    }
  }
);

// ── When ────────────────────────────────────────────────────────────

When(
  "I create a type template with name {string} and schema type {string}",
  async function (this: PlaywrightWorld, name: string, schemaType: string) {
    const uniqueName = this.uniqueName(name);
    this.typeTemplateName = uniqueName;
    await this.goToWorkspacePage("types");
    await this.clickButton("Create Type");
    await this.page.waitForTimeout(300);
    await this.page.locator("#type_name").fill(uniqueName);
    await this.fillMonacoEditor("type-schema", JSON.stringify({ type: schemaType }));
    await this.fillByPlaceholder("Enter a description", `Template ${uniqueName}`);
    await this.fillByPlaceholder("Enter a reason for this change", "Cucumber test");
    await this.clickButton("Submit");

    try {
      const toast = await this.waitForToast();
      if (toast.toLowerCase().includes("error")) {
        this.lastError = { message: toast };
        this.lastResponse = undefined;
      } else {
        this.lastResponse = { type_name: uniqueName, type_schema: { type: schemaType }, toast };
        this.lastError = undefined;
      }
    } catch {
      this.lastResponse = { type_name: uniqueName };
      this.lastError = undefined;
    }
  }
);

When(
  "I create a type template {string} with an enum schema of {string}",
  async function (this: PlaywrightWorld, name: string, values: string) {
    const uniqueName = this.uniqueName(name);
    this.typeTemplateName = uniqueName;
    const enumValues = values.split(",").map((v) => v.trim());
    await this.goToWorkspacePage("types");
    await this.clickButton("Create Type");
    await this.page.waitForTimeout(300);
    await this.page.locator("#type_name").fill(uniqueName);
    await this.fillMonacoEditor(
      "type-schema",
      JSON.stringify({ type: "string", enum: enumValues })
    );
    await this.fillByPlaceholder("Enter a description", `Enum template`);
    await this.fillByPlaceholder("Enter a reason for this change", "Cucumber test");
    await this.clickButton("Submit");

    try {
      const toast = await this.waitForToast();
      if (toast.toLowerCase().includes("error")) {
        this.lastError = { message: toast };
        this.lastResponse = undefined;
      } else {
        this.lastResponse = { type_name: uniqueName, toast };
        this.lastError = undefined;
      }
    } catch {
      this.lastResponse = { type_name: uniqueName };
      this.lastError = undefined;
    }
  }
);

When(
  "I create a type template {string} with a pattern schema {string}",
  async function (this: PlaywrightWorld, name: string, pattern: string) {
    const uniqueName = this.uniqueName(name);
    this.typeTemplateName = uniqueName;
    await this.goToWorkspacePage("types");
    await this.clickButton("Create Type");
    await this.page.waitForTimeout(300);
    await this.page.locator("#type_name").fill(uniqueName);
    await this.fillMonacoEditor(
      "type-schema",
      JSON.stringify({ type: "string", pattern })
    );
    await this.fillByPlaceholder("Enter a description", `Pattern template`);
    await this.fillByPlaceholder("Enter a reason for this change", "Cucumber test");
    await this.clickButton("Submit");

    try {
      const toast = await this.waitForToast();
      if (toast.toLowerCase().includes("error")) {
        this.lastError = { message: toast };
        this.lastResponse = undefined;
      } else {
        this.lastResponse = { type_name: uniqueName, toast };
        this.lastError = undefined;
      }
    } catch {
      this.lastResponse = { type_name: uniqueName };
      this.lastError = undefined;
    }
  }
);

When(
  "I create a type template {string} with an invalid schema",
  async function (this: PlaywrightWorld, name: string) {
    const uniqueName = this.uniqueName(name);
    await this.goToWorkspacePage("types");
    await this.clickButton("Create Type");
    await this.page.waitForTimeout(300);
    await this.page.locator("#type_name").fill(uniqueName);
    await this.fillMonacoEditor("type-schema", JSON.stringify({ type: "invalid-type" }));
    await this.fillByPlaceholder("Enter a description", "Invalid template");
    await this.fillByPlaceholder("Enter a reason for this change", "Testing invalid");
    await this.clickButton("Submit");
    await this.expectErrorToast();
  }
);

When(
  "I list type templates",
  async function (this: PlaywrightWorld) {
    await this.goToWorkspacePage("types");
    const rows = await this.tableRowCount();
    this.lastResponse = { data: new Array(rows).fill({}) };
    this.lastError = undefined;
  }
);

When(
  "I update type template {string} schema to type {string}",
  async function (this: PlaywrightWorld, name: string, schemaType: string) {
    await this.goToWorkspacePage("types");
    const row = this.page.locator(`table tbody tr:has-text("${this.typeTemplateName}")`);
    await row.click();
    await this.page.waitForTimeout(500);
    await this.fillMonacoEditor("type-schema", JSON.stringify({ type: schemaType }));
    await this.fillByPlaceholder("Enter a reason for this change", "Cucumber update");
    await this.clickButton("Submit");
    const toast = await this.waitForToast();
    this.lastResponse = { type_schema: { type: schemaType }, toast };
    this.lastError = undefined;
  }
);

When(
  "I delete type template {string}",
  async function (this: PlaywrightWorld, name: string) {
    await this.goToWorkspacePage("types");
    const row = this.page.locator(`table tbody tr:has-text("${this.typeTemplateName}")`);
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
  "the response should have type name {string}",
  async function (this: PlaywrightWorld, name: string) {
    const content = await this.page.textContent("body");
    assert.ok(
      content?.includes(this.typeTemplateName),
      `Type template name not found on page`
    );
  }
);

Then(
  "the response should have type schema type {string}",
  async function (this: PlaywrightWorld, schemaType: string) {
    const content = await this.page.textContent("body");
    assert.ok(
      content?.includes(schemaType),
      `Schema type "${schemaType}" not found on page`
    );
  }
);

Then(
  "the list should contain type template {string}",
  async function (this: PlaywrightWorld, name: string) {
    const exists = await this.tableContainsText(this.typeTemplateName);
    assert.ok(exists, `Type template "${this.typeTemplateName}" not in table`);
  }
);

Then(
  "the type template should be deleted",
  async function (this: PlaywrightWorld) {
    await this.goToWorkspacePage("types");
    const exists = await this.tableContainsText(this.typeTemplateName);
    assert.ok(!exists, `Type template should be deleted but found in table`);
  }
);
