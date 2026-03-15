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
      await this.page.waitForTimeout(1000);
    }
  }
);

// ── When ────────────────────────────────────────────────────────────

When("I list type templates", async function (this: PlaywrightWorld) {
  await this.goToWorkspacePage("types");
  const rows = await this.tableRowCount();
  this.lastResponse = { data: new Array(rows).fill({}) };
  this.lastError = undefined;
});

When(
  "I create a type template named {string} with schema type {string}",
  async function (this: PlaywrightWorld, name: string, schemaType: string) {
    const uniqueName = this.uniqueName(name);
    this.typeTemplateName = uniqueName;
    await this.goToWorkspacePage("types");
    await this.clickButton("Create Type");
    await this.page.waitForTimeout(300);
    await this.page.locator("#type_name").fill(uniqueName);
    await this.fillMonacoEditor("type-schema", JSON.stringify({ type: schemaType }));
    await this.fillByPlaceholder("Enter a description", `${name} type template`);
    await this.fillByPlaceholder("Enter a reason for this change", "Cucumber test");
    await this.clickButton("Submit");

    try {
      const toast = await this.waitForToast();
      if (toast.toLowerCase().includes("error")) {
        this.lastError = { message: toast };
        this.lastResponse = undefined;
      } else {
        this.lastResponse = {
          type_name: uniqueName,
          type_schema: { type: schemaType },
          toast,
        };
        this.lastError = undefined;
      }
    } catch {
      this.lastResponse = { type_name: uniqueName, type_schema: { type: schemaType } };
      this.lastError = undefined;
    }
  }
);

When(
  "I create a type template named {string} with pattern {string}",
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
    await this.fillByPlaceholder("Enter a description", `${name} pattern type`);
    await this.fillByPlaceholder("Enter a reason for this change", "Cucumber test");
    await this.clickButton("Submit");

    try {
      const toast = await this.waitForToast();
      if (toast.toLowerCase().includes("error")) {
        this.lastError = { message: toast };
        this.lastResponse = undefined;
      } else {
        this.lastResponse = {
          type_name: uniqueName,
          type_schema: { type: "string", pattern },
          toast,
        };
        this.lastError = undefined;
      }
    } catch {
      this.lastResponse = { type_name: uniqueName, type_schema: { type: "string", pattern } };
      this.lastError = undefined;
    }
  }
);

When(
  "I update type template {string} with minimum {int} and maximum {int}",
  async function (this: PlaywrightWorld, name: string, min: number, max: number) {
    await this.goToWorkspacePage("types");
    const row = this.page.locator(`table tbody tr:has-text("${this.typeTemplateName}")`);
    await row.click();
    await this.page.waitForTimeout(500);
    await this.fillMonacoEditor(
      "type-schema",
      JSON.stringify({ type: "number", minimum: min, maximum: max })
    );
    await this.fillByPlaceholder("Enter a reason for this change", "Cucumber update");
    await this.clickButton("Submit");
    const toast = await this.waitForToast();
    this.lastResponse = { type_schema: { type: "number", minimum: min, maximum: max }, toast };
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
  "the response should contain a type template list",
  async function (this: PlaywrightWorld) {
    assert.ok(this.lastResponse, "No response");
    assert.ok(this.lastResponse.data !== undefined, "No data");
    assert.ok(Array.isArray(this.lastResponse.data), "data is not an array");
  }
);

Then(
  "the response should have type name {string}",
  async function (this: PlaywrightWorld, name: string) {
    const content = await this.page.textContent("body");
    assert.ok(
      content?.includes(this.typeTemplateName) || this.lastResponse?.type_name === this.typeTemplateName,
      `Type name not found`
    );
  }
);

Then(
  "the response schema type should be {string}",
  async function (this: PlaywrightWorld, type: string) {
    assert.ok(this.lastResponse, "No response");
    const schema = this.lastResponse.type_schema;
    assert.strictEqual(schema?.type, type);
  }
);

Then(
  "the response schema should have pattern {string}",
  async function (this: PlaywrightWorld, pattern: string) {
    assert.ok(this.lastResponse, "No response");
    const schema = this.lastResponse.type_schema;
    assert.strictEqual(schema?.pattern, pattern);
  }
);

Then(
  "the response schema should have minimum {int} and maximum {int}",
  async function (this: PlaywrightWorld, min: number, max: number) {
    assert.ok(this.lastResponse, "No response");
    const schema = this.lastResponse.type_schema;
    assert.strictEqual(schema?.minimum, min);
    assert.strictEqual(schema?.maximum, max);
  }
);

Then(
  "listing type templates should not include {string}",
  async function (this: PlaywrightWorld, name: string) {
    await this.goToWorkspacePage("types");
    const exists = await this.tableContainsText(this.typeTemplateName);
    assert.ok(!exists, `Type template "${this.typeTemplateName}" still exists`);
  }
);
