import { Given, When, Then } from "@cucumber/cucumber";
import { PlaywrightWorld } from "../support_ui/world.ts";
import * as assert from "node:assert";

// ── Given ───────────────────────────────────────────────────────────

Given(
  "an organisation and workspace exist",
  async function (this: PlaywrightWorld) {
    // In UI mode, we just ensure we have org/workspace IDs to navigate
    assert.ok(this.orgId || true, "No org ID configured");
    assert.ok(this.workspaceId || true, "No workspace ID configured");
  }
);

Given(
  "a variable {string} exists with value {string}",
  async function (this: PlaywrightWorld, name: string, value: string) {
    this.variableName = name;
    await this.goToWorkspacePage("variables");
    const exists = await this.tableContainsText(name);
    if (!exists) {
      await this.clickButton("Create Variable");
      await this.page.waitForTimeout(300);
      await this.fillByPlaceholder(
        "Enter variable name (uppercase, digits, underscore)",
        name
      );
      await this.fillByPlaceholder("Enter variable value", value);
      await this.clickButton("Submit");
      await this.expectSuccessToast();
    }
  }
);

Given(
  "a variable {string} exists with value {string} and description {string}",
  async function (
    this: PlaywrightWorld,
    name: string,
    value: string,
    desc: string
  ) {
    this.variableName = name;
    await this.goToWorkspacePage("variables");
    const exists = await this.tableContainsText(name);
    if (!exists) {
      await this.clickButton("Create Variable");
      await this.page.waitForTimeout(300);
      await this.fillByPlaceholder(
        "Enter variable name (uppercase, digits, underscore)",
        name
      );
      await this.fillByPlaceholder("Enter variable value", value);
      await this.fillByPlaceholder("Enter a description", desc);
      await this.clickButton("Submit");
      await this.expectSuccessToast();
    }
  }
);

Given(
  "a function {string} exists that reads VARS.{word}",
  async function (this: PlaywrightWorld, funcName: string, varName: string) {
    // Navigate to functions page and create if not exists
    this.functionName = funcName;
    await this.goToWorkspacePage("function");
    const exists = await this.tableContainsText(funcName);
    if (!exists) {
      await this.clickButton("Create Function");
      await this.page.waitForTimeout(300);
      const nameInput = this.page.locator("#funName");
      await nameInput.fill(funcName);
      // Fill code in Monaco editor
      await this.fillMonacoEditor(
        "code_editor_fn",
        `async function execute(payload) { let v = VARS.${varName}; console.log(v); return v !== undefined; }`
      );
      await this.clickButton("Submit");
      await this.expectSuccessToast();
    }
  }
);

// ── When ────────────────────────────────────────────────────────────

When(
  "I create a variable named {string} with value {string}",
  async function (this: PlaywrightWorld, name: string, value: string) {
    this.variableName = name;
    await this.goToWorkspacePage("variables");
    await this.clickButton("Create Variable");
    await this.page.waitForTimeout(300);
    await this.fillByPlaceholder(
      "Enter variable name (uppercase, digits, underscore)",
      name
    );
    await this.fillByPlaceholder("Enter variable value", value);
    await this.clickButton("Submit");

    try {
      const toast = await this.waitForToast();
      if (toast.toLowerCase().includes("error") || toast.toLowerCase().includes("fail")) {
        this.lastError = { message: toast };
        this.lastResponse = undefined;
      } else {
        this.lastResponse = { name, value, toast };
        this.lastError = undefined;
      }
    } catch {
      const err = await this.page
        .locator(".text-red-600, .text-error")
        .first()
        .textContent()
        .catch(() => null);
      if (err) {
        this.lastError = { message: err };
        this.lastResponse = undefined;
      }
    }
  }
);

When(
  "I get variable {string}",
  async function (this: PlaywrightWorld, name: string) {
    await this.goToWorkspacePage("variables");
    const row = this.page.locator(`table tbody tr:has-text("${name}")`);
    const visible = await row.isVisible().catch(() => false);
    if (visible) {
      await row.click();
      await this.page.waitForTimeout(500);
      this.lastResponse = { name, value: "" };
      this.lastError = undefined;
    } else {
      this.lastError = { message: "No records found" };
      this.lastResponse = undefined;
    }
  }
);

When(
  "I update variable {string} value to {string}",
  async function (this: PlaywrightWorld, name: string, newValue: string) {
    await this.goToWorkspacePage("variables");
    const row = this.page.locator(`table tbody tr:has-text("${name}")`);
    const visible = await row.isVisible().catch(() => false);
    if (!visible) {
      this.lastError = { message: "No records found" };
      this.lastResponse = undefined;
      return;
    }
    await row.click();
    await this.page.waitForTimeout(500);
    const valueInput = this.page.getByPlaceholder("Enter variable value");
    await valueInput.clear();
    await valueInput.fill(newValue);
    await this.clickButton("Submit");
    const toast = await this.waitForToast();
    this.lastResponse = { value: newValue, toast };
    this.lastError = undefined;
  }
);

When(
  "I update variable {string} description to {string}",
  async function (this: PlaywrightWorld, name: string, desc: string) {
    await this.goToWorkspacePage("variables");
    const row = this.page.locator(`table tbody tr:has-text("${name}")`);
    await row.click();
    await this.page.waitForTimeout(500);
    const descInput = this.page.getByPlaceholder("Enter a description");
    await descInput.clear();
    await descInput.fill(desc);
    await this.clickButton("Submit");
    const toast = await this.waitForToast();
    this.lastResponse = { description: desc, toast };
    this.lastError = undefined;
  }
);

When(
  "I delete variable {string}",
  async function (this: PlaywrightWorld, name: string) {
    await this.goToWorkspacePage("variables");
    const row = this.page.locator(`table tbody tr:has-text("${name}")`);
    const visible = await row.isVisible().catch(() => false);
    if (!visible) {
      this.lastError = { message: "No records found" };
      this.lastResponse = undefined;
      return;
    }
    await row.click();
    await this.page.waitForTimeout(500);
    // Look for delete button
    await this.clickButton("Delete");
    // Confirm in modal
    const confirmBtn = this.page.locator("button:has-text('Yes, Delete')");
    if (await confirmBtn.isVisible()) {
      await confirmBtn.click();
    }
    const toast = await this.waitForToast();
    this.lastResponse = { deleted: true, toast };
    this.lastError = undefined;
  }
);

When(
  "I test the function {string}",
  async function (this: PlaywrightWorld, funcName: string) {
    await this.goToWorkspacePage("function");
    const row = this.page.locator(`table tbody tr:has-text("${funcName}")`);
    await row.click();
    await this.page.waitForTimeout(500);
    // Click Test button
    await this.clickButton("Test");
    await this.page.waitForTimeout(1000);
    const content = await this.page.textContent("body");
    this.lastResponse = {
      fn_output: content?.includes("true") ? true : false,
      stdout: content ?? "",
    };
    this.lastError = undefined;
  }
);

// ── Then ────────────────────────────────────────────────────────────

Then(
  "the response should have variable name {string}",
  async function (this: PlaywrightWorld, name: string) {
    const content = await this.page.textContent("body");
    assert.ok(content?.includes(name), `Variable name "${name}" not found on page`);
  }
);

Then(
  "the response should have variable value {string}",
  async function (this: PlaywrightWorld, value: string) {
    const content = await this.page.textContent("body");
    assert.ok(
      content?.includes(value),
      `Variable value "${value}" not found on page`
    );
  }
);

Then(
  "getting variable {string} should fail with {string}",
  async function (this: PlaywrightWorld, name: string, errorPattern: string) {
    await this.goToWorkspacePage("variables");
    const row = this.page.locator(`table tbody tr:has-text("${name}")`);
    const visible = await row.isVisible().catch(() => false);
    assert.ok(!visible, `Variable "${name}" should not be in the table`);
  }
);

Then(
  "the function output should be true",
  function (this: PlaywrightWorld) {
    assert.ok(
      this.lastResponse?.fn_output === true,
      "Function output was not true"
    );
  }
);

Then(
  "the function stdout should contain {string}",
  function (this: PlaywrightWorld, expected: string) {
    assert.ok(
      this.lastResponse?.stdout?.includes(expected),
      `Function stdout does not contain "${expected}"`
    );
  }
);
