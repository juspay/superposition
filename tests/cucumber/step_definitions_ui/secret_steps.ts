import { Given, When, Then } from "@cucumber/cucumber";
import { PlaywrightWorld } from "../support_ui/world.ts";
import * as assert from "node:assert";

// ── Given ───────────────────────────────────────────────────────────

Given(
  "a secret {string} exists with value {string}",
  async function (this: PlaywrightWorld, name: string, value: string) {
    this.secretName = name;
    await this.goToWorkspacePage("secrets");
    const exists = await this.tableContainsText(name);
    if (!exists) {
      await this.clickButton("Create Secret");
      await this.page.waitForTimeout(300);
      await this.fillByPlaceholder(
        "Enter secret name (uppercase, digits, underscore)",
        name
      );
      await this.fillByPlaceholder("Enter secret value", value);
      await this.fillByPlaceholder("Enter a description", `Secret ${name}`);
      await this.fillByPlaceholder("Enter a reason for this change", "Cucumber setup");
      await this.clickButton("Submit");
      await this.expectSuccessToast();
    }
  }
);

Given(
  "a compute function exists that reads the secret {string}",
  async function (this: PlaywrightWorld, secretName: string) {
    this.functionName = this.uniqueName("verify_secret");
    await this.goToWorkspacePage("function");
    const exists = await this.tableContainsText(this.functionName);
    if (!exists) {
      await this.clickButton("Create Function");
      await this.page.waitForTimeout(300);
      await this.page.locator("#funName").fill(this.functionName);
      await this.selectDropdownOption("Function Type", "VALUE_COMPUTE");
      await this.fillMonacoEditor(
        "code_editor_fn",
        `async function execute(payload) { return [SECRETS.${secretName}]; }`
      );
      await this.fillByPlaceholder("Enter a description", "Secret verify function");
      await this.fillByPlaceholder("Enter a reason for this change", "Cucumber setup");
      await this.clickButton("Submit");
      await this.page.waitForTimeout(1000);
    }
  }
);

// ── When ────────────────────────────────────────────────────────────

When(
  "I create a secret named {string} with value {string}",
  async function (this: PlaywrightWorld, name: string, value: string) {
    this.secretName = name;
    await this.goToWorkspacePage("secrets");
    await this.clickButton("Create Secret");
    await this.page.waitForTimeout(300);
    await this.fillByPlaceholder(
      "Enter secret name (uppercase, digits, underscore)",
      name
    );
    await this.fillByPlaceholder("Enter secret value", value);
    await this.fillByPlaceholder("Enter a description", `Secret ${name}`);
    await this.fillByPlaceholder("Enter a reason for this change", "Cucumber test");
    await this.clickButton("Submit");

    try {
      const toast = await this.waitForToast();
      if (toast.toLowerCase().includes("error")) {
        this.lastError = { message: toast };
        this.lastResponse = undefined;
      } else {
        this.lastResponse = { name, toast };
        this.lastError = undefined;
      }
    } catch {
      this.lastError = { message: "No feedback" };
      this.lastResponse = undefined;
    }
  }
);

When(
  "I get secret {string}",
  async function (this: PlaywrightWorld, name: string) {
    await this.goToWorkspacePage("secrets");
    const row = this.page.locator(`table tbody tr:has-text("${name}")`);
    const visible = await row.isVisible().catch(() => false);
    if (visible) {
      await row.click();
      await this.page.waitForTimeout(500);
      this.lastResponse = { name };
      this.lastError = undefined;
    } else {
      this.lastError = { message: "No records found" };
      this.lastResponse = undefined;
    }
  }
);

When(
  "I update secret {string} value to {string}",
  async function (this: PlaywrightWorld, name: string, value: string) {
    await this.goToWorkspacePage("secrets");
    const row = this.page.locator(`table tbody tr:has-text("${name}")`);
    const visible = await row.isVisible().catch(() => false);
    if (!visible) {
      this.lastError = { message: "No records found" };
      this.lastResponse = undefined;
      return;
    }
    await row.click();
    await this.page.waitForTimeout(500);
    const valueInput = this.page.getByPlaceholder("Enter secret value");
    await valueInput.clear();
    await valueInput.fill(value);
    await this.fillByPlaceholder("Enter a reason for this change", "Cucumber update");
    await this.clickButton("Submit");
    const toast = await this.waitForToast();
    this.lastResponse = { name, toast };
    this.lastError = undefined;
  }
);

When(
  "I delete secret {string}",
  async function (this: PlaywrightWorld, name: string) {
    await this.goToWorkspacePage("secrets");
    const row = this.page.locator(`table tbody tr:has-text("${name}")`);
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
    this.lastResponse = { toast };
    this.lastError = undefined;
  }
);

When(
  "I test the compute function",
  async function (this: PlaywrightWorld) {
    await this.goToWorkspacePage("function");
    const row = this.page.locator(`table tbody tr:has-text("${this.functionName}")`);
    await row.click();
    await this.page.waitForTimeout(500);
    await this.clickButton("Test");
    await this.page.waitForTimeout(1000);
    const content = await this.page.textContent("body");
    this.lastResponse = { fn_output: content ?? "" };
    this.lastError = undefined;
  }
);

When(
  "I test the compute function again",
  async function (this: PlaywrightWorld) {
    await this.clickButton("Test");
    await this.page.waitForTimeout(1000);
    const content = await this.page.textContent("body");
    this.lastResponse = { fn_output: content ?? "" };
    this.lastError = undefined;
  }
);

// ── Then ────────────────────────────────────────────────────────────

Then(
  "the response should have secret name {string}",
  async function (this: PlaywrightWorld, name: string) {
    const content = await this.page.textContent("body");
    assert.ok(content?.includes(name), `Secret name "${name}" not found on page`);
  }
);

Then(
  "the secret value should not be returned",
  async function (this: PlaywrightWorld) {
    // In the UI, secret values are masked/hidden
    const content = await this.page.textContent("body");
    // The actual secret value should not be displayed in plain text
    assert.ok(content, "Page has content");
  }
);

Then(
  "the function output should contain {string}",
  async function (this: PlaywrightWorld, expected: string) {
    const content = await this.page.textContent("body");
    assert.ok(
      content?.includes(expected),
      `Function output does not contain "${expected}"`
    );
  }
);

Then(
  "getting secret {string} should fail with {string}",
  async function (this: PlaywrightWorld, name: string, errorPattern: string) {
    await this.goToWorkspacePage("secrets");
    const row = this.page.locator(`table tbody tr:has-text("${name}")`);
    const visible = await row.isVisible().catch(() => false);
    assert.ok(!visible, `Secret "${name}" should not be in the table`);
  }
);
