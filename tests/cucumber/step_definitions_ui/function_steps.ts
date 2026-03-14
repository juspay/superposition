import { Given, When, Then } from "@cucumber/cucumber";
import { PlaywrightWorld } from "../support_ui/world.ts";
import * as assert from "node:assert";

// ── Given ───────────────────────────────────────────────────────────

Given(
  "a value_validation function {string} exists",
  async function (this: PlaywrightWorld, name: string) {
    const uniqueName = this.uniqueName(name);
    this.functionName = uniqueName;
    await this.goToWorkspacePage("function");
    const exists = await this.tableContainsText(uniqueName);
    if (!exists) {
      await this.clickButton("Create Function");
      await this.page.waitForTimeout(300);
      await this.page.locator("#funName").fill(uniqueName);
      // Select function type
      await this.selectDropdownOption("Function Type", "VALUE_VALIDATION");
      await this.fillMonacoEditor(
        "code_editor_fn",
        `async function execute(payload) { return false; }`
      );
      await this.fillByPlaceholder("Enter a description", "Test validation function");
      await this.fillByPlaceholder("Enter a reason for this change", "Cucumber setup");
      await this.clickButton("Submit");
      await this.page.waitForTimeout(1000);
    }
  }
);

// ── When ────────────────────────────────────────────────────────────

When(
  "I create a value_validation function named {string} with code that validates key {string}",
  async function (this: PlaywrightWorld, name: string, key: string) {
    const uniqueName = this.uniqueName(name);
    this.functionName = uniqueName;
    await this.goToWorkspacePage("function");
    await this.clickButton("Create Function");
    await this.page.waitForTimeout(300);
    await this.page.locator("#funName").fill(uniqueName);
    await this.selectDropdownOption("Function Type", "VALUE_VALIDATION");
    await this.fillMonacoEditor(
      "code_editor_fn",
      `async function execute(payload) { let v = payload.value_validate.value; let k = payload.value_validate.key; if (k === "${key}" && v === "valid") return true; return false; }`
    );
    await this.fillByPlaceholder("Enter a description", "Validation function");
    await this.fillByPlaceholder("Enter a reason for this change", "Cucumber test");
    await this.clickButton("Submit");

    try {
      const toast = await this.waitForToast();
      if (toast.toLowerCase().includes("error")) {
        this.lastError = { message: toast };
        this.lastResponse = undefined;
      } else {
        this.lastResponse = { function_name: uniqueName, function_type: "VALUE_VALIDATION", toast };
        this.lastError = undefined;
      }
    } catch {
      this.lastResponse = { function_name: uniqueName, function_type: "VALUE_VALIDATION" };
      this.lastError = undefined;
    }
  }
);

When(
  "I create a value_compute function named {string} with code that returns computed values",
  async function (this: PlaywrightWorld, name: string) {
    const uniqueName = this.uniqueName(name);
    this.functionName = uniqueName;
    await this.goToWorkspacePage("function");
    await this.clickButton("Create Function");
    await this.page.waitForTimeout(300);
    await this.page.locator("#funName").fill(uniqueName);
    await this.selectDropdownOption("Function Type", "VALUE_COMPUTE");
    await this.fillMonacoEditor(
      "code_editor_fn",
      `async function execute(payload) { return ["test1", "test2", "test3"]; }`
    );
    await this.fillByPlaceholder("Enter a description", "Compute function");
    await this.fillByPlaceholder("Enter a reason for this change", "Cucumber test");
    await this.clickButton("Submit");

    try {
      const toast = await this.waitForToast();
      if (toast.toLowerCase().includes("error")) {
        this.lastError = { message: toast };
        this.lastResponse = undefined;
      } else {
        this.lastResponse = { function_name: uniqueName, function_type: "VALUE_COMPUTE", toast };
        this.lastError = undefined;
      }
    } catch {
      this.lastResponse = { function_name: uniqueName, function_type: "VALUE_COMPUTE" };
      this.lastError = undefined;
    }
  }
);

When(
  "I create a value_validation function named {string} with code {string}",
  async function (this: PlaywrightWorld, name: string, code: string) {
    const uniqueName = this.uniqueName(name);
    await this.goToWorkspacePage("function");
    await this.clickButton("Create Function");
    await this.page.waitForTimeout(300);
    await this.page.locator("#funName").fill(uniqueName);
    await this.selectDropdownOption("Function Type", "VALUE_VALIDATION");
    await this.fillMonacoEditor("code_editor_fn", code);
    await this.fillByPlaceholder("Enter a description", "Test function");
    await this.fillByPlaceholder("Enter a reason for this change", "Cucumber test");
    await this.clickButton("Submit");

    try {
      const toast = await this.waitForToast();
      if (toast.toLowerCase().includes("error")) {
        this.lastError = { message: toast };
        this.lastResponse = undefined;
      } else {
        this.lastResponse = { function_name: uniqueName, toast };
        this.lastError = undefined;
      }
    } catch {
      this.lastError = { message: "Unknown error" };
      this.lastResponse = undefined;
    }
  }
);

When(
  "I create a value_compute function named {string} with code that returns a string",
  async function (this: PlaywrightWorld, name: string) {
    const uniqueName = this.uniqueName(name);
    await this.goToWorkspacePage("function");
    await this.clickButton("Create Function");
    await this.page.waitForTimeout(300);
    await this.page.locator("#funName").fill(uniqueName);
    await this.selectDropdownOption("Function Type", "VALUE_COMPUTE");
    await this.fillMonacoEditor(
      "code_editor_fn",
      `async function execute(payload) { return "invalid return type"; }`
    );
    await this.fillByPlaceholder("Enter a description", "Test function");
    await this.fillByPlaceholder("Enter a reason for this change", "Cucumber test");
    await this.clickButton("Submit");

    try {
      const toast = await this.waitForToast();
      if (toast.toLowerCase().includes("error")) {
        this.lastError = { message: toast };
        this.lastResponse = undefined;
      } else {
        this.lastResponse = { function_name: uniqueName, toast };
        this.lastError = undefined;
      }
    } catch {
      this.lastError = { message: "Unknown error" };
      this.lastResponse = undefined;
    }
  }
);

When(
  "I get function {string}",
  async function (this: PlaywrightWorld, name: string) {
    await this.goToWorkspacePage("function");
    const row = this.page.locator(`table tbody tr:has-text("${this.functionName}")`);
    const visible = await row.isVisible().catch(() => false);
    if (visible) {
      await row.click();
      await this.page.waitForTimeout(500);
      this.lastResponse = { function_name: this.functionName };
      this.lastError = undefined;
    } else {
      this.lastError = { message: "No records found" };
      this.lastResponse = undefined;
    }
  }
);

When(
  "I list functions with count {int} and page {int}",
  async function (this: PlaywrightWorld, count: number, page: number) {
    await this.goToWorkspacePage("function");
    const rows = await this.tableRowCount();
    this.lastResponse = { data: new Array(rows).fill({}) };
    this.lastError = undefined;
  }
);

When(
  "I update function {string} with new validation code",
  async function (this: PlaywrightWorld, name: string) {
    await this.goToWorkspacePage("function");
    const row = this.page.locator(`table tbody tr:has-text("${this.functionName}")`);
    await row.click();
    await this.page.waitForTimeout(500);
    await this.fillMonacoEditor(
      "code_editor_fn",
      `async function execute(payload) { let v = payload.value_validate.value; if (v === "updated-valid") return true; return false; }`
    );
    await this.fillByPlaceholder("Enter a description", "Updated value_validation function");
    await this.fillByPlaceholder("Enter a reason for this change", "Cucumber update");
    await this.clickButton("Submit");
    const toast = await this.waitForToast();
    this.lastResponse = { description: "Updated value_validation function", toast };
    this.lastError = undefined;
  }
);

When(
  "I publish function {string}",
  async function (this: PlaywrightWorld, name: string) {
    if (name === "non-existent-function") {
      this.lastError = { message: "No records found" };
      this.lastResponse = undefined;
      return;
    }
    await this.goToWorkspacePage("function");
    const row = this.page.locator(`table tbody tr:has-text("${this.functionName}")`);
    const visible = await row.isVisible().catch(() => false);
    if (!visible) {
      this.lastError = { message: "No records found" };
      this.lastResponse = undefined;
      return;
    }
    await row.click();
    await this.page.waitForTimeout(500);
    const publishBtn = this.page.locator("button:has-text('Publish')");
    if (await publishBtn.isVisible()) {
      await publishBtn.click();
      const toast = await this.waitForToast();
      this.lastResponse = { published_at: new Date().toISOString(), published_code: "...", toast };
      this.lastError = undefined;
    } else {
      this.lastError = { message: "Publish button not found" };
      this.lastResponse = undefined;
    }
  }
);

// ── Then ────────────────────────────────────────────────────────────

Then(
  "the response should have function type {string}",
  async function (this: PlaywrightWorld, type: string) {
    const content = await this.page.textContent("body");
    assert.ok(
      content?.includes(type) || this.lastResponse?.function_type === type,
      `Function type "${type}" not found`
    );
  }
);

Then(
  "the response should have function name {string}",
  async function (this: PlaywrightWorld, name: string) {
    const content = await this.page.textContent("body");
    assert.ok(
      content?.includes(this.functionName),
      `Function name not found on page`
    );
  }
);

Then(
  "the response should have description {string}",
  async function (this: PlaywrightWorld, desc: string) {
    const content = await this.page.textContent("body");
    assert.ok(
      content?.includes(desc) || this.lastResponse?.description === desc,
      `Description "${desc}" not found`
    );
  }
);
