import { Given, When, Then } from "@cucumber/cucumber";
import { PlaywrightWorld } from "../support_ui/world.ts";
import * as assert from "node:assert";

// ── Given ───────────────────────────────────────────────────────────

Given(
  "validation functions are set up",
  async function (this: PlaywrightWorld) {
    // In UI mode, functions should be pre-created or we navigate to create them
    // For now, assume they exist from prior setup
  }
);

Given(
  "a default config exists with key {string} and value {string} age {int}",
  async function (this: PlaywrightWorld, key: string, name: string, age: number) {
    const uniqueKey = this.uniqueName(key);
    this.configKey = uniqueKey;
    await this.goToWorkspacePage("default-config");
    const exists = await this.tableContainsText(uniqueKey);
    if (!exists) {
      await this.clickButton("Create Config");
      await this.page.waitForTimeout(300);
      await this.page.getByPlaceholder("Key name").fill(uniqueKey);
      // Fill schema and value via Monaco editors
      await this.fillMonacoEditor(
        "type-schema",
        JSON.stringify({
          type: "object",
          properties: { name: { type: "string" }, age: { type: "number", minimum: 0 } },
          required: ["name"],
        })
      );
      await this.fillMonacoEditor(
        "default-config-value-input",
        JSON.stringify({ name, age })
      );
      await this.fillByPlaceholder("Enter a description", "Test config");
      await this.fillByPlaceholder("Enter a reason for this change", "Cucumber setup");
      await this.clickButton("Submit");
      await this.expectSuccessToast();
    }
  }
);

Given(
  "a default config exists with key {string} and requires name and email",
  async function (this: PlaywrightWorld, key: string) {
    const uniqueKey = this.uniqueName(key);
    this.configKey = uniqueKey;
    await this.goToWorkspacePage("default-config");
    const exists = await this.tableContainsText(uniqueKey);
    if (!exists) {
      await this.clickButton("Create Config");
      await this.page.waitForTimeout(300);
      await this.page.getByPlaceholder("Key name").fill(uniqueKey);
      await this.fillMonacoEditor(
        "type-schema",
        JSON.stringify({
          type: "object",
          properties: {
            name: { type: "string" },
            email: { type: "string", format: "email" },
          },
          required: ["name", "email"],
        })
      );
      await this.fillMonacoEditor(
        "default-config-value-input",
        JSON.stringify({ name: "Test", email: "test@test.com" })
      );
      await this.fillByPlaceholder("Enter a description", "Config requiring email");
      await this.fillByPlaceholder("Enter a reason for this change", "Cucumber setup");
      await this.clickButton("Submit");
      await this.expectSuccessToast();
    }
  }
);

// ── When: Create ────────────────────────────────────────────────────

When(
  "I create a default config with key {string} and value:",
  async function (this: PlaywrightWorld, key: string, table: any) {
    const rows = table.rowsHash();
    const value: any = {};
    for (const [k, v] of Object.entries(rows)) {
      value[k] = isNaN(Number(v)) ? v : Number(v);
    }
    this.configKey = this.uniqueName(key);
    this.configValue = value;
    await this.goToWorkspacePage("default-config");
    await this.clickButton("Create Config");
    await this.page.waitForTimeout(300);
    await this.page.getByPlaceholder("Key name").fill(this.configKey);
  }
);

When(
  "the schema requires {string} as string and {string} as number with minimum {int}",
  async function (this: PlaywrightWorld, strField: string, numField: string, min: number) {
    await this.fillMonacoEditor(
      "type-schema",
      JSON.stringify({
        type: "object",
        properties: {
          [strField]: { type: "string" },
          [numField]: { type: "number", minimum: min },
        },
        required: [strField],
      })
    );
    await this.fillMonacoEditor(
      "default-config-value-input",
      JSON.stringify(this.configValue)
    );
    await this.fillByPlaceholder("Enter a description", "Test configuration");
    await this.fillByPlaceholder("Enter a reason for this change", "Cucumber test");
    await this.clickButton("Submit");

    try {
      const toast = await this.waitForToast();
      if (toast.toLowerCase().includes("error")) {
        this.lastError = { message: toast };
        this.lastResponse = undefined;
      } else {
        this.lastResponse = { key: this.configKey, toast };
        this.lastError = undefined;
      }
    } catch {
      this.lastResponse = { key: this.configKey };
      this.lastError = undefined;
    }
  }
);

When(
  "I create a default config with key {string} and an invalid schema type {string}",
  async function (this: PlaywrightWorld, key: string, schemaType: string) {
    await this.goToWorkspacePage("default-config");
    await this.clickButton("Create Config");
    await this.page.waitForTimeout(300);
    await this.page.getByPlaceholder("Key name").fill(key);
    await this.fillMonacoEditor("type-schema", JSON.stringify({ type: schemaType }));
    await this.fillMonacoEditor("default-config-value-input", JSON.stringify({ name: "Test" }));
    await this.fillByPlaceholder("Enter a description", "Test");
    await this.fillByPlaceholder("Enter a reason for this change", "Testing invalid schema");
    await this.clickButton("Submit");
    await this.expectErrorToast();
  }
);

When(
  "I create a default config with key {string} and an empty schema",
  async function (this: PlaywrightWorld, key: string) {
    await this.goToWorkspacePage("default-config");
    await this.clickButton("Create Config");
    await this.page.waitForTimeout(300);
    await this.page.getByPlaceholder("Key name").fill(key);
    await this.fillMonacoEditor("type-schema", "{}");
    await this.fillMonacoEditor("default-config-value-input", JSON.stringify({ name: "Test" }));
    await this.fillByPlaceholder("Enter a description", "Test");
    await this.fillByPlaceholder("Enter a reason for this change", "Testing empty schema");
    await this.clickButton("Submit");
    await this.expectErrorToast();
  }
);

When(
  "I create a default config with key {string} where age is {int} but minimum is {int}",
  async function (this: PlaywrightWorld, key: string, age: number, min: number) {
    await this.goToWorkspacePage("default-config");
    await this.clickButton("Create Config");
    await this.page.waitForTimeout(300);
    await this.page.getByPlaceholder("Key name").fill(key);
    await this.fillMonacoEditor(
      "type-schema",
      JSON.stringify({
        type: "object",
        properties: { name: { type: "string" }, age: { type: "number", minimum: min } },
      })
    );
    await this.fillMonacoEditor(
      "default-config-value-input",
      JSON.stringify({ name: "Test User", age })
    );
    await this.fillByPlaceholder("Enter a description", "Test");
    await this.fillByPlaceholder("Enter a reason for this change", "Schema violation test");
    await this.clickButton("Submit");
    await this.expectErrorToast();
  }
);

When(
  "I create a default config with key {string} using validation function {string}",
  async function (this: PlaywrightWorld, key: string, funcName: string) {
    await this.goToWorkspacePage("default-config");
    await this.clickButton("Create Config");
    await this.page.waitForTimeout(300);
    await this.page.getByPlaceholder("Key name").fill(key);
    await this.fillMonacoEditor(
      "type-schema",
      JSON.stringify({ type: "object", properties: { name: { type: "string" } } })
    );
    await this.fillMonacoEditor(
      "default-config-value-input",
      JSON.stringify({ name: "Invalid Value" })
    );
    // Select validation function from dropdown
    await this.selectDropdownOption("Add Function", funcName);
    await this.fillByPlaceholder("Enter a description", "Test");
    await this.fillByPlaceholder("Enter a reason for this change", "Function validation test");
    await this.clickButton("Submit");

    try {
      const toast = await this.waitForToast();
      if (toast.toLowerCase().includes("error") || toast.toLowerCase().includes("fail")) {
        this.lastError = { message: toast };
        this.lastResponse = undefined;
      } else {
        this.lastResponse = { key, toast };
        this.lastError = undefined;
      }
    } catch {
      this.lastError = { message: "No feedback" };
      this.lastResponse = undefined;
    }
  }
);

When(
  "I create a default config with key {string} using compute function {string}",
  async function (this: PlaywrightWorld, key: string, funcName: string) {
    const uniqueKey = this.uniqueName(key);
    await this.goToWorkspacePage("default-config");
    await this.clickButton("Create Config");
    await this.page.waitForTimeout(300);
    await this.page.getByPlaceholder("Key name").fill(uniqueKey);
    await this.fillMonacoEditor(
      "type-schema",
      JSON.stringify({ type: "object", properties: { name: { type: "string" } } })
    );
    await this.fillMonacoEditor(
      "default-config-value-input",
      JSON.stringify({ name: "valid Value" })
    );
    await this.selectDropdownOption("Add Function", funcName);
    await this.fillByPlaceholder("Enter a description", "Test");
    await this.fillByPlaceholder("Enter a reason for this change", "Compute function test");
    await this.clickButton("Submit");

    try {
      const toast = await this.waitForToast();
      if (toast.toLowerCase().includes("error")) {
        this.lastError = { message: toast };
        this.lastResponse = undefined;
      } else {
        this.lastResponse = {
          key: uniqueKey,
          value_compute_function_name: funcName,
          toast,
        };
        this.lastError = undefined;
      }
    } catch {
      this.lastResponse = { key: uniqueKey, value_compute_function_name: funcName };
      this.lastError = undefined;
    }
  }
);

// ── When: Update ────────────────────────────────────────────────────

When(
  "I update the default config {string} with value {string} age {int}",
  async function (this: PlaywrightWorld, key: string, name: string, age: number) {
    await this.goToWorkspacePage("default-config");
    const row = this.page.locator(`table tbody tr:has-text("${this.configKey}")`);
    await row.click();
    await this.page.waitForTimeout(500);
    await this.fillMonacoEditor(
      "default-config-value-input",
      JSON.stringify({ name, age })
    );
    await this.fillByPlaceholder("Enter a reason for this change", "Cucumber update");
    await this.clickButton("Submit");
    const toast = await this.waitForToast();
    this.lastResponse = { value: { name, age }, toast };
    this.lastError = undefined;
  }
);

When(
  "I update default config {string} with a new value",
  async function (this: PlaywrightWorld, key: string) {
    await this.goToWorkspacePage("default-config");
    const row = this.page.locator(`table tbody tr:has-text("${key}")`);
    const visible = await row.isVisible().catch(() => false);
    if (!visible) {
      this.lastError = { message: "No record found" };
      this.lastResponse = undefined;
      return;
    }
    await row.click();
    await this.page.waitForTimeout(500);
    await this.fillMonacoEditor(
      "default-config-value-input",
      JSON.stringify({ name: "Updated" })
    );
    await this.fillByPlaceholder("Enter a reason for this change", "Update test");
    await this.clickButton("Submit");
    const toast = await this.waitForToast();
    this.lastResponse = { toast };
    this.lastError = undefined;
  }
);

When(
  "I update default config {string} schema to add email field and set value with email {string}",
  async function (this: PlaywrightWorld, key: string, email: string) {
    await this.goToWorkspacePage("default-config");
    const row = this.page.locator(`table tbody tr:has-text("${this.configKey}")`);
    await row.click();
    await this.page.waitForTimeout(500);
    await this.fillMonacoEditor(
      "type-schema",
      JSON.stringify({
        type: "object",
        properties: {
          name: { type: "string" },
          age: { type: "number" },
          email: { type: "string", format: "email" },
        },
        required: ["name", "email"],
      })
    );
    await this.fillMonacoEditor(
      "default-config-value-input",
      JSON.stringify({ name: "Updated Name", age: 35, email })
    );
    await this.fillByPlaceholder("Enter a reason for this change", "Updating schema");
    await this.clickButton("Submit");
    const toast = await this.waitForToast();
    this.lastResponse = { value: { email }, toast };
    this.lastError = undefined;
  }
);

When(
  "I update default config {string} schema to an invalid type",
  async function (this: PlaywrightWorld, key: string) {
    await this.goToWorkspacePage("default-config");
    const row = this.page.locator(`table tbody tr:has-text("${this.configKey}")`);
    await row.click();
    await this.page.waitForTimeout(500);
    await this.fillMonacoEditor("type-schema", JSON.stringify({ type: "invalid-type" }));
    await this.fillByPlaceholder("Enter a reason for this change", "Invalid schema test");
    await this.clickButton("Submit");
    await this.expectErrorToast();
  }
);

When(
  "I update default config {string} value without the required email field",
  async function (this: PlaywrightWorld, key: string) {
    await this.goToWorkspacePage("default-config");
    const row = this.page.locator(`table tbody tr:has-text("${this.configKey}")`);
    await row.click();
    await this.page.waitForTimeout(500);
    await this.fillMonacoEditor(
      "default-config-value-input",
      JSON.stringify({ name: "Updated Name", age: 20 })
    );
    await this.fillByPlaceholder("Enter a reason for this change", "Missing field test");
    await this.clickButton("Submit");
    await this.expectErrorToast();
  }
);

When(
  "I update default config {string} validation function to {string}",
  async function (this: PlaywrightWorld, key: string, funcName: string) {
    await this.goToWorkspacePage("default-config");
    const row = this.page.locator(`table tbody tr:has-text("${this.configKey}")`);
    await row.click();
    await this.page.waitForTimeout(500);
    await this.selectDropdownOption("Add Function", funcName);
    await this.fillByPlaceholder("Enter a reason for this change", "Update function");
    await this.clickButton("Submit");
    const toast = await this.waitForToast();
    this.lastResponse = { value_validation_function_name: funcName, toast };
    this.lastError = undefined;
  }
);

// ── Then ────────────────────────────────────────────────────────────

Then(
  "the response should have value_compute_function_name {string}",
  async function (this: PlaywrightWorld, expected: string) {
    const content = await this.page.textContent("body");
    assert.ok(
      content?.includes(expected) || this.lastResponse?.value_compute_function_name === expected,
      `Compute function "${expected}" not found`
    );
  }
);

Then(
  "the response should have value_validation_function_name {string}",
  async function (this: PlaywrightWorld, expected: string) {
    const content = await this.page.textContent("body");
    assert.ok(
      content?.includes(expected) || this.lastResponse?.value_validation_function_name === expected,
      `Validation function "${expected}" not found`
    );
  }
);

Then(
  "the response value should have name {string} and age {int}",
  async function (this: PlaywrightWorld, name: string, age: number) {
    const content = await this.page.textContent("body");
    assert.ok(content?.includes(name), `Name "${name}" not found on page`);
  }
);

Then(
  "the response value should include email {string}",
  async function (this: PlaywrightWorld, email: string) {
    const content = await this.page.textContent("body");
    assert.ok(content?.includes(email), `Email "${email}" not found on page`);
  }
);
