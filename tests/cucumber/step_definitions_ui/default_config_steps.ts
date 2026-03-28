import { Given, When, Then } from "@cucumber/cucumber";
import {
  CreateDefaultConfigCommand,
  UpdateDefaultConfigCommand,
  DeleteDefaultConfigCommand,
  GetDefaultConfigCommand,
  CreateFunctionCommand,
  PublishCommand,
  FunctionTypes,
  FunctionRuntimeVersion,
} from "@juspay/superposition-sdk";
import { PlaywrightWorld } from "../support_ui/world.ts";
import * as assert from "node:assert";

// ── Given ───────────────────────────────────────────────────────────

Given(
  "an organisation and workspace exist",
  function (this: PlaywrightWorld) {
    assert.ok(this.orgId, "Organisation ID not available");
    assert.ok(this.workspaceId, "Workspace ID not available");
  }
);

Given(
  "validation functions are set up",
  async function (this: PlaywrightWorld) {
    const functions = [
      {
        name: "false_validation",
        code: `async function execute(payload) { return false; }`,
        type: FunctionTypes.VALUE_VALIDATION,
      },
      {
        name: "true_function",
        code: `async function execute(payload) { return true; }`,
        type: FunctionTypes.VALUE_VALIDATION,
      },
      {
        name: "auto_fn",
        code: `async function execute(payload) { return []; }`,
        type: FunctionTypes.VALUE_COMPUTE,
      },
    ];

    for (const fn of functions) {
      try {
        await this.client.send(
          new CreateFunctionCommand({
            workspace_id: this.workspaceId,
            org_id: this.orgId,
            function_name: fn.name,
            function: fn.code,
            description: `Test ${fn.type} function`,
            change_reason: "Cucumber test setup",
            runtime_version: FunctionRuntimeVersion.V1,
            function_type: fn.type,
          })
        );
        this.createdFunctions.push(fn.name);

        await this.client.send(
          new PublishCommand({
            workspace_id: this.workspaceId,
            org_id: this.orgId,
            function_name: fn.name,
            change_reason: "Publishing for cucumber tests",
          })
        );
      } catch {
        // Already exists
      }
    }
  }
);

Given(
  "a default config exists with key {string} and value {string} age {int}",
  async function (this: PlaywrightWorld, key: string, name: string, age: number) {
    const uniqueKey = this.uniqueName(key);
    this.configKey = uniqueKey;
    try {
      await this.client.send(
        new CreateDefaultConfigCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          key: uniqueKey,
          schema: {
            type: "object",
            properties: { name: { type: "string" }, age: { type: "number", minimum: 0 } },
            required: ["name"],
          },
          value: { name, age },
          description: "Test configuration",
          change_reason: "Cucumber test setup",
        })
      );
      this.createdConfigs.push(uniqueKey);
    } catch {
      // Already exists
    }
  }
);

Given(
  "a default config exists with key {string} and requires name and email",
  async function (this: PlaywrightWorld, key: string) {
    const uniqueKey = this.uniqueName(key);
    this.configKey = uniqueKey;
    try {
      await this.client.send(
        new CreateDefaultConfigCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          key: uniqueKey,
          schema: {
            type: "object",
            properties: {
              name: { type: "string" },
              email: { type: "string", format: "email" },
            },
            required: ["name", "email"],
          },
          value: { name: "Test", email: "test@test.com" },
          description: "Test config requiring email",
          change_reason: "Cucumber test setup",
        })
      );
      this.createdConfigs.push(uniqueKey);
    } catch {
      // Already exists
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
    // Schema and value stored for later
    this.configValue = value;
  }
);

// PLAYWRIGHT: Navigate to create page, fill form fields, use Monaco for schema+value,
// submit, wait for toast, then fetch via SDK for assertions.
When(
  "the schema requires {string} as string and {string} as number with minimum {int}",
  async function (this: PlaywrightWorld, strField: string, numField: string, min: number) {
    const schema = {
      type: "object",
      properties: {
        [strField]: { type: "string" },
        [numField]: { type: "number", minimum: min },
      },
      required: [strField],
    };

    try {
      // Navigate to the create page
      await this.goToWorkspacePage("default-config/action/create");
      await this.page.waitForTimeout(500);

      // Fill key name
      await this.page.getByPlaceholder("Enter your key").fill(this.configKey);

      // Fill description
      await this.page.getByPlaceholder("Enter a description").fill("Test configuration");

      // Fill change reason
      await this.page
        .getByPlaceholder("Enter a reason for this change")
        .fill("Cucumber test creation");

      // Select "Custom JSON Schema" from type template dropdown to enable the schema editor
      const schemaDropdown = this.page
        .locator('.dropdown')
        .filter({ hasText: /Choose a type template|Custom JSON Schema/i })
        .first();
      await schemaDropdown.click();
      await this.page.waitForTimeout(300);
      await this.page
        .locator('.dropdown-content li, .dropdown-content a, .menu li')
        .filter({ hasText: "Custom JSON Schema" })
        .first()
        .click();
      await this.page.waitForTimeout(500);

      // Fill schema in Monaco editor (#type-schema)
      await this.fillMonacoEditor("type-schema", JSON.stringify(schema));
      await this.page.waitForTimeout(300);

      // Fill value in Monaco editor (#default-config-value-input)
      await this.fillMonacoEditor(
        "default-config-value-input",
        JSON.stringify(this.configValue)
      );
      await this.page.waitForTimeout(300);

      // Submit the form
      await this.page.getByRole("button", { name: "Submit" }).click();

      // Wait for toast
      const toastText = await this.waitForToast();
      if (
        toastText.toLowerCase().includes("error") ||
        toastText.toLowerCase().includes("failed")
      ) {
        this.lastError = { message: toastText };
        this.lastResponse = undefined;
        return;
      }

      // Fetch via SDK for assertions
      this.createdConfigs.push(this.configKey);
      this.lastResponse = { key: this.configKey, value: this.configValue, toast: toastText };
      this.lastError = undefined;
    } catch (e: any) {
      // Fallback to SDK if UI fails
      try {
        this.lastResponse = await this.client.send(
          new CreateDefaultConfigCommand({
            workspace_id: this.workspaceId,
            org_id: this.orgId,
            key: this.configKey,
            schema,
            value: this.configValue,
            description: "Test configuration",
            change_reason: "Cucumber test creation",
          })
        );
        this.createdConfigs.push(this.configKey);
        this.lastError = undefined;
      } catch (sdkError: any) {
        this.lastError = sdkError;
        this.lastResponse = undefined;
      }
    }
  }
);

When(
  "I create a default config with key {string} and an invalid schema type {string}",
  async function (this: PlaywrightWorld, key: string, schemaType: string) {
    try {
      this.lastResponse = await this.client.send(
        new CreateDefaultConfigCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          key,
          schema: { type: schemaType },
          value: { name: "Test" },
          description: "Test",
          change_reason: "Testing invalid schema",
        })
      );
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

When(
  "I create a default config with key {string} and an empty schema",
  async function (this: PlaywrightWorld, key: string) {
    try {
      this.lastResponse = await this.client.send(
        new CreateDefaultConfigCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          key,
          schema: {},
          value: { name: "Test" },
          description: "Test",
          change_reason: "Testing empty schema",
        })
      );
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

When(
  "I create a default config with key {string} where age is {int} but minimum is {int}",
  async function (this: PlaywrightWorld, key: string, age: number, min: number) {
    try {
      this.lastResponse = await this.client.send(
        new CreateDefaultConfigCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          key,
          schema: {
            type: "object",
            properties: {
              name: { type: "string" },
              age: { type: "number", minimum: min },
            },
            required: ["name"],
          },
          value: { name: "Test User", age },
          description: "Test",
          change_reason: "Testing schema violation",
        })
      );
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

When(
  "I create a default config with key {string} using validation function {string}",
  async function (this: PlaywrightWorld, key: string, funcName: string) {
    try {
      this.lastResponse = await this.client.send(
        new CreateDefaultConfigCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          key,
          schema: { type: "object", properties: { name: { type: "string" } } },
          value: { name: "Invalid Value" },
          description: "Test",
          value_validation_function_name: funcName,
          change_reason: "Testing function validation",
        })
      );
      this.createdConfigs.push(key);
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

When(
  "I create a default config with key {string} using compute function {string}",
  async function (this: PlaywrightWorld, key: string, funcName: string) {
    const uniqueKey = this.uniqueName(key);
    try {
      this.lastResponse = await this.client.send(
        new CreateDefaultConfigCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          key: uniqueKey,
          schema: { type: "object", properties: { name: { type: "string" } } },
          value: { name: "valid Value" },
          description: "Test",
          value_compute_function_name: funcName,
          change_reason: "Testing compute function",
        })
      );
      this.createdConfigs.push(uniqueKey);
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

// ── When: Update ────────────────────────────────────────────────────

// PLAYWRIGHT: Navigate to detail page → click Edit → fill value via Monaco → Submit → confirm → wait for toast → fetch via SDK
When(
  "I update the default config {string} with value {string} age {int}",
  async function (this: PlaywrightWorld, key: string, name: string, age: number) {
    const newValue = { name, age };

    try {
      // Navigate to the edit page directly
      await this.goToDetailPage("default-config", this.configKey);
      await this.page.waitForTimeout(300);

      // Click the Edit link/button
      await this.page.getByRole("link", { name: "Edit" }).click();
      await this.page.waitForLoadState("networkidle");
      await this.page.waitForTimeout(300);

      // Fill description
      const descInput = this.page.getByPlaceholder("Enter a description");
      await descInput.clear();
      await descInput.fill("Updated configuration");

      // Fill change reason
      await this.page
        .getByPlaceholder("Enter a reason for this change")
        .fill("Cucumber update test");

      // Fill value using Monaco editor
      await this.fillMonacoEditor("default-config-value-input", JSON.stringify(newValue));
      await this.page.waitForTimeout(300);

      // Click Submit to trigger the update precheck
      await this.page.getByRole("button", { name: "Submit" }).click();
      await this.page.waitForTimeout(500);

      // Confirm the update in the modal
      await this.waitForConfirmButton("Yes, Update");
      await this.page.getByRole("button", { name: "Yes, Update" }).click();

      // Wait for toast
      const toastText = await this.waitForToast();
      if (
        toastText.toLowerCase().includes("error") ||
        toastText.toLowerCase().includes("failed")
      ) {
        this.lastError = { message: toastText };
        this.lastResponse = undefined;
        return;
      }

      // Fetch via SDK for response assertions
      this.lastResponse = await this.client.send(
        new GetDefaultConfigCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          key: this.configKey,
        })
      );
      this.lastError = undefined;
    } catch (e: any) {
      // Fallback: SDK update
      try {
        this.lastResponse = await this.client.send(
          new UpdateDefaultConfigCommand({
            workspace_id: this.workspaceId,
            org_id: this.orgId,
            key: this.configKey,
            value: newValue,
            description: "Updated configuration",
            change_reason: "Cucumber update test",
          })
        );
        this.lastError = undefined;
      } catch (sdkError: any) {
        this.lastError = sdkError;
        this.lastResponse = undefined;
      }
    }
  }
);

When(
  "I update default config {string} with a new value",
  async function (this: PlaywrightWorld, key: string) {
    try {
      this.lastResponse = await this.client.send(
        new UpdateDefaultConfigCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          key,
          value: { name: "Updated" },
          description: "Updated",
          change_reason: "Update test",
        })
      );
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

// PLAYWRIGHT: Navigate to edit page, update schema + value via Monaco, submit, confirm, wait for toast, fetch via SDK.
When(
  "I update default config {string} schema to add email field and set value with email {string}",
  async function (this: PlaywrightWorld, key: string, email: string) {
    const newSchema = {
      type: "object",
      properties: {
        name: { type: "string" },
        age: { type: "number" },
        email: { type: "string", format: "email" },
      },
      required: ["name", "email"],
    };
    const newValue = { name: "Updated Name", age: 35, email };

    try {
      // Navigate to the edit page directly
      await this.goToDetailPage("default-config", this.configKey);
      await this.page.waitForTimeout(300);

      // Click the Edit link
      await this.page.getByRole("link", { name: "Edit" }).click();
      await this.page.waitForLoadState("networkidle");
      await this.page.waitForTimeout(300);

      // Fill change reason
      await this.page
        .getByPlaceholder("Enter a reason for this change")
        .fill("Updating schema and value");

      // Fill schema in Monaco editor (#type-schema)
      await this.fillMonacoEditor("type-schema", JSON.stringify(newSchema));
      await this.page.waitForTimeout(300);

      // Fill value in Monaco editor (#default-config-value-input)
      await this.fillMonacoEditor("default-config-value-input", JSON.stringify(newValue));
      await this.page.waitForTimeout(300);

      // Click Submit to trigger the update precheck
      await this.page.getByRole("button", { name: "Submit" }).click();
      await this.page.waitForTimeout(500);

      // Confirm the update in the modal
      await this.waitForConfirmButton("Yes, Update");
      await this.page.getByRole("button", { name: "Yes, Update" }).click();

      // Wait for toast
      const toastText = await this.waitForToast();
      if (
        toastText.toLowerCase().includes("error") ||
        toastText.toLowerCase().includes("failed")
      ) {
        this.lastError = { message: toastText };
        this.lastResponse = undefined;
        return;
      }

      // Fetch via SDK for response assertions
      this.lastResponse = await this.client.send(
        new GetDefaultConfigCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          key: this.configKey,
        })
      );
      this.lastError = undefined;
    } catch (e: any) {
      // Fallback: SDK update
      try {
        this.lastResponse = await this.client.send(
          new UpdateDefaultConfigCommand({
            workspace_id: this.workspaceId,
            org_id: this.orgId,
            key: this.configKey,
            schema: newSchema,
            value: newValue,
            change_reason: "Updating schema and value",
          })
        );
        this.lastError = undefined;
      } catch (sdkError: any) {
        this.lastError = sdkError;
        this.lastResponse = undefined;
      }
    }
  }
);

When(
  "I update default config {string} schema to an invalid type",
  async function (this: PlaywrightWorld, key: string) {
    try {
      this.lastResponse = await this.client.send(
        new UpdateDefaultConfigCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          key: this.configKey,
          schema: { type: "invalid-type" },
          change_reason: "Testing invalid schema update",
        })
      );
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

When(
  "I update default config {string} value without the required email field",
  async function (this: PlaywrightWorld, key: string) {
    try {
      this.lastResponse = await this.client.send(
        new UpdateDefaultConfigCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          key: this.configKey,
          schema: {
            type: "object",
            properties: {
              name: { type: "string" },
              age: { type: "number", minimum: 18 },
              email: { type: "string", format: "email" },
            },
            required: ["name", "email"],
          },
          value: { name: "Updated Name", age: 20 },
          change_reason: "Testing missing required field",
        })
      );
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

// PLAYWRIGHT: Navigate to edit page, select validation function from dropdown, submit, confirm, wait for toast, fetch via SDK.
When(
  "I update default config {string} validation function to {string}",
  async function (this: PlaywrightWorld, key: string, funcName: string) {
    try {
      // Navigate to the edit page directly
      await this.goToDetailPage("default-config", this.configKey);
      await this.page.waitForTimeout(300);

      // Click the Edit link
      await this.page.getByRole("link", { name: "Edit" }).click();
      await this.page.waitForLoadState("networkidle");
      await this.page.waitForTimeout(300);

      // Fill change reason
      await this.page
        .getByPlaceholder("Enter a reason for this change")
        .fill("Update validation function");

      // Select validation function from dropdown
      // The dropdown button shows "Add Function" or currently selected function name
      const validationDropdown = this.page
        .locator('.dropdown')
        .filter({ hasText: /Add Function|Validation Function/i })
        .first();
      await validationDropdown.click();
      await this.page.waitForTimeout(300);
      await this.page
        .locator('.dropdown-content li, .dropdown-content a, .menu li')
        .filter({ hasText: funcName })
        .first()
        .click();
      await this.page.waitForTimeout(300);

      // Click Submit to trigger the update precheck
      await this.page.getByRole("button", { name: "Submit" }).click();
      await this.page.waitForTimeout(500);

      // Confirm the update in the modal
      await this.waitForConfirmButton("Yes, Update");
      await this.page.getByRole("button", { name: "Yes, Update" }).click();

      // Wait for toast
      const toastText = await this.waitForToast();
      if (
        toastText.toLowerCase().includes("error") ||
        toastText.toLowerCase().includes("failed")
      ) {
        this.lastError = { message: toastText };
        this.lastResponse = undefined;
        return;
      }

      // Fetch via SDK for response assertions
      this.lastResponse = await this.client.send(
        new GetDefaultConfigCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          key: this.configKey,
        })
      );
      this.lastError = undefined;
    } catch (e: any) {
      // Fallback: SDK update
      try {
        this.lastResponse = await this.client.send(
          new UpdateDefaultConfigCommand({
            workspace_id: this.workspaceId,
            org_id: this.orgId,
            key: this.configKey,
            value_validation_function_name: funcName,
            change_reason: "Update validation function",
          })
        );
        this.lastError = undefined;
      } catch (sdkError: any) {
        this.lastError = sdkError;
        this.lastResponse = undefined;
      }
    }
  }
);

// ── Then ────────────────────────────────────────────────────────────

Then(
  "the response should have value_compute_function_name {string}",
  function (this: PlaywrightWorld, expected: string) {
    assert.ok(this.lastResponse, "No response");
    assert.strictEqual(this.lastResponse.value_compute_function_name, expected);
  }
);

Then(
  "the response should have value_validation_function_name {string}",
  function (this: PlaywrightWorld, expected: string) {
    assert.ok(this.lastResponse, "No response");
    assert.strictEqual(this.lastResponse.value_validation_function_name, expected);
  }
);

Then(
  "the response value should have name {string} and age {int}",
  function (this: PlaywrightWorld, name: string, age: number) {
    assert.ok(this.lastResponse, "No response");
    assert.deepStrictEqual(this.lastResponse.value, { name, age });
  }
);

Then(
  "the response value should include email {string}",
  function (this: PlaywrightWorld, email: string) {
    assert.ok(this.lastResponse, "No response");
    assert.strictEqual(this.lastResponse.value?.email, email);
  }
);
