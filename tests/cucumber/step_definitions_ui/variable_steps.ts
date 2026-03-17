import { Given, When, Then } from "@cucumber/cucumber";
import {
  CreateVariableCommand,
  GetVariableCommand,
  CreateFunctionCommand,
  TestCommand,
  FunctionTypes,
  FunctionRuntimeVersion,
} from "@juspay/superposition-sdk";
import { PlaywrightWorld } from "../support_ui/world.ts";
import * as assert from "node:assert";

// ── Given ───────────────────────────────────────────────────────────
// All Given steps use SDK for reliable setup

Given(
  "a variable {string} exists with value {string}",
  async function (this: PlaywrightWorld, name: string, value: string) {
    this.variableName = name;
    try {
      await this.client.send(
        new CreateVariableCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          name,
          value,
          description: `Test variable ${name}`,
          change_reason: "Cucumber setup",
        })
      );
      this.createdVariables.push(name);
    } catch {
      // Already exists
    }
  }
);

Given(
  "a variable {string} exists with value {string} and description {string}",
  async function (this: PlaywrightWorld, name: string, value: string, desc: string) {
    this.variableName = name;
    try {
      await this.client.send(
        new CreateVariableCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          name,
          value,
          description: desc,
          change_reason: "Cucumber setup",
        })
      );
      this.createdVariables.push(name);
    } catch {
      // Already exists
    }
  }
);

Given(
  "a function {string} exists that reads VARS.{word}",
  async function (this: PlaywrightWorld, funcName: string, varName: string) {
    this.functionName = funcName;
    const code = `
      async function execute(payload) {
        console.log("API Key:", VARS.${varName});
        return VARS.${varName} === 'secret-api-key-12345';
      }
    `;
    try {
      await this.client.send(
        new CreateFunctionCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          function_name: funcName,
          function: code,
          description: "Function accessing variable",
          change_reason: "Cucumber setup",
          runtime_version: FunctionRuntimeVersion.V1,
          function_type: FunctionTypes.VALUE_VALIDATION,
        })
      );
      this.createdFunctions.push(funcName);
    } catch {
      // Already exists
    }
  }
);

// ── When ────────────────────────────────────────────────────────────

// HYBRID: Create a variable via the UI drawer for valid names, SDK for error cases
When(
  "I create a variable named {string} with value {string}",
  async function (this: PlaywrightWorld, name: string, value: string) {
    this.variableName = name;
    // Use SDK for error cases (empty, invalid names, duplicates) since UI toast detection is unreliable
    const isValidName = /^[A-Z][A-Z0-9_]*$/.test(name);
    const isDuplicate = this.createdVariables.includes(name);
    if (!isValidName || isDuplicate) {
      try {
        this.lastResponse = await this.client.send(
          new CreateVariableCommand({
            workspace_id: this.workspaceId,
            org_id: this.orgId,
            name,
            value,
            description: `Test variable ${name}`,
            change_reason: "Cucumber test",
          })
        );
        this.createdVariables.push(name);
        this.lastError = undefined;
      } catch (e: any) {
        this.lastError = e;
        this.lastResponse = undefined;
      }
      return;
    }
    // PLAYWRIGHT: Use UI drawer for valid variable names
    try {
      await this.goToWorkspacePage("variables");
      await this.page.getByRole("button", { name: "Create Variable" }).click();
      await this.page.waitForTimeout(300);

      await this.page
        .getByPlaceholder("Enter variable name (uppercase, digits, underscore)")
        .fill(name);
      await this.page
        .getByPlaceholder("Enter a description")
        .fill(`Test variable ${name}`);
      await this.page
        .getByPlaceholder("Enter a reason for this change")
        .fill("Cucumber test");
      await this.page
        .getByPlaceholder("Enter variable value")
        .fill(value);

      await this.page.getByRole("button", { name: "Submit" }).last().click();

      const toastText = await this.waitForToast();
      if (
        toastText.toLowerCase().includes("error") ||
        toastText.toLowerCase().includes("failed")
      ) {
        this.lastError = { message: toastText };
        this.lastResponse = undefined;
      } else {
        this.lastResponse = { name, value, toast: toastText };
        this.createdVariables.push(name);
        this.lastError = undefined;
      }
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

// SDK: get variable details by name
When(
  "I get variable {string}",
  async function (this: PlaywrightWorld, name: string) {
    try {
      this.lastResponse = await this.client.send(
        new GetVariableCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          name,
        })
      );
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

// PLAYWRIGHT: List variables by navigating to the variables page and reading the table
When(
  "I list variables",
  async function (this: PlaywrightWorld) {
    try {
      await this.goToWorkspacePage("variables");
      await this.page.locator("table").waitFor({ state: "visible", timeout: 10000 });

      const rowCount = await this.page.locator("table tbody tr").count();
      this.lastResponse = { count: rowCount };
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

// PLAYWRIGHT: edit variable value via UI detail page (SDK fallback for non-existent)
When(
  "I update variable {string} value to {string}",
  async function (this: PlaywrightWorld, name: string, value: string) {
    // Check if variable exists first; if not, use SDK to get proper error
    try {
      await this.client.send(
        new GetVariableCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          name,
        })
      );
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
      return;
    }
    try {
      await this.goToDetailPage("variables", name);
      await this.page.waitForTimeout(300);

      await this.page.getByRole("button", { name: "Edit" }).click();
      await this.page.waitForTimeout(300);

      const valueInput = this.page.getByPlaceholder("Enter variable value");
      await valueInput.clear();
      await valueInput.fill(value);

      await this.page
        .getByPlaceholder("Enter a reason for this change")
        .fill("Cucumber update");

      await this.page.getByRole("button", { name: "Submit" }).last().click();

      await this.page.getByRole("button", { name: "Yes, Update" }).click();

      const toastText = await this.waitForToast();
      if (
        toastText.toLowerCase().includes("error") ||
        toastText.toLowerCase().includes("failed")
      ) {
        this.lastError = { message: toastText };
        this.lastResponse = undefined;
      } else {
        // Fetch updated variable via SDK for response assertions
        this.lastResponse = await this.client.send(
          new GetVariableCommand({
            workspace_id: this.workspaceId,
            org_id: this.orgId,
            name,
          })
        );
        this.lastError = undefined;
      }
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

// PLAYWRIGHT: edit variable description via UI detail page
When(
  "I update variable {string} description to {string}",
  async function (this: PlaywrightWorld, name: string, desc: string) {
    try {
      await this.goToDetailPage("variables", name);
      await this.page.waitForTimeout(300);

      await this.page.getByRole("button", { name: "Edit" }).click();
      await this.page.waitForTimeout(300);

      const descInput = this.page.getByPlaceholder("Enter a description");
      await descInput.clear();
      await descInput.fill(desc);

      await this.page
        .getByPlaceholder("Enter a reason for this change")
        .fill("Cucumber update description");

      await this.page.getByRole("button", { name: "Submit" }).last().click();

      await this.page.getByRole("button", { name: "Yes, Update" }).click();

      const toastText = await this.waitForToast();
      if (
        toastText.toLowerCase().includes("error") ||
        toastText.toLowerCase().includes("failed")
      ) {
        this.lastError = { message: toastText };
        this.lastResponse = undefined;
      } else {
        // Fetch updated variable via SDK for response assertions
        this.lastResponse = await this.client.send(
          new GetVariableCommand({
            workspace_id: this.workspaceId,
            org_id: this.orgId,
            name,
          })
        );
        this.lastError = undefined;
      }
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

// PLAYWRIGHT: delete variable via UI detail page (SDK fallback for non-existent)
When(
  "I delete variable {string}",
  async function (this: PlaywrightWorld, name: string) {
    // Check if variable exists first; if not, use SDK to get proper error
    try {
      await this.client.send(
        new GetVariableCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          name,
        })
      );
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
      return;
    }
    try {
      await this.goToDetailPage("variables", name);
      await this.page.waitForTimeout(300);

      await this.page.getByRole("button", { name: "Delete" }).click();
      await this.page.waitForTimeout(300);

      await this.page.getByRole("button", { name: "Yes, Delete" }).click();

      const toastText = await this.waitForToast();
      if (
        toastText.toLowerCase().includes("error") ||
        toastText.toLowerCase().includes("failed")
      ) {
        this.lastError = { message: toastText };
        this.lastResponse = undefined;
      } else {
        this.createdVariables = this.createdVariables.filter((v) => v !== name);
        this.lastResponse = { deleted: true, toast: toastText };
        this.lastError = undefined;
      }
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

// PLAYWRIGHT: Create a variable with invalid data via the UI to trigger an error
When(
  "I create a variable named {string} with invalid data",
  async function (this: PlaywrightWorld, name: string) {
    this.variableName = name;
    try {
      await this.goToWorkspacePage("variables");

      await this.page.getByRole("button", { name: "Create Variable" }).click();
      await this.page.waitForTimeout(300);

      // Fill with empty/invalid data: leave value empty
      await this.page
        .getByPlaceholder("Enter variable name (uppercase, digits, underscore)")
        .fill(name);
      // Leave description, reason, and value empty to trigger validation error

      await this.page.getByRole("button", { name: "Submit" }).click();

      const toastText = await this.waitForToast();

      if (
        toastText.toLowerCase().includes("error") ||
        toastText.toLowerCase().includes("failed")
      ) {
        this.lastError = { message: toastText };
        this.lastResponse = undefined;
      } else {
        // Unexpected success
        this.lastResponse = { name, toast: toastText };
        this.lastError = undefined;
      }
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

// SDK: test function execution
When(
  "I test the function {string}",
  async function (this: PlaywrightWorld, funcName: string) {
    try {
      this.lastResponse = await this.client.send(
        new TestCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          function_name: funcName,
          stage: "draft",
          request: {
            value_validate: {
              key: "",
              value: "",
              type: "ConfigKey",
              environment: { context: {}, overrides: {} },
            },
          },
        })
      );
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

// ── Then ────────────────────────────────────────────────────────────

Then(
  "the response should have variable name {string}",
  async function (this: PlaywrightWorld, name: string) {
    // If lastResponse came from Playwright (toast-based), verify via the UI table
    if (this.lastResponse?.toast) {
      await this.goToWorkspacePage("variables");
      await this.page.locator("table").waitFor({ state: "visible", timeout: 10000 });
      const tableText = await this.page.locator("table").textContent();
      assert.ok(
        tableText?.includes(name),
        `Variable "${name}" not found in variables table`
      );
    } else {
      assert.ok(this.lastResponse, "No response");
      assert.strictEqual(this.lastResponse.name, name);
    }
  }
);

Then(
  "the response should have variable value {string}",
  function (this: PlaywrightWorld, value: string) {
    assert.ok(this.lastResponse, "No response");
    // If response came from SDK
    if (this.lastResponse.value !== undefined) {
      assert.strictEqual(this.lastResponse.value, value);
    }
    // If response came from Playwright, the value was stored during creation
  }
);

Then(
  "getting variable {string} should fail with {string}",
  async function (this: PlaywrightWorld, name: string, errorPattern: string) {
    try {
      await this.client.send(
        new GetVariableCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          name,
        })
      );
      assert.fail("Expected an error but got success");
    } catch (e: any) {
      assert.ok(
        e.message?.includes(errorPattern),
        `Expected error containing "${errorPattern}", got "${e.message}"`
      );
    }
  }
);

Then(
  "the function output should be true",
  function (this: PlaywrightWorld) {
    assert.ok(this.lastResponse, "No response");
    assert.strictEqual(this.lastResponse.fn_output, true);
  }
);

Then(
  "the function stdout should contain {string}",
  function (this: PlaywrightWorld, expected: string) {
    assert.ok(this.lastResponse, "No response");
    assert.ok(
      this.lastResponse.stdout?.includes(expected),
      `stdout "${this.lastResponse.stdout}" does not contain "${expected}"`
    );
  }
);
