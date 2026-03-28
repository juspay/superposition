import { Given, When, Then } from "@cucumber/cucumber";
import {
  CreateSecretCommand,
  GetSecretCommand,
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
  "a secret {string} exists with value {string}",
  async function (this: PlaywrightWorld, name: string, value: string) {
    this.secretName = name;
    try {
      await this.client.send(
        new CreateSecretCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          name,
          value,
          description: `Test secret ${name}`,
          change_reason: "Cucumber setup",
        })
      );
      this.createdSecrets.push(name);
    } catch {
      // Already exists
    }
  }
);

Given(
  "a compute function exists that reads the secret {string}",
  async function (this: PlaywrightWorld, secretName: string) {
    this.functionName = this.uniqueName("verify_secret");
    const code = `
      async function execute(payload) {
        return [SECRETS.${secretName}];
      }
    `;
    try {
      await this.client.send(
        new CreateFunctionCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          function_name: this.functionName,
          function: code,
          description: "Secret verification function",
          change_reason: "Cucumber setup",
          runtime_version: FunctionRuntimeVersion.V1,
          function_type: FunctionTypes.VALUE_COMPUTE,
        })
      );
      this.createdFunctions.push(this.functionName);
    } catch {
      // Already exists
    }
  }
);

// ── When ────────────────────────────────────────────────────────────

// HYBRID: Create a secret via the UI drawer for valid names, SDK for error cases
When(
  "I create a secret named {string} with value {string}",
  async function (this: PlaywrightWorld, name: string, value: string) {
    this.secretName = name;
    // Use SDK for error cases (empty, invalid names, duplicates) since UI toast detection is unreliable
    const isValidName = /^[A-Z][A-Z0-9_]*$/.test(name);
    const isDuplicate = this.createdSecrets.includes(name);
    if (!isValidName || isDuplicate) {
      try {
        this.lastResponse = await this.client.send(
          new CreateSecretCommand({
            workspace_id: this.workspaceId,
            org_id: this.orgId,
            name,
            value,
            description: `Test secret ${name}`,
            change_reason: "Cucumber test",
          })
        );
        this.createdSecrets.push(name);
        this.lastError = undefined;
      } catch (e: any) {
        this.lastError = e;
        this.lastResponse = undefined;
      }
      return;
    }
    // PLAYWRIGHT: Use UI drawer for valid secret names
    try {
      await this.goToWorkspacePage("secrets");
      await this.page.getByRole("button", { name: "Create Secret" }).click();
      await this.page.waitForTimeout(300);

      await this.page
        .getByPlaceholder("Enter secret name (uppercase, digits, underscore)")
        .fill(name);
      await this.page
        .getByPlaceholder("Enter a description")
        .fill(`Test secret ${name}`);
      await this.page
        .getByPlaceholder("Enter a reason for this change")
        .fill("Cucumber test");
      await this.page
        .getByPlaceholder("Enter secret value (will be encrypted)")
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
        this.lastResponse = { name, toast: toastText };
        this.createdSecrets.push(name);
        this.lastError = undefined;
      }
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

// PLAYWRIGHT: Navigate to secret detail page, then fetch via SDK for assertions
When(
  "I get secret {string}",
  async function (this: PlaywrightWorld, name: string) {
    try {
      // Navigate to the detail page for UI interaction
      await this.goToDetailPage("secrets", name);
      await this.page.waitForTimeout(300);

      // Fetch via SDK for response assertions
      this.lastResponse = await this.client.send(
        new GetSecretCommand({
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

// PLAYWRIGHT: List secrets by navigating to the secrets page
When(
  "I list secrets",
  async function (this: PlaywrightWorld) {
    try {
      await this.goToWorkspacePage("secrets");
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

// PLAYWRIGHT: edit secret value via UI detail page (SDK fallback for non-existent)
When(
  "I update secret {string} value to {string}",
  async function (this: PlaywrightWorld, name: string, value: string) {
    // Check if secret exists first; if not, use SDK to get proper error
    try {
      await this.client.send(
        new GetSecretCommand({
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
      await this.goToDetailPage("secrets", name);
      await this.page.waitForTimeout(300);

      await this.page.getByRole("button", { name: "Edit" }).click();
      await this.page.waitForTimeout(300);

      await this.page
        .getByPlaceholder("Enter secret value (will be encrypted)")
        .fill(value);

      await this.page
        .getByPlaceholder("Enter a reason for this change")
        .fill("Cucumber update");

      await this.page.getByRole("button", { name: "Submit" }).last().click();

      await this.waitForConfirmButton("Yes, Update");
      await this.page.getByRole("button", { name: "Yes, Update" }).click();

      const toastText = await this.waitForToast();
      if (
        toastText.toLowerCase().includes("error") ||
        toastText.toLowerCase().includes("failed")
      ) {
        this.lastError = { message: toastText };
        this.lastResponse = undefined;
      } else {
        // Fetch updated secret via SDK for response assertions
        this.lastResponse = await this.client.send(
          new GetSecretCommand({
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

// PLAYWRIGHT: delete secret via UI detail page (SDK fallback for non-existent)
When(
  "I delete secret {string}",
  async function (this: PlaywrightWorld, name: string) {
    // Check if secret exists first; if not, use SDK to get proper error
    try {
      await this.client.send(
        new GetSecretCommand({
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
      await this.goToDetailPage("secrets", name);
      await this.page.waitForTimeout(300);

      await this.page.getByRole("button", { name: "Delete" }).click();
      await this.page.waitForTimeout(300);

      await this.waitForConfirmButton("Yes, Delete");
      await this.page.getByRole("button", { name: "Yes, Delete" }).click();

      const toastText = await this.waitForToast();
      if (
        toastText.toLowerCase().includes("error") ||
        toastText.toLowerCase().includes("failed")
      ) {
        this.lastError = { message: toastText };
        this.lastResponse = undefined;
      } else {
        this.createdSecrets = this.createdSecrets.filter((s) => s !== name);
        this.lastResponse = { deleted: true, toast: toastText };
        this.lastError = undefined;
      }
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

// SDK: test compute function execution
When(
  "I test the compute function",
  async function (this: PlaywrightWorld) {
    try {
      this.lastResponse = await this.client.send(
        new TestCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          function_name: this.functionName,
          stage: "draft",
          request: {
            value_compute: {
              name: "",
              prefix: "",
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

When(
  "I test the compute function again",
  async function (this: PlaywrightWorld) {
    try {
      this.lastResponse = await this.client.send(
        new TestCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          function_name: this.functionName,
          stage: "draft",
          request: {
            value_compute: {
              name: "",
              prefix: "",
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
  "the response should have secret name {string}",
  async function (this: PlaywrightWorld, name: string) {
    // If lastResponse came from Playwright (toast-based), verify via the UI table
    if (this.lastResponse?.toast) {
      await this.goToWorkspacePage("secrets");
      await this.page.locator("table").waitFor({ state: "visible", timeout: 10000 });
      const tableText = await this.page.locator("table").textContent();
      assert.ok(
        tableText?.includes(name),
        `Secret "${name}" not found in secrets table`
      );
    } else {
      assert.ok(this.lastResponse, "No response");
      assert.strictEqual(this.lastResponse.name, name);
    }
  }
);

Then(
  "the secret value should not be returned",
  function (this: PlaywrightWorld) {
    assert.ok(this.lastResponse, "No response");
    assert.strictEqual(this.lastResponse.value, undefined);
  }
);

Then(
  "the function output should contain {string}",
  function (this: PlaywrightWorld, expected: string) {
    assert.ok(this.lastResponse, "No response");
    const output = JSON.stringify(this.lastResponse.fn_output);
    assert.ok(
      output.includes(expected),
      `Function output "${output}" does not contain "${expected}"`
    );
  }
);

Then(
  "getting secret {string} should fail with {string}",
  async function (this: PlaywrightWorld, name: string, errorPattern: string) {
    try {
      await this.client.send(
        new GetSecretCommand({
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
