import { Given, When, Then } from "@cucumber/cucumber";
import {
  CreateDefaultConfigCommand,
  UpdateDefaultConfigCommand,
  DeleteDefaultConfigCommand,
  CreateFunctionCommand,
  PublishCommand,
  FunctionTypes,
  FunctionRuntimeVersion,
} from "@juspay/superposition-sdk";
import { SuperpositionWorld } from "../support/world.ts";
import * as assert from "node:assert";

// ── Given ───────────────────────────────────────────────────────────

Given(
  "an organisation and workspace exist",
  function (this: SuperpositionWorld) {
    assert.ok(this.orgId, "Organisation ID not available");
    assert.ok(this.workspaceId, "Workspace ID not available");
  }
);

Given(
  "validation functions are set up",
  async function (this: SuperpositionWorld) {
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
  async function (this: SuperpositionWorld, key: string, name: string, age: number) {
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
  async function (this: SuperpositionWorld, key: string) {
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
  async function (this: SuperpositionWorld, key: string, table: any) {
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

When(
  "the schema requires {string} as string and {string} as number with minimum {int}",
  async function (this: SuperpositionWorld, strField: string, numField: string, min: number) {
    try {
      this.lastResponse = await this.client.send(
        new CreateDefaultConfigCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          key: this.configKey,
          schema: {
            type: "object",
            properties: {
              [strField]: { type: "string" },
              [numField]: { type: "number", minimum: min },
            },
            required: [strField],
          },
          value: this.configValue,
          description: "Test configuration",
          change_reason: "Cucumber test creation",
        })
      );
      this.createdConfigs.push(this.configKey);
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

When(
  "I create a default config with key {string} and an invalid schema type {string}",
  async function (this: SuperpositionWorld, key: string, schemaType: string) {
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
  async function (this: SuperpositionWorld, key: string) {
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
  async function (this: SuperpositionWorld, key: string, age: number, min: number) {
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
  async function (this: SuperpositionWorld, key: string, funcName: string) {
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
  async function (this: SuperpositionWorld, key: string, funcName: string) {
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

When(
  "I update the default config {string} with value {string} age {int}",
  async function (this: SuperpositionWorld, key: string, name: string, age: number) {
    try {
      this.lastResponse = await this.client.send(
        new UpdateDefaultConfigCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          key: this.configKey,
          value: { name, age },
          description: "Updated configuration",
          change_reason: "Cucumber update test",
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
  "I update default config {string} with a new value",
  async function (this: SuperpositionWorld, key: string) {
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

When(
  "I update default config {string} schema to add email field and set value with email {string}",
  async function (this: SuperpositionWorld, key: string, email: string) {
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
              age: { type: "number" },
              email: { type: "string", format: "email" },
            },
            required: ["name", "email"],
          },
          value: { name: "Updated Name", age: 35, email },
          change_reason: "Updating schema and value",
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
  "I update default config {string} schema to an invalid type",
  async function (this: SuperpositionWorld, key: string) {
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
  async function (this: SuperpositionWorld, key: string) {
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

When(
  "I update default config {string} validation function to {string}",
  async function (this: SuperpositionWorld, key: string, funcName: string) {
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
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

// ── Then ────────────────────────────────────────────────────────────

Then(
  "the response should have value_compute_function_name {string}",
  function (this: SuperpositionWorld, expected: string) {
    assert.ok(this.lastResponse, "No response");
    assert.strictEqual(this.lastResponse.value_compute_function_name, expected);
  }
);

Then(
  "the response should have value_validation_function_name {string}",
  function (this: SuperpositionWorld, expected: string) {
    assert.ok(this.lastResponse, "No response");
    assert.strictEqual(this.lastResponse.value_validation_function_name, expected);
  }
);

Then(
  "the response value should have name {string} and age {int}",
  function (this: SuperpositionWorld, name: string, age: number) {
    assert.ok(this.lastResponse, "No response");
    assert.deepStrictEqual(this.lastResponse.value, { name, age });
  }
);

Then(
  "the response value should include email {string}",
  function (this: SuperpositionWorld, email: string) {
    assert.ok(this.lastResponse, "No response");
    assert.strictEqual(this.lastResponse.value?.email, email);
  }
);
