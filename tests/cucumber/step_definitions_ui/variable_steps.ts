import { Given, When, Then } from "@cucumber/cucumber";
import {
  CreateVariableCommand,
  GetVariableCommand,
  UpdateVariableCommand,
  DeleteVariableCommand,
  CreateFunctionCommand,
  TestCommand,
  FunctionTypes,
  FunctionRuntimeVersion,
} from "@juspay/superposition-sdk";
import { PlaywrightWorld } from "../support_ui/world.ts";
import * as assert from "node:assert";

// ── Given ───────────────────────────────────────────────────────────

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

When(
  "I create a variable named {string} with value {string}",
  async function (this: PlaywrightWorld, name: string, value: string) {
    this.variableName = name;
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
      if (this.lastResponse.name) {
        this.createdVariables.push(this.lastResponse.name);
      }
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

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

When(
  "I update variable {string} value to {string}",
  async function (this: PlaywrightWorld, name: string, value: string) {
    try {
      this.lastResponse = await this.client.send(
        new UpdateVariableCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          name,
          value,
          change_reason: "Cucumber update",
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
  "I update variable {string} description to {string}",
  async function (this: PlaywrightWorld, name: string, desc: string) {
    try {
      this.lastResponse = await this.client.send(
        new UpdateVariableCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          name,
          description: desc,
          change_reason: "Cucumber update description",
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
  "I delete variable {string}",
  async function (this: PlaywrightWorld, name: string) {
    try {
      this.lastResponse = await this.client.send(
        new DeleteVariableCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          name,
        })
      );
      this.createdVariables = this.createdVariables.filter((v) => v !== name);
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

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
  function (this: PlaywrightWorld, name: string) {
    assert.ok(this.lastResponse, "No response");
    assert.strictEqual(this.lastResponse.name, name);
  }
);

Then(
  "the response should have variable value {string}",
  function (this: PlaywrightWorld, value: string) {
    assert.ok(this.lastResponse, "No response");
    assert.strictEqual(this.lastResponse.value, value);
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
