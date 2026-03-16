import { Given, When, Then } from "@cucumber/cucumber";
import {
  CreateFunctionCommand,
  GetFunctionCommand,
  ListFunctionCommand,
  UpdateFunctionCommand,
  PublishCommand,
  FunctionTypes,
  FunctionRuntimeVersion,
} from "@juspay/superposition-sdk";
import { PlaywrightWorld } from "../support_ui/world.ts";
import * as assert from "node:assert";

// ── Given ───────────────────────────────────────────────────────────

Given(
  "a value_validation function {string} exists",
  async function (this: PlaywrightWorld, name: string) {
    const uniqueName = this.uniqueName(name);
    this.functionName = uniqueName;
    const code = `
      async function execute(payload) {
        let value = payload.value_validate.value;
        let key = payload.value_validate.key;
        if (key === "test-dimension" && value === "valid") return true;
        return false;
      }
    `;
    try {
      await this.client.send(
        new CreateFunctionCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          function_name: uniqueName,
          function: code,
          description: "Test value_validation function",
          change_reason: "Cucumber setup",
          runtime_version: FunctionRuntimeVersion.V1,
          function_type: FunctionTypes.VALUE_VALIDATION,
        })
      );
      this.createdFunctions.push(uniqueName);
    } catch {
      // Already exists
    }
  }
);

// ── When ────────────────────────────────────────────────────────────

When(
  "I create a value_validation function named {string} with code that validates key {string}",
  async function (this: PlaywrightWorld, name: string, key: string) {
    const uniqueName = this.uniqueName(name);
    this.functionName = uniqueName;
    const code = `
      async function execute(payload) {
        let value = payload.value_validate.value;
        let key = payload.value_validate.key;
        if (key === "${key}" && value === "valid") return true;
        return false;
      }
    `;
    try {
      this.lastResponse = await this.client.send(
        new CreateFunctionCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          function_name: uniqueName,
          function: code,
          description: "Test value_validation function",
          change_reason: "Cucumber test",
          runtime_version: FunctionRuntimeVersion.V1,
          function_type: FunctionTypes.VALUE_VALIDATION,
        })
      );
      this.createdFunctions.push(uniqueName);
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

When(
  "I create a value_compute function named {string} with code that returns computed values",
  async function (this: PlaywrightWorld, name: string) {
    const uniqueName = this.uniqueName(name);
    this.functionName = uniqueName;
    const code = `
      async function execute(payload) {
        return ["test1", "test2", "test3"];
      }
    `;
    try {
      this.lastResponse = await this.client.send(
        new CreateFunctionCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          function_name: uniqueName,
          function: code,
          description: "Test value_compute function",
          change_reason: "Cucumber test",
          runtime_version: FunctionRuntimeVersion.V1,
          function_type: FunctionTypes.VALUE_COMPUTE,
        })
      );
      this.createdFunctions.push(uniqueName);
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

When(
  "I create a value_validation function named {string} with code {string}",
  async function (this: PlaywrightWorld, name: string, code: string) {
    const uniqueName = this.uniqueName(name);
    try {
      this.lastResponse = await this.client.send(
        new CreateFunctionCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          function_name: uniqueName,
          function: code,
          description: "Test function",
          change_reason: "Cucumber test",
          runtime_version: FunctionRuntimeVersion.V1,
          function_type: FunctionTypes.VALUE_VALIDATION,
        })
      );
      this.createdFunctions.push(uniqueName);
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

When(
  "I create a value_compute function named {string} with code that returns a string",
  async function (this: PlaywrightWorld, name: string) {
    const uniqueName = this.uniqueName(name);
    const code = `
      async function execute(payload) {
        return "invalid return type";
      }
    `;
    try {
      this.lastResponse = await this.client.send(
        new CreateFunctionCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          function_name: uniqueName,
          function: code,
          description: "Test function",
          change_reason: "Cucumber test",
          runtime_version: FunctionRuntimeVersion.V1,
          function_type: FunctionTypes.VALUE_COMPUTE,
        })
      );
      this.createdFunctions.push(uniqueName);
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

When(
  "I get function {string}",
  async function (this: PlaywrightWorld, name: string) {
    try {
      this.lastResponse = await this.client.send(
        new GetFunctionCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          function_name: this.functionName,
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
  "I list functions with count {int} and page {int}",
  async function (this: PlaywrightWorld, count: number, page: number) {
    try {
      this.lastResponse = await this.client.send(
        new ListFunctionCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          count,
          page,
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
  "I update function {string} with new validation code",
  async function (this: PlaywrightWorld, name: string) {
    const code = `
      async function execute(payload) {
        let value = payload.value_validate.value;
        if (value === "updated-valid") return true;
        return false;
      }
    `;
    try {
      this.lastResponse = await this.client.send(
        new UpdateFunctionCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          function_name: this.functionName,
          function: code,
          description: "Updated value_validation function",
          change_reason: "Cucumber update",
          runtime_version: FunctionRuntimeVersion.V1,
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
  "I publish function {string}",
  async function (this: PlaywrightWorld, name: string) {
    // Use the tracked function name if it matches, otherwise use as-is
    const funcName = this.functionName || this.uniqueName(name);
    try {
      this.lastResponse = await this.client.send(
        new PublishCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          function_name: name === "non-existent-function" ? name : funcName,
          change_reason: "Cucumber publish",
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
  "the response should have function type {string}",
  function (this: PlaywrightWorld, type: string) {
    assert.ok(this.lastResponse, "No response");
    assert.strictEqual(this.lastResponse.function_type, type);
  }
);

Then(
  "the response should have function name {string}",
  function (this: PlaywrightWorld, name: string) {
    assert.ok(this.lastResponse, "No response");
    assert.strictEqual(this.lastResponse.function_name, this.functionName);
  }
);

Then(
  "the response should have description {string}",
  function (this: PlaywrightWorld, desc: string) {
    assert.ok(this.lastResponse, "No response");
    assert.strictEqual(this.lastResponse.description, desc);
  }
);
