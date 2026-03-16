import { Given, When, Then } from "@cucumber/cucumber";
import {
  CreateSecretCommand,
  GetSecretCommand,
  UpdateSecretCommand,
  DeleteSecretCommand,
  CreateFunctionCommand,
  TestCommand,
  FunctionTypes,
  FunctionRuntimeVersion,
} from "@juspay/superposition-sdk";
import { PlaywrightWorld } from "../support_ui/world.ts";
import * as assert from "node:assert";

// ── Given ───────────────────────────────────────────────────────────

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

When(
  "I create a secret named {string} with value {string}",
  async function (this: PlaywrightWorld, name: string, value: string) {
    this.secretName = name;
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
      if (this.lastResponse.name) {
        this.createdSecrets.push(this.lastResponse.name);
      }
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

When(
  "I get secret {string}",
  async function (this: PlaywrightWorld, name: string) {
    try {
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

When(
  "I update secret {string} value to {string}",
  async function (this: PlaywrightWorld, name: string, value: string) {
    try {
      this.lastResponse = await this.client.send(
        new UpdateSecretCommand({
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
  "I delete secret {string}",
  async function (this: PlaywrightWorld, name: string) {
    try {
      this.lastResponse = await this.client.send(
        new DeleteSecretCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          name,
        })
      );
      this.createdSecrets = this.createdSecrets.filter((s) => s !== name);
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

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
  function (this: PlaywrightWorld, name: string) {
    assert.ok(this.lastResponse, "No response");
    assert.strictEqual(this.lastResponse.name, name);
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
