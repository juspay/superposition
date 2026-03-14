import { Given, When, Then } from "@cucumber/cucumber";
import {
  CreateTypeTemplatesCommand,
  UpdateTypeTemplatesCommand,
  DeleteTypeTemplatesCommand,
  GetTypeTemplatesListCommand,
} from "@juspay/superposition-sdk";
import { SuperpositionWorld } from "../support/world.ts";
import * as assert from "node:assert";

// ── Given ───────────────────────────────────────────────────────────

Given(
  "a type template {string} exists with schema type {string}",
  async function (this: SuperpositionWorld, name: string, schemaType: string) {
    const uniqueName = this.uniqueName(name);
    this.typeTemplateName = uniqueName;
    try {
      await this.client.send(
        new CreateTypeTemplatesCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          type_name: uniqueName,
          type_schema: { type: schemaType },
          description: `Test type template ${uniqueName}`,
          change_reason: "Cucumber setup",
        })
      );
      this.createdTypeTemplates.push(uniqueName);
    } catch {
      // Already exists
    }
  }
);

// ── When ────────────────────────────────────────────────────────────

When("I list type templates", async function (this: SuperpositionWorld) {
  try {
    this.lastResponse = await this.client.send(
      new GetTypeTemplatesListCommand({
        workspace_id: this.workspaceId,
        org_id: this.orgId,
      })
    );
    this.lastError = undefined;
  } catch (e: any) {
    this.lastError = e;
    this.lastResponse = undefined;
  }
});

When(
  "I create a type template named {string} with schema type {string}",
  async function (this: SuperpositionWorld, name: string, schemaType: string) {
    const uniqueName = this.uniqueName(name);
    this.typeTemplateName = uniqueName;
    try {
      this.lastResponse = await this.client.send(
        new CreateTypeTemplatesCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          type_name: uniqueName,
          type_schema: { type: schemaType },
          description: `${name} type template`,
          change_reason: "Cucumber test",
        })
      );
      this.createdTypeTemplates.push(uniqueName);
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

When(
  "I create a type template named {string} with pattern {string}",
  async function (this: SuperpositionWorld, name: string, pattern: string) {
    const uniqueName = this.uniqueName(name);
    this.typeTemplateName = uniqueName;
    try {
      this.lastResponse = await this.client.send(
        new CreateTypeTemplatesCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          type_name: uniqueName,
          type_schema: { type: "string", pattern },
          description: `${name} pattern type`,
          change_reason: "Cucumber test",
        })
      );
      this.createdTypeTemplates.push(uniqueName);
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

When(
  "I update type template {string} with minimum {int} and maximum {int}",
  async function (this: SuperpositionWorld, name: string, min: number, max: number) {
    try {
      this.lastResponse = await this.client.send(
        new UpdateTypeTemplatesCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          type_name: this.typeTemplateName,
          type_schema: { type: "number", minimum: min, maximum: max },
          description: "Updated type",
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
  "I delete type template {string}",
  async function (this: SuperpositionWorld, name: string) {
    try {
      this.lastResponse = await this.client.send(
        new DeleteTypeTemplatesCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          type_name: this.typeTemplateName,
        })
      );
      this.createdTypeTemplates = this.createdTypeTemplates.filter(
        (t) => t !== this.typeTemplateName
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
  "the response should contain a type template list",
  function (this: SuperpositionWorld) {
    assert.ok(this.lastResponse, "No response");
    assert.ok(this.lastResponse.data !== undefined, "No data in response");
    assert.ok(Array.isArray(this.lastResponse.data), "data is not an array");
  }
);

Then(
  "the response should have type name {string}",
  function (this: SuperpositionWorld, name: string) {
    assert.ok(this.lastResponse, "No response");
    assert.strictEqual(this.lastResponse.type_name, this.typeTemplateName);
  }
);

Then(
  "the response schema type should be {string}",
  function (this: SuperpositionWorld, type: string) {
    assert.ok(this.lastResponse, "No response");
    const schema =
      typeof this.lastResponse.type_schema === "string"
        ? JSON.parse(this.lastResponse.type_schema)
        : this.lastResponse.type_schema;
    assert.strictEqual(schema.type, type);
  }
);

Then(
  "the response schema should have pattern {string}",
  function (this: SuperpositionWorld, pattern: string) {
    assert.ok(this.lastResponse, "No response");
    const schema =
      typeof this.lastResponse.type_schema === "string"
        ? JSON.parse(this.lastResponse.type_schema)
        : this.lastResponse.type_schema;
    assert.strictEqual(schema.pattern, pattern);
  }
);

Then(
  "the response schema should have minimum {int} and maximum {int}",
  function (this: SuperpositionWorld, min: number, max: number) {
    assert.ok(this.lastResponse, "No response");
    const schema =
      typeof this.lastResponse.type_schema === "string"
        ? JSON.parse(this.lastResponse.type_schema)
        : this.lastResponse.type_schema;
    assert.strictEqual(schema.minimum, min);
    assert.strictEqual(schema.maximum, max);
  }
);

Then(
  "listing type templates should not include {string}",
  async function (this: SuperpositionWorld, name: string) {
    const list = await this.client.send(
      new GetTypeTemplatesListCommand({
        workspace_id: this.workspaceId,
        org_id: this.orgId,
      })
    );
    const found = (list.data ?? []).find(
      (t: any) => t.type_name === this.typeTemplateName
    );
    assert.strictEqual(found, undefined, `Type template "${this.typeTemplateName}" still exists`);
  }
);
