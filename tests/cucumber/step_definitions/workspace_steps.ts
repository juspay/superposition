import { Given, When, Then } from "@cucumber/cucumber";
import {
  CreateWorkspaceCommand,
  GetWorkspaceCommand,
  ListWorkspaceCommand,
  UpdateWorkspaceCommand,
  WorkspaceStatus,
} from "@juspay/superposition-sdk";
import { SuperpositionWorld } from "../support/world.ts";
import * as assert from "node:assert";

// ── Given ───────────────────────────────────────────────────────────

Given(
  "an organisation exists for workspace tests",
  function (this: SuperpositionWorld) {
    // orgId is set in the Before hook
    assert.ok(this.orgId, "Organisation ID not available");
  }
);

Given(
  "a workspace exists with name {string}",
  async function (this: SuperpositionWorld, name: string) {
    const uniqueName = this.uniqueName(name);
    try {
      const response = await this.client.send(
        new CreateWorkspaceCommand({
          org_id: this.orgId,
          workspace_admin_email: "admin@example.com",
          workspace_name: uniqueName,
          workspace_status: WorkspaceStatus.ENABLED,
          allow_experiment_self_approval: true,
          auto_populate_control: false,
          enable_context_validation: true,
          enable_change_reason_validation: true,
        })
      );
      this.workspaceName = uniqueName;
    } catch {
      // May already exist
      this.workspaceName = uniqueName;
    }
  }
);

// ── When ────────────────────────────────────────────────────────────

When(
  "I list workspaces with count {int} and page {int}",
  async function (this: SuperpositionWorld, count: number, page: number) {
    try {
      this.lastResponse = await this.client.send(
        new ListWorkspaceCommand({ count, page, org_id: this.orgId })
      );
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

When(
  "I create a workspace with name {string} and admin email {string}",
  async function (this: SuperpositionWorld, name: string, email: string) {
    const uniqueName = name ? this.uniqueName(name) : name;
    try {
      this.lastResponse = await this.client.send(
        new CreateWorkspaceCommand({
          org_id: this.orgId,
          workspace_admin_email: email,
          workspace_name: uniqueName,
          workspace_status: WorkspaceStatus.ENABLED,
          allow_experiment_self_approval: true,
          auto_populate_control: false,
          enable_context_validation: true,
          enable_change_reason_validation: true,
        })
      );
      this.workspaceName = uniqueName;
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

When(
  "I update workspace {string} admin email to {string}",
  async function (this: SuperpositionWorld, name: string, email: string) {
    const uniqueName = this.uniqueName(name);
    try {
      this.lastResponse = await this.client.send(
        new UpdateWorkspaceCommand({
          org_id: this.orgId,
          workspace_name: uniqueName,
          workspace_admin_email: email,
          workspace_status: WorkspaceStatus.ENABLED,
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
  "I get workspace {string}",
  async function (this: SuperpositionWorld, name: string) {
    const uniqueName = this.workspaceName || this.uniqueName(name);
    try {
      this.lastResponse = await this.client.send(
        new GetWorkspaceCommand({
          org_id: this.orgId,
          workspace_name: uniqueName,
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
  "I list workspaces filtered by status {string}",
  async function (this: SuperpositionWorld, _status: string) {
    // ListWorkspaceCommand does not yet support status filtering in the SDK
    try {
      this.lastResponse = await this.client.send(
        new ListWorkspaceCommand({
          count: 5,
          page: 1,
          org_id: this.orgId,
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
  "I list workspaces for organisation {string}",
  async function (this: SuperpositionWorld, orgId: string) {
    try {
      this.lastResponse = await this.client.send(
        new ListWorkspaceCommand({ count: 10, page: 1, org_id: orgId })
      );
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

// ── When: Metrics ─────────────────────────────────────────────────

When(
  "I create a workspace {string} with metric definitions {string}",
  async function (this: SuperpositionWorld, name: string, spec: string) {
    const uniqueName = this.uniqueName(name);
    try {
      this.lastResponse = await this.client.send(
        new CreateWorkspaceCommand({
          org_id: this.orgId,
          workspace_admin_email: "admin@example.com",
          workspace_name: uniqueName,
          workspace_status: WorkspaceStatus.ENABLED,
          metrics: { enabled: true, definitions: this.parseMetricDefinitions(spec) },
        })
      );
      this.workspaceName = uniqueName;
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

When(
  "I create a workspace {string} with metrics enabled and no definitions",
  async function (this: SuperpositionWorld, name: string) {
    const uniqueName = this.uniqueName(name);
    try {
      this.lastResponse = await this.client.send(
        new CreateWorkspaceCommand({
          org_id: this.orgId,
          workspace_admin_email: "admin@example.com",
          workspace_name: uniqueName,
          workspace_status: WorkspaceStatus.ENABLED,
          metrics: { enabled: true },
        })
      );
      this.workspaceName = uniqueName;
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

When(
  "I update workspace {string} metric definitions to {string}",
  async function (this: SuperpositionWorld, name: string, spec: string) {
    const uniqueName = this.workspaceName || this.uniqueName(name);
    try {
      this.lastResponse = await this.client.send(
        new UpdateWorkspaceCommand({
          org_id: this.orgId,
          workspace_name: uniqueName,
          metrics: { enabled: true, definitions: this.parseMetricDefinitions(spec) },
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
  "the workspace metrics should be enabled",
  function (this: SuperpositionWorld) {
    assert.ok(this.lastResponse, "No response");
    assert.strictEqual(this.lastResponse.metrics?.enabled, true, "Workspace metrics are not enabled");
  }
);

Then(
  "the workspace metric definitions should include {string} with direction {string}",
  function (this: SuperpositionWorld, name: string, direction: string) {
    assert.ok(this.lastResponse, "No response");
    const definitions = this.lastResponse.metrics?.definitions;
    assert.ok(Array.isArray(definitions), "No metric definitions in response");
    const found = definitions.find((d: any) => d.name === name);
    assert.ok(found, `Metric "${name}" not found in ${JSON.stringify(definitions)}`);
    assert.strictEqual(found.direction, direction);
  }
);

Then(
  "the workspace should have {int} metric definition(s)",
  function (this: SuperpositionWorld, count: number) {
    assert.ok(this.lastResponse, "No response");
    const definitions = this.lastResponse.metrics?.definitions ?? [];
    assert.strictEqual(definitions.length, count);
  }
);

Then(
  "the response should contain a workspace list",
  function (this: SuperpositionWorld) {
    assert.ok(this.lastResponse, "No response");
    assert.ok(this.lastResponse.data, "No data in response");
    assert.ok(Array.isArray(this.lastResponse.data), "data is not an array");
  }
);

Then(
  "the response should have a {string} count",
  function (this: SuperpositionWorld, field: string) {
    assert.ok(this.lastResponse, "No response");
    assert.ok(this.lastResponse[field] !== undefined, `Missing ${field}`);
    assert.strictEqual(typeof this.lastResponse[field], "number");
  }
);

Then(
  "the response should have workspace name {string}",
  function (this: SuperpositionWorld, name: string) {
    assert.ok(this.lastResponse, "No response");
    // The name was made unique, so check the original created name
    assert.ok(
      this.lastResponse.workspace_name?.includes(name) || this.lastResponse.workspace_name === this.workspaceName,
      `Expected workspace name containing "${name}", got "${this.lastResponse.workspace_name}"`
    );
  }
);

Then(
  "the response should have workspace status {string}",
  function (this: SuperpositionWorld, status: string) {
    assert.ok(this.lastResponse, "No response");
    assert.strictEqual(this.lastResponse.workspace_status, status);
  }
);

Then(
  "the response should have workspace admin email {string}",
  function (this: SuperpositionWorld, email: string) {
    assert.ok(this.lastResponse, "No response");
    assert.strictEqual(this.lastResponse.workspace_admin_email, email);
  }
);

Then(
  "the list should contain workspace {string}",
  function (this: SuperpositionWorld, name: string) {
    const data = this.lastResponse?.data;
    assert.ok(Array.isArray(data), "No list data");
    const found = data.find((w: any) => w.workspace_name === this.workspaceName);
    assert.ok(found, `Workspace "${this.workspaceName}" not found in list`);
  }
);

Then(
  "all returned workspaces should have status {string}",
  function (this: SuperpositionWorld, status: string) {
    const data = this.lastResponse?.data;
    assert.ok(Array.isArray(data), "No list data");
    for (const ws of data) {
      assert.strictEqual(ws.workspace_status, status);
    }
  }
);
