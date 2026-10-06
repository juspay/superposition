import { Given, When, Then } from "@cucumber/cucumber";
import {
  CreateWebhookCommand,
  GetWebhookCommand,
  ListWebhookCommand,
  UpdateWebhookCommand,
  DeleteWebhookCommand,
  GetWebhookByEventCommand,
  HttpMethod,
} from "@juspay/superposition-sdk";
import { SuperpositionWorld } from "../support/world.ts";
import * as assert from "node:assert";

// ── Given ───────────────────────────────────────────────────────────

Given(
  "a webhook {string} exists for event {string}",
  async function (this: SuperpositionWorld, name: string, event: string) {
    this.webhookName = name;
    try {
      await this.client.send(
        new CreateWebhookCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          name,
          description: `Test webhook ${name}`,
          enabled: true,
          url: "https://example.com/webhook",
          method: HttpMethod.POST,
          events: [event],
          change_reason: "Cucumber setup",
        })
      );
      this.createdWebhooks.push(name);
    } catch {
      // Already exists
    }
  }
);

// ── When ────────────────────────────────────────────────────────────

When(
  "I create a webhook named {string} for event {string}",
  async function (this: SuperpositionWorld, name: string, event: string) {
    this.webhookName = name;
    try {
      this.lastResponse = await this.client.send(
        new CreateWebhookCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          name,
          description: `Test webhook ${name}`,
          enabled: true,
          url: "https://example.com/webhook",
          method: HttpMethod.POST,
          events: [event],
          change_reason: "Cucumber test",
        })
      );
      if (this.lastResponse.name) {
        this.createdWebhooks.push(this.lastResponse.name);
      }
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

When(
  "I get webhook {string}",
  async function (this: SuperpositionWorld, name: string) {
    try {
      this.lastResponse = await this.client.send(
        new GetWebhookCommand({
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
  "I list webhooks",
  async function (this: SuperpositionWorld) {
    try {
      this.lastResponse = await this.client.send(
        new ListWebhookCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          count: 10,
          page: 1,
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
  "I update webhook {string} with description {string} and enabled {string}",
  async function (
    this: SuperpositionWorld,
    name: string,
    description: string,
    enabled: string
  ) {
    try {
      this.lastResponse = await this.client.send(
        new UpdateWebhookCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          name,
          description,
          enabled: enabled === "true",
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
  "I get webhook by event {string}",
  async function (this: SuperpositionWorld, event: string) {
    try {
      this.lastResponse = await this.client.send(
        new GetWebhookByEventCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          event,
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
  "I delete webhook {string}",
  async function (this: SuperpositionWorld, name: string) {
    try {
      this.lastResponse = await this.client.send(
        new DeleteWebhookCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          name,
        })
      );
      this.createdWebhooks = this.createdWebhooks.filter((w) => w !== name);
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

// ── Then ────────────────────────────────────────────────────────────

Then(
  "the response should have webhook name {string}",
  function (this: SuperpositionWorld, name: string) {
    assert.ok(this.lastResponse, "No response");
    assert.strictEqual(this.lastResponse.name, name);
  }
);

Then(
  "the response should have webhook enabled {string}",
  function (this: SuperpositionWorld, enabled: string) {
    assert.ok(this.lastResponse, "No response");
    assert.strictEqual(this.lastResponse.enabled, enabled === "true");
  }
);

Then(
  "the response should have webhook url {string}",
  function (this: SuperpositionWorld, url: string) {
    assert.ok(this.lastResponse, "No response");
    assert.strictEqual(this.lastResponse.url, url);
  }
);

Then(
  "getting webhook {string} should fail",
  async function (this: SuperpositionWorld, name: string) {
    try {
      await this.client.send(
        new GetWebhookCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          name,
        })
      );
      assert.fail("Expected an error but got success");
    } catch (e: any) {
      assert.ok(e, "Expected an error when getting deleted webhook");
    }
  }
);
