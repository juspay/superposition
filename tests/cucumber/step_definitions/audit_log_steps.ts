import { Given, When, Then } from "@cucumber/cucumber";
import { ListAuditLogsCommand } from "@juspay/superposition-sdk";
import { SuperpositionWorld } from "../support/world.ts";
import * as assert from "node:assert";

// ── When ────────────────────────────────────────────────────────────

When(
  "I list audit logs with count {int} and page {int}",
  async function (this: SuperpositionWorld, count: number, page: number) {
    try {
      this.lastResponse = await this.client.send(
        new ListAuditLogsCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          count: count,
          page: page,
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
  "I list audit logs filtered by action {string}",
  async function (this: SuperpositionWorld, action: string) {
    try {
      this.lastResponse = await this.client.send(
        new ListAuditLogsCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          count: 10,
          page: 1,
          action: [action as "INSERT" | "UPDATE" | "DELETE"],
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
  "I list audit logs filtered by table {string}",
  async function (this: SuperpositionWorld, table: string) {
    try {
      this.lastResponse = await this.client.send(
        new ListAuditLogsCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          count: 10,
          page: 1,
          tables: [table],
        })
      );
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);
