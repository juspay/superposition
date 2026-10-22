import { When } from "@cucumber/cucumber";
import {
  GetExperimentConfigCommand,
  ApplicableVariantsCommand,
} from "@juspay/superposition-sdk";
import { SuperpositionWorld } from "../support/world.ts";

// ── When ────────────────────────────────────────────────────────────

When(
  "I get the experiment config",
  async function (this: SuperpositionWorld) {
    try {
      this.lastResponse = await this.client.send(
        new GetExperimentConfigCommand({
          workspace_id: this.workspaceId,
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
  "I get the experiment config with prefix {string}",
  async function (this: SuperpositionWorld, prefix: string) {
    try {
      this.lastResponse = await this.client.send(
        new GetExperimentConfigCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          prefix: [prefix],
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
  "I get the experiment config with context {string} equals {string}",
  async function (this: SuperpositionWorld, dim: string, value: string) {
    try {
      this.lastResponse = await this.client.send(
        new GetExperimentConfigCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          context: { [dim]: value },
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
  "I get applicable variants with context {string} equals {string} and identifier {string}",
  async function (this: SuperpositionWorld, dim: string, value: string, identifier: string) {
    try {
      this.lastResponse = await this.client.send(
        new ApplicableVariantsCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          context: { [dim]: value },
          identifier,
        })
      );
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);
