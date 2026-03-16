import { Before, After, BeforeAll, AfterAll, Status } from "@cucumber/cucumber";
import { chromium, Browser } from "playwright";
import {
  CreateOrganisationCommand,
  ListOrganisationCommand,
  CreateWorkspaceCommand,
  ListWorkspaceCommand,
  MigrateWorkspaceSchemaCommand,
  WorkspaceStatus,
  DeleteDimensionCommand,
  DeleteDefaultConfigCommand,
  DeleteFunctionCommand,
  DeleteVariableCommand,
  DeleteSecretCommand,
  DeleteTypeTemplatesCommand,
  DeleteContextCommand,
  DiscardExperimentCommand,
  DeleteExperimentGroupCommand,
} from "@juspay/superposition-sdk";
import { PlaywrightWorld } from "./world.ts";

const TEST_ORG_NAME = "cucumberorg";
const TEST_WORKSPACE = "cucumberws";

let browser: Browser;

// Shared state across scenarios (org/workspace persist for the run)
let sharedOrgId: string = "";
let sharedWorkspaceId: string = "";

BeforeAll(async function () {
  // ── Launch browser ──────────────────────────────────────────────
  const headless = process.env.HEADLESS !== "false";
  browser = await chromium.launch({
    headless,
    slowMo: process.env.SLOW_MO ? parseInt(process.env.SLOW_MO) : 0,
  });

  // ── SDK-based org/workspace setup (mirrors API hooks) ──────────
  const { SuperpositionClient } = await import("@juspay/superposition-sdk");
  const client = new SuperpositionClient({
    endpoint: process.env.SUPERPOSITION_BASE_URL || "http://127.0.0.1:8080",
    token: { token: process.env.SUPERPOSITION_TOKEN || "some-token" },
  });

  // Setup org — find a matching org whose workspace is usable, or create a new one
  const { UpdateWorkspaceCommand, ListDimensionsCommand } = await import("@juspay/superposition-sdk");
  const listOrgs = await client.send(new ListOrganisationCommand({ count: 200 }));
  const matchingOrgs = listOrgs.data?.filter((o) => o.name?.startsWith(TEST_ORG_NAME)) ?? [];

  // Try to find an org with a usable cucumberws workspace (one that has dimensions)
  let foundUsableOrg = false;
  for (const org of matchingOrgs) {
    try {
      const listWs = await client.send(
        new ListWorkspaceCommand({ org_id: org.id!, count: 200 })
      );
      const ws = listWs.data?.find((w) => w.workspace_name === TEST_WORKSPACE);
      if (ws) {
        const dims = await client.send(
          new ListDimensionsCommand({ workspace_id: TEST_WORKSPACE, org_id: org.id!, all: true })
        );
        if ((dims.data?.length ?? 0) > 1) {
          sharedOrgId = org.id!;
          foundUsableOrg = true;
          break;
        }
      }
    } catch {
      continue;
    }
  }

  if (!foundUsableOrg) {
    // Use the first matching org or create a new one
    if (matchingOrgs.length > 0) {
      sharedOrgId = matchingOrgs[0].id ?? "";
    } else {
      const createResp = await client.send(
        new CreateOrganisationCommand({
          admin_email: "cucumber@test.com",
          name: TEST_ORG_NAME,
        })
      );
      sharedOrgId = createResp.id ?? "";
    }
  }

  // Setup workspace
  const listWs = await client.send(
    new ListWorkspaceCommand({ org_id: sharedOrgId, count: 200 })
  );
  const existingWs = listWs.data?.find(
    (w) => w.workspace_name === TEST_WORKSPACE
  );

  if (!existingWs) {
    await client.send(
      new CreateWorkspaceCommand({
        org_id: sharedOrgId,
        workspace_admin_email: "admin@example.com",
        workspace_name: TEST_WORKSPACE,
        workspace_status: WorkspaceStatus.ENABLED,
        allow_experiment_self_approval: true,
        auto_populate_control: false,
        enable_context_validation: true,
        enable_change_reason_validation: false,
      })
    );
  }

  // Ensure workspace settings are correct (in case workspace already existed)
  try {
    await client.send(
      new UpdateWorkspaceCommand({
        org_id: sharedOrgId,
        workspace_name: TEST_WORKSPACE,
        enable_change_reason_validation: false,
      })
    );
  } catch {
    // May not need updating
  }

  // Setup encryption
  try {
    await client.send(
      new MigrateWorkspaceSchemaCommand({
        org_id: sharedOrgId,
        workspace_name: TEST_WORKSPACE,
      })
    );
  } catch {
    // Migration may already be done
  }

  sharedWorkspaceId = TEST_WORKSPACE;
});

AfterAll(async function () {
  if (browser) {
    await browser.close();
  }
});

Before(async function (this: PlaywrightWorld) {
  // ── Shared org/workspace state ──────────────────────────────────
  this.orgId = sharedOrgId;
  this.workspaceId = sharedWorkspaceId;

  // ── Browser context/page setup ──────────────────────────────────
  this.browser = browser;
  this.context = await browser.newContext({
    viewport: { width: 1280, height: 720 },
    ignoreHTTPSErrors: true,
  });
  this.page = await this.context.newPage();

  // Reset state
  this.lastResponse = undefined;
  this.lastError = undefined;
  this.lastToastText = "";
});

After(async function (this: PlaywrightWorld, scenario) {
  // Take screenshot on failure for debugging
  if (scenario.result?.status === Status.FAILED) {
    const name = scenario.pickle.name.replace(/\s+/g, "-").toLowerCase();
    await this.page.screenshot({
      path: `reports/screenshots/${name}.png`,
      fullPage: true,
    });
  }

  // Cleanup browser context
  if (this.context) {
    await this.context.close();
  }

  // ── SDK cleanup of created resources ────────────────────────────

  // Cleanup experiment groups
  if (this.experimentGroupId) {
    try {
      await this.client.send(
        new DeleteExperimentGroupCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          id: this.experimentGroupId,
        })
      );
    } catch {
      // May have members or already deleted
    }
  }

  // Cleanup experiments
  for (const id of this.createdExperimentIds) {
    try {
      await this.client.send(
        new DiscardExperimentCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          id,
          change_reason: "Cucumber cleanup",
        })
      );
    } catch {
      // Already discarded or concluded
    }
  }

  // Cleanup contexts
  for (const id of this.createdContextIds) {
    try {
      await this.client.send(
        new DeleteContextCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          id,
        })
      );
    } catch {
      // May already be deleted
    }
  }

  // Cleanup configs
  for (const key of this.createdConfigs) {
    try {
      await this.client.send(
        new DeleteDefaultConfigCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          key,
        })
      );
    } catch {
      // May already be deleted
    }
  }

  // Cleanup functions
  for (const name of this.createdFunctions) {
    try {
      await this.client.send(
        new DeleteFunctionCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          function_name: name,
        })
      );
    } catch {
      // May already be deleted
    }
  }

  // Cleanup dimensions
  for (const dim of this.createdDimensions) {
    try {
      await this.client.send(
        new DeleteDimensionCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          dimension: dim,
        })
      );
    } catch {
      // May already be deleted
    }
  }

  // Cleanup variables
  for (const name of this.createdVariables) {
    try {
      await this.client.send(
        new DeleteVariableCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          name,
        })
      );
    } catch {
      // May already be deleted
    }
  }

  // Cleanup secrets
  for (const name of this.createdSecrets) {
    try {
      await this.client.send(
        new DeleteSecretCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          name,
        })
      );
    } catch {
      // May already be deleted
    }
  }

  // Cleanup type templates
  for (const name of this.createdTypeTemplates) {
    try {
      await this.client.send(
        new DeleteTypeTemplatesCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          type_name: name,
        })
      );
    } catch {
      // May already be deleted
    }
  }
});
