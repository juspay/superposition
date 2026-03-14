import { Before, After, BeforeAll, AfterAll } from "@cucumber/cucumber";
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
import { SuperpositionWorld } from "./world.ts";

const TEST_ORG_NAME = "cucumberorg";
const TEST_WORKSPACE = "cucumberws";

// Shared state across scenarios (org/workspace persist for the run)
let sharedOrgId: string = "";
let sharedWorkspaceId: string = "";

BeforeAll(async function () {
  // Create a temporary client for setup
  const { SuperpositionClient } = await import("@juspay/superposition-sdk");
  const client = new SuperpositionClient({
    endpoint: process.env.SUPERPOSITION_BASE_URL || "http://127.0.0.1:8080",
    token: { token: process.env.SUPERPOSITION_TOKEN || "some-token" },
  });

  // Setup org
  const listOrgs = await client.send(new ListOrganisationCommand({}));
  const existingOrg = listOrgs.data?.find((o) => o.name?.startsWith(TEST_ORG_NAME));

  if (existingOrg) {
    sharedOrgId = existingOrg.id ?? "";
  } else {
    const createResp = await client.send(
      new CreateOrganisationCommand({
        admin_email: "cucumber@test.com",
        name: TEST_ORG_NAME,
      })
    );
    sharedOrgId = createResp.id ?? "";
  }

  // Setup workspace
  const listWs = await client.send(
    new ListWorkspaceCommand({ org_id: sharedOrgId })
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
        enable_change_reason_validation: true,
      })
    );
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

Before(function (this: SuperpositionWorld) {
  this.orgId = sharedOrgId;
  this.workspaceId = sharedWorkspaceId;
  this.lastResponse = undefined;
  this.lastError = undefined;
});

After(async function (this: SuperpositionWorld) {
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
