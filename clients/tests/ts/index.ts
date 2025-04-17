import {
    CreateOrganisationCommand,
    CreateWorkspaceCommand,
    ListOrganisationCommand,
    ListWorkspaceCommand,
    WorkspaceStatus,
} from "@io.juspay/superposition-sdk";
import { ENV, superpositionClient } from "./env";

const TEST_ORG_NAME = "testorg";
const TEST_WORKSPACE = "testworkspace";

async function setupWorkspace() {
    const listWorkspaceCommand = new ListWorkspaceCommand({
        org_id: ENV.org_id,
    });
    const response = await superpositionClient.send(listWorkspaceCommand);
    const workspaces = response.data;
    const testWorkspace = workspaces?.find(
        (workspace) => workspace.workspace_name === TEST_WORKSPACE
    );

    if (!testWorkspace) {
        const createWorkspaceCommand = new CreateWorkspaceCommand({
            org_id: ENV.org_id,
            workspace_admin_email: "admin@example.com",
            workspace_name: TEST_WORKSPACE,
            workspace_status: WorkspaceStatus.ENABLED,
        });

        console.info(`Create test workspace as ${TEST_ORG_NAME}`);
        await superpositionClient.send(createWorkspaceCommand);
    } else {
        console.info(
            `Skipping test workspace creation. Found exisiting ${TEST_WORKSPACE}`
        );
    }

    ENV.workspace_id = TEST_WORKSPACE;
}

async function setupOrg() {
    const listCommand = new ListOrganisationCommand({});
    const response = await superpositionClient.send(listCommand);
    const orgs = response.data;
    const testOrg = orgs?.find(
        (org) => org.name?.startsWith(TEST_ORG_NAME) || false
    );
    if (!testOrg) {
        const createCommand = new CreateOrganisationCommand({
            admin_email: "test@gmail.com",
            name: TEST_ORG_NAME,
        });
        console.info(`Create test org as ${TEST_ORG_NAME}`);
        const response = await superpositionClient.send(createCommand);
        ENV.org_id = response.id;
    } else {
        ENV.org_id = testOrg?.id;
        console.info(
            `Skipping test org creation. Found exisiting ${TEST_ORG_NAME}`
        );
    }
}

await setupOrg();
await setupWorkspace();
