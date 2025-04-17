import {
    CreateOrganisationCommand,
    CreateWorkspaceCommand,
    ListOrganisationCommand,
    ListWorkspaceCommand,
    SuperpositionClient,
    WorkspaceStatus,
} from "@io.juspay/superposition-sdk";
import { beforeAll } from "bun:test";

export let ENV: any = {
    baseUrl: "http://127.0.0.1:8080",
    org_id: undefined,
    workspace_id: undefined,
};

const config: any = {
    endpoint: ENV.baseUrl,
    token: {
        token: "some-token",
    },
};

export const superpositionClient: SuperpositionClient = new SuperpositionClient(
    config
);
