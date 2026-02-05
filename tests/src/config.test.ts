import {
    GetConfigCommand,
    ListVersionsCommand,
    UpdateWorkspaceCommand,
    CreateDefaultConfigCommand,
    ListDefaultConfigsCommand,
    DeleteDefaultConfigCommand,
} from "@juspay/superposition-sdk";
import { superpositionClient, ENV } from "../env.ts";
import { describe, test, expect, beforeAll, afterAll } from "bun:test";

let configVersionId: string | undefined = undefined;
const testConfigKey = `test-config-${Math.random().toString(36).substring(7)}`;

beforeAll(async () => {
    // Check if config already exists
    const listCmd = new ListDefaultConfigsCommand({
        workspace_id: ENV.workspace_id,
        org_id: ENV.org_id,
        name: testConfigKey,
    });

    try {
        const existingConfigs = await superpositionClient.send(listCmd);

        if (!existingConfigs.data || existingConfigs.data.length === 0) {
            // Create default config if it doesn't exist
            const createCmd = new CreateDefaultConfigCommand({
                key: testConfigKey,
                value: { enabled: true, message: "test config" },
                schema: { type: "object" },
                description: "Test config for integration tests",
                change_reason: "Initial setup for tests",
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
            });

            await superpositionClient.send(createCmd);
            console.log(`Created default config: ${testConfigKey}`);
        } else {
            console.log(`Default config already exists: ${testConfigKey}`);
        }
    } catch (e: any) {
        console.error("Error in pre-test setup:", e);
        throw e;
    }
});

afterAll(async () => {
    try {
        const deleteCmd = new DeleteDefaultConfigCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            key: testConfigKey,
        });
        await superpositionClient.send(deleteCmd);
        console.log(`Deleted default config: ${testConfigKey}`);
    } catch (e: any) {
        console.error("Error deleting test config:", e);
    }
});

describe("Config API - GetConfig and GetConfigFast", () => {
    test("GetConfig: should fetch configuration with context and version", async () => {
        const cmd = new GetConfigCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            prefix: [testConfigKey],
            // version: 1,
        });
        try {
            const out = await superpositionClient.send(cmd);
            configVersionId = out.version ?? undefined;
            console.log(out);
        } catch (e: any) {
            console.error(e["$response"]);
            throw e;
        }

        const cmd_c = new UpdateWorkspaceCommand({
            org_id: ENV.org_id,
            workspace_name: ENV.workspace_id,
            workspace_admin_email: "admin@example.com",
            config_version: configVersionId,
            change_reason: "Setting config version for tests",
        });
        const resp_c = await superpositionClient.send(cmd_c);

        expect(resp_c).toBeDefined();
        expect(resp_c.workspace_name).toBe(ENV.workspace_id);
        expect(resp_c.workspace_admin_email).toBe("admin@example.com");
        expect(resp_c.config_version).toBeDefined();
        expect(resp_c.config_version?.toString()).toBe(
            configVersionId?.toString() ?? "",
        );

        //getConfigandCheck
        const input_1 = {
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            prefix: [testConfigKey],
            // version: 1,
            context: {},
        };
        const new_cmd = new GetConfigCommand(input_1);
        try {
            const out = await superpositionClient.send(new_cmd);
            expect(out).toBeDefined();
            expect(out.version).toBeDefined();
            expect(out.version?.toString()).toBe(
                configVersionId?.toString() ?? "",
            );
            console.log(out);
        } catch (e: any) {
            console.error(e["$response"]);
            throw e;
        }

        //unset update workspace
        const input_2 = {
            org_id: ENV.org_id,
            workspace_name: ENV.workspace_id,
            workspace_admin_email: "updated-admin@example.com",
            description: "Unset config version",
            config_version: "null",
            change_reason: "Unsetting config version for tests",
        };

        const cmd_2 = new UpdateWorkspaceCommand(input_2);
        const resp = await superpositionClient.send(cmd_2);

        expect(resp).toBeDefined();
        expect(resp.workspace_name).toBe(ENV.workspace_id);
        expect(resp.config_version).toBeUndefined();
    });

    // Enable when testing w/ redis.
    // test("GetConfigFast: should fetch configuration quickly", async () => {
    //     const input = {
    //         workspace_id: ENV.workspace_id,
    //         org_id: ENV.org_id
    //     };
    //     const cmd = new GetConfigFastCommand(input);
    //     try {
    //         const out = await superpositionClient.send(cmd);
    //         console.log(out);
    //     } catch (e) {
    //         console.error(e["$response"]);
    //         throw e;
    //     }
    // });
});

describe("Config API - ListVersions", () => {
    test("should list all configuration versions", async () => {
        const input = {
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            count: 10,
            page: 1,
        };
        const cmd = new ListVersionsCommand(input);
        try {
            const out = await superpositionClient.send(cmd);
            console.log(out.data);
        } catch (e: any) {
            console.error(e["$response"]);
            throw e;
        }
    });
});
