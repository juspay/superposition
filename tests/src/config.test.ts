import {
    GetConfigCommand,
    GetConfigJsonCommand,
    GetConfigTomlCommand,
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
    let lastModified: Date | undefined = undefined;
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
            config_version: configVersionId,
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
            lastModified = out.last_modified;
        } catch (e: any) {
            console.error(e["$response"]);
            throw e;
        }

        //unset update workspace
        const input_2 = {
            org_id: ENV.org_id,
            workspace_name: ENV.workspace_id,
            description: "Unset config version",
            config_version: "null",
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

    test("GetConfig: should return 304 Not Modified if config version is unchanged", async () => {
        if (!lastModified) {
            console.warn("Last modified timestamp not set. Skipping test.");
            return;
        }

        console.log(
            `Testing Not Modified with last modified timestamp: ${lastModified.toISOString()}`,
        );
        const input = {
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            prefix: [testConfigKey],
            if_modified_since: lastModified,
        };
        const cmd = new GetConfigCommand(input);
        try {
            await superpositionClient.send(cmd);
            throw new Error(
                "Expected Not Modified error, but request succeeded",
            );
        } catch (e: any) {
            expect(e).toBeDefined();
            expect(e["$response"]).toBeDefined();
            expect(e["$response"].statusCode).toBe(304);
            console.log("Received expected Not Modified response");
        }
    });
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

describe("Config API - get config file formats", () => {
    let tomlLastModified: Date | undefined = undefined;
    let jsonLastModified: Date | undefined = undefined;
    test("should fetch config in TOML format", async () => {
        try {
            const input = {
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
            };
            const cmd = new GetConfigTomlCommand(input);
            const out = await superpositionClient.send(cmd);
            // verify TOML content
            expect(out.toml_config).toBeDefined();
            expect(out.toml_config).toContain("enabled = true");
            expect(out.toml_config).toContain('message = "test config"');

            tomlLastModified = out.last_modified;
        } catch (e: any) {
            console.error(e["$response"]);
            throw e;
        }
    });

    test("should fetch config in JSON format", async () => {
        try {
            const input = {
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
            };
            const cmd = new GetConfigJsonCommand(input);
            const out = await superpositionClient.send(cmd);
            // verify JSON content
            expect(out.json_config).toBeDefined();
            jsonLastModified = out.last_modified;
        } catch (e: any) {
            console.error(e["$response"]);
            throw e;
        }
    });

    test("should return 304 Not Modified for TOML if config is unchanged", async () => {
        if (!tomlLastModified) {
            console.warn(
                "TOML last modified timestamp not set. Skipping test.",
            );
            return;
        }

        const input = {
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            if_modified_since: tomlLastModified,
        };
        const cmd = new GetConfigTomlCommand(input);
        try {
            await superpositionClient.send(cmd);
            throw new Error(
                "Expected Not Modified error, but request succeeded",
            );
        } catch (e: any) {
            expect(e).toBeDefined();
            expect(e["$response"]).toBeDefined();
            expect(e["$response"].statusCode).toBe(304);
            console.log("Received expected Not Modified response for TOML");
        }
    });

    test("should return 304 Not Modified for JSON if config is unchanged", async () => {
        if (!jsonLastModified) {
            console.warn(
                "JSON last modified timestamp not set. Skipping test.",
            );
            return;
        }

        const input = {
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            if_modified_since: jsonLastModified,
        };
        const cmd = new GetConfigJsonCommand(input);
        try {
            await superpositionClient.send(cmd);
            throw new Error(
                "Expected Not Modified error, but request succeeded",
            );
        } catch (e: any) {
            expect(e).toBeDefined();
            expect(e["$response"]).toBeDefined();
            expect(e["$response"].statusCode).toBe(304);
            console.log("Received expected Not Modified response for JSON");
        }
    });
});
