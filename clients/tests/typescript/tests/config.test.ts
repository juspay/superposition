import {
    GetConfigCommand,
    ListVersionsCommand,
    UpdateWorkspaceCommand,

} from "@io.juspay/superposition-sdk";
import { superpositionClient, ENV } from "../env.ts";
import { describe, test, expect } from "bun:test";

let configVersionId: string | null = null;

describe("Config API - GetConfig and GetConfigFast", () => {
    test("GetConfig: should fetch configuration with context and version", async () => {
        const input = {
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            prefix: "test-prefix",
            // version: 1,
            context: {},
        };
        const cmd = new GetConfigCommand(input);
        try {
            const out = await superpositionClient.send(cmd);
            configVersionId = out.version ?? null;
            console.log(out);
        } catch (e: any) {
            console.error(e["$response"]);
            throw e;
        }

        const input_c = {
            org_id: ENV.org_id,
            workspace_name: ENV.workspace_id,
            workspace_admin_email: "updated-admin@example.com",
            config_version: configVersionId,
        };
        console.log("chorg", ENV.org_id);
        console.log("orcong", configVersionId);
        const cmd_c = new UpdateWorkspaceCommand(input_c);
        console.log("chwork", cmd_c.input.workspace_name);
        const resp_c = await superpositionClient.send(cmd_c);

        expect(resp_c).toBeDefined();
        expect(resp_c.workspace_name).toBe(ENV.workspace_id);
        expect(resp_c.workspace_admin_email).toBe("updated-admin@example.com");
        expect(resp_c.config_version).toBeDefined();
        expect(resp_c.config_version?.toString()).toBeOneOf([undefined, configVersionId]);

        //getConfigandCheck
        const input_1 = {
            workspace_id: ENV.workspace_id,
            org_id:ENV.org_id,
            prefix: "test-prefix",
            // version: 1,
            context: {},
        };
        const new_cmd = new GetConfigCommand(input_1);
        try {
            const out = await superpositionClient.send(new_cmd);
            expect(out).toBeDefined();
            expect(out.version).toBeDefined();
            expect(out.version).toBeOneOf([undefined, configVersionId]);
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
            // if (out.data && out.data.length > 5) {
            //     configVersionId = out.data[5].id;
            // } else if (out.data && out.data.length > 0) {
            //     configVersionId = out.data[0].id;
            // }
        } catch (e: any) {
            console.error(e["$response"]);
            throw e;
        }


        
    });
});

