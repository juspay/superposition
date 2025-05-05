import {
    GetConfigCommand,
    GetConfigFastCommand,
    ListVersionsCommand,
} from "@io.juspay/superposition-sdk";
import { superpositionClient, ENV } from "../env.ts";
import { describe, test } from "bun:test";

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
            console.log(out);
        } catch (e: any) {
            console.error(e["$response"]);
            throw e;
        }
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
