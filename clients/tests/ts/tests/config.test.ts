import {
    GetConfigCommand,
    GetConfigFastCommand,
    ListVersionsCommand
} from "@io.juspay/superposition-sdk";
import { superpositionClient } from "./env.ts";

describe("Config API - GetConfig and GetConfigFast", () => {
    test("GetConfig: should fetch configuration with context and version", async () => {
        const input = {
            workspace_id: "test",
            org_id: "localorg",
            prefix: "test-prefix",
            // version: 1,
            context: {}
        };
        const cmd = new GetConfigCommand(input);
        try {
            const out = await superpositionClient.send(cmd);
            console.log(out);
        } catch (e) {
            console.error(e["$response"]);
            throw e;
        }
    });

    // Enable when testing w/ redis.
    // test("GetConfigFast: should fetch configuration quickly", async () => {
    //     const input = {
    //         workspace_id: "test",
    //         org_id: "localorg"
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
            workspace_id: "test",
            org_id: "localorg",
            count: 10,
            page: 1
        };
        const cmd = new ListVersionsCommand(input);
        try {
            const out = await superpositionClient.send(cmd);
            console.log(out.data);
        } catch (e) {
            console.error(e["$response"]);
            throw e;
        }
    });
});
