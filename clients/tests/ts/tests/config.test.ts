import { SuperpositionClient, GetConfigCommand } from "@io.juspay/superposition-sdk";
import { ENV } from "./env.test.ts";

describe("Config API", () => {
    test("GetConifg", async () => {
        const config = {
            endpoint: ENV.baseUrl,
            token: {
                token: "some-token",
            },
        };
        const client = new SuperpositionClient(config);
        const input = {
            workspace_id: "test",
            org_id: "localorg",
            prefix: "",
            context: {}
        };
        const cmd = new GetConfigCommand(input);
        try {
        const out = await client.send(cmd);
        console.log(out);
        } catch (e) {
            console.log(e["$response"])
            throw e
        }
    });
});
