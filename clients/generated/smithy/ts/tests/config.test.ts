import { SuperpositionClient, GetConfigCommand } from "../src";

describe("Config API", () => {
    test("GetConifg", async () => {
        const config = {
            endpoint: "http://127.0.0.1:8080",
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
