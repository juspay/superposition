import { SuperpositionClient, ListDimensionsCommand } from "@io.juspay/superposition-sdk";
import { ENV } from "./env.test.ts";

describe("Dimension API", () => {
    test("ListDimension", async () => {
        const config = {
            endpoint: ENV.baseUrl,
            token: {
                token: "some-token",
            },
        };
        const client = new SuperpositionClient(config);
        const input = {
            count: 10,
            page: 1,
            workspace_id: "test",
            org_id: "localorg",
        };
        const cmd = new ListDimensionsCommand(input);
        const out = await client.send(cmd);
        console.log(out);
    });
});
