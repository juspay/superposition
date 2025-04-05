import { SuperpositionClient, ListDimensionsCommand } from "../src";

describe("Dimension API", () => {
    test("ListDimension", async () => {
        const config = {
            endpoint: "http://127.0.0.1:8080",
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
