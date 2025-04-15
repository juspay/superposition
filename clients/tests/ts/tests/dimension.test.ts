import { ListDimensionsCommand, CreateDimensionCommand, UpdateDefaultConfigCommand, UpdateDimensionCommand, DeleteDimensionCommand } from "@io.juspay/superposition-sdk";
import { ENV, superpositionClient } from "./env.ts";

describe("Dimension API", () => {
    test("CreateDimension", async () => {
        const cmd = new CreateDimensionCommand({
            dimension: "dim1",
            workspace_id: "test",
            org_id: "localorg",
            schema: {
                type: "string",
            },
            position: 1,
            // WHY TF DO WE NEED A REASON HERE?
            change_reason: "test create",
            description: "test description",
        });
        try {
            const out = await superpositionClient.send(cmd);
            console.log(out);
        } catch (e) {
            console.log(e["$response"])
            throw e
        }
    });

    test("ListDimension", async () => {
        const cmd = new ListDimensionsCommand({
            workspace_id: "test",
            org_id: "localorg",
            count: 1,
            page: 1,
        });
        try {
            const out = await superpositionClient.send(cmd);
            console.log(out);
        } catch (e) {
            console.log(e["$response"])
            throw e
        }
    });

    test("UpdateDimension", async () => {
        const cmd = new UpdateDimensionCommand({
            dimension: "dim1",
            workspace_id: "test",
            org_id: "localorg",
            schema: {
                type: "string",
            },
            description: "test description",
            change_reason: "test update",
        });
        try {
            const out = await superpositionClient.send(cmd);
            console.log(out);
        } catch (e) {
            console.log(e["$response"])
            throw e
        }
    });

    test("DeleteDimension", async () => {
        const cmd = new DeleteDimensionCommand({
            dimension: "dim1",
            workspace_id: "test",
            org_id: "localorg",
        });
        try {
            const out = await superpositionClient.send(cmd);
            console.log(out);
        } catch (e) {
            console.log(e["$response"])
            throw e
        }
    });
});
