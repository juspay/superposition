import { ListDimensionsCommand, CreateDimensionCommand, UpdateDefaultConfigCommand, UpdateDimensionCommand, DeleteDimensionCommand } from "@io.juspay/superposition-sdk";
import { ENV, superpositionClient } from "./env.ts";
import { describe, test } from "bun:test";

describe("Dimension API", () => {
    test("CreateDimension", async () => {
        const cmd = new CreateDimensionCommand({
            dimension: "dim1",
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
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
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
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
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
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
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
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
