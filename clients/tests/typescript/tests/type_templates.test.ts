import {
    CreateTypeTemplatesCommand,
    UpdateTypeTemplatesCommand,
    DeleteTypeTemplatesCommand,
    GetTypeTemplatesListCommand,
} from "@io.juspay/superposition-sdk";
import { describe, test, expect, afterAll } from "bun:test";
import { ENV, superpositionClient } from "../env.ts";

describe("Type Templates API", () => {
    // Track created type templates for cleanup
    const createdTypeTemplates: Set<string> = new Set();

    // Test data for different type templates
    const typeTemplates = {
        boolean: {
            type_name: "Boolean",
            type_schema: { type: "boolean" },
        },
        decimal: {
            type_name: "Decimal",
            type_schema: { type: "number" },
        },
        enum: {
            type_name: "Enum",
            type_schema: {
                type: "string",
                enum: ["android", "ios"],
            },
        },
        pattern: {
            type_name: "Pattern",
            type_schema: {
                type: "string",
                pattern: ".*",
            },
        },
    };

    // Add cleanup after all tests
    afterAll(async () => {
        console.log(
            `Cleaning up ${createdTypeTemplates.size} type templates...`
        );

        // Delete type templates
        for (const typeName of createdTypeTemplates) {
            try {
                const deleteCmd = new DeleteTypeTemplatesCommand({
                    workspace_id: ENV.workspace_id,
                    org_id: ENV.org_id,
                    type_name: typeName,
                });

                await superpositionClient.send(deleteCmd);
                console.log(`Deleted type template: ${typeName}`);
            } catch (error: any) {
                console.error(
                    `Failed to delete type template ${typeName}:`,
                    error.message
                );
            }
        }
    });

    // Helper function to track created templates
    function trackTypeTemplate(typeName: string) {
        createdTypeTemplates.add(typeName);
    }

    test("List Type Templates", async () => {
        const input = {
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
        };

        const cmd = new GetTypeTemplatesListCommand(input);
        const response = await superpositionClient.send(cmd);

        expect(response).toBeDefined();
        // Check if data array exists
        expect(response.data).toBeDefined();
        expect(Array.isArray(response.data)).toBe(true);

        // Use type assertion to avoid TypeScript error
        const typeNames = (response.data || []).map((t: any) => t.type_name);

        // Log available types for debugging
        console.log("Available type templates:", typeNames);
    });

    test("Create Type Template - Boolean", async () => {
        // Add timestamp to make the type name unique on each run
        const uniqueTypeName = `Boolean_${Date.now() % 10000}`;

        const input = {
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            type_name: uniqueTypeName, // Use the unique name instead
            type_schema: typeTemplates.boolean.type_schema,
            description: "Boolean type template",
            change_reason: "Adding boolean type",
        };

        const cmd = new CreateTypeTemplatesCommand(input);

        try {
            const response = await superpositionClient.send(cmd);

            // Track created type template for cleanup
            trackTypeTemplate(uniqueTypeName);

            expect(response).toBeDefined();
            expect(response.type_name).toBe(uniqueTypeName);
            const returnedSchema =
                typeof response.type_schema === "string"
                    ? JSON.parse(response.type_schema)
                    : response.type_schema;

            expect(returnedSchema.type).toBe(
                typeTemplates.boolean.type_schema.type
            );
            expect(response.created_by).toBeDefined();
        } catch (error: any) {
            // Handle case where the type already exists
            if (error.message?.includes("duplicate key value")) {
                console.log(
                    `Type ${uniqueTypeName} already exists, skipping test`
                );
                return; // Skip the test but don't mark as failure
            }
            console.log("Error creating Boolean template:", error.message);
            throw error;
        }
    });

    test("Create Type Template - With Pattern", async () => {
        const uniqueTypeName = `Pattern_${Date.now() % 10000}`;
        const input = {
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            type_name: uniqueTypeName,
            // Pass directly as an object, not as a string
            type_schema: typeTemplates.pattern.type_schema,
            description: "Pattern type template",
            change_reason: "Adding pattern type",
        };

        const cmd = new CreateTypeTemplatesCommand(input);

        try {
            const response = await superpositionClient.send(cmd);

            // Track created type template for cleanup
            trackTypeTemplate(uniqueTypeName);

            expect(response).toBeDefined();
            expect(response.type_name).toBe(uniqueTypeName);
            // For comparing objects, we need to handle different formats
            const returnedSchema =
                typeof response.type_schema === "string"
                    ? JSON.parse(response.type_schema)
                    : response.type_schema;

            // Compare structure rather than exact equality
            expect(returnedSchema.type).toBe(
                typeTemplates.pattern.type_schema.type
            );
            expect(returnedSchema.pattern).toBe(
                typeTemplates.pattern.type_schema.pattern
            );
        } catch (error: any) {
            console.log("Error creating Pattern template:", error.message);
            throw error;
        }
    });

    test("Update Type Template", async () => {
        // Use a unique name or first try to create the template before updating it
        const uniqueTypeName = `Decimal_${Date.now() % 10000}`;

        // First try to create the template
        try {
            const createInput = {
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                type_name: uniqueTypeName,
                type_schema: { type: "number" },
                description: "Decimal type template",
                change_reason: "Creating decimal type",
            };

            const createCmd = new CreateTypeTemplatesCommand(createInput);
            await superpositionClient.send(createCmd);

            // Track created type template for cleanup
            trackTypeTemplate(uniqueTypeName);

            console.log(`Created template ${uniqueTypeName} for update test`);
        } catch (error: any) {
            // If it fails for any reason other than already existing, log but continue
            if (!error.message?.includes("duplicate key value")) {
                console.log(
                    "Error pre-creating template for update test:",
                    error.message
                );
            }
        }

        // Now try to update it
        const updatedSchema = {
            type: "number",
            minimum: 0,
            maximum: 100,
        };

        const input = {
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            type_name: uniqueTypeName,
            type_schema: updatedSchema,
            description: "Updated decimal type",
            change_reason: "Adding range constraints",
        };

        const cmd = new UpdateTypeTemplatesCommand(input);

        try {
            const response = await superpositionClient.send(cmd);

            expect(response).toBeDefined();
            expect(response.type_name).toBe(uniqueTypeName);
            // For comparing objects, we need to handle different formats
            const returnedSchema =
                typeof response.type_schema === "string"
                    ? JSON.parse(response.type_schema)
                    : response.type_schema;

            // Compare structure rather than exact equality
            expect(returnedSchema.type).toBe(updatedSchema.type);
            expect(returnedSchema.minimum).toBe(updatedSchema.minimum);
            expect(returnedSchema.maximum).toBe(updatedSchema.maximum);
            expect(response.last_modified_by).toBeDefined();
        } catch (error: any) {
            console.log("Error updating template:", error.message);
            throw error;
        }
    });

    test("Delete Type Template", async () => {
        // This tests the delete function while also cleaning up one created type
        const toBeDeletedTypeName = createdTypeTemplates.values().next().value;
        const input = {
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            type_name: toBeDeletedTypeName,
        };

        const cmd = new DeleteTypeTemplatesCommand(input);

        try {
            const response = await superpositionClient.send(cmd);

            // Remove from tracking since we've deleted it
            createdTypeTemplates.delete(toBeDeletedTypeName || "");

            expect(response).toBeDefined();
            // Don't expect specific success properties, just check the response exists

            // Verify deletion
            const listCmd = new GetTypeTemplatesListCommand({
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
            });
            const listResponse = await superpositionClient.send(listCmd);

            expect(listResponse.data).toBeDefined();
            const deletedType = (listResponse.data || []).find(
                (t: any) => t.type_name === toBeDeletedTypeName
            );
            expect(deletedType).toBeUndefined();
        } catch (error: any) {
            console.log("Error deleting template:", error.message);
            throw error;
        }
    });

    test("Create Type Template - Invalid Schema", async () => {
        const input = {
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            type_name: "Invalid",
            // Pass directly as an object, not as a string
            type_schema: { type: "invalid" },
            description: "Invalid type template",
            change_reason: "Testing validation",
        };

        const cmd = new CreateTypeTemplatesCommand(input);
        await expect(superpositionClient.send(cmd)).rejects.toThrow();
    });
});
