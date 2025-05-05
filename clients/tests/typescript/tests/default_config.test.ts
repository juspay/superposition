import {
    CreateDefaultConfigCommand,
    UpdateDefaultConfigCommand,
    CreateFunctionCommand,
    DeleteFunctionCommand,
    DeleteDefaultConfigCommand,
    FunctionTypes,
    PublishCommand,
} from "@io.juspay/superposition-sdk";
import { superpositionClient, ENV } from "../env.ts";
import type { UpdateDefaultConfigCommandOutput } from "@io.juspay/superposition-sdk";

import { describe, beforeAll, afterAll, test, expect } from "bun:test";

describe("Default Config API Integration Tests", () => {
    // Track created resources for cleanup
    let createdFunctions: string[] = [];
    let createdConfigs: string[] = [];

    beforeAll(async () => {
        // await createWorkspace(client);
        await createFunctions();
    });

    // Cleanup after tests complete
    afterAll(async () => {
        console.log("Cleaning up test resources...");

        // Delete configurations
        for (const key of createdConfigs) {
            try {
                await superpositionClient.send(
                    new DeleteDefaultConfigCommand({
                        workspace_id: ENV.workspace_id,
                        org_id: ENV.org_id,
                        key,
                    })
                );
                console.log(`Deleted config: ${key}`);
            } catch (error) {
                console.error(`Failed to delete config ${key}:`, error);
            }
        }

        // Delete functions
        for (const functionName of createdFunctions) {
            try {
                await superpositionClient.send(
                    new DeleteFunctionCommand({
                        workspace_id: ENV.workspace_id,
                        org_id: ENV.org_id,
                        function_name: functionName,
                    })
                );
                console.log(`Deleted function: ${functionName}`);
            } catch (error) {
                console.error(
                    `Failed to delete function ${functionName}:`,
                    error
                );
            }
        }
    });

    async function createFunctions() {
        const validateCode1 = `
            async function validate(key, value) {
                return false;
            }
        `;

        const validateCode2 = `
            async function validate(key, value) {
                return true;
            }
        `;

        console.log("Creating function false_validation");
        await superpositionClient.send(
            new CreateFunctionCommand({
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                function_name: "false_validation",
                function: validateCode1,
                description: "Test validate function",
                change_reason: "Initial creation",
                runtime_version: "1",
                function_type: FunctionTypes.Validation,
            })
        );
        // Track created function
        createdFunctions.push("false_validation");

        console.log("Creating function true_function");
        await superpositionClient.send(
            new CreateFunctionCommand({
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                function_name: "true_function",
                function: validateCode2,
                description: "Test validate function",
                change_reason: "Initial creation",
                runtime_version: "1",
                function_type: FunctionTypes.Validation,
            })
        );
        // Track created function
        createdFunctions.push("true_function");

        console.log("Publishing function false_validation");
        await superpositionClient.send(
            new PublishCommand({
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                function_name: "false_validation",
            })
        );

        console.log("Publishing function true_function");
        await superpositionClient.send(
            new PublishCommand({
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                function_name: "true_function",
            })
        );
    }

    describe("Create Default Config", () => {
        // add async in front of all closure functions
        test("should successfully create a valid default config", async () => {
            const input = {
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,

                key: "test-key",
                schema: {
                    type: "object",
                    properties: {
                        name: { type: "string" },
                        age: { type: "number", minimum: 0 },
                    },
                    required: ["name"],
                },
                value: {
                    name: "Test User",
                    age: 30,
                },
                description: "Test configuration",
                change_reason: "Initial creation for testing",
            };
            const cmd = new CreateDefaultConfigCommand(input);
            await superpositionClient.send(cmd);
            // Track created config
            createdConfigs.push("test-key");
        });

        test("should fail when schema is invalid", async () => {
            const input = {
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,

                key: "test-key-2",
                schema: {
                    type: "invalid-type",
                },
                value: {
                    name: "Test User",
                    age: 30,
                },
                description: "Test configuration",
                change_reason: "Initial creation for testing",
            };

            const cmd = new CreateDefaultConfigCommand(input);
            expect(superpositionClient.send(cmd)).rejects.toThrow(
                "Invalid JSON schema (failed to compile)"
            );
        });

        test("should fail when schema is empty", async () => {
            const input = {
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,

                key: "test-key-2",
                schema: {},
                value: {
                    name: "Test User",
                    age: 30,
                },
                description: "Test configuration",
                change_reason: "Initial creation for testing",
            };
            const cmd = new CreateDefaultConfigCommand(input);

            expect(superpositionClient.send(cmd)).rejects.toThrow(
                "Schema cannot be empty."
            );
        });

        test("should fail when value doesn't match schema", async () => {
            const input = {
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,

                key: "test-key-2",
                schema: {
                    type: "object",
                    properties: {
                        name: { type: "string" },
                        age: { type: "number", minimum: 0 },
                    },
                    required: ["name"],
                },
                value: {
                    name: "Test User",
                    age: -5, // Invalid age
                },
                description: "Test configuration",
                change_reason: "Initial creation for testing",
            };
            const cmd = new CreateDefaultConfigCommand(input);

            expect(superpositionClient.send(cmd)).rejects.toThrow(
                "Schema validation failed: value is too small, minimum is 0"
            );
        });

        test("should fail when function validation fails", async () => {
            const input = {
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,

                key: "test-key-2",
                schema: {
                    type: "object",
                    properties: {
                        name: { type: "string" },
                    },
                },
                value: { name: "Invalid Value" },
                description: "Test configuration",
                function_name: "false_validation",
                change_reason: "Test function validation",
            };

            const cmd = new CreateDefaultConfigCommand(input);
            expect(superpositionClient.send(cmd)).rejects.toThrow(
                "Function validation failed for test-key-2 with error Error: The function did not return a value that was expected. Check the return type and logic of the function\n. "
            );
        });

        test("should fail when function does not exist", async () => {
            const input = {
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,

                key: "test-key-2",
                schema: {
                    type: "object",
                    properties: {
                        name: { type: "string" },
                    },
                },
                value: { name: "Invalid Value" },
                description: "Test configuration",
                function_name: "non_existent_function",
                change_reason: "Test function validation",
            };
            const cmd = new CreateDefaultConfigCommand(input);
            expect(superpositionClient.send(cmd)).rejects.toThrow(
                "Function non_existent_function doesn't exist."
            );
        });
    });

    describe("Update Default Config", () => {
        test("should successfully update an existing default config", async () => {
            const input = {
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,

                key: "test-key",
                value: {
                    name: "Updated User",
                    age: 35,
                },
                description: "Updated configuration",
                change_reason: "Update for testing",
            };
            const cmd = new UpdateDefaultConfigCommand(input);

            const response: UpdateDefaultConfigCommandOutput =
                await superpositionClient.send(cmd);

            expect(response.value).toEqual({
                name: "Updated User",
                age: 35,
            });
            expect(response.schema).toEqual({
                type: "object",
                properties: {
                    name: { type: "string" },
                    age: { type: "number", minimum: 0 },
                },
                required: ["name"],
            });
            expect(response.description).toBe("Updated configuration");
            expect(response.change_reason).toBe("Update for testing");
            expect(response.$metadata.httpStatusCode).toBe(200);
        });

        test("should successfully update schema and value together", async () => {
            const input = {
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,

                key: "test-key",
                schema: {
                    type: "object",
                    properties: {
                        name: { type: "string" },
                        age: { type: "number" },
                        email: { type: "string", format: "email" },
                    },
                    required: ["name", "email"],
                },
                value: {
                    name: "Updated Name",
                    age: 35,
                    email: "updated@example.com",
                },
                change_reason: "Updating schema and value",
            };
            const cmd = new UpdateDefaultConfigCommand(input);
            const response = await superpositionClient.send(cmd);

            expect(response.value).toEqual({
                name: "Updated Name",
                age: 35,
                email: "updated@example.com",
            });
            expect(response.schema).toEqual({
                type: "object",
                properties: {
                    name: { type: "string" },
                    age: { type: "number" },
                    email: { type: "string", format: "email" },
                },
                required: ["name", "email"],
            });
            expect(response.change_reason).toBe("Updating schema and value");
            expect(response.description).toBe("Updated configuration");
            expect(response.$metadata.httpStatusCode).toBe(200);
        });

        test("should fail when updating non-existent key", async () => {
            const input = {
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,

                key: "non_existent_key",
                value: {
                    name: "Updated User",
                    age: 35,
                },
                description: "Updated configuration",
                change_reason: "Update for testing",
            };
            const cmd = new UpdateDefaultConfigCommand(input);
            expect(superpositionClient.send(cmd)).rejects.toThrow(
                "No record found for non_existent_key. Use create endpoint instead."
            );
        });

        test("should fail when updated schema is invalid", async () => {
            const input = {
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,

                key: "test-key",
                schema: {
                    type: "invalid-type",
                },
                change_reason: "Update for testing",
            };
            const cmd = new UpdateDefaultConfigCommand(input);
            expect(superpositionClient.send(cmd)).rejects.toThrow(
                "Invalid JSON schema."
            );
        });

        test("should fail when value does not match updated schema", async () => {
            const input = {
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,

                key: "test-key",
                schema: {
                    type: "object",
                    properties: {
                        name: { type: "string" },
                        age: { type: "number", minimum: 18 },
                        email: { type: "string", format: "email" },
                    },
                    required: ["name", "email"],
                },
                value: {
                    name: "Updated Name",
                    age: 20,
                    // Missing required email field
                },
                change_reason: "Update for testing",
            };
            const cmd = new UpdateDefaultConfigCommand(input);
            expect(superpositionClient.send(cmd)).rejects.toThrow(
                'Schema validation failed: required property `"email"` is missing'
            );
        });

        test("should update function_name", async () => {
            const input = {
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,

                key: "test-key",
                function_name: "true_function",
                change_reason:
                    "Update function to new_function_name for testing",
            };
            const cmd = new UpdateDefaultConfigCommand(input);
            const response = await superpositionClient.send(cmd);

            // write asserts
            expect(response.$metadata.httpStatusCode).toBe(200);
            expect(response.value).toEqual({
                name: "Updated Name",
                age: 35,
                email: "updated@example.com",
            });
            expect(response.schema).toEqual({
                type: "object",
                properties: {
                    name: { type: "string" },
                    age: { type: "number" },
                    email: { type: "string", format: "email" },
                },
                required: ["name", "email"],
            });
            expect(response.description).toBe("Updated configuration");
            expect(response.change_reason).toBe(
                "Update function to new_function_name for testing"
            );
            expect(response.function_name).toBe("true_function");
        });
    });
});
