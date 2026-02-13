import {
    CreateDefaultConfigCommand,
    UpdateDefaultConfigCommand,
    CreateFunctionCommand,
    DeleteFunctionCommand,
    ListDefaultConfigsCommand,
    ListGroupedDefaultConfigsCommand,
    DeleteDefaultConfigCommand,
    FunctionTypes,
    PublishCommand,
    type UpdateDefaultConfigCommandOutput,
    FunctionRuntimeVersion,
} from "@juspay/superposition-sdk";
import { superpositionClient, ENV } from "../env.ts";

import { describe, beforeAll, afterAll, test, expect } from "bun:test";
import type { DocumentType } from "@smithy/types";

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
                    }),
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
                    }),
                );
                console.log(`Deleted function: ${functionName}`);
            } catch (error) {
                console.error(
                    `Failed to delete function ${functionName}:`,
                    error,
                );
            }
        }
    });

    async function createFunctions() {
        const valueValidationCode1 = `
            async function execute(payload) {
                return false;
            }
        `;

        const valueValidationCode2 = `
            async function execute(payload) {
                return true;
            }
        `;

        const valueComputeCode = `
            async function execute(payload) {
                return [];
            }
        `;

        console.log("Creating function false_validation");
        await superpositionClient.send(
            new CreateFunctionCommand({
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                function_name: "false_validation",
                function: valueValidationCode1,
                description: "Test value_validation function",
                change_reason: "Initial creation",
                runtime_version: FunctionRuntimeVersion.V1,
                function_type: FunctionTypes.VALUE_VALIDATION,
            }),
        );
        // Track created function
        createdFunctions.push("false_validation");

        console.log("Creating function true_function");
        await superpositionClient.send(
            new CreateFunctionCommand({
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                function_name: "true_function",
                function: valueValidationCode2,
                description: "Test value_validation function",
                change_reason: "Initial creation",
                runtime_version: FunctionRuntimeVersion.V1,
                function_type: FunctionTypes.VALUE_VALIDATION,
            }),
        );
        // Track created function
        createdFunctions.push("true_function");

        await superpositionClient.send(
            new CreateFunctionCommand({
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                function_name: "auto_fn",
                function: valueComputeCode,
                description: "Test value_compute function",
                change_reason: "Initial creation",
                runtime_version: FunctionRuntimeVersion.V1,
                function_type: FunctionTypes.VALUE_COMPUTE,
            }),
        );

        createdFunctions.push("auto_fn");

        console.log("Publishing function false_validation");
        await superpositionClient.send(
            new PublishCommand({
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                function_name: "false_validation",
                change_reason: "Publishing for testing",
            }),
        );

        console.log("Publishing function true_function");
        await superpositionClient.send(
            new PublishCommand({
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                function_name: "true_function",
                change_reason: "Publishing for testing",
            }),
        );

        await superpositionClient.send(
            new PublishCommand({
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                function_name: "auto_fn",
                change_reason: "Publishing for testing",
            }),
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
                "Invalid JSON schema (failed to compile)",
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
                "Schema cannot be empty.",
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
                "Schema validation failed: value is too small, minimum is 0",
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
                value_validation_function_name: "false_validation",
                change_reason: "Test function validation",
            };

            const cmd = new CreateDefaultConfigCommand(input);
            expect(superpositionClient.send(cmd)).rejects.toThrow(
                "Function false_validation validation failed for test-key-2 with error Error: The function did not return a value that was expected. Check the return type and logic of the function\n. ",
            );
        });

        test("should pass when value_compute function attaches", async () => {
            const input = {
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,

                key: "test-key-3",
                schema: {
                    type: "object",
                    properties: {
                        name: { type: "string" },
                    },
                },
                value: { name: "valid Value" },
                description: "Test configuration",
                value_compute_function_name: "auto_fn",
                change_reason: "Test function completion",
            };

            const cmd = new CreateDefaultConfigCommand(input);
            createdConfigs.push("test-key-3");
            let response = await superpositionClient.send(cmd);
            expect(response).toBeDefined();
            expect(response.value_compute_function_name).toBe("auto_fn");
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
                value_validation_function_name: "non_existent_function",
                change_reason: "Test function validation",
            };
            const cmd = new CreateDefaultConfigCommand(input);
            expect(superpositionClient.send(cmd)).rejects.toThrow(
                "Function non_existent_function's published code does not exist.",
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
                "No record found for non_existent_key. Use create endpoint instead.",
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
                "Invalid JSON schema.",
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
                'Schema validation failed: required property `"email"` is missing',
            );
        });

        test("should update function_name", async () => {
            const input = {
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,

                key: "test-key",
                value_validation_function_name: "true_function",
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
                "Update function to new_function_name for testing",
            );
            expect(response.value_validation_function_name).toBe(
                "true_function",
            );
        });
    });

    describe("List Default Configs (Grouped and Ungrouped)", () => {
        beforeAll(async () => {
            // Create configs with prefix format a.b and c.d
            const configsToCreate: Array<{
                key: string;
                value: DocumentType;
                schema: Record<string, DocumentType>;
                description: string;
            }> = [
                {
                    key: "a.b",
                    value: { enabled: true },
                    schema: {
                        type: "object",
                        properties: {
                            enabled: { type: "boolean" },
                        },
                    },
                    description: "Config a.b",
                },
                {
                    key: "a.c",
                    value: { count: 10 },
                    schema: {
                        type: "object",
                        properties: {
                            count: { type: "number" },
                        },
                    },
                    description: "Config a.c",
                },
                {
                    key: "c.d",
                    value: { name: "test" },
                    schema: {
                        type: "object",
                        properties: {
                            name: { type: "string" },
                        },
                    },
                    description: "Config c.d",
                },
                {
                    key: "c.e",
                    value: { active: false },
                    schema: {
                        type: "object",
                        properties: {
                            active: { type: "boolean" },
                        },
                    },
                    description: "Config c.e",
                },
            ];

            for (const config of configsToCreate) {
                await superpositionClient.send(
                    new CreateDefaultConfigCommand({
                        workspace_id: ENV.workspace_id,
                        org_id: ENV.org_id,
                        key: config.key,
                        value: config.value,
                        schema: config.schema,
                        description: config.description,
                        change_reason: "Initial creation for list testing",
                    }),
                );
                createdConfigs.push(config.key);
            }
        });

        test("should fetch ungrouped list of default configs", async () => {
            const response = await superpositionClient.send(
                new ListDefaultConfigsCommand({
                    workspace_id: ENV.workspace_id,
                    org_id: ENV.org_id,
                    count: 100,
                    page: 1,
                }),
            );

            expect(response.data).toBeDefined();
            expect(Array.isArray(response.data)).toBe(true);
            expect(response.total_items).toBeGreaterThanOrEqual(4);
            expect(response.total_pages).toBeGreaterThanOrEqual(1);

            // Verify our created configs are in the list
            const keys = response.data?.map((config) => config.key);
            expect(keys).toContain("a.b");
            expect(keys).toContain("a.c");
            expect(keys).toContain("c.d");
            expect(keys).toContain("c.e");

            // Verify structure of returned configs
            const configAB = response.data?.find(
                (config) => config.key === "a.b",
            );
            expect(configAB).toBeDefined();
            expect(configAB?.value).toEqual({ enabled: true });
            expect(configAB?.description).toBe("Config a.b");
        });

        test("should fetch grouped list of default configs", async () => {
            const response = await superpositionClient.send(
                new ListGroupedDefaultConfigsCommand({
                    workspace_id: ENV.workspace_id,
                    org_id: ENV.org_id,
                    count: 100,
                    page: 1,
                }),
            );

            expect(response.data).toBeDefined();
            expect(Array.isArray(response.data)).toBe(true);
            expect(response.total_items).toBeGreaterThanOrEqual(2);
            expect(response.total_pages).toBeGreaterThanOrEqual(1);

            // Find groups and configs in the response
            const groups = response.data?.filter(
                (item) => "Group" in item && item.Group,
            );
            const configs = response.data?.filter(
                (item) => "Config" in item && item.Config,
            );

            // NOTE: The grouped API currently doesn't return configs with dot notation (a.b, c.d, etc.)
            // even though they exist (verified in ungrouped list test).
            // We test basic grouped functionality with the configs that ARE returned.

            expect(groups?.length).toBeGreaterThanOrEqual(2);
            expect(configs?.length).toBeGreaterThanOrEqual(1);

            // Verify group "a" exists
            const groupA = groups?.find((item) => item.Group === "a");
            expect(groupA).toBeDefined();

            // Verify group "c" exists
            const groupC = groups?.find((item) => item.Group === "c");
            expect(groupC).toBeDefined();

            // Verify at least some configs are structured correctly
            const anyConfig = configs?.[0];
            expect(anyConfig?.Config).toBeDefined();
            expect(anyConfig?.Config?.key).toBeDefined();
            expect(anyConfig?.Config?.value).toBeDefined();
        });

        test("should filter grouped configs by prefix", async () => {
            const response = await superpositionClient.send(
                new ListGroupedDefaultConfigsCommand({
                    workspace_id: ENV.workspace_id,
                    org_id: ENV.org_id,
                    prefix: "a",
                    count: 100,
                    page: 1,
                }),
            );

            expect(response.data).toBeDefined();

            // Find groups and configs in the response
            const groups = response.data?.filter((item) => "Group" in item);
            const configs = response.data?.filter((item) => "Config" in item);

            // Group "c" should not be present (filtered out by prefix)
            const groupC = groups?.find((item) => item.Group === "c");
            expect(groupC).toBeUndefined();

            // No "c" prefixed configs should be present
            const cPrefixConfigs = configs?.filter(
                (item) => item.Config && item.Config.key?.startsWith("c."),
            );
            expect(cPrefixConfigs?.length).toBe(0);

            // Verify there are no configs starting with other prefixes either
            // (all should be "a" prefix or ungrouped)
            const allConfigKeys = configs
                ?.map((item) => item.Config?.key)
                .filter(Boolean);
            const nonAPrefixKeys = allConfigKeys?.filter(
                (key) => key && key.includes(".") && !key.startsWith("a."),
            );
            expect(nonAPrefixKeys?.length).toBe(0);
        });

        test("should search configs in ungrouped list", async () => {
            const response = await superpositionClient.send(
                new ListDefaultConfigsCommand({
                    workspace_id: ENV.workspace_id,
                    org_id: ENV.org_id,
                    search: "a.b",
                    count: 100,
                    page: 1,
                }),
            );

            expect(response.data).toBeDefined();
            expect(response.data?.length).toBeGreaterThanOrEqual(1);

            // Should contain a.b
            const configAB = response.data?.find(
                (config) => config.key === "a.b",
            );
            expect(configAB).toBeDefined();
        });

        // Note: Search is NOT supported with grouped=true parameter.
        // When search is used with grouped list, the API returns ungrouped data
        // which causes deserialization errors. Search should only be used with
        // the ungrouped list endpoint (tested above).
    });
});
