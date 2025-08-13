import {
    SuperpositionClient,
    UpdateWorkspaceCommand,
    CreateContextCommand,
    UpdateOverrideCommand,
    MoveContextCommand,
    DeleteContextCommand,
    GetContextCommand,
    BulkOperationCommand,
    CreateDimensionCommand,
    DeleteDimensionCommand,
    CreateDefaultConfigCommand,
    DeleteDefaultConfigCommand,
    type CreateContextCommandOutput,
    type GetContextCommandOutput,
    WeightRecomputeCommand,
    WorkspaceStatus,
    UpdateDimensionCommand,
} from "@juspay/superposition-sdk";
import { ENV, superpositionClient } from "../env.ts";
import { describe, beforeAll, afterAll, test, expect } from "bun:test";

describe("Context API Integration Tests", () => {
    let client: SuperpositionClient;
    let contextId: string;
    let testWorkspaceId: string;
    let testOrgId: string;

    // Track resources for cleanup
    const createdDimensions: string[] = [];
    const createdDefaultConfigs: string[] = [];
    const createdContextIds: Set<string> = new Set(); // Using Set to avoid duplicates

    beforeAll(async () => {
        client = superpositionClient;
        testWorkspaceId = ENV.workspace_id;
        testOrgId = ENV.org_id;
        console.log(`Using org ${testOrgId} with workspace ${testWorkspaceId}`);
        await addMandatoryDimension(client);
        await setupDimensionsAndConfigs(client);
    });

    // Add cleanup after all tests
    afterAll(async () => {
        console.log("Cleaning up test resources...");

        // Delete contexts first
        console.log(`Cleaning up ${createdContextIds.size} contexts...`);
        for (const id of createdContextIds) {
            try {
                await client.send(
                    new DeleteContextCommand({
                        workspace_id: testWorkspaceId,
                        org_id: testOrgId,
                        id: id,
                    })
                );
                console.log(`Deleted context: ${id}`);
            } catch (error: any) {
                console.error(`Failed to delete context ${id}:`, error.message);
            }
        }

        // Delete default configs
        console.log(
            `Cleaning up ${createdDefaultConfigs.length} default configs...`
        );
        for (const key of createdDefaultConfigs) {
            try {
                await client.send(
                    new DeleteDefaultConfigCommand({
                        workspace_id: testWorkspaceId,
                        org_id: testOrgId,
                        key: key,
                    })
                );
                console.log(`Deleted default config: ${key}`);
            } catch (error: any) {
                console.error(
                    `Failed to delete default config ${key}:`,
                    error.message
                );
            }
        }

        // Delete dimensions
        console.log(`Cleaning up ${createdDimensions.length} dimensions...`);
        for (const dim of createdDimensions) {
            try {
                await client.send(
                    new DeleteDimensionCommand({
                        workspace_id: testWorkspaceId,
                        org_id: testOrgId,
                        dimension: dim,
                    })
                );
                console.log(`Deleted dimension: ${dim}`);
            } catch (error: any) {
                console.error(
                    `Failed to delete dimension ${dim}:`,
                    error.message
                );
            }
        }
    });

    // Helper function to track context IDs
    function trackContext(id: string | undefined) {
        if (id) createdContextIds.add(id);
    }

    async function addMandatoryDimension(client: SuperpositionClient) {
        const input = {
            org_id: testOrgId,
            workspace_name: testWorkspaceId,
            workspace_admin_email: "updated-admin@example.com",
            workspace_status: WorkspaceStatus.ENABLED,
            mandatory_dimensions: ["clientId"],
        };

        const cmd = new UpdateWorkspaceCommand(input);
        const response = await client.send(cmd);
    }

    /**
     * Sets up necessary dimensions and default configs for tests
     */
    async function setupDimensionsAndConfigs(client: SuperpositionClient) {
        // Create dimensions needed for tests
        const dimensions = [
            {
                dimension: "clientId",
                schema: { type: "string" },
                position: 1,
                description: "Client identifier dimension",
            },
            {
                dimension: "moveSource",
                schema: { type: "string" },
                position: 2,
                description: "Source dimension for move tests",
            },
            {
                dimension: "moveTarget",
                schema: { type: "string" },
                position: 3,
                description: "Target dimension for move tests",
            },
            {
                dimension: "bulkTest",
                schema: { type: "string" },
                position: 4,
                description: "Dimension for bulk operations tests",
            },
            {
                dimension: "rollbackTest",
                schema: { type: "string" },
                position: 5,
                description: "Dimension for rollback tests",
            },
            {
                dimension: "mixedTest",
                schema: { type: "string" },
                position: 6,
                description: "Dimension for mixed operation tests",
            },
            {
                dimension: "toDelete",
                schema: { type: "string" },
                position: 7,
                description: "Dimension for delete tests",
            },
            {
                dimension: "conflict1",
                schema: { type: "string" },
                position: 8,
                description: "Dimension for conflict tests",
            },
            {
                dimension: "conflict2",
                schema: { type: "string" },
                position: 9,
                description: "Dimension for conflict tests",
            },
        ];

        // Create each dimension, ignoring already exists errors
        for (const dim of dimensions) {
            try {
                const cmd = new CreateDimensionCommand({
                    workspace_id: testWorkspaceId,
                    org_id: testOrgId,
                    dimension: dim.dimension,
                    schema: dim.schema,
                    position: dim.position,
                    description: dim.description,
                    change_reason: `Create ${dim.dimension} dimension for tests`,
                });

                await client.send(cmd);
                // Track created dimension
                createdDimensions.push(dim.dimension);
                console.log(`Created dimension: ${dim.dimension}`);
            } catch (e: any) {
                // If dimension already exists, just log and continue
                if (e.message && e.message.includes("duplicate key")) {
                    console.log(
                        `Dimension ${dim.dimension} already exists, updating dimension`
                    );
                    const cmd = new UpdateDimensionCommand({
                        workspace_id: testWorkspaceId,
                        org_id: testOrgId,
                        dimension: dim.dimension,
                        schema: dim.schema,
                        position: dim.position,
                        description: dim.description,
                        change_reason: `Update ${dim.dimension} dimension for tests`,
                    });

                    await client.send(cmd);
                    // Track created dimension
                    createdDimensions.push(dim.dimension);
                } else {
                    console.error(
                        `Failed to create dimension ${dim.dimension}:`,
                        e.message
                    );
                    // Don't throw, we want setup to continue even if some dimensions fail
                }
            }
        }

        const defaultConfigs = [
            {
                key: "key1",
                schema: {
                    type: "string",
                },
                value: "defaultValue1",
                description: "Default config key1 for tests",
            },
            {
                key: "key2",
                schema: {
                    type: "number",
                    minimum: 0,
                },
                value: 42,
                description: "Default config key2 for tests",
            },
            {
                key: "key3",
                schema: {
                    type: "string",
                },
                value: "defaultValue3",
                description: "Default config key2 for tests",
            },
            {
                key: "key4",
                schema: {
                    type: "string",
                },
                value: "defaultValue3",
                description: "Default config key2 for tests",
            },
            {
                key: "bulkKey1",
                schema: {
                    type: "string",
                },
                value: "defaultBulkValue1",
                description: "Default config for bulk tests",
            },
            {
                key: "bulkKey2",
                schema: {
                    type: "string",
                },
                value: "defaultBulkValue2",
                description: "Default config for bulk tests",
            },
            {
                key: "moveKey",
                schema: {
                    type: "string",
                },
                value: "defaultMoveValue",
                description: "Default config for move tests",
            },
            {
                key: "deleteKey",
                schema: {
                    type: "string",
                },
                value: "defaultDeleteValue",
                description: "Default config for delete tests",
            },
            {
                key: "mixedKey",
                schema: {
                    type: "string",
                },
                value: "defaultMixedValue",
                description: "Default config for mixed operation tests",
            },
            {
                key: "rollbackKey",
                schema: {
                    type: "string",
                },
                value: "defaultRollbackValue",
                description: "Default config for rollback tests",
            },
            {
                key: "invalidKey",
                schema: {
                    type: "string",
                },
                value: "defaultInvalidValue",
                description: "Default config for invalid value tests",
            },
            {
                key: "uniqueKey1",
                schema: {
                    type: "string",
                },
                value: "uniqueValue1",
                description: "Default config for invalid value tests",
            },
            {
                key: "uniqueKey2",
                schema: {
                    type: "string",
                },
                value: "uniqueValue2",
                description: "Default config for invalid value tests",
            },
        ];

        // Create each default config, ignoring already exists errors
        for (const config of defaultConfigs) {
            try {
                const cmd = new CreateDefaultConfigCommand({
                    workspace_id: testWorkspaceId,
                    org_id: testOrgId,
                    key: config.key,
                    schema: config.schema,
                    value: config.value,
                    description: config.description,
                    change_reason: `Create ${config.key} default config for tests`,
                });

                await client.send(cmd);
                // Track created config
                createdDefaultConfigs.push(config.key);
                console.log(`Created default config: ${config.key}`);
            } catch (e: any) {
                // If config already exists, just log and continue
                if (e.message && e.message.includes("already exists")) {
                    console.log(
                        `Default config ${config.key} already exists, skipping creation`
                    );
                } else {
                    console.error(
                        `Failed to create default config ${config.key}:`,
                        e.message
                    );
                    // Don't throw, we want setup to continue even if some configs fail
                }
            }
        }
    }

    describe("PUT Context Endpoint", () => {
        test("should create a valid context successfully", async () => {
            const context = ENV.jsonlogic_enabled
                ? {
                      and: [
                          {
                              "==": [{ var: "clientId" }, "test-client"],
                          },
                      ],
                  }
                : {
                      clientId: "test-client",
                  };
            const input = {
                workspace_id: testWorkspaceId,
                org_id: testOrgId,
                override: {
                    key1: "value1",
                    key2: 42,
                },
                context,
                description: "Test context",
                change_reason: "Initial creation",
            };

            let response: CreateContextCommandOutput;
            try {
                const cmd = new CreateContextCommand(input);
                response = await client.send(cmd);
            } catch (err: any) {
                console.log(err.$response);
                throw err.$response;
            }

            // Track created context
            trackContext(response.id);

            expect(response.$metadata.httpStatusCode).toBe(200);
            expect(response.id).toBeDefined();

            contextId = response.id || "";

            const getCmd = new GetContextCommand({
                workspace_id: testWorkspaceId,
                org_id: testOrgId,
                id: contextId,
            });

            let fetchedContext: GetContextCommandOutput;
            try {
                fetchedContext = await client.send(getCmd);
            } catch (err: any) {
                console.log(err.$response);
                throw err.$response;
            }

            expect(fetchedContext.override).toEqual(input.override);

            expect(fetchedContext.value).toEqual(input.context);

            expect(fetchedContext.description).toBe(input.description);
            expect(fetchedContext.change_reason).toBe(input.change_reason);

            // Check that weight is calculated correctly - clientId has position 1, so weight should be 2^1 = 2
            expect(fetchedContext.weight).toBe("2");
        });

        test("should reject context creation with unwrapped JSON Logic", async () => {
            if (!ENV.jsonlogic_enabled) {
                console.log(
                    "Skipping unwrapped context test as JSON Logic is not enabled"
                );
                return;
            }
            // Attempt to create a context with an unwrapped condition
            const unwrappedInput = {
                workspace_id: testWorkspaceId,
                org_id: testOrgId,
                override: {
                    key1: "unwrapped-value",
                },
                context: {
                    "==": [{ var: "clientId" }, "validation-test-client"],
                },
                description: "Unwrapped context",
                change_reason: "Testing unwrapped context validation",
            };

            try {
                const unwrappedCmd = new CreateContextCommand(unwrappedInput);
                await client.send(unwrappedCmd);
            } catch (err: any) {
                expect(err.$response.body).toMatch(
                    /JSON Logic must be wrapped in an 'and' block/i
                );
            }
        });

        test("should reject empty context objects", async () => {
            const emptyInput = {
                workspace_id: testWorkspaceId,
                org_id: testOrgId,
                override: {
                    key1: "empty-context-value",
                },
                context: {},
                description: "Empty context",
                change_reason: "Testing empty context validation",
            };

            try {
                const unwrappedCmd = new CreateContextCommand(emptyInput);
                await client.send(unwrappedCmd);
            } catch (err: any) {
                expect(err.$response.body).toMatch(
                    ENV.jsonlogic_enabled
                        ? /Empty JSON Logic is not allowed/i
                        : /Context should not be empty/i
                );
            }
        });

        test("should create context with multiple dimensions and calculate weight correctly", async () => {
            const context = ENV.jsonlogic_enabled
                ? {
                      and: [
                          {
                              "==": [{ var: "clientId" }, "weight-test-client"],
                          },
                          {
                              "==": [
                                  { var: "moveSource" },
                                  "weight-test-source",
                              ],
                          },
                      ],
                  }
                : {
                      clientId: "weight-test-client",
                      moveSource: "weight-test-source",
                  };
            const input = {
                workspace_id: testWorkspaceId,
                org_id: testOrgId,
                override: {
                    key1: "multi-dimension-value",
                },
                context,
                description: "Multi-dimension context for weight test",
                change_reason: "Testing weight calculation",
            };
            const cmd = new CreateContextCommand(input);
            let response: CreateContextCommandOutput;

            try {
                response = await client.send(cmd);
            } catch (err: any) {
                console.error(err.$response);
                throw err.$response;
            }

            // Track created context
            trackContext(response.id);

            expect(response.$metadata.httpStatusCode).toBe(200);
            // Weight should be 2^1 + 2^2 = 2 + 4 = 6 (for clientId and moveSource)
            expect(response.weight).toBe("6");
        });

        test("should fail with invalid context condition", async () => {
            if (!ENV.jsonlogic_enabled) {
                console.log(
                    "Skipping invalid context condition test as JSON Logic is not enabled"
                );
                return;
            }
            const input = {
                workspace_id: testWorkspaceId,
                org_id: testOrgId,
                override: { key1: "value1" },
                context: {
                    and: [
                        {
                            invalid_operator: [
                                { var: "clientId" },
                                "test-client",
                            ],
                        },
                    ],
                },
                description: "Invalid context",
                change_reason: "Testing invalid input",
            };

            // Unexpected error response from the client
            // JSON Parse error: Unexpected identifier "Json"
            // Deserialization error: to see the raw response, inspect the hidden field {error}.$response on this object.
            // TODO: Check client implementation for better error handling
            const cmd = new CreateContextCommand(input);
            try {
                await client.send(cmd);
            } catch (err: any) {
                console.log(err.$response);
            }
        });

        test("should fail with missing required dimension", async () => {
            // Assuming a workspace has mandatory dimensions configured
            const context = ENV.jsonlogic_enabled
                ? {
                      and: [
                          {
                              "==": [{ var: "moveSource" }, "value"],
                          },
                      ],
                  }
                : {
                      moveSource: "value",
                  };
            const input = {
                workspace_id: testWorkspaceId,
                org_id: testOrgId,
                override: { key1: "value1" },
                context,
                description: "Testing missing mandatory dimension",
                change_reason: "Testing missing mandatory dimension",
            };

            const cmd = new CreateContextCommand(input);
            expect(client.send(cmd)).rejects.toThrow(
                /The context should contain all the mandatory dimensions/i
            );
        });

        test("should fail with invalid dimension schema", async () => {
            const context = ENV.jsonlogic_enabled
                ? {
                      and: [
                          {
                              "==": [{ var: "clientId" }, 123],
                          },
                      ],
                  }
                : {
                      clientId: 123,
                  };
            const input = {
                workspace_id: testWorkspaceId,
                org_id: testOrgId,
                override: {
                    key1: "123",
                },
                context,
                description: "Testing invalid dimension schema",
                change_reason: "Testing invalid dimension schema",
            };

            const cmd = new CreateContextCommand(input);
            expect(client.send(cmd)).rejects.toThrow(
                "failed to validate dimension value 123: value doesn't match the required type(s) `Single(String)`"
            );
        });

        test("should fail with invalid override schema", async () => {
            // Assuming key1 has a schema that requires string values
            const context = ENV.jsonlogic_enabled
                ? {
                      and: [
                          {
                              "==": [{ var: "clientId" }, "test-client"],
                          },
                      ],
                  }
                : {
                      clientId: "test-client",
                  };
            const input = {
                workspace_id: testWorkspaceId,
                org_id: testOrgId,
                override: {
                    key1: 123, // Assuming schema expects string
                },
                context,
                description: "Testing invalid override",
                change_reason: "Testing invalid override",
            };

            const cmd = new CreateContextCommand(input);
            // TODO: Write a display fmt for JSONSchema enum to get rid of Single from the message
            expect(client.send(cmd)).rejects.toThrow(
                "schema validation failed for key1: value doesn't match the required type(s) `Single(String)`"
            );
        });
    });

    describe("Update Context Override Endpoint", () => {
        test("should update override for existing context", async () => {
            const context = ENV.jsonlogic_enabled
                ? {
                      and: [
                          {
                              "==": [{ var: "clientId" }, "test-client"],
                          },
                      ],
                  }
                : {
                      clientId: "test-client",
                  };
            const input = {
                workspace_id: testWorkspaceId,
                org_id: testOrgId,
                request: {
                    override: {
                        key1: "updated-value",
                        key3: "new-value",
                    },
                    context: { context },
                    description: "Updated context",
                    change_reason: "Updating override",
                },
            };

            const cmd = new UpdateOverrideCommand(input);
            const response = await client.send(cmd);

            // Track context ID from update response
            trackContext(response.id);

            expect(response.$metadata.httpStatusCode).toBe(200);
            expect(response.id).toBeDefined();
            expect(response.override_id).toBeDefined();

            const getCmd = new GetContextCommand({
                workspace_id: testWorkspaceId,
                org_id: testOrgId,
                id: response.id,
            });

            const fetchedContext: GetContextCommandOutput = await client.send(
                getCmd
            );

            // Verify the overrides were updated correctly
            expect(fetchedContext.override?.key1).toBe("updated-value");
            expect(fetchedContext.override?.key3).toBe("new-value");
            expect(fetchedContext.change_reason).toBe("Updating override");
        });

        test("should replace all override values", async () => {
            const context = ENV.jsonlogic_enabled
                ? {
                      and: [
                          {
                              "==": [{ var: "clientId" }, "test-client"],
                          },
                      ],
                  }
                : {
                      clientId: "test-client",
                  };
            const input = {
                workspace_id: testWorkspaceId,
                org_id: testOrgId,
                request: {
                    override: {
                        key4: "replaced-value",
                    },
                    context: { context },
                    change_reason: "Replacing override",
                },
            };

            const cmd = new UpdateOverrideCommand(input);
            const response = await client.send(cmd);

            // Track context ID from update response
            trackContext(response.id);

            expect(response.$metadata.httpStatusCode).toBe(200);
            expect(response.id).toBeDefined();
            expect(response.override_id).toBeDefined();

            // Now fetch the context to verify replaced override
            const getCmd = new GetContextCommand({
                workspace_id: testWorkspaceId,
                org_id: testOrgId,
                id: response.id,
            });

            const fetchedContext = await client.send(getCmd);

            // Verify that previous keys are gone and only new ones exist
            expect(fetchedContext.override).toEqual({
                key4: "replaced-value",
            });
            expect(fetchedContext.override?.key1).toBeUndefined();
            expect(fetchedContext.override?.key3).toBeUndefined();
            expect(fetchedContext.change_reason).toBe("Replacing override");
        });

        test("should fail when context does not exist", async () => {
            const context = ENV.jsonlogic_enabled
                ? {
                      and: [
                          {
                              "==": [
                                  { var: "clientId" },
                                  "non-existent-context-test",
                              ],
                          },
                      ],
                  }
                : { clientId: "non-existent-context-test" };
            const input = {
                workspace_id: testWorkspaceId,
                org_id: testOrgId,
                request: {
                    override: {
                        key4: "replaced-value",
                    },
                    context: {
                        context,
                    },
                    change_reason: "Replacing override",
                },
            };
            const updateCmd = new UpdateOverrideCommand(input);

            await expect(client.send(updateCmd)).rejects.toThrow(
                "No records found. Please refine or correct your search parameters"
            );
        });
    });

    describe("Update Context Override By ID Endpoint", () => {
        test("should update override for context using ID", async () => {
            const context = ENV.jsonlogic_enabled
                ? {
                      and: [
                          {
                              "==": [
                                  { var: "clientId" },
                                  "update-by-id-test-client",
                              ],
                          },
                      ],
                  }
                : {
                      clientId: "update-by-id-test-client",
                  };
            // First create a context to get an ID
            const createCmd = new CreateContextCommand({
                workspace_id: testWorkspaceId,
                org_id: testOrgId,
                override: {
                    key1: "original-value",
                    key2: 100,
                },
                context,
                description: "Context for update by ID test",
                change_reason: "Creating for update by ID test",
            });

            const createResp = await client.send(createCmd);
            const contextId = createResp.id;

            // Track created context
            trackContext(contextId);

            // Now update the context by ID
            const updateByIdCmd = new UpdateOverrideCommand({
                workspace_id: testWorkspaceId,
                org_id: testOrgId,
                request: {
                    context: { id: contextId },
                    override: {
                        key1: "updated-by-id",
                        key3: "new-by-id",
                    },
                    description: "Updated context by ID",
                    change_reason: "Updating override by ID",
                },
            });

            const updateResp = await client.send(updateByIdCmd);

            expect(updateResp.$metadata.httpStatusCode).toBe(200);
            expect(updateResp.id).toBe(contextId);
            expect(updateResp.override_id).toBeDefined();

            // Fetch the context to verify updates
            const getCmd = new GetContextCommand({
                workspace_id: testWorkspaceId,
                org_id: testOrgId,
                id: contextId,
            });

            const fetchedContext = await client.send(getCmd);

            // Verify the overrides were updated correctly
            expect(fetchedContext.override?.key1).toBe("updated-by-id");
            expect(fetchedContext.override?.key2).toBeUndefined();
            expect(fetchedContext.override?.key3).toBe("new-by-id");
            expect(fetchedContext.description).toBe("Updated context by ID");
            expect(fetchedContext.change_reason).toBe(
                "Updating override by ID"
            );
        });

        //sdkjfbjsbfjbsjdfbhsj pending from here

        test("should replace all override values when updating by ID", async () => {
            const context = ENV.jsonlogic_enabled
                ? {
                      and: [
                          {
                              "==": [
                                  { var: "clientId" },
                                  "replace-by-id-test-client",
                              ],
                          },
                      ],
                  }
                : {
                      clientId: "replace-by-id-test-client",
                  };
            // First create a context to get an ID
            const createCmd = new CreateContextCommand({
                workspace_id: testWorkspaceId,
                org_id: testOrgId,
                override: {
                    key1: "replace-original",
                    key2: 200,
                },
                context,
                description: "Context for replace by ID test",
                change_reason: "Creating for replace by ID test",
            });

            const createResp = await client.send(createCmd);
            const contextId = createResp.id;

            // Track created context
            trackContext(contextId);

            // Now replace all override values
            const updateByIdCmd = new UpdateOverrideCommand({
                workspace_id: testWorkspaceId,
                org_id: testOrgId,
                request: {
                    context: { id: contextId },
                    override: {
                        key4: "completely-new-value",
                    },
                    change_reason: "Replacing all overrides by ID",
                },
            });

            const updateResp = await client.send(updateByIdCmd);

            expect(updateResp.$metadata.httpStatusCode).toBe(200);
            expect(updateResp.id).toBe(contextId);
            expect(updateResp.override_id).toBeDefined();

            // Fetch the context to verify replacement
            const getCmd = new GetContextCommand({
                workspace_id: testWorkspaceId,
                org_id: testOrgId,
                id: contextId,
            });

            const fetchedContext = await client.send(getCmd);

            // Verify that previous keys are gone and only new ones exist
            expect(fetchedContext.override).toEqual({
                key4: "completely-new-value",
            });
            expect(fetchedContext.override?.key1).toBeUndefined();
            expect(fetchedContext.override?.key2).toBeUndefined();
            expect(fetchedContext.change_reason).toBe(
                "Replacing all overrides by ID"
            );
        });

        test("should fail when context ID does not exist", async () => {
            const updateByIdCmd = new UpdateOverrideCommand({
                workspace_id: testWorkspaceId,
                org_id: testOrgId,
                request: {
                    context: { id: "non-existent-context-id" },
                    override: {
                        key1: "value-for-non-existent",
                    },
                    change_reason: "Updating non-existent context",
                },
            });

            await expect(client.send(updateByIdCmd)).rejects.toThrow(
                "No records found. Please refine or correct your search parameters"
            );
        });
    });

    describe("Move Context Endpoint", () => {
        test("should move context to new condition", async () => {
            const createContext = ENV.jsonlogic_enabled
                ? {
                      and: [
                          {
                              "==": [{ var: "clientId" }, "move-test-client"],
                          },
                          {
                              "==": [{ var: "moveSource" }, "source"],
                          },
                      ],
                  }
                : {
                      clientId: "move-test-client",
                      moveSource: "source",
                  };
            const createCmd = new CreateContextCommand({
                workspace_id: testWorkspaceId,
                org_id: testOrgId,
                override: { moveKey: "moveValue" },
                context: createContext,
                description: "Context to move",
                change_reason: "Creating for move test",
            });

            const createResp = await client.send(createCmd);

            // Track created context
            trackContext(createResp.id);

            const sourceId = createResp.id;

            const moveContext = ENV.jsonlogic_enabled
                ? {
                      and: [
                          {
                              "==": [{ var: "clientId" }, "move-test-client"],
                          },
                          {
                              "==": [{ var: "moveTarget" }, "target"],
                          },
                      ],
                  }
                : {
                      clientId: "move-test-client",
                      moveTarget: "target",
                  };
            const moveInput = {
                workspace_id: testWorkspaceId,
                org_id: testOrgId,
                context: moveContext,
                description: "Moved context",
                change_reason: "Testing move operation",
            };

            const moveCmd = new MoveContextCommand({
                ...moveInput,
                id: sourceId,
            });

            const moveResp = await client.send(moveCmd);

            // Track moved context (target ID)
            trackContext(moveResp.id);

            expect(moveResp.$metadata.httpStatusCode).toBe(200);
            expect(moveResp.id).not.toBe(sourceId);

            // fetch the moved context to verify the context and override
            const getCmd = new GetContextCommand({
                workspace_id: testWorkspaceId,
                org_id: testOrgId,
                id: moveResp.id,
            });
            const movedContext = await client.send(getCmd);
            expect(movedContext.value).toEqual(moveInput.context);
            expect(movedContext.override?.moveKey).toBe("moveValue");
            // TODO: we are updating the change_reason for move operation
            // expect(movedContext.change_reason).toBe("Testing move operation");
            expect(movedContext.description).toBe("Moved context");
            // moveTarget has position 3, and clientId has position 1, so weight should be 2^1 + 2^3 = 2 + 8 = 10
            expect(movedContext.weight).toBe("10");
        });

        test("should merge overrides when moving to existing condition", async () => {
            const firstCreateContext = ENV.jsonlogic_enabled
                ? {
                      and: [
                          {
                              "==": [{ var: "clientId" }, "merge-test-client"],
                          },
                          {
                              "==": [{ var: "moveSource" }, "merge-source"],
                          },
                      ],
                  }
                : {
                      clientId: "merge-test-client",
                      moveSource: "merge-source",
                  };
            // Create first context with some overrides
            const createFirstCmd = new CreateContextCommand({
                workspace_id: testWorkspaceId,
                org_id: testOrgId,
                override: {
                    key1: "source-value",
                    uniqueKey1: "only-in-source",
                },
                context: firstCreateContext,
                description: "Source context for merge test",
                change_reason: "Creating source for merge test",
            });

            const firstResp = await client.send(createFirstCmd);

            // Track created context
            trackContext(firstResp.id);

            const sourceId = firstResp.id;

            const secondCreateContext = ENV.jsonlogic_enabled
                ? {
                      and: [
                          {
                              "==": [{ var: "clientId" }, "merge-test-client"],
                          },
                          {
                              "==": [{ var: "moveTarget" }, "merge-target"],
                          },
                      ],
                  }
                : {
                      clientId: "merge-test-client",
                      moveTarget: "merge-target",
                  };
            // Create second context with overlapping and different overrides
            const createSecondCmd = new CreateContextCommand({
                workspace_id: testWorkspaceId,
                org_id: testOrgId,
                override: {
                    key1: "target-value", // Will be replaced
                    uniqueKey2: "only-in-target", // Will be preserved
                },
                context: secondCreateContext,
                description: "Target context for merge test",
                change_reason: "Creating target for merge test",
            });

            const secondResp = await client.send(createSecondCmd);

            // Track created context
            trackContext(secondResp.id);

            const targetId = secondResp.id;

            const moveContext = ENV.jsonlogic_enabled
                ? {
                      and: [
                          {
                              "==": [{ var: "clientId" }, "merge-test-client"],
                          },
                          {
                              "==": [{ var: "moveTarget" }, "merge-target"],
                          },
                      ],
                  }
                : {
                      clientId: "merge-test-client",
                      moveTarget: "merge-target",
                  };
            // Now attempt to move first context to location of second context
            const moveCmd = new MoveContextCommand({
                workspace_id: testWorkspaceId,
                org_id: testOrgId,
                id: sourceId,
                context: moveContext,
                description: "Moved and merged context",
                change_reason: "Testing merge behavior",
            });

            const moveResp = await client.send(moveCmd);
            expect(moveResp.$metadata.httpStatusCode).toBe(200);

            // Fetch the source context, and should result into a 404
            const getCmd = new GetContextCommand({
                workspace_id: testWorkspaceId,
                org_id: testOrgId,
                id: sourceId,
            });
            expect(client.send(getCmd)).rejects.toThrow(
                "No records found. Please refine or correct your search parameters"
            );

            // Fetch the resulting context to verify merge behavior
            const getCmd1 = new GetContextCommand({
                workspace_id: testWorkspaceId,
                org_id: testOrgId,
                id: moveResp.id,
            });

            const mergedContext = await client.send(getCmd1);

            // Verify the merged overrides have expected values
            expect(mergedContext.override).toBeDefined();
            expect(mergedContext.override?.key1).toBe("source-value"); // Source value should override target
            expect(mergedContext.override?.uniqueKey1).toBe("only-in-source"); // Source-unique key preserved
            expect(mergedContext.override?.uniqueKey2).toBe("only-in-target"); // Target-unique key preserved

            const expectContext = ENV.jsonlogic_enabled
                ? {
                      and: [
                          {
                              "==": [{ var: "clientId" }, "merge-test-client"],
                          },
                          {
                              "==": [{ var: "moveTarget" }, "merge-target"],
                          },
                      ],
                  }
                : {
                      clientId: "merge-test-client",
                      moveTarget: "merge-target",
                  };
            // Verify the condition matches the target context
            expect(mergedContext.value).toEqual(expectContext);

            // Verify the description and weight
            expect(mergedContext.description).toBe("Moved and merged context");
            // Weight should be clientId (2^1 = 2) + moveTarget (2^3 = 8) = 10
            expect(mergedContext.weight).toBe("10");
        });

        test("should fail when context id does not exist", async () => {
            const context = ENV.jsonlogic_enabled
                ? {
                      and: [
                          {
                              "==": [{ var: "clientId" }, "move-test-client"],
                          },
                          {
                              "==": [{ var: "moveTarget" }, "target"],
                          },
                      ],
                  }
                : {
                      clientId: "move-test-client",
                      moveTarget: "target",
                  };
            const moveCmd = new MoveContextCommand({
                workspace_id: testWorkspaceId,
                org_id: testOrgId,
                id: "non-existent-id",
                context,
                description: "Testing non-existent move",
                change_reason: "Testing non-existent move",
            });

            // Warn(TODO): validation for context's content is made beforehand the valid id check
            expect(client.send(moveCmd)).rejects.toThrow(
                "No records found. Please refine or correct your search parameters"
            );
        });
    });

    /**
     * TODO
     *  - List Contexts tests
     *  - Delete Context tests
     *  - Fetch Context
     */

    describe("Bulk Operations Endpoint", () => {
        test("should perform multiple operations in bulk", async () => {
            const context1 = ENV.jsonlogic_enabled
                ? {
                      and: [
                          {
                              "==": [{ var: "clientId" }, "bulk-test-client"],
                          },
                          {
                              "==": [{ var: "bulkTest" }, "value1"],
                          },
                      ],
                  }
                : {
                      clientId: "bulk-test-client",
                      bulkTest: "value1",
                  };
            const context2 = ENV.jsonlogic_enabled
                ? {
                      and: [
                          {
                              "==": [{ var: "clientId" }, "bulk-test-client"],
                          },
                          {
                              "==": [{ var: "bulkTest" }, "value2"],
                          },
                      ],
                  }
                : {
                      clientId: "bulk-test-client",
                      bulkTest: "value2",
                  };
            const bulkCmd = new BulkOperationCommand({
                workspace_id: testWorkspaceId,
                org_id: testOrgId,
                bulk_operation: {
                    operations: [
                        {
                            PUT: {
                                context: context1,
                                override: { bulkKey1: "bulkValue1" },
                                description: "Bulk test context 1",
                                change_reason: "Bulk operation 1",
                            },
                        },
                        {
                            PUT: {
                                context: context2,
                                override: { bulkKey2: "bulkValue2" },
                                description: "Bulk test context 2",
                                change_reason: "Bulk operation 2",
                            },
                        },
                    ],
                },
            });

            const response = await client.send(bulkCmd);
            expect(response.$metadata.httpStatusCode).toBe(200);
            expect(response.bulk_operation_output?.output?.length).toBe(2);

            // Track created contexts from bulk operations
            if (response.bulk_operation_output?.output) {
                for (const output of response.bulk_operation_output.output) {
                    if (output.PUT?.id) {
                        trackContext(output.PUT.id);
                    }
                    if (output.MOVE?.id) {
                        trackContext(output.MOVE.id);
                    }
                }
            }

            // Verify first operation
            const firstOp = response.bulk_operation_output?.output?.[0];
            expect(firstOp?.PUT).toBeDefined();
            expect(firstOp?.PUT?.id).toBeDefined();

            // Verify second operation
            const secondOp = response.bulk_operation_output?.output?.[1];
            expect(secondOp?.PUT).toBeDefined();
            expect(secondOp?.PUT?.id).toBeDefined();

            // fetch both the context and assert the context and override
            const getFirstCmd = new GetContextCommand({
                workspace_id: testWorkspaceId,
                org_id: testOrgId,
                id: firstOp?.PUT?.id || "",
            });
            const firstContext = await client.send(getFirstCmd);
            const expectContext1 = ENV.jsonlogic_enabled
                ? {
                      and: [
                          {
                              "==": [{ var: "clientId" }, "bulk-test-client"],
                          },
                          {
                              "==": [{ var: "bulkTest" }, "value1"],
                          },
                      ],
                  }
                : {
                      clientId: "bulk-test-client",
                      bulkTest: "value1",
                  };
            expect(firstContext.value).toEqual(expectContext1);
            expect(firstContext.override?.bulkKey1).toBe("bulkValue1");
            expect(firstContext.change_reason).toBe("Bulk operation 1");
            expect(firstContext.description).toBe("Bulk test context 1");
            expect(firstContext.weight).toBe("18"); // clientId (1) + bulkTest (4) = 2 + 16 = 18

            // fetch second context and assert the context and override
            const getSecondCmd = new GetContextCommand({
                workspace_id: testWorkspaceId,
                org_id: testOrgId,
                id: secondOp?.PUT?.id || "",
            });
            const secondContext = await client.send(getSecondCmd);
            const expectContext2 = ENV.jsonlogic_enabled
                ? {
                      and: [
                          {
                              "==": [{ var: "clientId" }, "bulk-test-client"],
                          },
                          {
                              "==": [{ var: "bulkTest" }, "value2"],
                          },
                      ],
                  }
                : {
                      clientId: "bulk-test-client",
                      bulkTest: "value2",
                  };
            expect(secondContext.value).toEqual(expectContext2);
            expect(secondContext.override?.bulkKey2).toBe("bulkValue2");
            expect(secondContext.change_reason).toBe("Bulk operation 2");
            expect(secondContext.description).toBe("Bulk test context 2");
            expect(secondContext.weight).toBe("18"); // clientId (1) + bulkTest (4) = 2 + 16 = 18
        });

        test("should rollback all operations if one fails", async () => {
            const context1 = ENV.jsonlogic_enabled
                ? {
                      // here too with and and clientId
                      and: [
                          {
                              "==": [{ var: "clientId" }, "bulk-test-client"],
                          },
                          {
                              "==": [{ var: "rollbackTest" }, "valid"],
                          },
                      ],
                  }
                : {
                      clientId: "bulk-test-client",
                      rollbackTest: "valid",
                  };
            const context2 = ENV.jsonlogic_enabled
                ? {
                      and: [
                          {
                              "==": [{ var: "clientId" }, "bulk-test-client"],
                          },
                          {
                              "==": [{ var: "rollbackTest" }, "invalid"],
                          },
                      ],
                  }
                : {
                      clientId: "bulk-test-client",
                      rollbackTest: "invalid",
                  };
            const bulkCmd = new BulkOperationCommand({
                workspace_id: testWorkspaceId,
                org_id: testOrgId,
                bulk_operation: {
                    operations: [
                        {
                            PUT: {
                                context: context1,
                                override: { rollbackKey: "rollbackValue" },
                                description: "Valid context",
                                change_reason: "Valid operation",
                            },
                        },
                        {
                            PUT: {
                                context: context2,
                                override: { invalidKey: 123 }, // Schema expects string
                                description: "Invalid context",
                                change_reason: "Invalid operation",
                            },
                        },
                    ],
                },
            });

            expect(client.send(bulkCmd)).rejects.toThrow(
                "schema validation failed for invalidKey: value doesn't match the required type(s) `Single(String)`"
            );
            // TODO: Should add fetching a context by jsonlogic and then assert the first context creation was rolled back
        });

        test("should perform mixed operations (put, move, delete)", async () => {
            /** TODO
             * Move payload for Bulk Operation is as such (id, MoveReq)
             *
             * Smitthy has modeled it as { id, ...other_fields }
             *
             * Causing desearilization issues on backend
             */
            // First create a context we can later move and delete
            // const createCmd = new CreateContextCommand({
            //     workspace_id: testWorkspaceId,
            //     org_id: testOrgId,
            //     override: { "mixedKey": "mixedValue" },
            //     context: {
            //         "and": [
            //             {
            //                 "==": [
            //                     { "var": "clientId" },
            //                     "mixed-test-client"
            //                 ]
            //             },
            //             {
            //                 "==": [
            //                     { "var": "mixedTest" },
            //                     "toMove"
            //                 ]
            //             }
            //         ]
            //     },
            //     description: "Context for mixed operations test",
            //     change_reason: "Creating for mixed bulk test"
            // });
            // const createResp = await client.send(createCmd);
            // const sourceId = createResp.context_id;
            // // Now do bulk operations
            // const bulkCmd = new BulkOperationCommand({
            //     workspace_id: testWorkspaceId,
            //     org_id: testOrgId,
            //     bulk_operation: {
            //         operations: [
            //             {
            //                 PUT: {
            //                     context: {
            //                         "and": [
            //                             {
            //                                 "==": [
            //                                     { "var": "clientId" },
            //                                     "mixed-test-client"
            //                                 ]
            //                             },
            //                             {
            //                                 "==": [
            //                                     { "var": "mixedTest" },
            //                                     "new"
            //                                 ]
            //                             }
            //                         ]
            //                     },
            //                     override: { "newKey": "newValue" },
            //                     description: "New context",
            //                     change_reason: "New context creation"
            //                 }
            //             },
            //             {
            //                 MOVE: {
            //                     id: sourceId,
            //                     context: {
            //                         "and": [
            //                             {
            //                                 "==": [
            //                                     { "var": "clientId" },
            //                                     "mixed-test-client"
            //                                 ]
            //                             },
            //                             {
            //                                 "==": [
            //                                     { "var": "mixedTest" },
            //                                     "moved"
            //                                 ]
            //                             }
            //                         ]
            //                     },
            //                     description: "Moved context",
            //                     change_reason: "Moving context"
            //                 }
            //             }
            //         ]
            //     }
            // });
            // const response = await client.send(bulkCmd);
            // expect(response.$metadata.httpStatusCode).toBe(200);
            // expect(response.bulk_operation_output?.output?.length).toBe(2);
            // // Verify the PUT operation
            // const putOp = response.bulk_operation_output?.output?.[0];
            // expect(putOp?.PUT).toBeDefined();
            // expect(putOp?.PUT?.context_id).toBeDefined();
            // // Verify the MOVE operation
            // const moveOp = response.bulk_operation_output?.output?.[1];
            // expect(moveOp?.MOVE).toBeDefined();
            // expect(moveOp?.MOVE?.context_id).toBeDefined();
            // // Test DELETE operation in bulk
            // const deleteCtxCmd = new BulkOperationCommand({
            //     workspace_id: testWorkspaceId,
            //     org_id: testOrgId,
            //     bulk_operation: {
            //         operations: [
            //             {
            //                 DELETE: moveOp?.MOVE?.context_id || "invalid_context_id"
            //             }
            //         ]
            //     }
            // });
            // const deleteResp = await client.send(deleteCtxCmd);
            // expect(deleteResp.$metadata.httpStatusCode).toBe(200);
            // expect(deleteResp?.bulk_operation_output?.output?.length).toBe(1);
            // expect(deleteResp.bulk_operation_output?.output?.[0]?.DELETE).toBeDefined();
            // expect(deleteResp.bulk_operation_output?.output?.[0]?.DELETE).toBe(moveOp?.MOVE?.context_id);
        });
    });

    describe("Weight Recompute Endpoint", () => {
        test("should recompute weights for all contexts", async () => {
            const cmd = new WeightRecomputeCommand({
                workspace_id: testWorkspaceId,
                org_id: testOrgId,
            });

            const response = await client.send(cmd);

            expect(response.$metadata.httpStatusCode).toBe(200);
            expect(response.data).toBeDefined();
            expect(Array.isArray(response.data)).toBe(true);

            // Each context should have old_weight and new_weight
            if (response.data && response.data.length > 0) {
                for (const item of response.data) {
                    expect(item.id).toBeDefined();
                    expect(item.old_weight).toBeDefined();
                    expect(item.new_weight).toBeDefined();
                    expect(item.condition).toBeDefined();
                }
            }
        });
    });
});
