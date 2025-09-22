import {
    CreateDimensionCommand,
    DeleteDimensionCommand,
    GetDimensionCommand,
    ListDimensionsCommand,
    UpdateDimensionCommand,
    CreateFunctionCommand,
    DeleteFunctionCommand,
    FunctionTypes,
    ResourceNotFound,
    PublishCommand,
} from "@juspay/superposition-sdk";
import { superpositionClient, ENV } from "../env.ts";
import { describe, afterAll, test, expect } from "bun:test";

describe("Dimension API", () => {
    // Test variables
    const testDimension = {
        dimension: `test-dimension-${Date.now()}`,
        position: 1, // Position 0 is reserved, start from 1
        schema: { type: "string" },
        description: "Test dimension for automated testing",
        change_reason: "Creating test dimension",
    };
    
    const testLocalCohort = {
        dimension: `test-cohort-${Date.now()}`,
        position: 2,
        schema: {
            "type": "string",
            "enum": ["small", "big"],
            "definitions": {
                small: {
                    and: [
                        {
                            "==": [
                                {
                                    var: testDimension.dimension,
                                },
                                "hdfc",
                            ],
                        },
                    ],
                },
                big: {
                    and: [
                        {
                            "==": [
                                {
                                    var: testDimension.dimension,
                                },
                                "kotak",
                            ],
                        },
                    ],
                },
            }
        },
        dimension_type: {
            "LOCAL_COHORT": testDimension.dimension
        },
        description: "Test cohort for automated testing",
        change_reason: "Creating test cohort",
    };
    
    const testRemoteCohort = {
        dimension: `test-remote-cohort-${Date.now()}`,
        position: 3,
        schema: {
            "type": "string"
        },
        dimension_type: {
            "REMOTE_COHORT": testDimension.dimension
        },
        description: "Test cohort for automated testing",
        change_reason: "Creating test cohort",
    };

    // Track all created dimensions and functions for cleanup
    const createdDimensions: string[] = [];
    let createdDimension: any;
    let validationFunctionName: string;
    let autocompleteFunctionName: string;

    // Clean up after tests
    afterAll(async () => {
        // Clean up all dimensions created during tests
        for (const dimensionName of createdDimensions) {
            try {
                const deleteCmd = new DeleteDimensionCommand({
                    workspace_id: ENV.workspace_id,
                    org_id: ENV.org_id,
                    dimension: dimensionName,
                });
                await superpositionClient.send(deleteCmd);
                console.log(`Cleaned up test dimension: ${dimensionName}`);
            } catch (e) {
                console.error(
                    `Failed to clean up dimension ${dimensionName}: ${e.message}`
                );
            }
        }

        // Try to delete the main test dimension if it exists and wasn't already cleaned up
        if (
            createdDimension &&
            !createdDimensions.includes(createdDimension.dimension)
        ) {
            try {
                const deleteCmd = new DeleteDimensionCommand({
                    workspace_id: ENV.workspace_id,
                    org_id: ENV.org_id,
                    dimension: createdDimension.dimension,
                });
                await superpositionClient.send(deleteCmd);
                console.log(
                    `Cleaned up test dimension: ${createdDimension.dimension}`
                );
            } catch (e) {
                console.error(`Failed to clean up dimension: ${e.message}`);
            }
        }

        // Clean up validation function if it exists
        if (validationFunctionName) {
            try {
                const deleteCmd = new DeleteFunctionCommand({
                    workspace_id: ENV.workspace_id,
                    org_id: ENV.org_id,
                    function_name: validationFunctionName,
                });
                await superpositionClient.send(deleteCmd);
                console.log(
                    `Cleaned up validation function: ${validationFunctionName}`
                );
            } catch (e) {
                console.error(
                    `Failed to clean up validation function: ${e.message}`
                );
            }
        }

        if (autocompleteFunctionName) {
            try {
                const deleteCmd = new DeleteFunctionCommand({
                    workspace_id: ENV.workspace_id,
                    org_id: ENV.org_id,
                    function_name: autocompleteFunctionName,
                });
                await superpositionClient.send(deleteCmd);
                console.log(
                    `Cleaned up validation function: ${autocompleteFunctionName}`
                );
            } catch (e) {
                console.error(
                    `Failed to clean up validation function: ${e.message}`
                );
            }
        }
    });

    // ==================== CREATE DIMENSION TESTS ====================

    test("CreateDimension: should create a new dimension", async () => {
        const input = {
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            ...testDimension,
        };

        const cmd = new CreateDimensionCommand(input);

        try {
            const response = await superpositionClient.send(cmd);
            console.log("Created dimension:", response);

            // Save for later use and cleanup
            createdDimension = response;
            createdDimensions.push(response.dimension);

            // Assertions
            expect(response).toBeDefined();
            expect(response.dimension).toBe(testDimension.dimension);
            expect(response.position).toBe(testDimension.position);
            expect(response.description).toBe(testDimension.description);
            expect(response.created_by).toBeDefined();
            expect(response.created_at).toBeDefined();
            expect(response.last_modified_at).toBeDefined();
            expect(response.last_modified_by).toBeDefined();
        } catch (e) {
            console.error(e["$response"]);
            throw e;
        }
    });

    test("CreateDimension: should handle validation errors", async () => {
        const invalidInput = {
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            dimension: "invalid dimension with spaces",
            position: 2,
            schema: { type: "string" },
            description: "Invalid dimension",
            change_reason: "Testing validation",
        };

        const cmd = new CreateDimensionCommand(invalidInput);

        try {
            await superpositionClient.send(cmd);
            // Should not reach here
            fail("Expected validation error but request succeeded");
        } catch (e) {
            // Expect an error response
            expect(e).toBeDefined();
            console.log("Received expected validation error:", e.message);
        }
    });

    test("CreateDimension: should reject position zero", async () => {
        const invalidPositionInput = {
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            dimension: `test-dimension-reserved-${Date.now()}`,
            position: 0, // Position 0 is reserved and should be rejected
            schema: { type: "string" },
            description: "Test dimension with reserved position",
            change_reason: "Testing position validation",
            function_name: "identity",
        };

        const cmd = new CreateDimensionCommand(invalidPositionInput);

        try {
            await superpositionClient.send(cmd);
            // Should not reach here
            fail(
                "Expected validation error for reserved position but request succeeded"
            );
        } catch (e) {
            // Expect an error response
            expect(e).toBeDefined();
            console.log(
                "Received expected position validation error:",
                e.message
            );
        }
    });

    test("CreateDimension: should reject non primitive type for dimension", async () => {
        if (ENV.jsonlogic_enabled) {
            console.log(
                "Skipping non primitive type update test because JSONLogic is enabled"
            );
            return;
        }
        const invalidPositionInput = {
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            dimension: `test-dimension-non-primitive-${Date.now()}`,
            position: 1,
            schema: { type: "object" },
            description: "Test dimension with non primitive type",
            change_reason: "Testing non primitive validation",
        };

        const cmd = new CreateDimensionCommand(invalidPositionInput);
        expect(superpositionClient.send(cmd)).rejects.toThrow(
            /Invalid schema: expected a primitive type or an array of primitive types/i
        );
    });

    test("CreateDimension: should reject duplicate position", async () => {
        // Fail if dimension wasn't created
        if (!createdDimension) {
            throw new Error(
                "Cannot run duplicate position test because the dimension creation test failed"
            );
        }

        const duplicatePositionInput = {
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            dimension: `test-dimension-duplicate-${Date.now()}`,
            position: createdDimension.position, // Using same position as existing dimension
            schema: { type: "string" },
            description: "Test dimension with duplicate position",
            change_reason: "Testing position uniqueness",
            function_name: "identity",
        };

        const cmd = new CreateDimensionCommand(duplicatePositionInput);

        try {
            await superpositionClient.send(cmd);
            // Should not reach here
            fail(
                "Expected validation error for duplicate position but request succeeded"
            );
        } catch (e) {
            // Expect an error response
            expect(e).toBeDefined();
            console.log(
                "Received expected duplicate position error:",
                e.message
            );
        }
    });

    // ==================== GET DIMENSION TESTS ====================

    test("GetDimension: should retrieve an existing dimension", async () => {
        // Fail if dimension wasn't created
        if (!createdDimension) {
            throw new Error(
                "Cannot run get test because the dimension creation test failed"
            );
        }

        const input = {
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            dimension: createdDimension.dimension,
        };

        const cmd = new GetDimensionCommand(input);

        try {
            const response = await superpositionClient.send(cmd);
            console.log("Retrieved dimension:", response);

            // Assertions
            expect(response).toBeDefined();
            expect(response.dimension).toBe(testDimension.dimension);
            expect(response.position).toBe(testDimension.position);
            expect(response.description).toBe(testDimension.description);
            expect(response.created_by).toBeDefined();
            expect(response.created_at).toBeDefined();
            expect(response.last_modified_at).toBeDefined();
            expect(response.last_modified_by).toBeDefined();
        } catch (e) {
            console.error(e["$response"]);
            throw e;
        }
    });

    test("GetDimension: should handle non-existent dimension", async () => {
        const input = {
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            dimension: "non-existent-dimension-123456789",
        };

        const cmd = new GetDimensionCommand(input);

        try {
            await superpositionClient.send(cmd);
            // Should not reach here
            fail("Expected resource not found error but request succeeded");
        } catch (e: ResourceNotFound) {
            // Expect an error response
            expect(e).toBeDefined();
            console.log("Received expected not found error:", e.message);
        }
    });
    // ==================== LIST DIMENSION TESTS ====================

    test("ListDimensions: should list all dimensions", async () => {
        const input = {
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            count: 10,
            page: 1,
        };

        const cmd = new ListDimensionsCommand(input);

        try {
            const response = await superpositionClient.send(cmd);
            console.log("Dimensions list:", response);

            // Assertions
            expect(response).toBeDefined();
            expect(response.data).toBeDefined();
            expect(Array.isArray(response.data)).toBe(true);
            expect(response.total_items).toBeGreaterThan(0);

            // Verify our created dimension is in the list
            const foundDimension = response.data.find(
                (d) => d.dimension === testDimension.dimension
            );
            expect(foundDimension).toBeDefined();
        } catch (e) {
            console.error(e["$response"]);
            throw e;
        }
    });

    // ==================== UPDATE DIMENSION TESTS ====================

    test("UpdateDimension: should update an existing dimension", async () => {
        // Fail if dimension wasn't created
        if (!createdDimension) {
            throw new Error(
                "Cannot run update test because the dimension creation test failed"
            );
        }

        const updatedDescription = "Updated test dimension description";
        const input = {
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            dimension: createdDimension.dimension,
            description: updatedDescription,
            change_reason: "Updating test dimension",
        };

        const cmd = new UpdateDimensionCommand(input);

        try {
            const response = await superpositionClient.send(cmd);
            console.log("Updated dimension:", response);

            // Update our reference
            createdDimension = response;

            // Assertions
            expect(response).toBeDefined();
            expect(response.dimension).toBe(testDimension.dimension);
            expect(response.description).toBe(updatedDescription);
            expect(response.last_modified_at).toBeDefined();
        } catch (e) {
            console.error(e["$response"]);
            throw e;
        }
    });

    test("UpdateDimension: should reject updating to non primitive type", async () => {
        if (ENV.jsonlogic_enabled) {
            console.log(
                "Skipping non primitive type update test because JSONLogic is enabled"
            );
            return;
        }
        const input = {
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            dimension: createdDimension.dimension,
            schema: { type: "object" }, // Non primitive type
            change_reason: "Testing error handling",
        };

        const cmd = new UpdateDimensionCommand(input);
        expect(superpositionClient.send(cmd)).rejects.toThrow(
            /Invalid schema: expected a primitive type or an array of primitive types/i
        );
    });

    test("UpdateDimension: should handle non-existent dimension", async () => {
        const input = {
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            dimension: "non-existent-dimension-123456789",
            description: "This should fail",
            change_reason: "Testing error handling",
        };

        const cmd = new UpdateDimensionCommand(input);

        try {
            await superpositionClient.send(cmd);
            // Should not reach here
            fail("Expected resource not found error but request succeeded");
        } catch (e: ResourceNotFound) {
            // Expect an error response
            expect(e).toBeDefined();
            console.log("Received expected not found error:", e.message);
        }
    });

    // ==================== VALIDATION FUNCTION TESTS ====================

    test("CreateDimension: should create dimension with validation function", async () => {
        // First create a validation function
        const functionName = `dimension-validator-${Date.now()}`;
        const validationCode = `
            async function validate(key, value) {
                // Simple validation: value must be a string with length between 3 and 20
                if (typeof value !== 'string') {
                    return false;
                }
                return value.length >= 3 && value.length <= 20;
            }
        `;

        const createFunctionCmd = new CreateFunctionCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            function_name: functionName,
            function: validationCode,
            description: "Validation function for dimension test",
            change_reason: "Creating test validation function",
            runtime_version: "1",
            function_type: FunctionTypes.Validation,
        });

        try {
            const functionResponse = await superpositionClient.send(
                createFunctionCmd
            );
            console.log("Created validation function:", functionResponse);
            validationFunctionName = functionResponse.function_name;

            // Now create a dimension that uses this validation function
            const validatedDimension = {
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                dimension: `validated-dimension-${Date.now()}`,
                position: 2,
                schema: { type: "string" },
                description: "Dimension with validation function",
                change_reason: "Testing validation function",
                function_name: validationFunctionName,
            };

            const createDimensionCmd = new CreateDimensionCommand(
                validatedDimension
            );
            const dimensionResponse = await superpositionClient.send(
                createDimensionCmd
            );

            console.log(
                "Created dimension with validation:",
                dimensionResponse
            );

            // Add to cleanup list
            createdDimensions.push(dimensionResponse.dimension);

            // Assertions
            expect(dimensionResponse).toBeDefined();
            expect(dimensionResponse.dimension).toBe(
                validatedDimension.dimension
            );
            expect(dimensionResponse.function_name).toBe(
                validationFunctionName
            );
        } catch (e) {
            console.error(e["$response"]);
            throw e;
        }
    });

    test("CreateDimension: should create dimension with autocomplete function", async () => {
        // First create a validation function
        const functionName = `dimension-completor-${Date.now()}`;
        const autocompleteCode = `
            async function autocomplete(name, prefix, environment) {
                return ["hello", "world"];
            }
        `;

        const createFunctionCmd = new CreateFunctionCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            function_name: functionName,
            function: autocompleteCode,
            description: "autocomplete function for dimension test",
            change_reason: "Creating test autocomplete function",
            runtime_version: "1",
            function_type: FunctionTypes.Autocomplete,
        });

        try {
            const functionResponse = await superpositionClient.send(
                createFunctionCmd
            );
            console.log("Created autocomplete function:", functionResponse);
            autocompleteFunctionName = functionResponse.function_name;
            await superpositionClient.send(
                new PublishCommand({
                    workspace_id: ENV.workspace_id,
                    org_id: ENV.org_id,
                    function_name: functionResponse.function_name,
                    change_reason:
                        "Publishing autocomplete function for dimension test",
                })
            );
            // Now create a dimension that uses this validation function
            const validatedDimension = {
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                dimension: `validated-dimension-${Date.now()}`,
                position: 2,
                schema: { type: "string" },
                description: "Dimension with validation function",
                change_reason: "Testing validation function",
                autocomplete_function_name: autocompleteFunctionName,
            };

            const createDimensionCmd = new CreateDimensionCommand(
                validatedDimension
            );
            const dimensionResponse = await superpositionClient.send(
                createDimensionCmd
            );

            console.log(
                "Created dimension with validation:",
                dimensionResponse
            );

            // Add to cleanup list
            createdDimensions.push(dimensionResponse.dimension);

            // Assertions
            expect(dimensionResponse).toBeDefined();
            expect(dimensionResponse.dimension).toBe(
                validatedDimension.dimension
            );
            expect(dimensionResponse.autocomplete_function_name).toBe(
                autocompleteFunctionName
            );
        } catch (e) {
            console.error(e["$response"]);
            throw e;
        }
    });
    
    // Cohort dimension tests
    
    test("fail2create a local cohort dimension without cohort_based_on", async () => {
        const wrongCohort = {
            dimension: `test-cohort-${Date.now()}`,
            position: 2, // Position 0 is reserved, start from 1
            schema:{
                "type": "string",
                "enum": ["small", "big", "otherwise"],
                "definitions": {
                    small: {
                        and: [
                            {
                                "==": [
                                    {
                                        var: testDimension.dimension,
                                    },
                                    "hdfc",
                                ],
                            },
                        ],
                    },
                    big: {
                        and: [
                            {
                                "==": [
                                    {
                                        var: testDimension.dimension,
                                    },
                                    "kotak",
                                ],
                            },
                        ],
                    },
                }
            },
            dimension_type: {
                "LOCAL_COHORT": ""            
            },
            description: "Test cohort for automated testing",
            change_reason: "Creating test cohort",
        };
        const input = {
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            ...wrongCohort,
        };
        expect(
            superpositionClient.send(new CreateDimensionCommand(input)),
        ).rejects.toThrow(
            `the cohort_based_on field is mandatory for creating cohorts`,
        );
    });

    test("fail2create a local cohort dimension with cohort_based_on mismatch", async () => {
        const wrongCohort = {
            dimension: `test-cohort-${Date.now()}`,
            position: 2, // Position 0 is reserved, start from 1
            schema: {
                "type": "string",
                "enum": ["small", "big", "otherwise"],
                "definitions": {
                    small: {
                        and: [
                            {
                                "==": [
                                    {
                                        var: testDimension.dimension,
                                    },
                                    "hdfc",
                                ],
                            },
                        ],
                    },
                }
            },
            dimension_type: {
                "LOCAL_COHORT": "random_dimension"
            },
            description: "Test cohort for automated testing",
            change_reason: "Creating test cohort",
        };
        const input = {
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            ...wrongCohort,
        };
        expect(
            superpositionClient.send(new CreateDimensionCommand(input)),
        ).rejects.toThrow(
            `Dimension ${testDimension.dimension} used in cohort schema does not match the cohort_based_on field`,
        );
    });

    test("fail2create a local cohort dimension with an invalid dimension", async () => {
        const wrongCohort = {
            dimension: `test-cohort-${Date.now()}`,
            position: 2, // Position 0 is reserved, start from 1
            schema: {
                "type": "string",
                "enum": ["small", "big", "otherwise"],
                "definitions": {
                    small: {
                        and: [
                            {
                                "==": [
                                    {
                                        var: "sdk",
                                    },
                                    "hdfc",
                                ],
                            },
                        ],
                    },
                    big: {
                        and: [
                            {
                                "==": [
                                    {
                                        var: "sdk",
                                    },
                                    "kotak",
                                ],
                            },
                        ],
                    },
                }
            },
            dimension_type: {
                "LOCAL_COHORT": "sdk"
            },
            description: "Test cohort for automated testing",
            change_reason: "Creating test cohort",
        };

        const input = {
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            ...wrongCohort,
        };

        expect(
            superpositionClient.send(new CreateDimensionCommand(input)),
        ).rejects.toThrow(
            "Dimension sds used in cohort schema does not exist in dimensions table",
        );
    });
    
    test("fail2create a local cohort dimension with definition mismatch", async () => {
        const wrongCohort = {
            dimension: `test-cohort-${Date.now()}`,
            position: 2, // Position 0 is reserved, start from 1
            schema: {
                "type": "string",
                "enum": ["small", "otherwise"],
                "definitions": {
                    small: {
                        and: [
                            {
                                "==": [
                                    {
                                        var: "sdk",
                                    },
                                    "hdfc",
                                ],
                            },
                        ],
                    },
                    big: {
                        and: [
                            {
                                "==": [
                                    {
                                        var: "sdk",
                                    },
                                    "kotak",
                                ],
                            },
                        ],
                    },
                }
            },
            dimension_type: {
                "LOCAL_COHORT": "sdk"
            },
            description: "Test cohort for automated testing",
            change_reason: "Creating test cohort",
        };

        const input = {
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            ...wrongCohort,
        };

        expect(
            superpositionClient.send(new CreateDimensionCommand(input)),
        ).rejects.toThrow(
            "",
        );
    });
    
    test("fail2create a local cohort dimension with multiple dimensions", async () => {
        const wrongCohort = {
            dimension: `test-cohort-${Date.now()}`,
            position: 2, // Position 0 is reserved, start from 1
            schema: {
                "type": "string",
                "enum": ["small", "big"],
                "definitions": {
                    small: {
                        and: [
                            {
                                "==": [
                                    {
                                        var: testDimension.dimension,
                                    },
                                    "hdfc",
                                ],
                            },
                        ],
                    },
                    big: {
                        and: [
                            {
                                "==": [
                                    {
                                        var: "wrong-dimension",
                                    },
                                    "sd",
                                ],
                            },
                        ],
                    },
                }
            },
            dimension_type: {
                "LOCAL_COHORT": testDimension.dimension
            },
            description: "Test cohort for automated testing",
            change_reason: "Creating test cohort",
        };

        const input = {
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            ...wrongCohort,
        };

        expect(
            superpositionClient.send(new CreateDimensionCommand(input)),
        ).rejects.toThrow(
            /Multiple dimensions used in cohort schema and that is not allowed: .* /,
        );
    });

    test("create a local cohort dimension", async () => {
        const input = {
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            ...testLocalCohort,
        };

        const cmd = new CreateDimensionCommand(input);

        try {
            const response = await superpositionClient.send(cmd);
            console.log("Created dimension:", response);

            // Save for later use and cleanup
            createdDimension = response;
            createdDimensions.push(response.dimension);

            // Assertions
            expect(response).toBeDefined();
            expect(response.dimension).toBe(testLocalCohort.dimension);
            expect(response.position).toBe(testLocalCohort.position);
            expect(response.description).toBe(testLocalCohort.description);
            expect(response.created_by).toBeDefined();
            expect(response.created_at).toBeDefined();
            expect(response.last_modified_at).toBeDefined();
            expect(response.last_modified_by).toBeDefined();
        } catch (e) {
            console.error(e["$response"]);
            throw e;
        }
    });

    test("fail2create a remote cohort dimension on a local cohort", async () => {
        const wrongCohort = {
            dimension: `test-remote-cohort-${Date.now()}`,
            position: 3,
            schema: {
                "type": "string"
            },
            cohort_based_on: testLocalCohort.dimension,
            dimension_type: {
                "REMOTE_COHORT": testLocalCohort.dimension
            },
            description: "Test cohort for automated testing",
            change_reason: "Creating test cohort",
        };
        const input = {
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            ...wrongCohort,
        };

        expect(
            superpositionClient.send(new CreateDimensionCommand(input)),
        ).rejects.toThrow(
            `Cohort dimension cannot be based on another cohort dimension ${testLocalCohort.dimension}`,
        );
    });
    
    test("create a remote cohort dimension", async () => {
        const input = {
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            ...testRemoteCohort,
        };

        const cmd = new CreateDimensionCommand(input);

        try {
            const response = await superpositionClient.send(cmd);
            console.log("Created dimension:", response);

            // Save for later use and cleanup
            createdDimension = response;
            createdDimensions.push(response.dimension);

            // Assertions
            expect(response).toBeDefined();
            expect(response.dimension).toBe(testRemoteCohort.dimension);
            expect(response.position).toBe(testRemoteCohort.position);
            expect(response.description).toBe(testRemoteCohort.description);
            expect(response.created_by).toBeDefined();
            expect(response.created_at).toBeDefined();
            expect(response.last_modified_at).toBeDefined();
            expect(response.last_modified_by).toBeDefined();
        } catch (e) {
            console.error(e["$response"]);
            throw e;
        }
    });
    
    test("fail2create a local cohort dimension on a remote dimension", async () => {
        const wrongCohort = {
            dimension: `test-cohort-remote-${Date.now()}`,
            position: 4, // Position 0 is reserved, start from 1
            schema: {
                "type": "string",
                "enum": ["small", "big", "otherwise"],
                "definitions": {
                    small: {
                        and: [
                            {
                                "==": [
                                    {
                                        var: testRemoteCohort.dimension,
                                    },
                                    "hdfc",
                                ],
                            },
                        ],
                    },
                    big: {
                        and: [
                            {
                                "==": [
                                    {
                                        var: testRemoteCohort.dimension,
                                    },
                                    "kotak",
                                ],
                            },
                        ],
                    },
                }
            },
            dimension_type: {
                "LOCAL_COHORT": testRemoteCohort.dimension
            },
            description: "Test cohort for automated testing",
            change_reason: "Creating test cohort",
        };

        const input = {
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            ...wrongCohort,
        };

        expect(
            superpositionClient.send(new CreateDimensionCommand(input)),
        ).rejects.toThrow(
            "",
        );
    });

    // ==================== DELETE DIMENSION TESTS ====================

    // TODO: Enable this test after fixing delete dimension handler
    // test("DeleteDimension: should delete a dimension", async () => {
    //     // Fail if dimension wasn't created
    //     if (!createdDimension) {
    //         throw new Error(
    //             "Cannot run delete test because the dimension creation test failed"
    //         );
    //     }

    //     const input = {
    //         workspace_id: ENV.workspace_id,
    //         org_id: ENV.org_id,
    //         dimension: createdDimension.dimension,
    //     };

    //     const cmd = new DeleteDimensionCommand(input);

    //     try {
    //         await superpositionClient.send(cmd);
    //         console.log(`Deleted dimension: ${createdDimension.dimension}`);

    //         // Verify deletion by trying to list and find the dimension
    //         const listCmd = new ListDimensionsCommand({
    //             workspace_id: ENV.workspace_id,
    //             org_id: ENV.org_id,
    //             count: 100,
    //             page: 1,
    //         });

    //         const listResponse = await superpositionClient.send(listCmd);
    //         const foundDimension = listResponse.data.find(
    //             (d) => d.dimension === testDimension.dimension
    //         );

    //         expect(foundDimension).toBeUndefined();

    //         // Clear reference since we've deleted it
    //         createdDimension = null;
    //         // Remove from the cleanup array since we've already deleted it
    //         const index = createdDimensions.indexOf(input.dimension);
    //         if (index > -1) {
    //             createdDimensions.splice(index, 1);
    //         }
    //     } catch (e) {
    //         console.debug("tried deleting: ", createdDimension.dimension);
    //         console.error(e["$response"]);
    //         throw e;
    //     }
    // });
});
