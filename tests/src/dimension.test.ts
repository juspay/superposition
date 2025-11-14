import {
    CreateDimensionCommand,
    DeleteDimensionCommand,
    GetDimensionCommand,
    ListDimensionsCommand,
    UpdateDimensionCommand,
    CreateFunctionCommand,
    DeleteFunctionCommand,
    FunctionTypes,
    PublishCommand,
} from "@juspay/superposition-sdk";
import { superpositionClient, ENV } from "../env.ts";
import { describe, afterAll, test, expect, beforeAll } from "bun:test";

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
        dimension: `test-local-cohort-${Date.now()}`,
        position: testDimension.position,
        schema: {
            type: "string",
            enum: ["small", "big", "otherwise"],
            definitions: {
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
            },
        },
        dimension_type: {
            LOCAL_COHORT: testDimension.dimension,
        },
        description: "Test cohort for automated testing",
        change_reason: "Creating test cohort",
    };

    const testRemoteCohort = {
        dimension: `test-remote-cohort-${Date.now()}`,
        position: testDimension.position,
        schema: {
            type: "string",
        },
        dimension_type: {
            REMOTE_COHORT: testDimension.dimension,
        },
        description: "Test cohort for automated testing",
        change_reason: "Creating test cohort",
    };

    // Track all created dimensions and functions for cleanup
    const createdDimensions: string[] = [];
    let createdDimension: any;
    let valueValidationFunctionName: string;
    let valueComputeFunctionName: string;

    beforeAll(async () => {
        const functionName = `dimension-validator-${Date.now()}`;
        const valueValidationCode = `
            async function validate_value(key, value) {
                // Simple value_validation: value must be a string with length between 3 and 20
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
            function: valueValidationCode,
            description: "Value Validation function for dimension test",
            change_reason: "Creating test value validation function",
            runtime_version: "1",
            function_type: FunctionTypes.VALUE_VALIDATION,
        });

        try {
            const functionResponse = await superpositionClient.send(
                createFunctionCmd
            );
            console.log("Created value validation function:", functionResponse);
            valueValidationFunctionName =
                functionResponse.function_name ?? functionName;
        } catch (e: any) {
            console.error(e["$response"]);
            throw e;
        }

        const functionNameAC = `dimension-completor-${Date.now()}`;
        const valueComputeCode = `
            async function value_compute(name, prefix, environment) {
                return ["hello", "world"];
            }
        `;

        const createFunctionCmdAC = new CreateFunctionCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            function_name: functionNameAC,
            function: valueComputeCode,
            description: "value compute function for dimension test",
            change_reason: "Creating test value compute function",
            runtime_version: "1",
            function_type: FunctionTypes.VALUE_COMPUTE,
        });

        try {
            const functionResponse = await superpositionClient.send(
                createFunctionCmdAC
            );
            console.log("Created value compute function:", functionResponse);
            valueComputeFunctionName =
                functionResponse.function_name ?? functionNameAC;
        } catch (e: any) {
            console.error(e["$response"]);
            throw e;
        }
    });
    // Clean up after tests
    afterAll(async () => {
        // Clean up all dimensions created during tests
        for (const dimensionName of createdDimensions.reverse()) {
            try {
                const deleteCmd = new DeleteDimensionCommand({
                    workspace_id: ENV.workspace_id,
                    org_id: ENV.org_id,
                    dimension: dimensionName,
                });
                await superpositionClient.send(deleteCmd);
                console.log(`Cleaned up test dimension: ${dimensionName}`);
            } catch (e: any) {
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
            } catch (e: any) {
                console.error(`Failed to clean up dimension: ${e.message}`);
            }
        }

        // Clean up value validation function if it exists
        if (valueValidationFunctionName) {
            try {
                const deleteCmd = new DeleteFunctionCommand({
                    workspace_id: ENV.workspace_id,
                    org_id: ENV.org_id,
                    function_name: valueValidationFunctionName,
                });
                await superpositionClient.send(deleteCmd);
                console.log(
                    `Cleaned up value validation function: ${valueValidationFunctionName}`
                );
            } catch (e: any) {
                console.error(
                    `Failed to clean up value validation function: ${e.message}`
                );
            }
        }

        if (valueComputeFunctionName) {
            try {
                const deleteCmd = new DeleteFunctionCommand({
                    workspace_id: ENV.workspace_id,
                    org_id: ENV.org_id,
                    function_name: valueComputeFunctionName,
                });
                await superpositionClient.send(deleteCmd);
                console.log(
                    `Cleaned up value validation function: ${valueComputeFunctionName}`
                );
            } catch (e: any) {
                console.error(
                    `Failed to clean up value validation function: ${e.message}`
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
            createdDimensions.push(
                response.dimension ?? testDimension.dimension
            );

            // Assertions
            expect(response).toBeDefined();
            expect(response.dimension).toBe(testDimension.dimension);
            expect(response.position).toBe(testDimension.position);
            expect(response.description).toBe(testDimension.description);
            expect(response.created_by).toBeDefined();
            expect(response.created_at).toBeDefined();
            expect(response.last_modified_at).toBeDefined();
            expect(response.last_modified_by).toBeDefined();
        } catch (e: any) {
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

        expect(superpositionClient.send(cmd)).rejects.toThrow(
            /JSON Parse error: Unexpected identifier /
        );
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
            value_validation_function_name: "identity",
        };

        const cmd = new CreateDimensionCommand(invalidPositionInput);

        expect(superpositionClient.send(cmd)).rejects.toThrow(
            "Oth position is reserved for variantIds"
        );
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

    test("CreateDimension: should reject invalid function", async () => {
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
            value_validation_function_name: "identity",
        };

        const cmd = new CreateDimensionCommand(duplicatePositionInput);

        expect(superpositionClient.send(cmd)).rejects.toThrow(
            "No records found. Please refine or correct your search parameters"
        );
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
        } catch (e: any) {
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

        expect(superpositionClient.send(cmd)).rejects.toThrow(
            "No records found. Please refine or correct your search parameters"
        );
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
            const foundDimension = response.data?.find(
                (d) => d.dimension === testDimension.dimension
            );
            expect(foundDimension).toBeDefined();
        } catch (e: any) {
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
        } catch (e: any) {
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

        expect(superpositionClient.send(cmd)).rejects.toThrow(
            "No records found. Please refine or correct your search parameters"
        );
    });

    // ==================== VALUE VALIDATION FUNCTION TESTS ====================

    test("CreateDimension: should fail to create dimension with value validation function which is not published", async () => {
        try {
            const validatedDimension = {
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                dimension: `validated-dimension-${Date.now()}`,
                position: 2,
                schema: { type: "string" },
                description: "Dimension with value validation function",
                change_reason: "Testing value validation function",
                value_validation_function_name: valueValidationFunctionName,
            };

            const dimensionCmd = new CreateDimensionCommand(validatedDimension);

            // Assertions
            expect(superpositionClient.send(dimensionCmd)).rejects.toThrow(
                `Function ${valueValidationFunctionName} doesn't exist / function code not published yet.`
            );
        } catch (e: any) {
            console.error(e["$response"]);
            throw e;
        }
    });

    test("CreateDimension: should create dimension with value validation function", async () => {
        try {
            // Now create a dimension that uses this value validation function
            const dimension = `validated-dimension-${Date.now()}`;
            const validatedDimension = {
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                dimension,
                position: 2,
                schema: { type: "string" },
                description: "Dimension with value validation function",
                change_reason: "Testing value validation function",
                value_validation_function_name: valueValidationFunctionName,
            };
            await superpositionClient.send(
                new PublishCommand({
                    workspace_id: ENV.workspace_id,
                    org_id: ENV.org_id,
                    function_name: valueValidationFunctionName,
                    change_reason:
                        "Publishing value validation function for dimension test",
                })
            );

            const createDimensionCmd = new CreateDimensionCommand(
                validatedDimension
            );
            const dimensionResponse = await superpositionClient.send(
                createDimensionCmd
            );

            console.log(
                "Created dimension with value validation:",
                dimensionResponse
            );

            // Add to cleanup list
            createdDimensions.push(dimensionResponse.dimension ?? dimension);

            // Assertions
            expect(dimensionResponse).toBeDefined();
            expect(dimensionResponse.dimension).toBe(
                validatedDimension.dimension
            );
            expect(dimensionResponse.value_validation_function_name).toBe(
                valueValidationFunctionName
            );
        } catch (e: any) {
            console.error(e["$response"]);
            throw e;
        }
    });

    test("CreateDimension: should fail to create dimension with value compute function which is not published", async () => {
        try {
            const validatedDimension = {
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                dimension: `validated-dimension-${Date.now()}`,
                position: 2,
                schema: { type: "string" },
                description: "Dimension with value compute function",
                change_reason: "Testing value compute function",
                value_compute_function_name: valueComputeFunctionName,
            };

            const dimensionCmd = new CreateDimensionCommand(validatedDimension);

            // Assertions
            expect(superpositionClient.send(dimensionCmd)).rejects.toThrow(
                `Function ${valueComputeFunctionName} doesn't exist / function code not published yet.`
            );
        } catch (e: any) {
            console.error(e["$response"]);
            throw e;
        }
    });

    test("CreateDimension: should create dimension with value compute function", async () => {
        try {
            await superpositionClient.send(
                new PublishCommand({
                    workspace_id: ENV.workspace_id,
                    org_id: ENV.org_id,
                    function_name: valueComputeFunctionName,
                    change_reason:
                        "Publishing value compute function for dimension test",
                })
            );
            const dimension = `validated-dimension-${Date.now()}`;
            // Now create a dimension that uses this value validation function
            const validatedDimension = {
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                dimension,
                position: 2,
                schema: { type: "string" },
                description: "Dimension with value compute function",
                change_reason: "Testing value compute function",
                value_compute_function_name: valueComputeFunctionName,
            };

            const createDimensionCmd = new CreateDimensionCommand(
                validatedDimension
            );
            const dimensionResponse = await superpositionClient.send(
                createDimensionCmd
            );

            console.log(
                "Created dimension with value compute:",
                dimensionResponse
            );

            // Add to cleanup list
            createdDimensions.push(dimensionResponse.dimension ?? dimension);

            // Assertions
            expect(dimensionResponse).toBeDefined();
            expect(dimensionResponse.dimension).toBe(
                validatedDimension.dimension
            );
            expect(dimensionResponse.value_compute_function_name).toBe(
                valueComputeFunctionName
            );
        } catch (e: any) {
            console.error(e["$response"]);
            throw e;
        }
    });

    // Cohort dimension tests

    test("fail2create a local cohort dimension without cohort_based_on", async () => {
        const wrongCohort = {
            dimension: `test-cohort-${Date.now()}`,
            position: 2, // Position 0 is reserved, start from 1
            schema: {
                type: "string",
                enum: ["small", "big", "otherwise"],
                definitions: {
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
                },
            },
            dimension_type: {
                LOCAL_COHORT: "",
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
            superpositionClient.send(new CreateDimensionCommand(input))
        ).rejects.toThrow(
            `Please specify a valid dimension that this cohort can derive from. Refer our API docs for examples`
        );
    });

    test("fail2create a local cohort dimension without definitions", async () => {
        const wrongCohort = {
            dimension: `test-cohort-${Date.now()}`,
            position: 2, // Position 0 is reserved, start from 1
            schema: {
                type: "string",
                enum: ["small", "big", "otherwise"],
            },
            dimension_type: {
                LOCAL_COHORT: "random_dimension",
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
            superpositionClient.send(new CreateDimensionCommand(input))
        ).rejects.toThrow(
            'schema validation failed: required property `"definitions"` is missing'
        );
    });

    test("fail2create a local cohort dimension with cohort_based_on mismatch", async () => {
        const wrongCohort = {
            dimension: `test-cohort-${Date.now()}`,
            position: 2, // Position 0 is reserved, start from 1
            schema: {
                type: "string",
                enum: ["small", "big", "otherwise"],
                definitions: {
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
                },
            },
            dimension_type: {
                LOCAL_COHORT: "random_dimension",
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
            superpositionClient.send(new CreateDimensionCommand(input))
        ).rejects.toThrow(
            `The definition of the cohort and the enum options do not match. Some enum options do not have a definition, found 1 cohorts and 2 enum options (not including otherwise)`
        );
    });

    test("fail2create a local cohort dimension with an invalid dimension", async () => {
        const wrongCohort = {
            dimension: `test-cohort-${Date.now()}`,
            position: 2, // Position 0 is reserved, start from 1
            schema: {
                type: "string",
                enum: ["small", "big", "otherwise"],
                definitions: {
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
                },
            },
            dimension_type: {
                LOCAL_COHORT: "sdk",
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
            superpositionClient.send(new CreateDimensionCommand(input))
        ).rejects.toThrow(
            "Dimension sdk used in cohort schema has not been created or does not exist. Please create the dimension first before using it in cohort schema."
        );
    });

    test("fail2create a local cohort dimension with definition/enum mismatch", async () => {
        const wrongCohort = {
            dimension: `test-cohort-${Date.now()}`,
            position: 2, // Position 0 is reserved, start from 1
            schema: {
                type: "string",
                enum: ["small", "otherwise"],
                definitions: {
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
                },
            },
            dimension_type: {
                LOCAL_COHORT: "sdk",
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
            superpositionClient.send(new CreateDimensionCommand(input))
        ).rejects.toThrow(
            "The definition of the cohort and the enum options do not match. Some enum options do not have a definition, found 2 cohorts and 1 enum options (not including otherwise)"
        );
    });

    test("fail2create a local cohort dimension without enum otherwise", async () => {
        const wrongCohort = {
            dimension: `test-cohort-${Date.now()}`,
            position: 2, // Position 0 is reserved, start from 1
            schema: {
                type: "string",
                enum: ["small", "big"],
                definitions: {
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
                                    "sd",
                                ],
                            },
                        ],
                    },
                },
            },
            dimension_type: {
                LOCAL_COHORT: testDimension.dimension,
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
            superpositionClient.send(new CreateDimensionCommand(input))
        ).rejects.toThrow(
            "schema validation failed: array doesn't contain items conforming to the specified schema"
        );
    });

    test("fail2create a local cohort dimension with multiple dimensions", async () => {
        const wrongCohort = {
            dimension: `test-local-cohort-${Date.now()}`,
            position: 2, // Position 0 is reserved, start from 1
            schema: {
                type: "string",
                enum: ["small", "big", "otherwise"],
                definitions: {
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
                },
            },
            dimension_type: {
                LOCAL_COHORT: testDimension.dimension,
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
            superpositionClient.send(new CreateDimensionCommand(input))
        ).rejects.toThrow(
            /Multiple dimensions used in cohort schema and that is not allowed: .* /
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
            createdDimensions.push(
                response.dimension ?? testLocalCohort.dimension
            );

            // Assertions
            expect(response).toBeDefined();
            expect(response.dimension).toBe(testLocalCohort.dimension);
            expect(response.position).toBe(testLocalCohort.position);
            expect(response.description).toBe(testLocalCohort.description);
            expect(response.created_by).toBeDefined();
            expect(response.created_at).toBeDefined();
            expect(response.last_modified_at).toBeDefined();
            expect(response.last_modified_by).toBeDefined();
        } catch (e: any) {
            console.error(e["$response"]);
            throw e;
        }
    });

    test("fail to create a remote cohort dimension on a local cohort", async () => {
        const wrongCohort = {
            dimension: `test-remote-cohort-${Date.now()}`,
            position: testLocalCohort.position,
            schema: {
                type: "string",
            },
            cohort_based_on: testLocalCohort.dimension,
            dimension_type: {
                REMOTE_COHORT: testLocalCohort.dimension,
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
            superpositionClient.send(new CreateDimensionCommand(input))
        ).rejects.toThrow(
            `Dimension ${testLocalCohort.dimension} is a local cohort and cannot be used in cohorting`
        );
    });

    test("create a remote cohort dimension", async () => {
        const input = {
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            value_compute_function_name: valueComputeFunctionName,
            ...testRemoteCohort,
        };

        const cmd = new CreateDimensionCommand(input);

        try {
            const response = await superpositionClient.send(cmd);
            console.log("Created dimension:", response);

            // Save for later use and cleanup
            createdDimension = response;
            createdDimensions.push(
                response.dimension ?? testRemoteCohort.dimension
            );

            // Assertions
            expect(response).toBeDefined();
            expect(response.dimension).toBe(testRemoteCohort.dimension);
            expect(response.position).toBe(testRemoteCohort.position);
            expect(response.description).toBe(testRemoteCohort.description);
            expect(response.created_by).toBeDefined();
            expect(response.created_at).toBeDefined();
            expect(response.last_modified_at).toBeDefined();
            expect(response.last_modified_by).toBeDefined();
            expect(response.value_compute_function_name).toBe(
                valueComputeFunctionName
            );
        } catch (e: any) {
            console.error(e["$response"]);
            throw e;
        }
    });

    test("fail to update a remote cohort with position >= based on position", async () => {
        const basedOnDimensionInput = {
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            dimension: testRemoteCohort.dimension_type.REMOTE_COHORT,
        };

        const basedOnDimension = await superpositionClient.send(
            new GetDimensionCommand(basedOnDimensionInput)
        );

        const input = {
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            dimension: testRemoteCohort.dimension,
            position: (basedOnDimension.position ?? 0) + 1,
            change_reason: "Updating position to invalid value",
        };
        try {
            expect(
                superpositionClient.send(new UpdateDimensionCommand(input))
            ).rejects.toThrow(
                `While updating dimension, Cohort dimension position ${input.position} must be less than the position ${basedOnDimension.position} of the dimension it is based on`
            );
        } catch (e: any) {
            console.error(e["$response"]);
            throw e;
        }
    });

    test("create a local cohort dimension on a remote dimension", async () => {
        const cohortData = {
            dimension: `test-cohort-local-${Date.now()}`,
            position: testRemoteCohort.position, // Position 0 is reserved, start from 1
            schema: {
                type: "string",
                enum: ["small", "big", "otherwise"],
                definitions: {
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
                },
            },
            dimension_type: {
                LOCAL_COHORT: testRemoteCohort.dimension,
            },
            description: "Test cohort for automated testing",
            change_reason: "Creating test cohort",
        };

        const input = {
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            ...cohortData,
        };

        expect(
            superpositionClient.send(new CreateDimensionCommand(input))
        ).resolves.toBeDefined();
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
    //     } catch (e: any) {
    //         console.debug("tried deleting: ", createdDimension.dimension);
    //         console.error(e["$response"]);
    //         throw e;
    //     }
    // });
});
