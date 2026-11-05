import {
    CreateFunctionCommand,
    ListFunctionCommand,
    GetFunctionCommand,
    UpdateFunctionCommand,
    DeleteFunctionCommand,
    PublishCommand,
    TestCommand,
    FunctionTypes,
    FunctionRuntimeVersion,
    Stage,
} from "@juspay/superposition-sdk";
import { expect, describe, it, afterAll } from "bun:test";
import { ENV, superpositionClient } from "../env.ts";

describe("Function Operations", () => {
    let valueValidationFunctionName: string;
    let valueComputeFunctionName: string;

    function valueValidationTest(
        functionName: string,
        stage: Stage,
        key: string,
        value: string,
    ) {
        return new TestCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            function_name: functionName,
            stage,
            request: {
                value_validate: {
                    key,
                    value,
                    type: "Dimension",
                    environment: { context: {}, overrides: {} },
                },
            },
        });
    }

    function valueComputeTest(
        functionName: string,
        stage: Stage,
        name: string,
        prefix: string,
    ) {
        return new TestCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            function_name: functionName,
            stage,
            request: {
                value_compute: {
                    name,
                    prefix,
                    type: "Dimension",
                    environment: { context: {}, overrides: {} },
                },
            },
        });
    }

    // Value Validation Function Tests
    it("should create and test value_validation function", async () => {
        const valueValidationCode = `
            async function execute(payload) {
                let value = payload.value_validate.value;
                let key = payload.value_validate.key;
                if (key === "test-dimension" && value === "valid") {
                    return true;
                }
                return false;
            }
        `;

        const createCommand = new CreateFunctionCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            function_name: "test-value-validation",
            function: valueValidationCode,
            description: "Test value_validation function",
            change_reason: "Initial creation",
            runtime_version: FunctionRuntimeVersion.V1,
            function_type: FunctionTypes.VALUE_VALIDATION,
        });

        try {
            const createResponse =
                await superpositionClient.send(createCommand);
            valueValidationFunctionName = createResponse.function_name ?? "";

            const getCommand = new GetFunctionCommand({
                org_id: ENV.org_id,
                workspace_id: ENV.workspace_id,
                function_name: valueValidationFunctionName,
            });

            const getResponse = await superpositionClient.send(getCommand);

            expect(getResponse.function_name).toBe(valueValidationFunctionName);
            expect(getResponse.function_type).toBe(
                FunctionTypes.VALUE_VALIDATION,
            );
        } catch (error) {
            console.error(error);
            throw error;
        }
    });

    // Value Compute Function Tests
    it("should create and test value_compute function", async () => {
        const valueComputeCode = `
            async function execute(payload) {
                let name = payload.value_compute.name;
                let prefix = payload.value_compute.prefix;
                if (name === "test-dimension" && prefix === "t") {
                    return ["test1", "test2", "test3"];
                }
                return [];
            }
        `;

        const createCommand = new CreateFunctionCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            function_name: "test-value-compute",
            function: valueComputeCode,
            description: "Test value_compute function",
            change_reason: "Initial creation",
            runtime_version: FunctionRuntimeVersion.V1,
            function_type: FunctionTypes.VALUE_COMPUTE,
        });

        try {
            const createResponse =
                await superpositionClient.send(createCommand);
            valueComputeFunctionName = createResponse.function_name ?? "";

            const getCommand = new GetFunctionCommand({
                org_id: ENV.org_id,
                workspace_id: ENV.workspace_id,
                function_name: valueComputeFunctionName,
            });

            const getResponse = await superpositionClient.send(getCommand);

            expect(getResponse.function_name).toBe(valueComputeFunctionName);
            expect(getResponse.function_type).toBe(FunctionTypes.VALUE_COMPUTE);
        } catch (error) {
            console.error(error);
            throw error;
        }
    });

    // Test endpoint tests
    it("should test draft value_validation function", async () => {
        try {
            const validResponse = await superpositionClient.send(
                valueValidationTest(
                    valueValidationFunctionName,
                    Stage.DRAFT,
                    "test-dimension",
                    "valid",
                ),
            );
            expect(validResponse.fn_output).toBe(true);
            expect(validResponse.function_type).toBe(
                FunctionTypes.VALUE_VALIDATION,
            );
            expect(typeof validResponse.stdout).toBe("string");

            const invalidResponse = await superpositionClient.send(
                valueValidationTest(
                    valueValidationFunctionName,
                    Stage.DRAFT,
                    "test-dimension",
                    "invalid",
                ),
            );
            expect(invalidResponse.fn_output).toBe(false);
        } catch (error) {
            console.error(error);
            throw error;
        }
    });

    it("should test draft value_compute function", async () => {
        try {
            const matchResponse = await superpositionClient.send(
                valueComputeTest(
                    valueComputeFunctionName,
                    Stage.DRAFT,
                    "test-dimension",
                    "t",
                ),
            );
            expect(matchResponse.fn_output).toEqual(["test1", "test2", "test3"]);
            expect(matchResponse.function_type).toBe(
                FunctionTypes.VALUE_COMPUTE,
            );

            const noMatchResponse = await superpositionClient.send(
                valueComputeTest(
                    valueComputeFunctionName,
                    Stage.DRAFT,
                    "other-dimension",
                    "t",
                ),
            );
            expect(noMatchResponse.fn_output).toEqual([]);
        } catch (error) {
            console.error(error);
            throw error;
        }
    });

    it("should fail to test published stage of unpublished function", async () => {
        await expect(
            superpositionClient.send(
                valueValidationTest(
                    valueValidationFunctionName,
                    Stage.PUBLISHED,
                    "test-dimension",
                    "valid",
                ),
            ),
        ).rejects.toThrow("function not published yet");
    });

    it("should fail to test non-existent function", async () => {
        await expect(
            superpositionClient.send(
                valueValidationTest(
                    "non-existent-function",
                    Stage.DRAFT,
                    "test-dimension",
                    "valid",
                ),
            ),
        ).rejects.toThrow();
    });

    it("should capture stdout and surface errors when testing a function", async () => {
        const functionName = "test-function-stdout-and-error";
        const code = `
            async function execute(payload) {
                let value = payload.value_validate.value;
                console.log("validating value:", value);
                if (value === "boom") {
                    throw new Error("intentional failure");
                }
                return true;
            }
        `;

        await superpositionClient.send(
            new CreateFunctionCommand({
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                function_name: functionName,
                function: code,
                description: "Function for test endpoint stdout and errors",
                change_reason: "Initial creation",
                runtime_version: FunctionRuntimeVersion.V1,
                function_type: FunctionTypes.VALUE_VALIDATION,
            }),
        );

        try {
            const response = await superpositionClient.send(
                valueValidationTest(
                    functionName,
                    Stage.DRAFT,
                    "test-dimension",
                    "hello",
                ),
            );
            expect(response.fn_output).toBe(true);
            expect(response.stdout).toContain("validating value: hello");

            await expect(
                superpositionClient.send(
                    valueValidationTest(
                        functionName,
                        Stage.DRAFT,
                        "test-dimension",
                        "boom",
                    ),
                ),
            ).rejects.toThrow("intentional failure");
        } finally {
            await superpositionClient.send(
                new DeleteFunctionCommand({
                    workspace_id: ENV.workspace_id,
                    org_id: ENV.org_id,
                    function_name: functionName,
                }),
            );
        }
    });

    it("should list functions with both types", async () => {
        const listCommand = new ListFunctionCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            count: 10,
            page: 1,
        });
        try {
            const response = await superpositionClient.send(listCommand);

            expect(Array.isArray(response.data)).toBe(true);
            expect(response.data?.length).toBeGreaterThan(0);
        } catch (error) {
            console.error(error);
            throw error;
        }
    });
    // Update function tests
    it("should update value_validation function", async () => {
        const updatedValueValidationCode = `
            async function execute(payload) {
                let value = payload.value_validate.value;
                let key = payload.value_validate.key;
                if (key === "test-dimension" && value === "updated-valid") {
                    return true;
                }
                return false;
            }
        `;

        const updateCommand = new UpdateFunctionCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            function_name: valueValidationFunctionName,
            function: updatedValueValidationCode,
            description: "Updated value_validation function",
            change_reason: "Test update",
            runtime_version: FunctionRuntimeVersion.V1,
        });

        try {
            const updateResponse =
                await superpositionClient.send(updateCommand);
            expect(updateResponse.function_name).toBe(
                valueValidationFunctionName,
            );
            expect(updateResponse.description).toBe(
                "Updated value_validation function",
            );
        } catch (error) {
            console.error(error);
            throw error;
        }
    });

    it("should update value_compute function", async () => {
        const updatedValueComputeCode = `
            async function execute(payload) {
                let name = payload.value_compute.name;
                let prefix = payload.value_compute.prefix;
                if (name === "test-dimension" && prefix === "updated") {
                    return ["updated1", "updated2", "updated3"];
                }
                return [];
            }
        `;

        const updateCommand = new UpdateFunctionCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            function_name: valueComputeFunctionName,
            function: updatedValueComputeCode,
            description: "Updated value_compute function",
            change_reason: "Test update",
            runtime_version: FunctionRuntimeVersion.V1,
        });

        try {
            const updateResponse =
                await superpositionClient.send(updateCommand);
            expect(updateResponse.function_name).toBe(valueComputeFunctionName);
            expect(updateResponse.description).toBe(
                "Updated value_compute function",
            );
        } catch (error) {
            console.error(error);
            throw error;
        }
    });

    it("should test updated draft code", async () => {
        try {
            const validationResponse = await superpositionClient.send(
                valueValidationTest(
                    valueValidationFunctionName,
                    Stage.DRAFT,
                    "test-dimension",
                    "updated-valid",
                ),
            );
            expect(validationResponse.fn_output).toBe(true);

            const staleValueResponse = await superpositionClient.send(
                valueValidationTest(
                    valueValidationFunctionName,
                    Stage.DRAFT,
                    "test-dimension",
                    "valid",
                ),
            );
            expect(staleValueResponse.fn_output).toBe(false);

            const computeResponse = await superpositionClient.send(
                valueComputeTest(
                    valueComputeFunctionName,
                    Stage.DRAFT,
                    "test-dimension",
                    "updated",
                ),
            );
            expect(computeResponse.fn_output).toEqual([
                "updated1",
                "updated2",
                "updated3",
            ]);
        } catch (error) {
            console.error(error);
            throw error;
        }
    });

    // Negative Tests
    it("should fail to create value_validation function with invalid code", async () => {
        const command = new CreateFunctionCommand({
            function_name: "invalid-value-validation",
            function: "invalid code",
            description: "Test value_validation function",
            change_reason: "Initial creation",
            runtime_version: FunctionRuntimeVersion.V1,
            function_type: FunctionTypes.VALUE_VALIDATION,
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
        });

        try {
            await superpositionClient.send(command);
            throw new Error("Should have failed");
        } catch (error) {
            expect(error).toBeDefined();
        }
    });

    it("should fail to create value_compute function with invalid return type", async () => {
        const invalidCode = `
            async function execute(payload) {
                return "invalid return type";
            }
        `;

        const command = new CreateFunctionCommand({
            function_name: "invalid-value_compute",
            function: invalidCode,
            description: "Test value_validation function",
            change_reason: "Initial creation",
            runtime_version: FunctionRuntimeVersion.V1,
            function_type: FunctionTypes.VALUE_COMPUTE,
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
        });

        try {
            await superpositionClient.send(command);
            throw new Error("Should have failed");
        } catch (error) {
            expect(error).toBeDefined();
        }
    });

    // Publish function tests
    it("should successfully publish value_validation function", async () => {
        const publishCommand = new PublishCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            function_name: valueValidationFunctionName,
            change_reason: "Publishing for testing",
        });

        try {
            const response = await superpositionClient.send(publishCommand);
            expect(response.function_name).toBe(valueValidationFunctionName);
            expect(response.published_at).toBeDefined();
            expect(response.published_by).toBeDefined();
            expect(response.published_runtime_version).toBeDefined();
            expect(response.published_code).toBeDefined();
        } catch (error) {
            console.error(error);
            throw error;
        }
    });

    it("should successfully publish value_compute function", async () => {
        const publishCommand = new PublishCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            function_name: valueComputeFunctionName,
            change_reason: "Publishing for testing",
        });

        try {
            const response = await superpositionClient.send(publishCommand);
            expect(response.function_name).toBe(valueComputeFunctionName);
            expect(response.published_at).toBeDefined();
            expect(response.published_by).toBeDefined();
            expect(response.published_runtime_version).toBeDefined();
            expect(response.published_code).toBeDefined();
        } catch (error) {
            console.error(error);
            throw error;
        }
    });

    it("should test published stage after publishing", async () => {
        try {
            const validationResponse = await superpositionClient.send(
                valueValidationTest(
                    valueValidationFunctionName,
                    Stage.PUBLISHED,
                    "test-dimension",
                    "updated-valid",
                ),
            );
            expect(validationResponse.fn_output).toBe(true);
            expect(validationResponse.function_type).toBe(
                FunctionTypes.VALUE_VALIDATION,
            );

            const computeResponse = await superpositionClient.send(
                valueComputeTest(
                    valueComputeFunctionName,
                    Stage.PUBLISHED,
                    "test-dimension",
                    "updated",
                ),
            );
            expect(computeResponse.fn_output).toEqual([
                "updated1",
                "updated2",
                "updated3",
            ]);
        } catch (error) {
            console.error(error);
            throw error;
        }
    });

    it("should fail to publish non-existent function", async () => {
        const publishCommand = new PublishCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            function_name: "non-existent-function",
            change_reason: "Publishing for testing",
        });

        await expect(superpositionClient.send(publishCommand)).rejects.toThrow(
            "No records found. Please refine or correct your search parameters",
        );
    });

    // Cleanup
    afterAll(async () => {
        if (valueValidationFunctionName) {
            const deleteCommand = new DeleteFunctionCommand({
                function_name: valueValidationFunctionName,
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
            });
            try {
                await superpositionClient.send(deleteCommand);
            } catch (error) {
                console.error(error);
                throw error;
            }
        }
        if (valueComputeFunctionName) {
            const deleteCommand = new DeleteFunctionCommand({
                function_name: valueComputeFunctionName,
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
            });
            try {
                await superpositionClient.send(deleteCommand);
            } catch (error) {
                console.error(error);
                throw error;
            }
        }
    });
});
