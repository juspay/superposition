import {
    CreateFunctionCommand,
    ListFunctionCommand,
    GetFunctionCommand,
    UpdateFunctionCommand,
    DeleteFunctionCommand,
    PublishCommand,
    FunctionTypes,
} from "@juspay/superposition-sdk";
import { expect, describe, it, afterAll } from "bun:test";
import { ENV, superpositionClient } from "../env.ts";

describe("Function Operations", () => {
    let valueValidationFunctionName: string;
    let valueComputeFunctionName: string;

    // Value Validation Function Tests
    it("should create and test value_validation function", async () => {
        const valueValidationCode = `
            async function validate_value(key, value) {
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
            runtime_version: "1",
            function_type: FunctionTypes.VALUE_VALIDATION,
        });

        try {
            const createResponse = await superpositionClient.send(
                createCommand
            );
            valueValidationFunctionName = createResponse.function_name ?? "";

            const getCommand = new GetFunctionCommand({
                org_id: ENV.org_id,
                workspace_id: ENV.workspace_id,
                function_name: valueValidationFunctionName,
            });

            const getResponse = await superpositionClient.send(getCommand);

            expect(getResponse.function_name).toBe(valueValidationFunctionName);
            expect(getResponse.function_type).toBe(FunctionTypes.VALUE_VALIDATION);
        } catch (error) {
            console.error(error);
            throw error;
        }
    });

    // Value Compute Function Tests
    it("should create and test value_compute function", async () => {
        const valueComputeCode = `
            async function value_compute(name, prefix, environment) {
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
            runtime_version: "1",
            function_type: FunctionTypes.VALUE_COMPUTE,
        });

        try {
            const createResponse = await superpositionClient.send(
                createCommand
            );
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
            async function validate_value(key, value) {
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
            runtime_version: "1",
        });

        try {
            const updateResponse = await superpositionClient.send(
                updateCommand
            );
            expect(updateResponse.function_name).toBe(valueValidationFunctionName);
            expect(updateResponse.description).toBe(
                "Updated value_validation function"
            );
        } catch (error) {
            console.error(error);
            throw error;
        }
    });

    it("should update value_compute function", async () => {
        const updatedValueComputeCode = `
            async function value_compute(name, prefix, environment) {
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
            runtime_version: "1",
        });

        try {
            const updateResponse = await superpositionClient.send(
                updateCommand
            );
            expect(updateResponse.function_name).toBe(valueComputeFunctionName);
            expect(updateResponse.description).toBe(
                "Updated value_compute function"
            );
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
            runtime_version: "1",
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
            async function value_compute(name, prefix, environment) {
                return "invalid return type";
            }
        `;

        const command = new CreateFunctionCommand({
            function_name: "invalid-value_compute",
            function: invalidCode,
            description: "Test value_validation function",
            change_reason: "Initial creation",
            runtime_version: "1",
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

    it("should fail to publish non-existent function", async () => {
        const publishCommand = new PublishCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            function_name: "non-existent-function",
            change_reason: "Publishing for testing",
        });

        expect(superpositionClient.send(publishCommand)).rejects.toThrow(
            "No records found. Please refine or correct your search parameters"
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
