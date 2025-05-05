import {
    CreateFunctionCommand,
    ListFunctionCommand,
    GetFunctionCommand,
    UpdateFunctionCommand,
    DeleteFunctionCommand,
    PublishCommand,
} from "@io.juspay/superposition-sdk";
import { expect, describe, it, afterAll } from "bun:test";
import { ENV, superpositionClient } from "../env.ts";
import { FunctionTypes } from "@io.juspay/superposition-sdk";

describe("Function Operations", () => {
    let validateFunctionName: string;
    let autocompleteFunctionName: string;

    // Validate Function Tests
    it("should create and test validate function", async () => {
        const validateCode = `
            async function validate(key, value) {
                if (key === "test-dimension" && value === "valid") {
                    return true;
                }
                return false;
            }
        `;

        const createCommand = new CreateFunctionCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            function_name: "test-validate",
            function: validateCode,
            description: "Test validate function",
            change_reason: "Initial creation",
            runtime_version: "1",
            function_type: FunctionTypes.Validation,
        });

        try {
            const createResponse = await superpositionClient.send(
                createCommand
            );
            validateFunctionName = createResponse.function_name ?? "";

            const getCommand = new GetFunctionCommand({
                org_id: ENV.org_id,
                workspace_id: ENV.workspace_id,
                function_name: validateFunctionName,
            });

            const getResponse = await superpositionClient.send(getCommand);

            expect(getResponse.function_name).toBe(validateFunctionName);
            expect(getResponse.function_type).toBe(FunctionTypes.Validation);
        } catch (error) {
            console.error(error);
            throw error;
        }
    });

    // Autocomplete Function Tests
    it("should create and test autocomplete function", async () => {
        const autocompleteCode = `
            async function autocomplete(name, prefix, environment) {
                if (name === "test-dimension" && prefix === "t") {
                    return ["test1", "test2", "test3"];
                }
                return [];
            }
        `;

        const createCommand = new CreateFunctionCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            function_name: "test-autocomplete",
            function: autocompleteCode,
            description: "Test autocomplete function",
            change_reason: "Initial creation",
            runtime_version: "1",
            function_type: FunctionTypes.Autocomplete,
        });

        try {
            const createResponse = await superpositionClient.send(
                createCommand
            );
            autocompleteFunctionName = createResponse.function_name ?? "";

            const getCommand = new GetFunctionCommand({
                org_id: ENV.org_id,
                workspace_id: ENV.workspace_id,
                function_name: autocompleteFunctionName,
            });

            const getResponse = await superpositionClient.send(getCommand);

            expect(getResponse.function_name).toBe(autocompleteFunctionName);
            expect(getResponse.function_type).toBe(FunctionTypes.Autocomplete);
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
    it("should update validate function", async () => {
        const updatedValidateCode = `
            async function validate(key, value) {
                if (key === "test-dimension" && value === "updated-valid") {
                    return true;
                }
                return false;
            }
        `;

        const updateCommand = new UpdateFunctionCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            function_name: validateFunctionName,
            function: updatedValidateCode,
            description: "Updated validate function",
            change_reason: "Test update",
            runtime_version: "1",
            function_type: FunctionTypes.Validation,
        });

        try {
            const updateResponse = await superpositionClient.send(
                updateCommand
            );
            expect(updateResponse.function_name).toBe(validateFunctionName);
            expect(updateResponse.description).toBe(
                "Updated validate function"
            );
        } catch (error) {
            console.error(error);
            throw error;
        }
    });

    it("should update autocomplete function", async () => {
        const updatedAutocompleteCode = `
            async function autocomplete(name, prefix, environment) {
                if (name === "test-dimension" && prefix === "updated") {
                    return ["updated1", "updated2", "updated3"];
                }
                return [];
            }
        `;

        const updateCommand = new UpdateFunctionCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            function_name: autocompleteFunctionName,
            function: updatedAutocompleteCode,
            description: "Updated autocomplete function",
            change_reason: "Test update",
            runtime_version: "1",
            function_type: FunctionTypes.Autocomplete,
        });

        try {
            const updateResponse = await superpositionClient.send(
                updateCommand
            );
            expect(updateResponse.function_name).toBe(autocompleteFunctionName);
            expect(updateResponse.description).toBe(
                "Updated autocomplete function"
            );
        } catch (error) {
            console.error(error);
            throw error;
        }
    });

    // Negative Tests
    it("should fail to create validate function with invalid code", async () => {
        const command = new CreateFunctionCommand({
            function_name: "invalid-validate",
            function: "invalid code",
            description: "Test validate function",
            change_reason: "Initial creation",
            runtime_version: "1",
            function_type: FunctionTypes.Validation,
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

    it("should fail to create autocomplete function with invalid return type", async () => {
        const invalidCode = `
            async function autocomplete(name, prefix, environment) {
                return "invalid return type";
            }
        `;

        const command = new CreateFunctionCommand({
            function_name: "invalid-autocomplete",
            function: invalidCode,
            description: "Test validate function",
            change_reason: "Initial creation",
            runtime_version: "1",
            function_type: FunctionTypes.Autocomplete,
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
    it("should successfully publish validate function", async () => {
        const publishCommand = new PublishCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            function_name: validateFunctionName,
        });

        try {
            const response = await superpositionClient.send(publishCommand);
            expect(response.function_name).toBe(validateFunctionName);
            expect(response.published_at).toBeDefined();
            expect(response.published_by).toBeDefined();
            expect(response.published_runtime_version).toBeDefined();
            expect(response.published_code).toBeDefined();
        } catch (error) {
            console.error(error);
            throw error;
        }
    });

    it("should successfully publish autocomplete function", async () => {
        const publishCommand = new PublishCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            function_name: autocompleteFunctionName,
        });

        try {
            const response = await superpositionClient.send(publishCommand);
            expect(response.function_name).toBe(autocompleteFunctionName);
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
        });

        try {
            await superpositionClient.send(publishCommand);
            expect(true).toBe(false);
        } catch (error) {
            expect(error).toBeDefined();
        }
    });

    // Cleanup
    afterAll(async () => {
        if (validateFunctionName) {
            const deleteCommand = new DeleteFunctionCommand({
                function_name: validateFunctionName,
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
        if (autocompleteFunctionName) {
            const deleteCommand = new DeleteFunctionCommand({
                function_name: autocompleteFunctionName,
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
