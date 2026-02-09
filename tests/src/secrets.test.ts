import {
    CreateSecretCommand,
    GetSecretCommand,
    CreateFunctionCommand,
    DeleteFunctionCommand,
    UpdateSecretCommand,
    DeleteSecretCommand,
    TestCommand,
    FunctionTypes,
    FunctionRuntimeVersion,
} from "@juspay/superposition-sdk";
import { expect, describe, test, afterAll } from "bun:test";
import { ENV, superpositionClient } from "../env.ts";

describe("Secret Operations", () => {
    const createdSecrets: Set<string> = new Set();
    const createdFunctions: Set<string> = new Set();

    function trackSecret(name: string) {
        createdSecrets.add(name);
    }

    function trackFunction(name: string) {
        createdFunctions.add(name);
    }

    afterAll(async () => {
        console.log(`Cleaning up ${createdSecrets.size} secrets...`);

        for (const secretName of createdSecrets) {
            try {
                const deleteCommand = new DeleteSecretCommand({
                    workspace_id: ENV.workspace_id,
                    org_id: ENV.org_id,
                    name: secretName,
                });
                await superpositionClient.send(deleteCommand);
                console.log(`Deleted secret: ${secretName}`);
            } catch (error: any) {
                console.error(
                    `Failed to delete secret ${secretName}:`,
                    error.message
                );
            }
        }

        console.log(`Cleaning up ${createdFunctions.size} functions...`);

        for (const funcName of createdFunctions) {
            try {
                const deleteCommand = new DeleteFunctionCommand({
                    workspace_id: ENV.workspace_id,
                    org_id: ENV.org_id,
                    function_name: funcName,
                });
                await superpositionClient.send(deleteCommand);
                console.log(`Deleted function: ${funcName}`);
            } catch (error: any) {
                console.error(
                    `Failed to delete function ${funcName}:`,
                    error.message
                );
            }
        }
    });

    test("Create secret", async () => {
        const secretName = `TEST_API_KEY_CREATE`;
        const createCommand = new CreateSecretCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            name: secretName,
            value: "test-key-12345",
            description: "Test API key secret",
            change_reason: "Initial creation for testing",
        });

        try {
            const response = await superpositionClient.send(createCommand);

            trackSecret(response.name!);

            expect(response.name).toBe(secretName);
            expect(response.description).toBe("Test API key secret");
            expect(response.created_by).toBeDefined();
            expect(response.created_at).toBeDefined();
        } catch (error: any) {
            console.log("Error creating secret:", error.message);
            throw error;
        }
    });

    test("Get secret by Name", async () => {

        try {
            let secretName = "TEST_API_KEY_CREATE";

            const getCommand = new GetSecretCommand({
                org_id: ENV.org_id,
                workspace_id: ENV.workspace_id,
                name: secretName,
            });

            const getResponse = await superpositionClient.send(getCommand);

            expect(getResponse.name).toBe(secretName);
            expect(getResponse.value).toBeUndefined();
            expect(getResponse.description).toBe("Test API key secret");
        } catch (error: any) {
            console.log("Error getting secret:", error.message);
            throw error;
        }
    });

    test("Verify secret value update via Function", async () => {
        const secretName = `UPDATE_VERIFY_${Date.now()}`;
        const functionName = `verify_update_${Date.now()}`;
        const originalValue = "original-secret-value";
        const updatedValue = "updated-secret-value";

        const createCmd = new CreateSecretCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            name: secretName,
            value: originalValue,
            description: "Testing update verification",
            change_reason: "Initial creation",
        });

        const updateCmd = new UpdateSecretCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            name: secretName,
            value: updatedValue,
            change_reason: "Updating for verification",
        });

        const functionCode = `
        async function execute(payload) {
            return [SECRETS.${secretName}];
        }`;

        const createFuncCmd = new CreateFunctionCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            function_name: functionName,
            function: functionCode,
            description: "Verify secret update",
            change_reason: "Testing",
            runtime_version: FunctionRuntimeVersion.V1,
            function_type: FunctionTypes.VALUE_COMPUTE,
        });

        const testCmd = new TestCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            function_name: functionName,
            stage: "draft",
            request: {
                value_compute: {
                    name: "",
                    prefix: "",
                    type: "ConfigKey",
                    environment: { context: {}, overrides: {} },
                },
            },
        });

        try {
            await superpositionClient.send(createCmd);
            trackSecret(secretName);
            await superpositionClient.send(createFuncCmd);
            trackFunction(functionName);

            const testResponse1 = await superpositionClient.send(testCmd);
            expect(testResponse1.fn_output).toEqual([originalValue]);

            await superpositionClient.send(updateCmd);

            const testResponse2 = await superpositionClient.send(testCmd);
            expect(testResponse2.fn_output).toEqual([updatedValue]);
        } catch (error: any) {
            console.log("Error verifying secret update:", error.message);
            throw error;
        }
    });

    test("Delete secret", async () => {
        const createCommand = new CreateSecretCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            name: `DELETE_TEST_${Date.now()}`,
            value: "test-value",
            description: "creating for delete test",
            change_reason: "Creating for delete test",
        });

        try {
            const createResponse = await superpositionClient.send(
                createCommand
            );
            const secretName = createResponse.name!;

            const deleteCommand = new DeleteSecretCommand({
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                name: secretName,
            });

            await superpositionClient.send(deleteCommand);

            // Verify deletion
            const getCommand = new GetSecretCommand({
                org_id: ENV.org_id,
                workspace_id: ENV.workspace_id,
                name: secretName,
            });

            expect(superpositionClient.send(getCommand)).rejects.toThrow(
                "No records found"
            );
        } catch (error: any) {
            console.log("Error testing delete:", error.message);
            throw error;
        }
    });

    test("Fail on Duplicate secret Name", async () => {
        const name = `DUPLICATE_TEST_${Date.now()}`;

        const createCommand = new CreateSecretCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            name: name,
            value: "test-value",
            description: "creating duplicate test",
            change_reason: "Creating for duplicate test",
        });

        try {
            const createResponse = await superpositionClient.send(
                createCommand
            );
            trackSecret(createResponse.name!);

            // Try to create duplicate
            const duplicateCommand = new CreateSecretCommand({
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                name: name,
                value: "different-value",
                description: "Testing duplicate",
                change_reason: "Testing duplicate",
            });

            expect(superpositionClient.send(duplicateCommand)).rejects.toThrow(
                "duplicate key value violates unique constraint"
            );
        } catch (error: any) {
            console.log("Error testing duplicate prevention:", error.message);
            throw error;
        }
    });

    test("Fail on Get Non-Existent secret", async () => {
        const getCommand = new GetSecretCommand({
            org_id: ENV.org_id,
            workspace_id: ENV.workspace_id,
            name: "NON_EXISTENT_secret",
        });

        expect(superpositionClient.send(getCommand)).rejects.toThrow(
            "No records found"
        );
    });

    test("Fail on Update Non-Existent secret", async () => {
        const updateCommand = new UpdateSecretCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            name: "NON_EXISTENT_secret",
            value: "new-value",
            change_reason: "Testing update of non-existent secret",
        });

        expect(superpositionClient.send(updateCommand)).rejects.toThrow(
            "No records found"
        );
    });

    test("Fail on Delete Non-Existent secret", async () => {
        const deleteCommand = new DeleteSecretCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            name: "NON_EXISTENT_secret",
        });

        expect(superpositionClient.send(deleteCommand)).rejects.toThrow(
            "No records found"
        );
    });

    test("Fail on Empty secret Name", async () => {
        const command = new CreateSecretCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            name: "",
            value: "test-value",
            description: "Testing empty name validation",
            change_reason: "Testing empty name validation",
        });

        expect(superpositionClient.send(command)).rejects.toThrow(
            "Parse error"
        );
    });

    test("Fail on Invalid secret Name Pattern", async () => {
        const invalidNames = [
            "invalid-name-with-dashes", // dashes not allowed
            "invalid name with spaces", // spaces not allowed
            "invalid.name.with.dots", // dots not allowed
            "123_STARTS_WITH_NUMBER", // starts with number
            "special@chars#not$allowed", // special characters
            "lowercase_not_allowed", // lowercase not allowed
            "Mixed_Case_Name", // mixed case not allowed
        ];

        for (const invalidName of invalidNames) {
            const command = new CreateSecretCommand({
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                name: invalidName,
                value: "test-value",
                description: `Testing invalid name: ${invalidName}`,
                change_reason: "Testing invalid name pattern",
            });

            expect(superpositionClient.send(command)).rejects.toThrow(
                "Parse error"
            );
        }
    });
});
