import {
    CreateVariableCommand,
    GetVariableCommand,
    CreateFunctionCommand,
    DeleteFunctionCommand,
    UpdateVariableCommand,
    DeleteVariableCommand,
    TestCommand,
    FunctionTypes,
} from "@juspay/superposition-sdk";
import { expect, describe, test, afterAll } from "bun:test";
import { ENV, superpositionClient } from "../env.ts";

describe("Variable Operations", () => {
    const createdVariables: Set<string> = new Set();
    const createdFunctions: Set<string> = new Set();

    function trackVariable(name: string) {
        createdVariables.add(name);
    }

    function trackFunction(name: string) {
        createdFunctions.add(name);
    }

    afterAll(async () => {
        console.log(`Cleaning up ${createdVariables.size} variables...`);

        for (const varName of createdVariables) {
            try {
                const deleteCommand = new DeleteVariableCommand({
                    workspace_id: ENV.workspace_id,
                    org_id: ENV.org_id,
                    name: varName,
                });
                await superpositionClient.send(deleteCommand);
                console.log(`Deleted variable: ${varName}`);
            } catch (error: any) {
                console.error(`Failed to delete variable ${varName}:`, error.message);
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
                console.error(`Failed to delete function ${funcName}:`, error.message);
            }
        }
    });

    test("Create Variable", async () => {
        const createCommand = new CreateVariableCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            name: "TEST_API_KEY",
            value: "test-key-12345",
            description: "Test API key variable",
            change_reason: "Initial creation for testing",
        });

        try {
            const response = await superpositionClient.send(createCommand);

            trackVariable(response.name!);

            expect(response.name).toBe("TEST_API_KEY");
            expect(response.value).toBe("test-key-12345");
            expect(response.description).toBe("Test API key variable");
            expect(response.created_by).toBeDefined();
            expect(response.created_at).toBeDefined();
        } catch (error: any) {
            console.log("Error creating variable:", error.message);
            throw error;
        }
    });

    test("Get Variable by Name", async () => {
        const createCommand = new CreateVariableCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            name: `GET_TEST_${Date.now()}`,
            value: "test-value",
            description: "Test get variable",
            change_reason: "Testing get operation",
        });

        try {
            const createResponse = await superpositionClient.send(createCommand);
            const varName = createResponse.name!;
            trackVariable(varName);

            const getCommand = new GetVariableCommand({
                org_id: ENV.org_id,
                workspace_id: ENV.workspace_id,
                name: varName,
            });

            const getResponse = await superpositionClient.send(getCommand);

            expect(getResponse.name).toBe(varName);
            expect(getResponse.value).toBe("test-value");
            expect(getResponse.description).toBe("Test get variable");
        } catch (error: any) {
            console.log("Error getting variable:", error.message);
            throw error;
        }
    });

    test("Update Variable Value", async () => {

        const uniqueVariableName = `UPDATE_TEST_${Date.now()}`;

        try {
            const createInput = {
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                name: uniqueVariableName,
                value: "original-value",
                description: "Creating for update test",
                change_reason: "Creating for update test",
            };

            const createCmd = new CreateVariableCommand(createInput);
            await superpositionClient.send(createCmd);

            trackVariable(uniqueVariableName);

            console.log(`Created template ${uniqueVariableName} for update test`);
        } catch (error: any) {
            if (!error.message?.includes("duplicate key value")) {
                console.log(
                    "Error pre-creating variable for update test:",
                    error.message
                );
            }
        }

        const input = {
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            name: uniqueVariableName,
            value: "updated-value",
            change_reason: "Updating value for testing",
        };

        const cmd = new UpdateVariableCommand(input);

        try {
            const updateResponse = await superpositionClient.send(cmd);

            expect(updateResponse.name).toBe(uniqueVariableName);
            expect(updateResponse.value).toBe("updated-value");
            expect(updateResponse.last_modified_by).toBeDefined();
            expect(updateResponse.last_modified_at).toBeDefined();
        } catch (error: any) {
            console.log("Error updating variable:", error.message);
            throw error;
        }
    });

    test("Update Variable Description", async () => {
        const createCommand = new CreateVariableCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            name: `DESC_TEST_${Date.now()}`,
            value: "test-value",
            description: "Original description",
            change_reason: "Creating for description update test",
        });

        try {
            const createResponse = await superpositionClient.send(createCommand);
            const varName = createResponse.name!;
            trackVariable(varName);

            const updateCommand = new UpdateVariableCommand({
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                name: varName,
                description: "Updated description",
                change_reason: "Updating description",
            });

            const updateResponse = await superpositionClient.send(updateCommand);

            expect(updateResponse.description).toBe("Updated description");
        } catch (error: any) {
            console.log("Error updating description:", error.message);
            throw error;
        }
    });

    test("Delete Variable", async () => {
        const createCommand = new CreateVariableCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            name: `DELETE_TEST_${Date.now()}`,
            value: "test-value",
            description: "creating for delete test",
            change_reason: "Creating for delete test",
        });

        try {
            const createResponse = await superpositionClient.send(createCommand);
            const varName = createResponse.name!;

            const deleteCommand = new DeleteVariableCommand({
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                name: varName,
            });

            await superpositionClient.send(deleteCommand);

            // Verify deletion
            const getCommand = new GetVariableCommand({
                org_id: ENV.org_id,
                workspace_id: ENV.workspace_id,
                name: varName,
            });

            // const response = await superpositionClient.send(getCommand);
            // console.log(`1 response: ${response}`);

            expect(superpositionClient.send(getCommand)).rejects.toThrow("No records found");
        } catch (error: any) {
            console.log("Error testing delete:", error.message);
            throw error;
        }
    });

    test("Fail on Duplicate Variable Name", async () => {
        const name = `DUPLICATE_TEST_${Date.now()}`;

        const createCommand = new CreateVariableCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            name: name,
            value: "test-value",
            description: "creating duplicate test",
            change_reason: "Creating for duplicate test",
        });

        try {
            const createResponse = await superpositionClient.send(createCommand);
            trackVariable(createResponse.name!);

            // Try to create duplicate
            const duplicateCommand = new CreateVariableCommand({
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

    test("Fail on Get Non-Existent Variable", async () => {
        const getCommand = new GetVariableCommand({
            org_id: ENV.org_id,
            workspace_id: ENV.workspace_id,
            name: "NON_EXISTENT_VARIABLE",
        });

        expect(superpositionClient.send(getCommand)).rejects.toThrow(
            "No records found"
        );
    });

    test("Fail on Update Non-Existent Variable", async () => {
        const updateCommand = new UpdateVariableCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            name: "NON_EXISTENT_VARIABLE",
            value: "new-value",
            change_reason: "Testing update of non-existent variable",
        });

        expect(superpositionClient.send(updateCommand)).rejects.toThrow(
            "No records found"
        );
    });

    test("Fail on Delete Non-Existent Variable", async () => {
        const deleteCommand = new DeleteVariableCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            name: "NON_EXISTENT_VARIABLE",
        });

        expect(superpositionClient.send(deleteCommand)).rejects.toThrow(
            "No records found"
        );
    });

    test("Fail on Empty Variable Name", async () => {
        const command = new CreateVariableCommand({
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

    test("Fail on Invalid Variable Name Pattern", async () => {
        const invalidNames = [
            "invalid-name-with-dashes",  // dashes not allowed
            "invalid name with spaces",   // spaces not allowed
            "invalid.name.with.dots",     // dots not allowed
            "123_STARTS_WITH_NUMBER",     // starts with number
            "special@chars#not$allowed",  // special characters
            "lowercase_not_allowed",      // lowercase not allowed
            "Mixed_Case_Name",            // mixed case not allowed
        ];

        for (const invalidName of invalidNames) {
            const command = new CreateVariableCommand({
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

    test("Use Variable in Function and Verify Test Output", async () => {

        const variableName = `API_KEY`;
        const functionName = `test_variable_function_${Date.now()}`;
        const variableValue = `secret-api-key-12345`;

        try {
            // Step 1: Create a variable
            console.log("Step 1: Creating variable...");
            const createVarCommand = new CreateVariableCommand({
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                name: variableName,
                value: variableValue,
                description: "Test variable for function integration",
                change_reason: "Testing variable usage in functions",
            });

            const varResponse = await superpositionClient.send(createVarCommand);
            trackVariable(varResponse.name!);


            console.log("Step 2: Creating function that uses the variable...");
            const functionCode = `
                    async function validate(key, value) {
                    console.log("API Key:", VARS.API_KEY);
                    return VARS.API_KEY === 'secret-api-key-12345'
                    }
            `;

            const createFuncCommand = new CreateFunctionCommand({
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                function_name: functionName,
                function: functionCode,
                description: "Function that accesses variable",
                change_reason: "Testing variable integration",
                runtime_version: "1",
                function_type: FunctionTypes.VALIDATION,
            });

            const funcResponse = await superpositionClient.send(createFuncCommand);
            trackFunction(funcResponse.function_name!);

            expect(funcResponse.function_name).toBe(functionName);
            console.log(`✅ Created function: ${functionName}`);

            // Step 3: Test the function
            console.log("Step 3: Testing function...");
            const testCommand = new TestCommand({
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                function_name: functionName,
                stage: "draft",
                request: {
                    ValidateFunctionRequest: {
                        key: "",
                        value: "",
                    },
                },
            });

            const testResponse = await superpositionClient.send(testCommand);
            console.log("Test response:", testResponse);
            console.log("Function output:", testResponse.fn_output);
            console.log("Function stdout:", testResponse.stdout);

            // Step 4: Verify the output
            expect(testResponse.fn_output).toBe(true);
            expect(testResponse.stdout).toContain(variableValue);

            console.log(`✅ Function successfully accessed variable value: ${variableValue}`);

        } catch (error: any) {
            console.error("Error in variable-function integration test:", error.message);
            throw error;
        }
    });

});