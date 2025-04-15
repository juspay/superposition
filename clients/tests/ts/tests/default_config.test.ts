import { SuperpositionClient, CreateDefaultConfigCommand, UpdateDefaultConfigCommand } from "@io.juspay/superposition-sdk";
import { ENV } from "./env.test.ts";
import type { UpdateDefaultConfigCommandOutput } from "@io.juspay/superposition-sdk";

import { describe, beforeAll, test, expect } from "bun:test";

describe("Default Config API Integration Tests", () => {
    let client: SuperpositionClient;

    beforeAll(() => {
        client = new SuperpositionClient({

            endpoint: ENV.baseUrl,
            token: {
                token: "some-token",
            },

        });
    });

    describe("Create Default Config", () => {
        // add async in front of all closure functions
        test('should successfully create a valid default config', async () => {
            const input = {
                workspace_id: "test",
                org_id: "localorg",

                key: "test-key",
                schema: {
                    type: 'object',
                    properties: {
                        name: { type: 'string' },
                        age: { type: 'number', minimum: 0 }
                    },
                    required: ['name']
                },
                value: {
                    name: 'Test User',
                    age: 30
                },
                description: 'Test configuration',
                change_reason: 'Initial creation for testing'
            };
            const cmd = new CreateDefaultConfigCommand(input);
            await client.send(cmd);
        })

        test('should fail when schema is invalid', async () => {
            const input = {
                workspace_id: "test",
                org_id: "localorg",

                key: "test-key",
                schema: {
                    type: 'invalid-type'
                },
                value: {
                    name: 'Test User',
                    age: 30
                },
                description: 'Test configuration',
                change_reason: 'Initial creation for testing'
            };

            const cmd = new CreateDefaultConfigCommand(input);
            expect(client.send(cmd)).rejects.toThrow('Invalid JSON schema (failed to compile)');
        })

        test('should fail when schema is empty', async () => {
            const input = {
                workspace_id: "test",
                org_id: "localorg",

                key: "test-key",
                schema: {},
                value: {
                    name: 'Test User',
                    age: 30
                },
                description: 'Test configuration',
                change_reason: 'Initial creation for testing'
            };
            const cmd = new CreateDefaultConfigCommand(input);

            expect(client.send(cmd)).rejects.toThrow('Schema cannot be empty.');
        })

        test('should fail when value doesn\'t match schema', async () => {
            const input = {
                workspace_id: "test",
                org_id: "localorg",

                key: "test-key",
                schema: {
                    type: 'object',
                    properties: {
                        name: { type: 'string' },
                        age: { type: 'number', minimum: 0 }
                    },
                    required: ['name']
                },
                value: {
                    name: 'Test User',
                    age: -5 // Invalid age
                },
                description: 'Test configuration',
                change_reason: 'Initial creation for testing'
            };
            const cmd = new CreateDefaultConfigCommand(input);

            expect(client.send(cmd)).rejects.toThrow('Schema validation failed: value is too small, minimum is 0');
        })

        test('should fail when function validation fails', async () => {
            const input = {
                workspace_id: "test",
                org_id: "localorg",

                key: 'test-key',
                schema: {
                    type: 'object',
                    properties: {
                        name: { type: 'string' }
                    }
                },
                value: { name: 'Invalid Value' },
                description: 'Test configuration',
                function_name: 'test_validation_function',
                change_reason: 'Test function validation'
            };

            const cmd = new CreateDefaultConfigCommand(input);
            expect(client.send(cmd)).rejects.toThrow('Function validation failed for test-key with error Error: The function did not return a value that was expected. Check the return type and logic of the function\n. ');
        })

        test('should fail when function does not exist', async () => {
            const input = {
                workspace_id: "test",
                org_id: "localorg",

                key: 'test-key',
                schema: {
                    type: 'object',
                    properties: {
                        name: { type: 'string' }
                    }
                },
                value: { name: 'Invalid Value' },
                description: 'Test configuration',
                function_name: 'non_existent_function',
                change_reason: 'Test function validation'
            };
            const cmd = new CreateDefaultConfigCommand(input);
            expect(client.send(cmd)).rejects.toThrow('Function non_existent_function doesn\'t exist.');
        })

        test('should create config with tags', async () => {
            // todo
        })
    })

    describe('Update Default Config', () => {
        test('should successfully update an existing default config', async () => {
            const input = {
                workspace_id: "test",
                org_id: "localorg",

                key: "test-key",
                value: {
                    name: 'Updated User',
                    age: 35
                },
                description: 'Updated configuration',
                change_reason: 'Update for testing'
            };
            const cmd = new UpdateDefaultConfigCommand(input);

            const response: UpdateDefaultConfigCommandOutput = await client.send(cmd);

            expect(response.value).toEqual({
                name: 'Updated User',
                age: 35
            });
            expect(response.schema).toEqual({
                type: 'object',
                properties: {
                    name: { type: 'string' },
                    age: { type: 'number', minimum: 0 }
                },
                required: ['name']
            });
            expect(response.description).toBe('Updated configuration');
            expect(response.change_reason).toBe('Update for testing');
            expect(response.$metadata.httpStatusCode).toBe(200);
        })

        test('should successfully update schema and value together', async () => {
            const input = {
                workspace_id: "test",
                org_id: "localorg",

                key: "test-key",
                schema: {
                    type: 'object',
                    properties: {
                        name: { type: 'string' },
                        age: { type: 'number' },
                        email: { type: 'string', format: 'email' }
                    },
                    required: ['name', 'email']
                },
                value: {
                    name: 'Updated Name',
                    age: 35,
                    email: 'updated@example.com'
                },
                change_reason: 'Updating schema and value'
            };
            const cmd = new UpdateDefaultConfigCommand(input);
            const response = await client.send(cmd);

            expect(response.value).toEqual({
                name: 'Updated Name',
                age: 35,
                email: 'updated@example.com'
            });
            expect(response.schema).toEqual({
                type: 'object',
                properties: {
                    name: { type: 'string' },
                    age: { type: 'number' },
                    email: { type: 'string', format: 'email' }
                },
                required: ['name', 'email']
            });
            expect(response.change_reason).toBe('Updating schema and value');
            expect(response.description).toBe('Updated configuration');
            expect(response.$metadata.httpStatusCode).toBe(200);
        })

        test('should fail when updating non-existent key', async () => {
            const input = {
                workspace_id: "test",
                org_id: "localorg",

                key: "non_existent_key",
                value: {
                    name: 'Updated User',
                    age: 35
                },
                description: 'Updated configuration',
                change_reason: 'Update for testing'
            };
            const cmd = new UpdateDefaultConfigCommand(input);
            expect(client.send(cmd)).rejects.toThrow('No record found for non_existent_key. Use create endpoint instead.');
        })

        test('should fail when updated schema is invalid', async () => {
            const input = {
                workspace_id: "test",
                org_id: "localorg",

                key: "test-key",
                schema: {
                    type: 'invalid-type'
                },
                change_reason: 'Update for testing'
            };
            const cmd = new UpdateDefaultConfigCommand(input);
            expect(client.send(cmd)).rejects.toThrow('Invalid JSON schema.');
        })

        test('should fail when value does not match updated schema', async () => {
            const input = {
                workspace_id: "test",
                org_id: "localorg",

                key: "test-key",
                schema: {
                    type: 'object',
                    properties: {
                        name: { type: 'string' },
                        age: { type: 'number', minimum: 18 },
                        email: { type: 'string', format: 'email' }
                    },
                    required: ['name', 'email']
                },
                value: {
                    name: 'Updated Name',
                    age: 20
                    // Missing required email field
                },
                change_reason: 'Update for testing'
            };
            const cmd = new UpdateDefaultConfigCommand(input);
            expect(client.send(cmd)).rejects.toThrow('Schema validation failed: required property `\"email\"` is missing');
        })

        test('should update function_name', async () => {
            const input = {
                workspace_id: "test",
                org_id: "localorg",

                key: "test-key",
                function_name: "new_function_name",
                change_reason: 'Update function to new_function_name for testing'
            };
            const cmd = new UpdateDefaultConfigCommand(input);
            const response = await client.send(cmd);

            // write asserts
            expect(response.$metadata.httpStatusCode).toBe(200);
            expect(response.value).toEqual({
                name: 'Updated Name',
                age: 35,
                email: 'updated@example.com'
            });
            expect(response.schema).toEqual({
                type: 'object',
                properties: {
                    name: { type: 'string' },
                    age: { type: 'number' },
                    email: { type: 'string', format: 'email' }
                },
                required: ['name', 'email']
            });
            expect(response.description).toBe('Updated configuration');
            expect(response.change_reason).toBe('Update function to new_function_name for testing');
            expect(response.function_name).toBe("new_function_name");
        })
    })
})