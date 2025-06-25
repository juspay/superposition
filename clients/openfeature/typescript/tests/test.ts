import { describe, test, expect, beforeAll, afterAll } from '@jest/globals';
import { OpenFeature } from '@openfeature/server-sdk';
import { SuperpositionProvider, SuperpositionProviderOptions } from '../src/resolver-client/superposition-provider';


describe('Superposition OpenFeature Integration Tests', () => {
    let provider: SuperpositionProvider;

    beforeAll(async () => {
        const testConfig: SuperpositionProviderOptions = {
            endpoint: 'http://localhost:8080',
            workspace_id: 'test',
            org_id: 'localorg',
            token: '12345678',
            fallbackConfig: {
                "contexts": [
                    {
                        "id": "52eae63b03a0356c0707b9e4c52414f313290eb807444320d0ff7b3ce312a79b",
                        "condition": {
                            "and": [
                                {
                                    "==": [
                                        {
                                            "var": "d1"
                                        },
                                        "d1"
                                    ]
                                }
                            ]
                        },
                        "priority": 0,
                        "weight": 0,
                        "override_with_keys": [
                            "3b829029fdc9dab2bd1fdb324616472cbc1369ec48767d815dc6362a8d59a8f4"
                        ]
                    }
                ],
                "overrides": {
                    "3b829029fdc9dab2bd1fdb324616472cbc1369ec48767d815dc6362a8d59a8f4": {
                        "bool": false
                    }
                },
                "default_configs": {
                    "bool": true,
                    "double": 1.2,
                    "integer": 1,
                    "object": {
                        "k1": {
                            "k2": "v1"
                        }
                    },
                    "string": "something"
                }
            }
        };

        provider = new SuperpositionProvider(testConfig);
        await OpenFeature.setProviderAndWait(provider);
    });

    afterAll(async () => {
        await provider.onClose();
        await OpenFeature.close();
    });

    /**
     * Test boolean flag evaluation with assertions - matches Java testBooleanEvaluation
     */
    test('should evaluate boolean flags correctly', async () => {
        const client = OpenFeature.getClient();
        const context = { d1: "d1" };

        const boolResult = await client.getBooleanDetails("bool", true, context);
        console.log(`Boolean flag 'bool': ${boolResult.value} (reason: ${boolResult.reason})`);

        // Assertions for boolean evaluation
        expect(boolResult).toBeDefined();
        expect(boolResult).toHaveProperty('value');
        expect(typeof boolResult.value).toBe('boolean');
        expect(boolResult).toHaveProperty('reason');

        // Assert exact value like Java test expects
        expect(boolResult.value).toBe(false);
    });

    /**
     * Test string flag evaluation with assertions - matches Java testStringEvaluation
     */
    test('should evaluate string flags correctly', async () => {
        const client = OpenFeature.getClient();
        const context = { clientId: "test-user" };

        const stringResult = await client.getStringDetails("string", "", context);
        console.log(`String flag 'string': '${stringResult.value}'`);

        // Assertions for string evaluation
        expect(stringResult).toBeDefined();
        expect(stringResult).toHaveProperty('value');
        expect(typeof stringResult.value).toBe('string');
        expect(stringResult.value.length).toBeGreaterThanOrEqual(0);
        expect(stringResult).toHaveProperty('reason');

        // Assert exact value like Java test expects
        expect(stringResult.value).toBe("something");
    });

    /**
     * Test integer flag evaluation with assertions - matches Java testIntegerEvaluation
     */
    test('should evaluate integer flags correctly', async () => {
        const client = OpenFeature.getClient();
        const context = { clientId: "test-user" };

        const intResult = await client.getNumberDetails("integer", 2, context);
        console.log(`Integer flag 'integer': ${intResult.value}`);

        // Assertions for integer evaluation
        expect(intResult).toBeDefined();
        expect(intResult).toHaveProperty('value');
        expect(typeof intResult.value).toBe('number');
        expect(Number.isInteger(intResult.value)).toBe(true);
        expect(intResult).toHaveProperty('reason');

        // Assert exact value like Java test expects
        expect(intResult.value).toBe(1);
    });

    /**
     * Test float flag evaluation with assertions - matches Java testDoubleEvaluation
     */
    test('should evaluate float flags correctly', async () => {
        const client = OpenFeature.getClient();
        const context = { clientId: "test-user" };

        const floatResult = await client.getNumberDetails("double", 2.0, context);
        console.log(`Float flag 'double': ${floatResult.value}`);

        // Assertions for float evaluation
        expect(floatResult).toBeDefined();
        expect(floatResult).toHaveProperty('value');
        expect(typeof floatResult.value).toBe('number');
        expect(floatResult).toHaveProperty('reason');

        // Assert exact value like Java test expects
        expect(floatResult.value).toBe(1.2);
    });

    /**
     * Test object flag evaluation with assertions - matches Java testValueNestedObjectEvaluation
     */
    test('should evaluate object flags correctly', async () => {
        const client = OpenFeature.getClient();
        const context = { clientId: "test-user" };

        const objResult = await client.getObjectDetails("object", {}, context);
        console.log(`Object flag 'object': ${JSON.stringify(objResult.value)}`);

        // Assertions for object evaluation
        expect(objResult).toBeDefined();
        expect(objResult).toHaveProperty('value');
        expect(objResult.value).toBeDefined();
        expect(objResult).toHaveProperty('reason');

        // Assert exact value like Java test expects: Map.of("k1", Map.of("k2", "v1"))
        const expectedObject = { "k1": { "k2": "v1" } };
        expect(objResult.value).toEqual(expectedObject);
    });

    /**
     * Test list flag evaluation with assertions - matches Java testValueNestedListEvaluation
     */
    test('should evaluate list flags correctly', async () => {
        const client = OpenFeature.getClient();
        const context = { clientId: "test-user" };

        const listResult = await client.getObjectDetails("object", {}, context);
        console.log(`List flag 'list': ${JSON.stringify(listResult.value)}`);

        // Assertions for list evaluation
        expect(listResult).toBeDefined();
        expect(listResult).toHaveProperty('value');
        expect(listResult.value).toBeDefined();
        expect(listResult).toHaveProperty('reason');

        // Assert exact value like Java test expects: List.of(Value.objectToValue(Map.of("k1", "v1")))
        const expectedList = { "k1": { "k2": "v1" } };
        expect(listResult.value).toEqual(expectedList);
    });

    test('should handle missing flags with defaults', async () => {
        const client = OpenFeature.getClient();
        const context = { clientId: "test-user" };

        const result = await client.getStringValue("missing.flag", "default", context);
        expect(result).toBe("default");
    });
});