import {
    GetConfigCommand,
    GetResolvedConfigWithIdentifierCommand,
    ListVersionsCommand,
    UpdateWorkspaceCommand,
    CreateDimensionCommand,
    CreateDefaultConfigCommand,
    CreateExperimentCommand,
    RampExperimentCommand,
    ConcludeExperimentCommand,
    DeleteDimensionCommand,
    DeleteDefaultConfigCommand,
    DeleteContextCommand,
    ListContextsCommand,
    VariantType,
    ExperimentStatusType,
    type Variant,
    DiscardExperimentCommand,
    type ExperimentResponse,
    GetExperimentCommand,
} from "@juspay/superposition-sdk";
import { superpositionClient, ENV } from "../env.ts";
import { describe, test, expect, beforeAll, afterAll } from "bun:test";
import { nanoid } from "nanoid";

let configVersionId: string | undefined = undefined;

describe("Resolve Config API - GetResolvedConfigWithIdentifier", () => {
    let experimentId: string | undefined;
    let dimensionName: string;
    let configKey: string;
    const testClientId = "test-client-bucketing-123";
    const testIdentifier = "test-identifier-bucketing-456";
    const defaultValue = "default-bucketing-value";
    const experimentalValue = "experimental-bucketing-value";

    beforeAll(async () => {
        // Create unique names
        dimensionName = `clientId`;
        configKey = `testKey_1234`;

        try {
            // 1. Create dimension for clientId
            try {
                console.log(`Creating dimension: ${dimensionName}`);
                const createDimCmd = new CreateDimensionCommand({
                    dimension: dimensionName,
                    workspace_id: ENV.workspace_id,
                    org_id: ENV.org_id,
                    schema: { type: "string" },
                    position: 1,
                    change_reason: "Automated Test - Bucketing test dimension",
                    description: "Client ID dimension for bucketing test",
                });
                await superpositionClient.send(createDimCmd);
                console.log(`Dimension created: ${dimensionName}`);
            } catch (e: any) {
                // If dimension already exists (409 conflict), continue
                if (e.$response?.statusCode === 409 || e.statusCode === 409) {
                    console.log(
                        `Dimension ${dimensionName} already exists, continuing...`
                    );
                } else {
                    throw e;
                }
            }

            // 2. Create default config
            try {
                console.log(`Creating default config: ${configKey}`);
                const createDefaultCmd = new CreateDefaultConfigCommand({
                    workspace_id: ENV.workspace_id,
                    org_id: ENV.org_id,
                    key: configKey,
                    value: defaultValue,
                    schema: { type: "string" },
                    description: `Default config for bucketing test`,
                    change_reason: "Automated Test - Bucketing default config",
                });
                await superpositionClient.send(createDefaultCmd);
                console.log(
                    `Default config created: ${configKey} = ${defaultValue}`
                );
            } catch (e: any) {
                // If default config already exists (409 conflict), continue
                const statusCode = e.$response?.statusCode || e.statusCode;
                const errorMessage = e.$response?.body || e.message || "";

                if (
                    statusCode === 409 ||
                    errorMessage.includes("already exists")
                ) {
                    console.log(
                        `Default config ${configKey} already exists, continuing...`
                    );
                } else {
                    console.error(
                        "Error creating default config:",
                        e?.$response || e.message
                    );
                    throw e;
                }
            }

            // 3. Create experiment
            console.log("Creating experiment...");
            const experimentContext = {
                [dimensionName]: testClientId,
            };

            const experimentVariants: Omit<
                Variant,
                "id" | "context_id" | "override_id"
            >[] = [
                {
                    variant_type: VariantType.CONTROL,
                    overrides: {
                        [configKey]: defaultValue,
                    },
                },
                {
                    variant_type: VariantType.EXPERIMENTAL,
                    overrides: {
                        [configKey]: experimentalValue,
                    },
                },
            ];

            const createExpCmd = new CreateExperimentCommand({
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                name: `bucketing-test-exp-${nanoid(8)}`,
                context: experimentContext,
                variants: experimentVariants.map((v, index) => ({
                    id: index === 0 ? "control" : `test${index}`,
                    variant_type: v.variant_type,
                    overrides: v.overrides,
                    description: "Bucketing test variant",
                    change_reason: "Automated Test - Bucketing variant",
                })),
                description: "Experiment for bucketing test",
                change_reason: "Automated Test - Bucketing experiment",
            });

            const expResponse = await superpositionClient.send(createExpCmd);
            experimentId = expResponse.id;
            console.log(`Experiment created: ${experimentId}`);

            // 4. Ramp experiment to 50%
            console.log("Ramping experiment to 50%...");
            const rampCmd = new RampExperimentCommand({
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                id: experimentId!,
                traffic_percentage: 50,
                change_reason: "Automated Test - Ramping to 50%",
            });
            await superpositionClient.send(rampCmd);
            console.log("Experiment ramped to 50%");
        } catch (e: any) {
            console.error("Error in beforeAll:", e?.$response || e.message);
            throw e;
        }
    });

    afterAll(async () => {
        try {
            // 1. Discard experiment
            try {
                if (!experimentId) {
                    throw new Error("Experiment ID not set, cannot discard.");
                }
                const cmd = new DiscardExperimentCommand({
                    workspace_id: ENV.workspace_id,
                    org_id: ENV.org_id,
                    id: experimentId,
                    change_reason: "Discarding experiment after bucketing test",
                });
                const out: ExperimentResponse = await superpositionClient.send(
                    cmd
                );
                expect(out).toBeDefined();
                expect(out.id).toBe(experimentId);
                expect(out.status).toBe(ExperimentStatusType.DISCARDED);
                expect(out.experiment_group_id).toBeUndefined();

                const getCmd = new GetExperimentCommand({
                    workspace_id: ENV.workspace_id,
                    org_id: ENV.org_id,
                    id: experimentId,
                });
                const updatedExp = await superpositionClient.send(getCmd);
                expect(updatedExp.status).toBe(ExperimentStatusType.DISCARDED);
                expect(updatedExp.experiment_group_id).toBeUndefined();
            } catch (e: any) {
                console.error(
                    "Error discarding experiment:",
                    e?.$response || e.message
                );
                throw e;
            }

            // 2. Delete default config
            if (configKey) {
                console.log(`Deleting default config: ${configKey}`);
                const deleteDefaultCmd = new DeleteDefaultConfigCommand({
                    workspace_id: ENV.workspace_id,
                    org_id: ENV.org_id,
                    key: configKey,
                });
                await superpositionClient.send(deleteDefaultCmd);
                console.log("Default config deleted");
            }

            // 3. Delete dimension
            if (dimensionName) {
                console.log(`Deleting dimension: ${dimensionName}`);
                const deleteDimCmd = new DeleteDimensionCommand({
                    workspace_id: ENV.workspace_id,
                    org_id: ENV.org_id,
                    dimension: dimensionName,
                });
                await superpositionClient.send(deleteDimCmd);
                console.log("Dimension deleted");
            }
        } catch (e: any) {
            console.error(
                "Error in afterAll cleanup:",
                e?.$response || e.message
            );
        }
    });

    test("GetResolvedConfigWithIdentifier: should return either default or experimental value based on bucketing", async () => {
        try {
            // Call GetResolvedConfigWithIdentifier with matching context
            const context = {
                [dimensionName]: testClientId,
            };

            console.log(
                `Calling GetResolvedConfigWithIdentifier with identifier: ${testIdentifier}`
            );
            console.log(`Context:`, context);

            const cmd = new GetResolvedConfigWithIdentifierCommand({
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                prefix: [configKey],
                identifier: testIdentifier,
                context: context,
            });

            const out = await superpositionClient.send(cmd);

            console.log("Response:", JSON.stringify(out, null, 2));

            expect(out).toBeDefined();
            expect(out.version).toBeDefined();

            // The config should contain our key
            expect(out.config).toBeDefined();
            expect(out.config).toHaveProperty(configKey);

            const receivedValue = (out.config as any)[configKey] as string;
            console.log(`Received value for ${configKey}: ${receivedValue}`);

            // Based on bucketing, we should get either default or experimental value
            const isDefaultValue = receivedValue === defaultValue;
            const isExperimentalValue = receivedValue === experimentalValue;

            expect(isDefaultValue || isExperimentalValue).toBe(true);

            if (isExperimentalValue) {
                console.log("✓ Identifier bucketed into EXPERIMENTAL variant");
            } else {
                console.log(
                    "✓ Identifier bucketed into CONTROL variant (default value)"
                );
            }

            // Store version for next test
            configVersionId = out.version;
        } catch (e: any) {
            console.error(
                "Error in bucketing test:",
                e?.$response || e.message
            );
            throw e;
        }
    });
});
