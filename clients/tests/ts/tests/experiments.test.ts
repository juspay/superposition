import {
    ListExperimentCommand,
    CreateExperimentCommand,
    GetExperimentCommand,
    UpdateOverridesExperimentCommand,
    RampExperimentCommand,
    ConcludeExperimentCommand,
    type ExperimentResponse,
    type Variant,
    VariantType,
    ExperimentStatusType,
    type VariantUpdateRequest,
    CreateDimensionCommand,
    ListDimensionsCommand,
} from "@io.juspay/superposition-sdk";
import { superpositionClient, ENV } from "./env.ts";
import { expect, describe, test, beforeAll } from "bun:test";
import { versionMajorMinor } from "typescript";

const delay = (ms: number) => new Promise(resolve => setTimeout(resolve, ms));

describe("Experiments API", () => {
    let experimentId1: string | undefined;
    let experiment1Variants: Variant[] | undefined;

    const defaultChangeReason = "Automated Test";
    const defaultDescription = "Created by automated test";

    const experiment1Context = {
        "and": [
            { "==": [{ "var": "os" }, "ios"] },
            { "==": [{ "var": "client" }, "testClientCac1"] }
        ]
    };
    const experiment1InitialVariants: Omit<Variant, 'id' | 'context_id' | 'override_id'>[] = [
        {
            variant_type: VariantType.CONTROL,
            overrides: { "pmTestKey1": "value1-control", "pmTestKey2": "value1-control" }
        },
        {
            variant_type: VariantType.EXPERIMENTAL,
            overrides: { "pmTestKey1": "value2-test", "pmTestKey2": "value2-test" }
        }
    ];

    const experiment2Context = {
        "and": [
            { "==": [{ "var": "os" }, "ios"] },
            { "==": [{ "var": "client" }, "testClientCac02"] }
        ]
    };
    const experiment2InitialVariants: Omit<Variant, 'id' | 'context_id' | 'override_id'>[] = [
        {
            variant_type: VariantType.CONTROL,
            overrides: { "pmTestKey3": "value3-control", "pmTestKey4": "value3-control" }
        },
        {
            variant_type: VariantType.EXPERIMENTAL,
            overrides: { "pmTestKey3": "value4-test", "pmTestKey4": "value4-test" }
        }
    ];

    beforeAll(async () => {
        const cmd = new ListDimensionsCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            count: 100,
            page: 1,
        });
        const out = await superpositionClient.send(cmd);
        const dimensions = out.data || [];
        const requiredDimensions = [
            {
                name: "client",
                description: "Client dimension for testing",
                schema: { "type": "string" }
            },
            {
                name: "os",
                description: "Operating system dimension for testing",
                schema: {
                    "type": "string",
                    "enum": ["ios", "android", "web"]
                }
            },
        ];

        for (const dimension of requiredDimensions) {
            const exists = dimensions.some(d => d.dimension === dimension.name);
            if (!exists) {
                const createCmd = new CreateDimensionCommand({
                    dimension: dimension.name,
                    workspace_id: ENV.workspace_id,
                    org_id: ENV.org_id,
                    schema: dimension.schema,
                    position: dimensions.length + 1,
                    change_reason: "Automated Test - Adding required dimension",
                    description: dimension.description,
                });
                await superpositionClient.send(createCmd);
            }
        }

        const defaultConfigs = {
            pmTestKey1: "default-value1",
            pmTestKey2: "default-value2",
            pmTestKey3: "default-value3",
            pmTestKey4: "default-value4",
        };

        for (const [key, value] of Object.entries(defaultConfigs)) {
            const existingConfig = dimensions.find(d => d.dimension === key);
            if (!existingConfig) {
            const createCmd = new CreateDimensionCommand({
                dimension: key,
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                schema: { type: "string" },
                position: dimensions.length + 1,
                change_reason: "Automated Test - Adding default config",
                description: `Default config for ${key}`,
            });
            await superpositionClient.send(createCmd);
            } else {
            console.log(`Default config for key "${key}" already exists.`);
            }
        }
    });

    test("1. Create Experiment 1", async () => {
        const cmd = new CreateExperimentCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            name: "experiment-1-from-test",
            context: experiment1Context,
            variants: experiment1InitialVariants.map((v, index) => ({
                id: index === 0 ? 'control' : `test${index}`,
                variant_type: v.variant_type,
                overrides: v.overrides,
                description: defaultDescription,
                change_reason: defaultChangeReason,
            })),
            description: defaultDescription,
            change_reason: defaultChangeReason,
        });

        const out: ExperimentResponse = await superpositionClient.send(cmd);
        expect(out).toBeDefined();
        expect(out.id).toBeDefined();
        expect(out.name).toBe("experiment-1-from-test");
        expect(out.status).toBe(ExperimentStatusType.CREATED);
        expect(out.traffic_percentage).toBe(0);
        expect(out.override_keys).toEqual(Object.keys(experiment1InitialVariants[0].overrides));
        expect(out.chosen_variant).toBeUndefined();
        expect(out.context).toEqual(experiment1Context);
        expect(out.variants).toHaveLength(experiment1InitialVariants.length);

        experimentId1 = out.id;
        experiment1Variants = out.variants;
    });

    test("2. Get Experiment 1", async () => {
        if (!experimentId1) {
            throw new Error("Experiment 1 ID not set, cannot get.");
        }
        const cmd = new GetExperimentCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            id: experimentId1,
        });
        const out: ExperimentResponse = await superpositionClient.send(cmd);
        expect(out).toBeDefined();
        expect(out.id).toBe(experimentId1);
        expect(out.name).toBe("experiment-1-from-test");
        expect(out.variants).toHaveLength(experiment1InitialVariants.length);
    });

    test("3. Ramp Experiment 1", async () => {
        if (!experimentId1) {
            throw new Error("Experiment 1 ID not set, cannot ramp.");
        }
        const rampPercentage = 46;
        const cmd = new RampExperimentCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            id: experimentId1,
            traffic_percentage: rampPercentage,
            change_reason: "Testing ramp functionality",
        });
        const out: ExperimentResponse = await superpositionClient.send(cmd);
        expect(out).toBeDefined();
        expect(out.id).toBe(experimentId1);
        expect(out.traffic_percentage).toBe(rampPercentage);
        expect(out.status).toBe(ExperimentStatusType.INPROGRESS);

        await delay(500);
        const getCmd = new GetExperimentCommand({ workspace_id: ENV.workspace_id, org_id: ENV.org_id, id: experimentId1 });
        const updatedExp = await superpositionClient.send(getCmd);
        expect(updatedExp.traffic_percentage).toBe(rampPercentage);
    });

    test("4. Conclude Experiment 1", async () => {
        if (!experimentId1 || !experiment1Variants) {
            throw new Error("Experiment 1 ID or variants not set, cannot conclude.");
        }
        const controlVariant = experiment1Variants.find(v => v.variant_type === VariantType.CONTROL);
        if (!controlVariant || !controlVariant.id) {
            throw new Error("Could not find control variant ID for Experiment 1.");
        }
        const winnerVariantId = controlVariant.id;

        const cmd = new ConcludeExperimentCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            id: experimentId1,
            chosen_variant: winnerVariantId,
            description: "Concluding experiment, control wins",
            change_reason: "Testing conclude functionality",
        });
        const out: ExperimentResponse = await superpositionClient.send(cmd);
        expect(out).toBeDefined();
        expect(out.id).toBe(experimentId1);
        expect(out.status).toBe(ExperimentStatusType.CONCLUDED);
        expect(out.chosen_variant).toBe(winnerVariantId);

        await delay(500);
        const getCmd = new GetExperimentCommand({ workspace_id: ENV.workspace_id, org_id: ENV.org_id, id: experimentId1 });
        const updatedExp = await superpositionClient.send(getCmd);
        expect(updatedExp.status).toBe(ExperimentStatusType.CONCLUDED);
        expect(updatedExp.chosen_variant).toBe(winnerVariantId);
    });

    let experimentId2: string | undefined;
    let experiment2Variants: Variant[] | undefined;

    test("5. Create Experiment 2", async () => {
        const cmd = new CreateExperimentCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            name: "experiment-2-from-test",
            context: experiment2Context,
            variants: experiment2InitialVariants.map((v, index) => ({
                id: index === 0 ? 'control' : `test${index}`,
                variant_type: v.variant_type,
                overrides: v.overrides,
                description: defaultDescription,
                change_reason: defaultChangeReason,
            })),
            description: defaultDescription,
            change_reason: defaultChangeReason,
        });

        const out: ExperimentResponse = await superpositionClient.send(cmd);
        expect(out).toBeDefined();
        expect(out.id).toBeDefined();
        expect(out.name).toBe("experiment-2-from-test");
        expect(out.status).toBe(ExperimentStatusType.CREATED);
        expect(out.override_keys).toEqual(Object.keys(experiment2InitialVariants[0].overrides));

        experimentId2 = out.id;
        experiment2Variants = out.variants;
    });

    test("6. Update Experiment 2 Overrides", async () => {
        if (!experimentId2 || !experiment2Variants) {
            throw new Error("Experiment 2 ID or variants not set, cannot update overrides.");
        }

        const controlVariant = experiment2Variants.find(v => v.variant_type === VariantType.CONTROL);
        const testVariant = experiment2Variants.find(v => v.variant_type === VariantType.EXPERIMENTAL);

        if (!controlVariant?.id || !testVariant?.id) {
            throw new Error("Could not find variant IDs for Experiment 2.");
        }

        const updatedVariants: VariantUpdateRequest[] = [
            {
                id: controlVariant.id,
                overrides: { "pmTestKey1": "value-7910-an-control", "pmTestKey2": "value-6910-an-control" }
            },
            {
                id: testVariant.id,
                overrides: { "pmTestKey1": "value-7920-an-test", "pmTestKey2": "value-6930-an-test" }
            }
        ];

        const cmd = new UpdateOverridesExperimentCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            id: experimentId2,
            variant_list: updatedVariants,
            description: "Updating override keys and values",
            change_reason: "Testing override update",
        });

        const out: ExperimentResponse = await superpositionClient.send(cmd);
        expect(out).toBeDefined();
        expect(out.id).toBe(experimentId2);
        expect(out.override_keys).toEqual(Object.keys(updatedVariants[0].overrides));
        expect(out.variants).toHaveLength(experiment2Variants.length);
        expect(out.status).toBe(ExperimentStatusType.CREATED);
        expect(out.traffic_percentage).toBe(0);
        expect(out.chosen_variant).toBeUndefined();
        expect(out.context).toEqual(experiment2Context);
        expect(out.name).toBe("experiment-2-from-test");

        const updatedControl = out.variants?.find(v => v.id === controlVariant.id);
        const updatedTest = out.variants?.find(v => v.id === testVariant.id);

        expect(updatedControl?.overrides).toEqual(updatedVariants[0].overrides);
        expect(updatedTest?.overrides).toEqual(updatedVariants[1].overrides);
    });

    test("7. List Experiments (Basic)", async () => {
        const cmd = new ListExperimentCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
        });
        const out = await superpositionClient.send(cmd);
        expect(out).toBeDefined();
        expect(Array.isArray(out.data)).toBe(true);
    });
});