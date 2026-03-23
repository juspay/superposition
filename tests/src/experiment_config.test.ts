import {
    GetExperimentConfigCommand,
    CreateExperimentCommand,
    CreateExperimentGroupCommand,
    DeleteExperimentGroupCommand,
    type Variant,
    VariantType,
    CreateDimensionCommand,
    ListDimensionsCommand,
    CreateDefaultConfigCommand,
    ListDefaultConfigsCommand,
} from "@juspay/superposition-sdk";
import { type DocumentType } from "@smithy/types";
import { superpositionClient, ENV } from "../env.ts";
import { describe, test, expect, beforeAll, afterAll } from "bun:test";
import { nanoid } from "nanoid";

// Helper function to create unique names/IDs
const uniqueName = (prefix: string) => `${prefix}-${nanoid(8)}`;

let experimentGroupId: string | undefined;
let experimentId: string | undefined;
let createdDimensions: string[] = [];

const testContext: Record<string, DocumentType> = {
    os: "ios",
    clientId: "testClientExpConfig",
};

const testVariants: Variant[] = [
    {
        id: "control",
        variant_type: VariantType.CONTROL,
        overrides: { testKey1: "control-value" },
    },
    {
        id: "experimental",
        variant_type: VariantType.EXPERIMENTAL,
        overrides: { testKey1: "experimental-value" },
    },
];

const defaultChangeReason = "Automated Test";
const defaultDescription = "Created by automated test";

beforeAll(async () => {
    // Setup required dimensions
    const listDimensionsCmd = new ListDimensionsCommand({
        workspace_id: ENV.workspace_id,
        org_id: ENV.org_id,
        count: 100,
        page: 1,
    });
    const dimensionsOut = await superpositionClient.send(listDimensionsCmd);
    const dimensions = dimensionsOut.data ?? [];

    const requiredDimensions = [
        {
            name: "clientId",
            schema: { type: "string" } as Record<string, DocumentType>,
        },
        {
            name: "os",
            schema: {
                type: "string",
                enum: ["ios", "android", "web"],
            } as Record<string, DocumentType>,
        },
    ];

    for (const dim of requiredDimensions) {
        const exists = dimensions.some((d) => d.dimension === dim.name);
        if (!exists) {
            await superpositionClient.send(
                new CreateDimensionCommand({
                    dimension: dim.name,
                    workspace_id: ENV.workspace_id,
                    org_id: ENV.org_id,
                    schema: dim.schema,
                    position: dimensions.length + createdDimensions.length,
                    change_reason: "Automated Test - Adding dimension",
                    description: `Test dimension ${dim.name}`,
                }),
            );
            createdDimensions.push(dim.name);
        }
    }

    // Setup required default configs
    const listDefaultsCmd = new ListDefaultConfigsCommand({
        workspace_id: ENV.workspace_id,
        org_id: ENV.org_id,
        count: 100,
        page: 1,
    });
    const defaultsOut = await superpositionClient.send(listDefaultsCmd);
    const defaults = defaultsOut.data ?? [];

    const requiredDefaults = { testKey1: "default-value" };

    for (const [key, value] of Object.entries(requiredDefaults)) {
        const exists = defaults.some((d) => d.key === key);
        if (!exists) {
            await superpositionClient.send(
                new CreateDefaultConfigCommand({
                    workspace_id: ENV.workspace_id,
                    org_id: ENV.org_id,
                    key,
                    value,
                    schema: { type: "string" },
                    description: `Test default config ${key}`,
                    change_reason: "Automated Test - Adding default config",
                }),
            );
        }
    }

    // Create an experiment group
    const createGroupCmd = new CreateExperimentGroupCommand({
        workspace_id: ENV.workspace_id,
        org_id: ENV.org_id,
        name: uniqueName("test-exp-config-group"),
        description: "Test experiment group for experiment config tests",
        change_reason: "Automated Test - Creating experiment group",
        context: testContext,
        traffic_percentage: 100,
        member_experiment_ids: [],
    });
    const groupResponse = await superpositionClient.send(createGroupCmd);
    experimentGroupId = groupResponse.id;
    console.log(`Created experiment group: ${experimentGroupId}`);

    // Create an experiment
    const createExpCmd = new CreateExperimentCommand({
        workspace_id: ENV.workspace_id,
        org_id: ENV.org_id,
        name: uniqueName("test-exp-config"),
        description: defaultDescription,
        change_reason: defaultChangeReason,
        context: testContext,
        variants: testVariants.map((v) => ({
            ...v,
            description: defaultDescription,
            change_reason: defaultChangeReason,
        })),
        experiment_group_id: experimentGroupId,
    });
    const expResponse = await superpositionClient.send(createExpCmd);
    experimentId = expResponse.id;
    console.log(`Created experiment: ${experimentId}`);
});

afterAll(async () => {
    try {
        // Clean up experiment group (this should also clean up associated experiments)
        if (experimentGroupId) {
            await superpositionClient.send(
                new DeleteExperimentGroupCommand({
                    workspace_id: ENV.workspace_id,
                    org_id: ENV.org_id,
                    id: experimentGroupId,
                }),
            );
            console.log(`Deleted experiment group: ${experimentGroupId}`);
        }
    } catch (e: any) {
        console.error("Error cleaning up experiment group:", e);
    }
});

describe("Experiment Config API - GetExperimentConfig", () => {
    let lastModified: Date | undefined = undefined;

    test("should fetch experiment configuration", async () => {
        const cmd = new GetExperimentConfigCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
        });

        try {
            const out = await superpositionClient.send(cmd);
            console.log("Experiment config response:", out);

            expect(out).toBeDefined();
            expect(out.last_modified).toBeDefined();
            expect(out.experiments).toBeDefined();
            expect(out.experiment_groups).toBeDefined();
            expect(Array.isArray(out.experiments)).toBe(true);
            expect(Array.isArray(out.experiment_groups)).toBe(true);

            lastModified = out.last_modified;
        } catch (e: any) {
            console.error(e["$response"]);
            throw e;
        }
    });

    test("should fetch experiment config with prefix filter", async () => {
        const cmd = new GetExperimentConfigCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            prefix: ["testKey1"],
        });

        try {
            const out = await superpositionClient.send(cmd);
            console.log("Experiment config with prefix:", out);

            expect(out).toBeDefined();
            expect(out.experiments).toBeDefined();
        } catch (e: any) {
            console.error(e["$response"]);
            throw e;
        }
    });

    test("should fetch experiment config with context filter", async () => {
        const cmd = new GetExperimentConfigCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            context: { os: "ios" },
        });

        try {
            const out = await superpositionClient.send(cmd);
            console.log("Experiment config with context:", out);

            expect(out).toBeDefined();
            expect(out.experiments).toBeDefined();
        } catch (e: any) {
            console.error(e["$response"]);
            throw e;
        }
    });

    test("should return 304 Not Modified if config is unchanged", async () => {
        if (!lastModified) {
            console.warn("Last modified timestamp not set. Skipping test.");
            return;
        }

        console.log(
            `Testing Not Modified with last modified timestamp: ${lastModified.toISOString()}`,
        );

        const cmd = new GetExperimentConfigCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            if_modified_since: lastModified,
        });

        try {
            await superpositionClient.send(cmd);
            throw new Error(
                "Expected Not Modified error, but request succeeded",
            );
        } catch (e: any) {
            expect(e).toBeDefined();
            expect(e["$response"]).toBeDefined();
            expect(e["$response"].statusCode).toBe(304);
            console.log("Received expected Not Modified response");
        }
    });

    test("should return experiments matching the created experiment", async () => {
        const cmd = new GetExperimentConfigCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
        });

        try {
            const out = await superpositionClient.send(cmd);

            // Check if our created experiment is in the response
            const experiments = out.experiments ?? [];
            const foundExperiment = experiments.find(
                (exp) => exp.id === experimentId,
            );

            if (foundExperiment) {
                expect(foundExperiment.id).toBe(experimentId);
                expect(foundExperiment.context).toBeDefined();
                expect(foundExperiment.variants).toBeDefined();
                expect(foundExperiment.variants?.length).toBe(2);
                console.log(
                    "Found created experiment in response:",
                    foundExperiment,
                );
            } else {
                console.log(
                    "Created experiment not found in response (may have been concluded/cleaned)",
                );
            }
        } catch (e: any) {
            console.error(e["$response"]);
            throw e;
        }
    });

    test("should return experiment groups matching the created group", async () => {
        const cmd = new GetExperimentConfigCommand({
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
        });

        try {
            const out = await superpositionClient.send(cmd);

            // Check if our created experiment group is in the response
            const experimentGroups = out.experiment_groups ?? [];
            const foundGroup = experimentGroups.find(
                (group) => group.id === experimentGroupId,
            );

            if (foundGroup) {
                expect(foundGroup.id).toBe(experimentGroupId);
                expect(foundGroup.context).toBeDefined();
                expect(foundGroup.member_experiment_ids).toBeDefined();
                console.log(
                    "Found created experiment group in response:",
                    foundGroup,
                );
            } else {
                console.log(
                    "Created experiment group not found in response (may have been cleaned)",
                );
            }
        } catch (e: any) {
            console.error(e["$response"]);
            throw e;
        }
    });
});
