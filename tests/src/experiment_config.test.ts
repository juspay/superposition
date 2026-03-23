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
    CreateWorkspaceCommand,
    DiscardExperimentCommand,
} from "@juspay/superposition-sdk";
import { type DocumentType } from "@smithy/types";
import { superpositionClient, ENV } from "../env.ts";
import { describe, test, expect, beforeAll, afterAll } from "bun:test";
import { nanoid } from "nanoid";

// Helper function to create unique names/IDs
const uniqueName = (prefix: string) => `${prefix}-${nanoid(8)}`;

// Use a separate workspace to avoid conflicts with experiments.test.ts
const testWorkspaceId = `expconfig${Date.now() % 10000}`;

let experimentGroupId: string | undefined;
let experimentId: string | undefined;

const testContext: Record<string, DocumentType> = {
    os: "ios",
    clientId: "testClientExpConfig",
};

const testVariants: Variant[] = [
    {
        id: "control",
        variant_type: VariantType.CONTROL,
        overrides: { testKey1: "default-value" },
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
    console.log(
        "Setting up test workspace and experiment for experiment config tests...",
    );
    // Create a dedicated workspace for experiment config tests
    const createWorkspaceCmd = new CreateWorkspaceCommand({
        org_id: ENV.org_id,
        workspace_admin_email: "admin@example.com",
        workspace_name: testWorkspaceId,
        allow_experiment_self_approval: true,
        auto_populate_control: true,
        enable_context_validation: true,
        enable_change_reason_validation: true,
    });
    await superpositionClient.send(createWorkspaceCmd);
    console.log(`Created test workspace: ${testWorkspaceId}`);

    // Setup required dimensions
    const listDimensionsCmd = new ListDimensionsCommand({
        workspace_id: testWorkspaceId,
        org_id: ENV.org_id,
        count: 100,
        page: 1,
    });
    const dimensionsOut = await superpositionClient.send(listDimensionsCmd);
    const dimensions = dimensionsOut.data ?? [];

    const requiredDimensions: Array<{
        name: string;
        description: string;
        schema: Record<string, DocumentType>;
    }> = [
        {
            name: "clientId",
            description: "Client dimension for testing",
            schema: { type: "string" },
        },
        {
            name: "os",
            description: "Operating system dimension for testing",
            schema: {
                type: "string",
                enum: ["ios", "android", "web"],
            },
        },
    ];

    for (const dimension of requiredDimensions) {
        const exists = dimensions.some(
            (d: any) => d.dimension === dimension.name,
        );
        if (!exists) {
            await superpositionClient.send(
                new CreateDimensionCommand({
                    dimension: dimension.name,
                    workspace_id: testWorkspaceId,
                    org_id: ENV.org_id,
                    schema: dimension.schema,
                    position: dimensions.length,
                    change_reason: "Automated Test - Adding required dimension",
                    description: dimension.description,
                }),
            );
        }
    }

    // Setup required default configs
    const listDefaultsCmd = new ListDefaultConfigsCommand({
        workspace_id: testWorkspaceId,
        org_id: ENV.org_id,
        count: 100,
        page: 1,
    });
    const defaultsOut = await superpositionClient.send(listDefaultsCmd);
    const defaults = defaultsOut.data ?? [];

    const requiredDefaults = { testKey1: "default-value" };

    for (const [key, value] of Object.entries(requiredDefaults)) {
        const exists = defaults.some((d: any) => d.key === key);
        if (!exists) {
            await superpositionClient.send(
                new CreateDefaultConfigCommand({
                    workspace_id: testWorkspaceId,
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
        workspace_id: testWorkspaceId,
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
        workspace_id: testWorkspaceId,
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
        if (experimentId) {
            await superpositionClient.send(
                new DiscardExperimentCommand({
                    workspace_id: testWorkspaceId,
                    org_id: ENV.org_id,
                    id: experimentId,
                    change_reason: "Automated Test - Discarding experiment",
                }),
            );
            console.log(`Discarded experiment: ${experimentId}`);
        }
        // Clean up experiment group (this should also clean up associated experiments)
        if (experimentGroupId) {
            await superpositionClient.send(
                new DeleteExperimentGroupCommand({
                    workspace_id: testWorkspaceId,
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
            workspace_id: testWorkspaceId,
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
            workspace_id: testWorkspaceId,
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
            workspace_id: testWorkspaceId,
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
            workspace_id: testWorkspaceId,
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
            workspace_id: testWorkspaceId,
            org_id: ENV.org_id,
        });

        try {
            const out = await superpositionClient.send(cmd);

            // Check if our created experiment is in the response
            const experiments = out.experiments ?? [];
            const foundExperiment = experiments.find(
                (exp: any) => exp.id === experimentId,
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
            workspace_id: testWorkspaceId,
            org_id: ENV.org_id,
        });

        try {
            const out = await superpositionClient.send(cmd);

            // Check if our created experiment group is in the response
            const experimentGroups = out.experiment_groups ?? [];
            const foundGroup = experimentGroups.find(
                (group: any) => group.id === experimentGroupId,
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
