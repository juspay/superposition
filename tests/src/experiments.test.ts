import {
    ListExperimentCommand,
    CreateExperimentCommand,
    GetExperimentCommand,
    UpdateOverridesExperimentCommand,
    RampExperimentCommand,
    ConcludeExperimentCommand,
    CreateExperimentGroupCommand,
    DeleteExperimentGroupCommand,
    RemoveMembersFromGroupCommand,
    type ExperimentResponse,
    type Variant,
    VariantType,
    ExperimentStatusType,
    type VariantUpdateRequest,
    type CreateExperimentGroupCommandInput,
    CreateDimensionCommand,
    ListDimensionsCommand,
    CreateDefaultConfigCommand,
    ListDefaultConfigsCommand,
    DeleteDimensionCommand,
    ListContextsCommand,
    DeleteContextCommand,
    GetContextCommand,
    DeleteDefaultConfigCommand,
    WorkspaceStatus,
    UpdateWorkspaceCommand,
    SuperpositionClient,
    GetExperimentGroupCommand,
    DiscardExperimentCommand,
    CreateContextCommand,
    CreateWorkspaceCommand,
} from "@juspay/superposition-sdk";
import { superpositionClient, ENV } from "../env.ts";
import { expect, describe, test, beforeAll, afterAll } from "bun:test";
import { nanoid } from "nanoid";

// Helper function to create unique names/IDs
const uniqueName = (prefix: string) => `${prefix}-${nanoid(8)}`;

describe("Experiments API", () => {
    let experimentId1: string | undefined;
    let experiment1Variants: Variant[] | undefined;
    let experimentGroupId: string | undefined;
    let created_dimensions: string[] = [];

    const defaultChangeReason = "Automated Test";
    const defaultDescription = "Created by automated test";

    const experiment1Context = ENV.jsonlogic_enabled
        ? {
              and: [
                  { "==": [{ var: "os" }, "ios"] },
                  { "==": [{ var: "clientId" }, "testClientCac1"] },
              ],
          }
        : {
              os: "ios",
              clientId: "testClientCac1",
          };
    const experiment1InitialVariants: Omit<
        Variant,
        "id" | "context_id" | "override_id"
    >[] = [
        {
            variant_type: VariantType.CONTROL,
            overrides: {
                pmTestKey1: "value1-control",
                pmTestKey2: "value1-control",
            },
        },
        {
            variant_type: VariantType.EXPERIMENTAL,
            overrides: { pmTestKey1: "value2-test", pmTestKey2: "value2-test" },
        },
    ];

    const experiment2Context = ENV.jsonlogic_enabled
        ? {
              and: [
                  { "==": [{ var: "os" }, "ios"] },
                  { "==": [{ var: "clientId" }, "testClientCac02"] },
              ],
          }
        : {
              os: "ios",
              clientId: "testClientCac02",
          };
    const experiment2InitialVariants: Omit<
        Variant,
        "id" | "context_id" | "override_id"
    >[] = [
        {
            variant_type: VariantType.CONTROL,
            overrides: {
                pmTestKey3: "value3-control",
                pmTestKey4: "value3-control",
            },
        },
        {
            variant_type: VariantType.EXPERIMENTAL,
            overrides: { pmTestKey3: "value4-test", pmTestKey4: "value4-test" },
        },
    ];

    // Experiment group context (common base for both experiments)
    const experimentGroupContext = ENV.jsonlogic_enabled
        ? {
              and: [
                  { "==": [{ var: "os" }, "ios"] },
                  { "==": [{ var: "clientId" }, "testClientCac1"] },
              ],
          }
        : {
              os: "ios",
              clientId: "testClientCac1",
          };

    const auto_populate_test_workspace = `temptestexp${Date.now() % 10000}`;

    beforeAll(async () => {
        async function setupWorkspace(workspaceId: string) {
            const cmd = new ListDimensionsCommand({
                workspace_id: workspaceId,
                org_id: ENV.org_id,
                count: 100,
                page: 1,
            });
            const out = await superpositionClient.send(cmd);
            const dimensions = out.data || [];
            const requiredDimensions = [
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
                    (d) => d.dimension === dimension.name
                );
                if (!exists) {
                    const createCmd = new CreateDimensionCommand({
                        dimension: dimension.name,
                        workspace_id: workspaceId,
                        org_id: ENV.org_id,
                        schema: dimension.schema,
                        position: dimensions.length,
                        change_reason:
                            "Automated Test - Adding required dimension",
                        description: dimension.description,
                    });
                    await superpositionClient.send(createCmd);
                    created_dimensions.push(dimension.name);
                }
            }

            const defaultConfigsNeeded = {
                pmTestKey1: "default-value1",
                pmTestKey2: "default-value2",
                pmTestKey3: "default-value3",
                pmTestKey4: "default-value4",
            };

            const defaultConfigCmg = new ListDefaultConfigsCommand({
                workspace_id: workspaceId,
                org_id: ENV.org_id,
                count: 100,
                page: 1,
            });
            const defaultConfigOut = await superpositionClient.send(
                defaultConfigCmg
            );
            const defaultConfigs = defaultConfigOut.data || [];

            for (const [key, value] of Object.entries(defaultConfigsNeeded)) {
                const existingConfig = defaultConfigs.find(
                    (d) => d.key === key
                );
                if (!existingConfig) {
                    const createCmd = new CreateDefaultConfigCommand({
                        workspace_id: workspaceId,
                        org_id: ENV.org_id,
                        key: key,
                        value: value,
                        schema: { type: "string" },
                        description: `Default config for ${key}`,
                        change_reason: "Automated Test - Adding default config",
                    });
                    await superpositionClient.send(createCmd);
                } else {
                    console.log(
                        `Default config for key "${key}" already exists.`
                    );
                }
            }

            // Create experiment group
            console.log("Creating experiment group...");
            const groupName = uniqueName("test-exp-group");
            const createGroupInput: CreateExperimentGroupCommandInput = {
                workspace_id: workspaceId,
                org_id: ENV.org_id,
                name: groupName,
                description: "Test experiment group for automated tests",
                change_reason: "Automated Test - Creating experiment group",
                context: experimentGroupContext,
                traffic_percentage: 100,
                member_experiment_ids: [], // Start with empty, will add experiments later
            };

            const groupResponse = await superpositionClient.send(
                new CreateExperimentGroupCommand(createGroupInput)
            );
            experimentGroupId = groupResponse.id!;
            console.log(`Created experiment group: ${experimentGroupId}`);
        }
        const createWorkspaceCmd = new CreateWorkspaceCommand({
            org_id: ENV.org_id,
            workspace_admin_email: "admin@example.com",
            workspace_name: auto_populate_test_workspace,
            strict_mode: true,
            allow_experiment_self_approval: true,
            auto_populate_control: true,
        });
        await superpositionClient.send(createWorkspaceCmd);

        await setupWorkspace(auto_populate_test_workspace);

        await setupWorkspace(ENV.workspace_id);
    });

    afterAll(async () => {
        try {
            // Clean up experiments
            const experimentsCmd = new ListExperimentCommand({
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                count: 100,
                page: 1,
            });
            const experimentsOut = await superpositionClient.send(
                experimentsCmd
            );
            const experiments = experimentsOut.data || [];
            for (const experiment of experiments) {
                if (experiment.name && experiment.name.includes("-from-test")) {
                    if (
                        experiment.status !== ExperimentStatusType.CONCLUDED &&
                        experiment.status !== ExperimentStatusType.DISCARDED
                    ) {
                        const controlVariant = experiment.variants?.find(
                            (v) => v.variant_type === VariantType.CONTROL
                        );
                        if (controlVariant && controlVariant.id) {
                            const concludeCmd = new ConcludeExperimentCommand({
                                workspace_id: ENV.workspace_id,
                                org_id: ENV.org_id,
                                id: experiment.id,
                                chosen_variant: controlVariant.id,
                                description:
                                    "Cleanup - concluding test experiment",
                                change_reason: "Automated test cleanup",
                            });
                            await superpositionClient.send(concludeCmd);
                            console.log(
                                `Concluded experiment: ${experiment.name} (${experiment.id})`
                            );
                        }
                    }
                }
            }

            // Clean up experiment group
            if (experimentGroupId) {
                try {
                    // First remove all members if any
                    console.log(
                        `Cleaning up experiment group: ${experimentGroupId}`
                    );

                    // Remove all members from the group first
                    if (experimentId1 || experimentId2) {
                        const membersToRemove = [];
                        if (experimentId1) membersToRemove.push(experimentId1);
                        if (experimentId2) membersToRemove.push(experimentId2);

                        if (membersToRemove.length > 0) {
                            await superpositionClient.send(
                                new RemoveMembersFromGroupCommand({
                                    workspace_id: ENV.workspace_id,
                                    org_id: ENV.org_id,
                                    id: experimentGroupId,
                                    member_experiment_ids: membersToRemove,
                                    change_reason:
                                        "Cleanup - removing members before deleting group",
                                })
                            );
                            console.log(
                                `Removed members from experiment group: ${experimentGroupId}`
                            );
                        }
                    }

                    // Now delete the empty group
                    await superpositionClient.send(
                        new DeleteExperimentGroupCommand({
                            workspace_id: ENV.workspace_id,
                            org_id: ENV.org_id,
                            id: experimentGroupId,
                        })
                    );
                    console.log(
                        `Deleted experiment group: ${experimentGroupId}`
                    );
                } catch (error: any) {
                    if (error.name !== "ResourceNotFound") {
                        console.error(
                            `Failed to clean up experiment group ${experimentGroupId}:`,
                            error.message
                        );
                    }
                }
            }

            // Clean up contexts
            const contextsCmd = new ListContextsCommand({
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                prefix: "pmTestKey",
                count: 100,
                page: 1,
            });
            const contextsOut = await superpositionClient.send(contextsCmd);
            const contexts = contextsOut.data || [];
            for (const context of contexts) {
                const deleteCmd = new DeleteContextCommand({
                    workspace_id: ENV.workspace_id,
                    org_id: ENV.org_id,
                    id: context.id,
                });
                await superpositionClient.send(deleteCmd);
            }

            // Clean up default configs
            const defaultConfigsCmd = new ListDefaultConfigsCommand({
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                count: 100,
                page: 1,
            });
            const defaultConfigsOut = await superpositionClient.send(
                defaultConfigsCmd
            );
            const defaultConfigs = defaultConfigsOut.data || [];
            for (const config of defaultConfigs) {
                const deleteCmd = new DeleteDefaultConfigCommand({
                    workspace_id: ENV.workspace_id,
                    org_id: ENV.org_id,
                    key: config.key,
                });
                await superpositionClient.send(deleteCmd);
            }

            // Clean up dimensions
            console.log("Created dimensions to delete:", created_dimensions);
            for (const dimension of created_dimensions) {
                const deleteCmd = new DeleteDimensionCommand({
                    workspace_id: ENV.workspace_id,
                    org_id: ENV.org_id,
                    dimension: dimension,
                });
                await superpositionClient.send(deleteCmd);
            }
        } catch (e: any) {
            console.error(
                "Error in afterAll cleanup:",
                e?.$response || e.message
            );
        }
    });

    async function addMandatoryDimension(client: SuperpositionClient) {
        const input = {
            org_id: ENV.org_id,
            workspace_name: ENV.workspace_id,
            workspace_admin_email: "updated-admin@example.com",
            workspace_status: WorkspaceStatus.ENABLED,
            mandatory_dimensions: ["clientId"],
        };

        const cmd = new UpdateWorkspaceCommand(input);
        const response = await client.send(cmd);
    }

    async function removeMandatoryDimension(client: SuperpositionClient) {
        const input = {
            org_id: ENV.org_id,
            workspace_name: ENV.workspace_id,
            workspace_admin_email: "updated-admin@example.com",
            workspace_status: WorkspaceStatus.ENABLED,
            mandatory_dimensions: [],
        };

        const cmd = new UpdateWorkspaceCommand(input);
        const response = await client.send(cmd);
    }

    test("0. Create experiment for default config", async () => {
        try {
            await removeMandatoryDimension(superpositionClient);
            const cmd = new CreateExperimentCommand({
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                name: "experiment-0-for-default-config",
                context: {},
                variants: experiment1InitialVariants.map((v, index) => ({
                    id: index === 0 ? "control" : `test${index}`,
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
            expect(out.id).toBeString();
            expect(out.name).toBe("experiment-0-for-default-config");
            expect(out.status).toBe(ExperimentStatusType.CREATED);
            expect(out.traffic_percentage).toBe(0);
            const allInitialOverrideKeys1 = new Set<string>();
            experiment1InitialVariants.forEach((v) =>
                Object.keys(v.overrides).forEach((k) =>
                    allInitialOverrideKeys1.add(k)
                )
            );
            expect(out.override_keys?.sort()).toEqual(
                Array.from(allInitialOverrideKeys1).sort()
            );
            expect(out.chosen_variant).toBeUndefined();
            expect(out.context).toEqual({});
            expect(out.variants).toHaveLength(
                experiment1InitialVariants.length
            );
            expect(out.variants?.[0].variant_type).toBe(VariantType.CONTROL);
            expect(out.variants?.[1].variant_type).toBe(
                VariantType.EXPERIMENTAL
            );
            expect(out.variants?.[0].overrides).toEqual(
                experiment1InitialVariants[0].overrides
            );
            expect(out.variants?.[1].overrides).toEqual(
                experiment1InitialVariants[1].overrides
            );
            expect(out.variants?.[0].context_id).toBeString();
            expect(out.variants?.[1].context_id).toBeString();

            experimentId1 = out.id;
            experiment1Variants = out.variants;

            if (experiment1Variants) {
                for (const variant of experiment1Variants) {
                    if (variant.context_id) {
                        const getContextCmd = new GetContextCommand({
                            workspace_id: ENV.workspace_id,
                            org_id: ENV.org_id,
                            id: variant.context_id,
                        });
                        const contextOut = await superpositionClient.send(
                            getContextCmd
                        );
                        expect(contextOut).toBeDefined();
                        expect(contextOut.override).toEqual(variant.overrides);
                    } else {
                        throw new Error(
                            `Variant ${variant.id} created without a context_id`
                        );
                    }
                }
            }
        } catch (e: any) {
            console.error(
                "Error in test '0. Create experiment for default config':",
                e?.$response || e.message
            );
            throw e;
        } finally {
            await addMandatoryDimension(superpositionClient);
        }
    });

    test("1. Create Experiment 1", async () => {
        try {
            const cmd = new CreateExperimentCommand({
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                name: "experiment-1-from-test",
                context: experiment1Context,
                variants: experiment1InitialVariants.map((v, index) => ({
                    id: index === 0 ? "control" : `test${index}`,
                    variant_type: v.variant_type,
                    overrides: v.overrides,
                    description: defaultDescription,
                    change_reason: defaultChangeReason,
                })),
                description: defaultDescription,
                change_reason: defaultChangeReason,
                experiment_group_id: experimentGroupId, // Add experiment group ID
            });

            const out: ExperimentResponse = await superpositionClient.send(cmd);
            expect(out).toBeDefined();
            expect(out.id).toBeString();
            expect(out.name).toBe("experiment-1-from-test");
            expect(out.status).toBe(ExperimentStatusType.CREATED);
            expect(out.traffic_percentage).toBe(0);
            expect(out.experiment_group_id).toBe(experimentGroupId); // Verify group ID is set
            const allInitialOverrideKeys1 = new Set<string>();
            experiment1InitialVariants.forEach((v) =>
                Object.keys(v.overrides).forEach((k) =>
                    allInitialOverrideKeys1.add(k)
                )
            );
            expect(out.override_keys?.sort()).toEqual(
                Array.from(allInitialOverrideKeys1).sort()
            );
            expect(out.chosen_variant).toBeUndefined();
            expect(out.context).toEqual(experiment1Context);
            expect(out.variants).toHaveLength(
                experiment1InitialVariants.length
            );
            expect(out.variants?.[0].variant_type).toBe(VariantType.CONTROL);
            expect(out.variants?.[1].variant_type).toBe(
                VariantType.EXPERIMENTAL
            );
            expect(out.variants?.[0].overrides).toEqual(
                experiment1InitialVariants[0].overrides
            );
            expect(out.variants?.[1].overrides).toEqual(
                experiment1InitialVariants[1].overrides
            );
            expect(out.variants?.[0].context_id).toBeString();
            expect(out.variants?.[1].context_id).toBeString();

            experimentId1 = out.id;
            experiment1Variants = out.variants;

            if (experiment1Variants) {
                for (const variant of experiment1Variants) {
                    if (variant.context_id) {
                        const getContextCmd = new GetContextCommand({
                            workspace_id: ENV.workspace_id,
                            org_id: ENV.org_id,
                            id: variant.context_id,
                        });
                        const contextOut = await superpositionClient.send(
                            getContextCmd
                        );
                        expect(contextOut).toBeDefined();
                        expect(contextOut.override).toEqual(variant.overrides);
                    } else {
                        throw new Error(
                            `Variant ${variant.id} created without a context_id`
                        );
                    }
                }
            }
        } catch (e: any) {
            console.error(
                "Error in test '1. Create Experiment 1':",
                e?.$response || e.message
            );
            throw e;
        }
    });

    test("2. Get Experiment 1", async () => {
        try {
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
            expect(out.experiment_group_id).toBe(experimentGroupId); // Verify group ID is preserved
            expect(out.variants).toHaveLength(
                experiment1InitialVariants.length
            );
            expect(out.variants).toEqual(experiment1Variants);
        } catch (e: any) {
            console.error(
                "Error in test '2. Get Experiment 1':",
                e?.$response || e.message
            );
            throw e;
        }
    });

    test("3.1 Check Auto populate Control", async () => {
        let contextResponse;

        try {
            const createFailingTest = new CreateExperimentCommand({
                workspace_id: auto_populate_test_workspace,
                org_id: ENV.org_id,
                name: "experiment-1-auto-populate-control",
                context: experiment1Context,
                variants: experiment1InitialVariants.map((v, index) => ({
                    id: index === 0 ? "control" : `test${index}`,
                    variant_type: v.variant_type,
                    overrides: v.overrides,
                    description: defaultDescription,
                    change_reason: defaultChangeReason,
                })),
                description: defaultDescription,
                change_reason: defaultChangeReason,
            });
            expect(
                superpositionClient.send(createFailingTest)
            ).rejects.toThrow();

            const experimentVariants: Omit<
                Variant,
                "id" | "context_id" | "override_id"
            >[] = [
                {
                    variant_type: VariantType.CONTROL,
                    overrides: {
                        pmTestKey1: "default-value1",
                        pmTestKey2: "default-value2",
                    },
                },
                {
                    variant_type: VariantType.EXPERIMENTAL,
                    overrides: {
                        pmTestKey1: "value2-test",
                        pmTestKey2: "value2-test",
                    },
                },
            ];

            const createExperimentCmd = new CreateExperimentCommand({
                workspace_id: auto_populate_test_workspace,
                org_id: ENV.org_id,
                name: "experiment-1-auto-populate-control",
                context: experiment1Context,
                variants: experimentVariants.map((v, index) => ({
                    id: index === 0 ? "control" : `test${index}`,
                    variant_type: v.variant_type,
                    overrides: v.overrides,
                })),
                description: defaultDescription,
                change_reason: defaultChangeReason,
            });
            const expResponse = await superpositionClient.send(
                createExperimentCmd
            );

            console.log("Experiment created successfully:", expResponse.id);

            const createContextCmd = new CreateContextCommand({
                workspace_id: auto_populate_test_workspace,
                org_id: ENV.org_id,
                context: experiment1Context,
                override: {
                    pmTestKey1: "new-control",
                    pmTestKey2: "new-control",
                },
                description: "Context for auto-populate control test",
                change_reason: "Testing auto-populate control",
            });

            contextResponse = await superpositionClient.send(createContextCmd);

            console.log("Context created successfully:", contextResponse.id);

            // Attempt to ramp the experiment, which should fail due to missing overrides
            const rampCmd = new RampExperimentCommand({
                workspace_id: auto_populate_test_workspace,
                org_id: ENV.org_id,
                id: expResponse.id,
                traffic_percentage: 50, // Example ramp percentage
                change_reason: "Testing auto-populate control ramp",
            });
            expect(superpositionClient.send(rampCmd)).rejects.toThrow();

            const updatedVariants: VariantUpdateRequest[] =
                expResponse.variants?.map((variant, index) => ({
                    id: variant.id,
                    overrides:
                        variant.variant_type === VariantType.CONTROL
                            ? {
                                  pmTestKey1: "new-control",
                                  pmTestKey2: "new-control",
                              }
                            : variant.overrides,
                }));

            const updateOverridesCmd = new UpdateOverridesExperimentCommand({
                workspace_id: auto_populate_test_workspace,
                org_id: ENV.org_id,
                id: expResponse.id,
                variant_list: updatedVariants,
                description: "Updating overrides for auto-populate control",
                change_reason: "Testing auto-populate control update",
            });

            await superpositionClient.send(updateOverridesCmd);

            // Now ramp the experiment again, which should succeed
            const rampSuccessCmd = new RampExperimentCommand({
                workspace_id: auto_populate_test_workspace,
                org_id: ENV.org_id,
                id: expResponse.id,
                traffic_percentage: 50, // Example ramp percentage
                change_reason: "Testing auto-populate control ramp success",
            });
            const rampResponse = await superpositionClient.send(rampSuccessCmd);
        } catch (e: any) {
            console.error(
                "Error in test '3.1 Check Auto populate Control':",
                e?.$response || e.message
            );
            throw e;
        }
        if (contextResponse) {
            // delete the created context
            const deleteContextCmd = new DeleteContextCommand({
                workspace_id: auto_populate_test_workspace,
                org_id: ENV.org_id,
                id: contextResponse.id,
            });
            await superpositionClient.send(deleteContextCmd);
        }
    });

    test("3.2 Update Experiment 1 Overrides", async () => {
        try {
            if (!experimentId1 || !experiment1Variants) {
                throw new Error(
                    "Experiment 1 ID or variants not set, cannot update overrides."
                );
            }

            const controlVariant = experiment1Variants.find(
                (v) => v.variant_type === VariantType.CONTROL
            );
            const testVariant = experiment1Variants.find(
                (v) => v.variant_type === VariantType.EXPERIMENTAL
            );

            if (!controlVariant?.id || !testVariant?.id) {
                throw new Error("Could not find variant IDs for Experiment 1.");
            }

            const updatedVariants: VariantUpdateRequest[] = [
                {
                    id: controlVariant.id,
                    overrides: {
                        pmTestKey1: "value-7910-an-control",
                        pmTestKey2: "value-6910-an-control",
                    },
                },
                {
                    id: testVariant.id,
                    overrides: {
                        pmTestKey1: "value-7920-an-test",
                        pmTestKey2: "value-6930-an-test",
                    },
                },
            ];

            const cmd = new UpdateOverridesExperimentCommand({
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                id: experimentId1,
                variant_list: updatedVariants,
                experiment_group_id: "null",
                description: "Updating override keys and values",
                change_reason: "Testing override update",
            });

            const out: ExperimentResponse = await superpositionClient.send(cmd);
            expect(out).toBeDefined();
            expect(out.id).toBe(experimentId1);

            const allUpdatedOverrideKeys = new Set<string>();
            updatedVariants.forEach((uv) =>
                Object.keys(uv.overrides).forEach((k) =>
                    allUpdatedOverrideKeys.add(k)
                )
            );
            expect(out.override_keys?.sort()).toEqual(
                Array.from(allUpdatedOverrideKeys).sort()
            );

            expect(out.variants).toHaveLength(experiment1Variants.length);
            expect(out.status).toBe(ExperimentStatusType.CREATED);
            expect(out.chosen_variant).toBeUndefined();
            expect(out.experiment_group_id).toBeUndefined();
            expect(out.context).toEqual(experiment1Context);
            expect(out.name).toBe("experiment-1-from-test");
        } catch (e: any) {
            console.error(
                "Error in test '3.2 Update Experiment 1 Overrides':",
                e?.$response || e.message
            );
            throw e;
        }
    });

    test("3.3 Ramp Experiment 1", async () => {
        try {
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
            expect(out.experiment_group_id).toBeDefined();
            experimentGroupId = out.experiment_group_id;

            const getCmd = new GetExperimentCommand({
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                id: experimentId1,
            });
            const updatedExp = await superpositionClient.send(getCmd);
            expect(updatedExp.traffic_percentage).toBe(rampPercentage);
            expect(updatedExp.status).toBe(ExperimentStatusType.INPROGRESS);
            expect(updatedExp.experiment_group_id).toBe(experimentGroupId);
        } catch (e: any) {
            console.error(
                "Error in test '3.3 Ramp Experiment 1':",
                e?.$response || e.message
            );
            throw e;
        }
    });

    test("4. Conclude Experiment 1", async () => {
        try {
            if (!experimentId1 || !experiment1Variants) {
                throw new Error(
                    "Experiment 1 ID or variants not set, cannot conclude."
                );
            }
            const controlVariant = experiment1Variants.find(
                (v) => v.variant_type === VariantType.CONTROL
            );
            if (!controlVariant || !controlVariant.id) {
                throw new Error(
                    "Could not find control variant ID for Experiment 1."
                );
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
            expect(out.experiment_group_id).toBeUndefined();

            const getCmd = new GetExperimentCommand({
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                id: experimentId1,
            });
            const updatedExp = await superpositionClient.send(getCmd);
            expect(updatedExp.status).toBe(ExperimentStatusType.CONCLUDED);
            expect(updatedExp.chosen_variant).toBe(winnerVariantId);
            expect(updatedExp.experiment_group_id).toBeUndefined();

            // Verify that the undependent experiment group is now deleted
            if (experimentGroupId) {
                try {
                    console.log("Checking if experiment group is deleted...");
                    expect(
                        superpositionClient.send(
                            new GetExperimentGroupCommand({
                                workspace_id: ENV.workspace_id,
                                org_id: ENV.org_id,
                                id: experimentGroupId,
                            })
                        )
                    ).rejects.toThrow(
                        "No records found. Please refine or correct your search parameters"
                    );
                } catch (error: any) {
                    if (error.name !== "ResourceNotFound") {
                        console.error(
                            "Unexpected error when checking experiment group:",
                            error.message
                        );
                        throw error;
                    }
                }
            }
        } catch (e: any) {
            console.error(
                "Error in test '4. Conclude Experiment 1':",
                e?.$response || e.message
            );
            throw e;
        }
    });

    let experimentId2: string | undefined;
    let experiment2Variants: Variant[] | undefined;

    test("5. Create Experiment 2", async () => {
        try {
            const cmd = new CreateExperimentCommand({
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                name: "experiment-2-from-test",
                context: experiment2Context,
                variants: experiment2InitialVariants.map((v, index) => ({
                    id: index === 0 ? "control" : `test${index}`,
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
            expect(out.id).toBeString();
            expect(out.name).toBe("experiment-2-from-test");
            expect(out.status).toBe(ExperimentStatusType.CREATED);
            expect(out.experiment_group_id).toBeUndefined();
            const allInitialOverrideKeys2 = new Set<string>();
            experiment2InitialVariants.forEach((v) =>
                Object.keys(v.overrides).forEach((k) =>
                    allInitialOverrideKeys2.add(k)
                )
            );
            expect(out.override_keys?.sort()).toEqual(
                Array.from(allInitialOverrideKeys2).sort()
            );
            expect(out.variants).toHaveLength(
                experiment2InitialVariants.length
            );
            expect(out.variants?.[0].variant_type).toBe(VariantType.CONTROL);
            expect(out.variants?.[1].variant_type).toBe(
                VariantType.EXPERIMENTAL
            );
            expect(out.variants?.[0].overrides).toEqual(
                experiment2InitialVariants[0].overrides
            );
            expect(out.variants?.[1].overrides).toEqual(
                experiment2InitialVariants[1].overrides
            );
            expect(out.variants?.[0].context_id).toBeString();
            expect(out.variants?.[1].context_id).toBeString();

            experimentId2 = out.id;
            experiment2Variants = out.variants;

            if (experiment2Variants) {
                for (const variant of experiment2Variants) {
                    if (variant.context_id) {
                        const getContextCmd = new GetContextCommand({
                            workspace_id: ENV.workspace_id,
                            org_id: ENV.org_id,
                            id: variant.context_id,
                        });
                        const contextOut = await superpositionClient.send(
                            getContextCmd
                        );
                        expect(contextOut).toBeDefined();
                        expect(contextOut.override).toEqual(variant.overrides);
                    } else {
                        throw new Error(
                            `Variant ${variant.id} created without a context_id`
                        );
                    }
                }
            }
        } catch (e: any) {
            console.error(
                "Error in test '5. Create Experiment 2':",
                e?.$response || e.message
            );
            throw e;
        }
    });

    test("6. Update Experiment 2 Overrides", async () => {
        try {
            if (!experimentId2 || !experiment2Variants) {
                throw new Error(
                    "Experiment 2 ID or variants not set, cannot update overrides."
                );
            }

            const controlVariant = experiment2Variants.find(
                (v) => v.variant_type === VariantType.CONTROL
            );
            const testVariant = experiment2Variants.find(
                (v) => v.variant_type === VariantType.EXPERIMENTAL
            );

            if (!controlVariant?.id || !testVariant?.id) {
                throw new Error("Could not find variant IDs for Experiment 2.");
            }

            const updatedVariants: VariantUpdateRequest[] = [
                {
                    id: controlVariant.id,
                    overrides: {
                        pmTestKey1: "value-7910-an-control",
                        pmTestKey2: "value-6910-an-control",
                    },
                },
                {
                    id: testVariant.id,
                    overrides: {
                        pmTestKey1: "value-7920-an-test",
                        pmTestKey2: "value-6930-an-test",
                    },
                },
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

            const allUpdatedOverrideKeys = new Set<string>();
            updatedVariants.forEach((uv) =>
                Object.keys(uv.overrides).forEach((k) =>
                    allUpdatedOverrideKeys.add(k)
                )
            );
            expect(out.override_keys?.sort()).toEqual(
                Array.from(allUpdatedOverrideKeys).sort()
            );

            expect(out.variants).toHaveLength(experiment2Variants.length);
            expect(out.status).toBe(ExperimentStatusType.CREATED);
            expect(out.traffic_percentage).toBe(0);
            expect(out.chosen_variant).toBeUndefined();
            expect(out.context).toEqual(experiment2Context);
            expect(out.name).toBe("experiment-2-from-test");
            expect(out.experiment_group_id).toBeUndefined();

            for (const updatedVariantRequest of updatedVariants) {
                const returnedVariant = out.variants?.find(
                    (v) => v.id === updatedVariantRequest.id
                );
                expect(returnedVariant).toBeDefined();
                expect(returnedVariant?.context_id).toBeString();

                if (returnedVariant?.context_id) {
                    const getContextCmd = new GetContextCommand({
                        workspace_id: ENV.workspace_id,
                        org_id: ENV.org_id,
                        id: returnedVariant.context_id,
                    });
                    const contextOut = await superpositionClient.send(
                        getContextCmd
                    );
                    expect(contextOut).toBeDefined();

                    expect(contextOut.override).toEqual(
                        updatedVariantRequest.overrides
                    );
                    expect(returnedVariant.overrides).toEqual(
                        updatedVariantRequest.overrides
                    );
                }
            }
            experiment2Variants = out.variants;
        } catch (e: any) {
            console.error(
                "Error in test '6. Update Experiment 2 Overrides':",
                e?.$response || e.message
            );
            throw e;
        }
    });

    test("7. Ramp Experiment 2", async () => {
        try {
            if (!experimentId2) {
                throw new Error("Experiment 2 ID not set, cannot ramp.");
            }
            const rampPercentage = 46;
            const cmd = new RampExperimentCommand({
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                id: experimentId2,
                traffic_percentage: rampPercentage,
                change_reason: "Testing ramp functionality",
            });
            const out: ExperimentResponse = await superpositionClient.send(cmd);
            expect(out).toBeDefined();
            expect(out.id).toBe(experimentId2);
            expect(out.traffic_percentage).toBe(rampPercentage);
            expect(out.status).toBe(ExperimentStatusType.INPROGRESS);
            expect(out.experiment_group_id).toBeDefined();
            experimentGroupId = out.experiment_group_id;

            const getCmd = new GetExperimentCommand({
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                id: experimentId2,
            });
            const updatedExp = await superpositionClient.send(getCmd);
            expect(updatedExp.traffic_percentage).toBe(rampPercentage);
            expect(updatedExp.status).toBe(ExperimentStatusType.INPROGRESS);
            expect(updatedExp.experiment_group_id).toBe(experimentGroupId);
        } catch (e: any) {
            console.error(
                "Error in test '7. Ramp Experiment 2':",
                e?.$response || e.message
            );
            throw e;
        }
    });

    test("8. Discard Experiment 2", async () => {
        try {
            if (!experimentId2) {
                throw new Error("Experiment 2 ID not set, cannot discard.");
            }
            const cmd = new DiscardExperimentCommand({
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                id: experimentId2,
                change_reason: "Testing discard functionality",
            });
            const out: ExperimentResponse = await superpositionClient.send(cmd);
            expect(out).toBeDefined();
            expect(out.id).toBe(experimentId2);
            expect(out.status).toBe(ExperimentStatusType.DISCARDED);
            expect(out.experiment_group_id).toBeUndefined();
            experimentGroupId = out.experiment_group_id;

            const getCmd = new GetExperimentCommand({
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                id: experimentId2,
            });
            const updatedExp = await superpositionClient.send(getCmd);
            expect(updatedExp.status).toBe(ExperimentStatusType.DISCARDED);
            expect(updatedExp.experiment_group_id).toBeUndefined();
        } catch (e: any) {
            console.error(
                "Error in test '8. Discard Experiment 2':",
                e?.$response || e.message
            );
            throw e;
        }

        // Verify that the experiment group is now deleted
        if (experimentGroupId) {
            try {
                console.log("Checking if experiment group is deleted...");
                expect(
                    superpositionClient.send(
                        new GetExperimentGroupCommand({
                            workspace_id: ENV.workspace_id,
                            org_id: ENV.org_id,
                            id: experimentGroupId,
                        })
                    )
                ).rejects.toThrow(
                    "No records found. Please refine or correct your search parameters"
                );
            } catch (error: any) {
                if (error.name !== "ResourceNotFound") {
                    console.error(
                        "Unexpected error when checking experiment group:",
                        error.message
                    );
                    throw error;
                }
            }
        }
    });

    test("9. List Experiments (Basic)", async () => {
        try {
            const cmd = new ListExperimentCommand({
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
            });
            const out = await superpositionClient.send(cmd);
            expect(out).toBeDefined();
            expect(Array.isArray(out.data)).toBe(true);
            const foundExp1 = out.data?.some((exp) => exp.id === experimentId1);
            const foundExp2 = out.data?.some((exp) => exp.id === experimentId2);
            expect(foundExp1).toBe(true);
            expect(foundExp2).toBe(true);

            const exp1 = out.data?.find((exp) => exp.id === experimentId1);
            const exp2 = out.data?.find((exp) => exp.id === experimentId2);
            expect(exp1?.experiment_group_id).toBeUndefined();
            expect(exp2?.experiment_group_id).toBeUndefined();
        } catch (e: any) {
            console.error(
                "Error in test '9. List Experiments (Basic)':",
                e?.$response || e.message
            );
            throw e;
        }
    });
});
