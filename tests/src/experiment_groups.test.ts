import {
    SuperpositionClient,
    CreateExperimentCommand,
    RampExperimentCommand,
    DiscardExperimentCommand,
    CreateExperimentGroupCommand,
    GetExperimentGroupCommand,
    UpdateExperimentGroupCommand,
    AddMembersToGroupCommand,
    RemoveMembersFromGroupCommand,
    ListExperimentGroupsCommand,
    DeleteExperimentGroupCommand,
    CreateDimensionCommand,
    ListDimensionsCommand,
    DeleteDimensionCommand,
    ListDefaultConfigsCommand,
    CreateDefaultConfigCommand,
    DeleteDefaultConfigCommand,
    VariantType,
    SortBy,
    ExperimentGroupSortOn,
    GroupType,
    type CreateExperimentGroupCommandInput,
    type UpdateExperimentGroupCommandInput,
    type AddMembersToGroupCommandInput,
    type RemoveMembersFromGroupCommandInput,
    type CreateExperimentCommandInput,
    type DimensionExt,
    type DefaultConfigFull,
    WorkspaceStatus,
    UpdateWorkspaceCommand,
} from "@juspay/superposition-sdk";
import { superpositionClient, ENV } from "../env.ts";
import { describe, beforeAll, afterAll, test, expect } from "bun:test";
import { nanoid } from "nanoid";

// Helper function to create unique names/IDs
const uniqueName = (prefix: string) => `${prefix}-${nanoid(8)}`;

describe("Experiment Groups API Integration Tests", () => {
    let createdExperimentIds: string[] = [];
    let expGroupId: string = "";
    let createdDimensionNames: string[] = [];
    let createdDefaultConfigKeys: string[] = [];

    // Experiments to be created
    let expValid1Id: string; // Exact context match
    let expValid2Id: string; // Superset context
    let expInvalidInProgressId: string; // InProgress status
    let expInvalidContextId: string; // Conflicting context

    // Define dimensions based on experiments.test.ts and group test needs
    const dimensionsToEnsure: {
        name: string;
        schema: any;
        description: string;
    }[] = [
        {
            name: "os",
            schema: { type: "string", enum: ["ios", "android", "web"] },
            description: "OS dimension from experiments.test",
        },
        {
            name: "clientId",
            schema: { type: "string" },
            description: "Client ID from experiments.test",
        },
        {
            name: "app_version",
            schema: { type: "string" },
            description: "App version for superset context",
        },
        {
            name: "device_specific_id",
            schema: { type: "string" },
            description: "Dimension for invalid context conflict",
        },
    ];

    // Define default configs based on experiments.test.ts
    const defaultConfigsToEnsure: {
        key: string;
        value: any;
        schema: any;
        description: string;
    }[] = [
        {
            key: "pmTestKey1",
            value: "default_group_val_1",
            schema: { type: "string" },
            description: "Default for pmTestKey1 (group tests)",
        },
        {
            key: "pmTestKey2",
            value: "default_group_val_2",
            schema: { type: "string" },
            description: "Default for pmTestKey2 (group tests)",
        },
    ];

    // Contexts using the ensured dimensions
    const groupContext = ENV.jsonlogic_enabled
        ? {
              and: [
                  { "==": [{ var: "os" }, "ios"] },
                  { "==": [{ var: "clientId" }, "clientForExpGroup"] },
              ],
          }
        : {
              os: "ios",
              clientId: "clientForExpGroup",
          };
    const expValid1Context = { ...groupContext }; // Exact match
    const expValid2Context = ENV.jsonlogic_enabled
        ? {
              and: [
                  ...(groupContext.and as any[]),
                  { "==": [{ var: "app_version" }, "2.0.0"] },
              ],
          }
        : { ...groupContext, app_version: "2.0.0" }; // Superset
    const expInvalidContextConflict = ENV.jsonlogic_enabled
        ? {
              and: [
                  { "==": [{ var: "device_specific_id" }, "devSpecificXYZ"] },
              ],
          }
        : {
              device_specific_id: "devSpecificXYZ",
          };

    // Variants using the ensured default configs (matching CreateExperimentCommandInput['variants'] structure)
    const defaultVariantInputsForGroupExperiments: CreateExperimentCommandInput["variants"] =
        [
            {
                variant_type: VariantType.CONTROL,
                id: "group_control_A",
                overrides: {
                    pmTestKey1: "group_control_A_val",
                    pmTestKey2: "group_control_B_val",
                },
            },
            {
                variant_type: VariantType.EXPERIMENTAL,
                id: "group_experimental_A",
                overrides: {
                    pmTestKey1: "group_experimental_A_val",
                    pmTestKey2: "group_experimental_B_val",
                },
            },
        ];

    async function ensureDimensionExists(
        name: string,
        schema: any,
        description: string,
        existingDimensions: DimensionExt[]
    ): Promise<void> {
        const existing = existingDimensions.find((d) => d.dimension === name);
        if (!existing) {
            let position = 1;
            const takenPositions = new Set(
                existingDimensions.map((d) => d.position)
            );
            while (takenPositions.has(position)) {
                position++;
            }

            console.log(
                `Dimension ${name} not found, creating with position ${position}...`
            );
            await superpositionClient.send(
                new CreateDimensionCommand({
                    workspace_id: ENV.workspace_id,
                    org_id: ENV.org_id,
                    dimension: name,
                    position: position,
                    schema: schema,
                    description: description,
                    change_reason: "Test setup for experiment groups",
                })
            );
            createdDimensionNames.push(name);
            console.log(`Dimension ${name} created.`);
            // Add to existingDimensions to reflect creation for subsequent calls in the loop
            existingDimensions.push({
                dimension: name,
                position,
            } as DimensionExt);
        } else {
            console.log(`Dimension ${name} already exists.`);
        }
    }

    async function ensureDefaultConfigExists(
        key: string,
        value: any,
        schema: any,
        description: string,
        existingConfigs: DefaultConfigFull[]
    ): Promise<void> {
        const existing = existingConfigs.find((c) => c.key === key);
        if (!existing) {
            console.log(`Default config ${key} not found, creating...`);
            await superpositionClient.send(
                new CreateDefaultConfigCommand({
                    workspace_id: ENV.workspace_id,
                    org_id: ENV.org_id,
                    key: key,
                    value: value,
                    schema: schema,
                    description: description,
                    change_reason: "Test setup for experiment groups",
                })
            );
            createdDefaultConfigKeys.push(key);
            console.log(`Default config ${key} created.`);
        } else {
            console.log(`Default config ${key} already exists.`);
        }
    }

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

    beforeAll(async () => {
        console.log(
            "Ensuring dimensions and default configs exist for Experiment Group tests..."
        );
        const listDimResponse = await superpositionClient.send(
            new ListDimensionsCommand({
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                count: 200,
            })
        );
        const existingDimensions = listDimResponse.data || [];

        await removeMandatoryDimension(superpositionClient); // Ensure no mandatory dimensions before setup

        for (const dim of dimensionsToEnsure) {
            await ensureDimensionExists(
                dim.name,
                dim.schema,
                dim.description,
                existingDimensions
            );
        }

        const listConfigResponse = await superpositionClient.send(
            new ListDefaultConfigsCommand({
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                count: 200,
            })
        );
        const existingConfigs = listConfigResponse.data || [];

        for (const cfg of defaultConfigsToEnsure) {
            await ensureDefaultConfigExists(
                cfg.key,
                cfg.value,
                cfg.schema,
                cfg.description,
                existingConfigs
            );
        }
        console.log("Finished dimension and default config setup.");

        console.log("Setting up experiments for group tests...");

        const expValid1Name = uniqueName("exp-valid1");
        const createExp1Input: CreateExperimentCommandInput = {
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            name: expValid1Name,
            context: expValid1Context,
            variants: defaultVariantInputsForGroupExperiments,
            description: "Valid experiment 1 for group (exact context)",
            change_reason: "Test setup",
        };
        const exp1Response = await superpositionClient.send(
            new CreateExperimentCommand(createExp1Input)
        );
        expValid1Id = exp1Response.id!;
        createdExperimentIds.push(expValid1Id);
        console.log(`Created expValid1Id: ${expValid1Id}`);

        const expValid2Name = uniqueName("exp-valid2");
        const createExp2Input: CreateExperimentCommandInput = {
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            name: expValid2Name,
            context: expValid2Context,
            variants: defaultVariantInputsForGroupExperiments,
            description: "Valid experiment 2 for group (superset context)",
            change_reason: "Test setup",
        };
        const exp2Response = await superpositionClient.send(
            new CreateExperimentCommand(createExp2Input)
        );
        expValid2Id = exp2Response.id!;
        createdExperimentIds.push(expValid2Id);
        console.log(`Created expValid2Id: ${expValid2Id}`);

        const expInvalidProgName = uniqueName("exp-invalid-prog");
        const context = ENV.jsonlogic_enabled
            ? { and: [{ "==": [{ var: "os" }, "android"] }] } // Use an ensured dimension
            : { os: "android" };
        const createExp3Input: CreateExperimentCommandInput = {
            // Context for this one can be simple as status is the key
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            name: expInvalidProgName,
            context,
            variants: defaultVariantInputsForGroupExperiments,
            description: "Invalid experiment (in progress) for group",
            change_reason: "Test setup",
        };
        const exp3Response = await superpositionClient.send(
            new CreateExperimentCommand(createExp3Input)
        );
        expInvalidInProgressId = exp3Response.id!;
        createdExperimentIds.push(expInvalidInProgressId);
        await superpositionClient.send(
            new RampExperimentCommand({
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                id: expInvalidInProgressId,
                traffic_percentage: 50,
                change_reason: "Move to in-progress for test",
            })
        );
        console.log(
            `Created and ramped expInvalidInProgressId: ${expInvalidInProgressId}`
        );

        const expInvalidCtxName = uniqueName("exp-invalid-ctx");
        const createExp4Input: CreateExperimentCommandInput = {
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            name: expInvalidCtxName,
            context: expInvalidContextConflict,
            variants: defaultVariantInputsForGroupExperiments,
            description: "Invalid experiment (conflicting context) for group",
            change_reason: "Test setup",
        };
        const exp4Response = await superpositionClient.send(
            new CreateExperimentCommand(createExp4Input)
        );
        expInvalidContextId = exp4Response.id!;
        createdExperimentIds.push(expInvalidContextId);
        console.log(`Created expInvalidContextId: ${expInvalidContextId}`);
    });

    afterAll(async () => {
        console.log("Cleaning up experiment group test resources...");
        addMandatoryDimension(superpositionClient); // Restore mandatory dimensions if needed
        for (const id of createdExperimentIds) {
            try {
                await superpositionClient.send(
                    new DiscardExperimentCommand({
                        workspace_id: ENV.workspace_id,
                        org_id: ENV.org_id,
                        id,
                        change_reason: "Test cleanup",
                    })
                );
                console.log(`Discarded experiment: ${id}`);
            } catch (error: any) {
                if (
                    error.name !== "ResourceNotFound" &&
                    error.name !== "ExperimentNotFound"
                ) {
                    console.error(
                        `Failed to discard experiment ${id} during cleanup:`,
                        error.message
                    );
                }
            }
        }

        for (const dimName of createdDimensionNames) {
            try {
                await superpositionClient.send(
                    new DeleteDimensionCommand({
                        workspace_id: ENV.workspace_id,
                        org_id: ENV.org_id,
                        dimension: dimName,
                    })
                );
                console.log(`Cleaned up dimension: ${dimName}`);
            } catch (error: any) {
                if (error.name !== "ResourceNotFound") {
                    console.error(
                        `Failed to clean up dimension ${dimName}:`,
                        error.message
                    );
                }
            }
        }
        for (const configKey of createdDefaultConfigKeys) {
            try {
                await superpositionClient.send(
                    new DeleteDefaultConfigCommand({
                        workspace_id: ENV.workspace_id,
                        org_id: ENV.org_id,
                        key: configKey,
                    })
                );
                console.log(`Cleaned up default config: ${configKey}`);
            } catch (error: any) {
                if (error.name !== "ResourceNotFound") {
                    console.error(
                        `Failed to clean up default config ${configKey}:`,
                        error.message
                    );
                }
            }
        }
    });

    describe("CreateExperimentGroupCommand", () => {
        const baseCreateInput: Omit<
            CreateExperimentGroupCommandInput,
            "name" | "member_experiment_ids"
        > = {
            workspace_id: ENV.workspace_id,
            org_id: ENV.org_id,
            description: "Test experiment group",
            change_reason: "Creating group for test",
            context: groupContext,
            traffic_percentage: 100,
        };

        test("should successfully create an experiment group with valid members", async () => {
            const groupName = uniqueName("test-group");
            const input: CreateExperimentGroupCommandInput = {
                ...baseCreateInput,
                name: groupName,
                member_experiment_ids: [expValid2Id],
            };
            try {
                const response = await superpositionClient.send(
                    new CreateExperimentGroupCommand(input)
                );
                console.log("Response on create", response);
                expGroupId = response.id!;
                expect(response.name).toBe(groupName);
                expect(response.member_experiment_ids).toEqual(
                    expect.arrayContaining([expValid2Id])
                );
                expect(response.traffic_percentage).toBe(100);
                expect(response.group_type).toBe(GroupType.USER_CREATED);
            } catch (error) {
                console.error("Error creating experiment group:", error);
                throw error;
            }
        });

        // Remove expValid2Id
        test("should successfully remove members from a group", async () => {
            expect(expGroupId).toBeString();

            const removeInput: RemoveMembersFromGroupCommandInput = {
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                id: expGroupId!,
                member_experiment_ids: [expValid2Id],
                change_reason: "Removing members",
            };
            const response = await superpositionClient.send(
                new RemoveMembersFromGroupCommand(removeInput)
            );
            expect(response.member_experiment_ids).not.toContain(expValid2Id);
        });

        test("should successfully create an experiment group with no members", async () => {
            const groupName = uniqueName("empty-group");

            const input: CreateExperimentGroupCommandInput = {
                ...baseCreateInput,
                name: groupName,
                member_experiment_ids: [],
            };
            const response = await superpositionClient.send(
                new CreateExperimentGroupCommand(input)
            );
            expGroupId = response.id!;
            expect(response.name).toBe(groupName);
            expect(response.member_experiment_ids).toEqual([]);
            expect(response.group_type).toBe(GroupType.USER_CREATED);
        });

        test("should fail to create with an in-progress experiment member", async () => {
            const groupName = uniqueName("fail-group-prog");
            const input: CreateExperimentGroupCommandInput = {
                ...baseCreateInput,
                name: groupName,
                member_experiment_ids: [expValid1Id, expInvalidInProgressId],
            };
            expect(
                superpositionClient.send(
                    new CreateExperimentGroupCommand(input)
                )
            ).rejects.toThrow(
                `The following experiment IDs are not present in the database/are not in the created stage: ${expInvalidInProgressId}`
            );
        });

        test("should fail to create with a conflicting context experiment member", async () => {
            const groupName = uniqueName("fail-group-ctx");
            const input: CreateExperimentGroupCommandInput = {
                ...baseCreateInput,
                name: groupName,
                member_experiment_ids: [expValid1Id, expInvalidContextId],
            };
            expect(
                superpositionClient.send(
                    new CreateExperimentGroupCommand(input)
                )
            ).rejects.toThrow(
                /Experiment with id .* does not fit in with the experiment group. The contexts do not match./
            );
        });

        test("should fail if name is missing", async () => {
            const input = {
                ...baseCreateInput,
                member_experiment_ids: [],
            } as any;
            delete input.name;
            expect(
                superpositionClient.send(
                    new CreateExperimentGroupCommand(input)
                )
            ).rejects.toThrow();
        });

        test("should fail if traffic_percentage is invalid (e.g., > 100)", async () => {
            const groupName = uniqueName("fail-group-traffic");
            const input: CreateExperimentGroupCommandInput = {
                ...baseCreateInput,
                name: groupName,
                traffic_percentage: 101,
                member_experiment_ids: [],
            };
            expect(
                superpositionClient.send(
                    new CreateExperimentGroupCommand(input)
                )
            ).rejects.toThrow(
                'JSON Parse error: Unexpected identifier "Json"\n  Deserialization error: to see the raw response, inspect the hidden field {error}.$response on this object.'
            );
        });
    });

    describe("GetExperimentGroupCommand", () => {
        test("should successfully get an existing experiment group", async () => {
            console.log("Group ID that was tried: ", expGroupId);
            const response = await superpositionClient.send(
                new GetExperimentGroupCommand({
                    workspace_id: ENV.workspace_id,
                    org_id: ENV.org_id,
                    id: expGroupId!,
                })
            );
            expect(response.name).toBeString();
        });

        test("should fail to get a non-existent experiment group", async () => {
            expect(
                superpositionClient.send(
                    new GetExperimentGroupCommand({
                        workspace_id: ENV.workspace_id,
                        org_id: ENV.org_id,
                        id: "123",
                    })
                )
            ).rejects.toThrow(
                "No records found. Please refine or correct your search parameters"
            );
        });
    });

    describe("UpdateExperimentGroupCommand", () => {
        test("should successfully update traffic_percentage", async () => {
            expect(expGroupId).toBeString();
            const currentGroup = await superpositionClient.send(
                new GetExperimentGroupCommand({
                    workspace_id: ENV.workspace_id,
                    org_id: ENV.org_id,
                    id: expGroupId!,
                })
            );

            const input: UpdateExperimentGroupCommandInput = {
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                id: expGroupId!,
                traffic_percentage: 75,
                change_reason: "Updating traffic percentage",
            };
            const response = await superpositionClient.send(
                new UpdateExperimentGroupCommand(input)
            );
            expect(response.traffic_percentage).toBe(75);
            expect(response.member_experiment_ids).toEqual(
                currentGroup.member_experiment_ids || []
            );
            expect(response.group_type).toBe(GroupType.USER_CREATED);
        });

        test("should successfully update description", async () => {
            expect(expGroupId).toBeString();
            const newDescription = "Updated experiment group description";

            const input: UpdateExperimentGroupCommandInput = {
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                id: expGroupId!,
                description: newDescription,
                change_reason: "Updating description",
            };
            const response = await superpositionClient.send(
                new UpdateExperimentGroupCommand(input)
            );
            expect(response.description).toBe(newDescription);
        });

        test("should fail to update a non-existent group", async () => {
            const input: UpdateExperimentGroupCommandInput = {
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                id: "123",
                traffic_percentage: 50,
                change_reason: "Updating non-existent group",
            };
            expect(
                superpositionClient.send(
                    new UpdateExperimentGroupCommand(input)
                )
            ).rejects.toThrow(
                "No records found. Please refine or correct your search parameters"
            );
        });
    });

    describe("AddMembersToGroupCommand", () => {
        test("should successfully add members to a group", async () => {
            expect(expGroupId).toBeString();
            const input: AddMembersToGroupCommandInput = {
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                id: expGroupId!,
                member_experiment_ids: [expValid2Id],
                change_reason: "Adding members",
            };
            const response = await superpositionClient.send(
                new AddMembersToGroupCommand(input)
            );
            expect(response.member_experiment_ids).toContain(expValid2Id);
        });

        // Remove the expValid2Id
        test("should successfully remove members from a group", async () => {
            expect(expGroupId).toBeString();

            const removeInput: RemoveMembersFromGroupCommandInput = {
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                id: expGroupId!,
                member_experiment_ids: [expValid2Id],
                change_reason: "Removing members",
            };
            const response = await superpositionClient.send(
                new RemoveMembersFromGroupCommand(removeInput)
            );
            expect(response.member_experiment_ids).not.toContain(expValid2Id);
        });

        test("should fail to add an invalid member experiment", async () => {
            expect(expGroupId).toBeString();
            const input: AddMembersToGroupCommandInput = {
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                id: expGroupId!,
                member_experiment_ids: [expInvalidInProgressId],
                change_reason: "Attempting to add invalid member",
            };
            expect(
                superpositionClient.send(new AddMembersToGroupCommand(input))
            ).rejects.toThrow(
                `The following experiment IDs are not present in the database/are not in the created stage: ${expInvalidInProgressId}`
            );
        });

        test("should fail to add members to a non-existent group", async () => {
            const input: AddMembersToGroupCommandInput = {
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                id: "123",
                member_experiment_ids: [],
                change_reason: "Adding to non-existent group",
            };
            expect(
                superpositionClient.send(new AddMembersToGroupCommand(input))
            ).rejects.toThrow(
                "Please provide at least one experiment ID to add to the group"
            );
        });
    });

    describe("RemoveMembersFromGroupCommand", () => {
        test("should successfully remove members from a group", async () => {
            expect(expGroupId).toBeString();

            // First ensure we have a member to remove
            const addInput: AddMembersToGroupCommandInput = {
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                id: expGroupId!,
                member_experiment_ids: [expValid2Id],
                change_reason: "Adding member before removal test",
            };
            await superpositionClient.send(
                new AddMembersToGroupCommand(addInput)
            );

            // Now remove the member
            const removeInput: RemoveMembersFromGroupCommandInput = {
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                id: expGroupId!,
                member_experiment_ids: [expValid2Id],
                change_reason: "Removing members",
            };
            const response = await superpositionClient.send(
                new RemoveMembersFromGroupCommand(removeInput)
            );
            expect(response.member_experiment_ids).not.toContain(expValid2Id);
        });

        test("should successfully handle removing non-existent members", async () => {
            expect(expGroupId).toBeString();

            // Get current group state
            const currentGroup = await superpositionClient.send(
                new GetExperimentGroupCommand({
                    workspace_id: ENV.workspace_id,
                    org_id: ENV.org_id,
                    id: expGroupId!,
                })
            );

            // Try to remove a member that doesn't exist in the group
            const nonExistentId = "999999999";
            const removeInput: RemoveMembersFromGroupCommandInput = {
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                id: expGroupId!,
                member_experiment_ids: [nonExistentId],
                change_reason: "Removing non-existent member",
            };
            expect(
                superpositionClient.send(
                    new RemoveMembersFromGroupCommand(removeInput)
                )
            ).rejects.toThrow(
                `The following experiment IDs are not present in the database: ${nonExistentId}`
            );
        });

        test("should fail to remove members from a non-existent group", async () => {
            const input: RemoveMembersFromGroupCommandInput = {
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                id: "123",
                change_reason: "Test",
                member_experiment_ids: ["999"],
            };
            expect(
                superpositionClient.send(
                    new RemoveMembersFromGroupCommand(input)
                )
            ).rejects.toThrow(
                "No records found. Please refine or correct your search parameters"
            );
        });
    });

    describe("ListExperimentGroupsCommand", () => {
        test("should list experiment groups and find the created one", async () => {
            expect(expGroupId).toBeString();
            const groupDetails = await superpositionClient.send(
                new GetExperimentGroupCommand({
                    workspace_id: ENV.workspace_id,
                    org_id: ENV.org_id,
                    id: expGroupId!,
                })
            );

            const response = await superpositionClient.send(
                new ListExperimentGroupsCommand({
                    workspace_id: ENV.workspace_id,
                    org_id: ENV.org_id,
                    name: groupDetails.name!,
                })
            );
            expect(response.data).toBeArray();
            expect(response.data!.length).toBeGreaterThanOrEqual(1);
            const found = response.data!.find((g) => g.id === expGroupId);
            expect(found).toBeDefined();
            expect(found!.name).toBe(groupDetails.name!);
        });

        test("should respect pagination parameters (all=true)", async () => {
            const response = await superpositionClient.send(
                new ListExperimentGroupsCommand({
                    workspace_id: ENV.workspace_id,
                    org_id: ENV.org_id,
                    all: true,
                })
            );
            expect(response.data).toBeArray();
        });

        test("should filter by sort_on and sort_by", async () => {
            const response = await superpositionClient.send(
                new ListExperimentGroupsCommand({
                    workspace_id: ENV.workspace_id,
                    org_id: ENV.org_id,
                    sort_on: ExperimentGroupSortOn.CreatedAt,
                    sort_by: SortBy.Desc,
                    count: 5,
                })
            );
            expect(response.data).toBeArray();
            if (response.data!.length > 1) {
                const date1 = new Date(response.data![0].created_at!);
                const date2 = new Date(response.data![1].created_at!);
                expect(date1.getTime()).toBeGreaterThanOrEqual(date2.getTime());
            }
        });
    });

    describe("DeleteExperimentGroupCommand", () => {
        let tempGroupId: string;

        beforeAll(async () => {
            const groupName = uniqueName("delete-me-group");
            const createInput: CreateExperimentGroupCommandInput = {
                workspace_id: ENV.workspace_id,
                org_id: ENV.org_id,
                name: groupName,
                description: "Group for delete tests",
                change_reason: "Test setup for delete",
                context: groupContext,
                traffic_percentage: 100,
                member_experiment_ids: [expValid1Id],
            };
            const response = await superpositionClient.send(
                new CreateExperimentGroupCommand(createInput)
            );
            tempGroupId = response.id!;
        });

        test("should fail to delete a group with active members", async () => {
            expect(
                superpositionClient.send(
                    new DeleteExperimentGroupCommand({
                        workspace_id: ENV.workspace_id,
                        org_id: ENV.org_id,
                        id: tempGroupId,
                    })
                )
            ).rejects.toThrow(
                /Cannot delete experiment group .* since it has members/
            );
        });

        test("should fail to delete a non-existent group", async () => {
            expect(
                superpositionClient.send(
                    new DeleteExperimentGroupCommand({
                        workspace_id: ENV.workspace_id,
                        org_id: ENV.org_id,
                        id: "22",
                    })
                )
            ).rejects.toThrow(
                "No records found. Please refine or correct your search parameters"
            );
        });
    });
});
