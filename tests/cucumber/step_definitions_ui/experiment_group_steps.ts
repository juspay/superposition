import { Given, When, Then } from "@cucumber/cucumber";
import {
  CreateExperimentCommand,
  CreateExperimentGroupCommand,
  GetExperimentGroupCommand,
  UpdateExperimentGroupCommand,
  AddMembersToGroupCommand,
  RemoveMembersFromGroupCommand,
  ListExperimentGroupsCommand,
  DeleteExperimentGroupCommand,
  CreateDimensionCommand,
  CreateDefaultConfigCommand,
  RampExperimentCommand,
  VariantType,
  ExperimentGroupSortOn,
  SortBy,
} from "@juspay/superposition-sdk";
import { PlaywrightWorld } from "../support_ui/world.ts";
import * as assert from "node:assert";

// Track group-specific experiment IDs
let validExpId: string;
let validExp2Id: string;
let inProgressExpId: string;
let conflictingContextExpId: string;

const groupContext = { os: "ios", clientId: "groupClient" };

// ── Given ───────────────────────────────────────────────────────────

Given(
  "dimensions and default configs are set up for experiment group tests",
  async function (this: PlaywrightWorld) {
    // Ensure dimensions exist
    for (const dim of [
      { name: "os", schema: { type: "string", enum: ["ios", "android", "web"] } },
      { name: "clientId", schema: { type: "string" } },
      { name: "app_version", schema: { type: "string" } },
      { name: "device_specific_id", schema: { type: "string" } },
    ]) {
      try {
        await this.client.send(
          new CreateDimensionCommand({
            workspace_id: this.workspaceId,
            org_id: this.orgId,
            dimension: dim.name,
            position: 1,
            schema: dim.schema,
            description: `Dim ${dim.name}`,
            change_reason: "Cucumber group setup",
          })
        );
        this.createdDimensions.push(dim.name);
      } catch {
        // Already exists
      }
    }

    // Ensure default configs exist
    for (const cfg of ["pmTestKey1", "pmTestKey2"]) {
      try {
        await this.client.send(
          new CreateDefaultConfigCommand({
            workspace_id: this.workspaceId,
            org_id: this.orgId,
            key: cfg,
            value: `default_${cfg}`,
            schema: { type: "string" },
            description: `Config ${cfg}`,
            change_reason: "Cucumber group setup",
          })
        );
        this.createdConfigs.push(cfg);
      } catch {
        // Already exists
      }
    }
  }
);

Given(
  "experiments are set up for group tests",
  async function (this: PlaywrightWorld) {
    const variants = [
      {
        variant_type: VariantType.CONTROL,
        id: "grp_control",
        overrides: { pmTestKey1: "ctrl_val1", pmTestKey2: "ctrl_val2" },
      },
      {
        variant_type: VariantType.EXPERIMENTAL,
        id: "grp_experimental",
        overrides: { pmTestKey1: "exp_val1", pmTestKey2: "exp_val2" },
      },
    ];

    // Valid experiment 1 (exact context match)
    const exp1 = await this.client.send(
      new CreateExperimentCommand({
        workspace_id: this.workspaceId,
        org_id: this.orgId,
        name: this.uniqueName("grp-exp1"),
        context: { ...groupContext },
        variants,
        description: "Valid exp 1",
        change_reason: "Cucumber setup",
      })
    );
    validExpId = exp1.id!;
    this.createdExperimentIds.push(validExpId);

    // Valid experiment 2 (superset context)
    const exp2 = await this.client.send(
      new CreateExperimentCommand({
        workspace_id: this.workspaceId,
        org_id: this.orgId,
        name: this.uniqueName("grp-exp2"),
        context: { ...groupContext, app_version: "2.0.0" },
        variants,
        description: "Valid exp 2 (superset)",
        change_reason: "Cucumber setup",
      })
    );
    validExp2Id = exp2.id!;
    this.createdExperimentIds.push(validExp2Id);

    // In-progress experiment
    const exp3 = await this.client.send(
      new CreateExperimentCommand({
        workspace_id: this.workspaceId,
        org_id: this.orgId,
        name: this.uniqueName("grp-exp-prog"),
        context: { os: "android" },
        variants,
        description: "In-progress exp",
        change_reason: "Cucumber setup",
      })
    );
    inProgressExpId = exp3.id!;
    this.createdExperimentIds.push(inProgressExpId);
    await this.client.send(
      new RampExperimentCommand({
        workspace_id: this.workspaceId,
        org_id: this.orgId,
        id: inProgressExpId,
        traffic_percentage: 50,
        change_reason: "Ramp for test",
      })
    );

    // Conflicting context experiment
    const exp4 = await this.client.send(
      new CreateExperimentCommand({
        workspace_id: this.workspaceId,
        org_id: this.orgId,
        name: this.uniqueName("grp-exp-conflict"),
        context: { device_specific_id: "devXYZ" },
        variants,
        description: "Conflicting context exp",
        change_reason: "Cucumber setup",
      })
    );
    conflictingContextExpId = exp4.id!;
    this.createdExperimentIds.push(conflictingContextExpId);
  }
);

Given(
  "an experiment group exists",
  async function (this: PlaywrightWorld) {
    if (!this.experimentGroupId) {
      const response = await this.client.send(
        new CreateExperimentGroupCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          name: this.uniqueName("grp-test"),
          description: "Test group",
          change_reason: "Cucumber setup",
          context: groupContext,
          traffic_percentage: 100,
          member_experiment_ids: [],
        })
      );
      this.experimentGroupId = response.id!;
    }
  }
);

Given(
  "an experiment group exists with no members",
  async function (this: PlaywrightWorld) {
    const response = await this.client.send(
      new CreateExperimentGroupCommand({
        workspace_id: this.workspaceId,
        org_id: this.orgId,
        name: this.uniqueName("grp-empty"),
        description: "Empty group",
        change_reason: "Cucumber setup",
        context: groupContext,
        traffic_percentage: 100,
        member_experiment_ids: [],
      })
    );
    this.experimentGroupId = response.id!;
  }
);

Given(
  "an experiment group exists with members",
  async function (this: PlaywrightWorld) {
    const response = await this.client.send(
      new CreateExperimentGroupCommand({
        workspace_id: this.workspaceId,
        org_id: this.orgId,
        name: this.uniqueName("grp-members"),
        description: "Group with members",
        change_reason: "Cucumber setup",
        context: groupContext,
        traffic_percentage: 100,
        member_experiment_ids: [validExpId],
      })
    );
    this.experimentGroupId = response.id!;
  }
);

// ── When ────────────────────────────────────────────────────────────

When(
  "I create an experiment group with name {string} and member experiments",
  async function (this: PlaywrightWorld, name: string) {
    try {
      this.lastResponse = await this.client.send(
        new CreateExperimentGroupCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          name: this.uniqueName(name),
          description: "Test group",
          change_reason: "Cucumber test",
          context: groupContext,
          traffic_percentage: 100,
          member_experiment_ids: [validExp2Id],
        })
      );
      this.experimentGroupId = this.lastResponse.id!;
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

When(
  "I create an experiment group with name {string} and no members",
  async function (this: PlaywrightWorld, name: string) {
    try {
      this.lastResponse = await this.client.send(
        new CreateExperimentGroupCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          name: this.uniqueName(name),
          description: "Empty group",
          change_reason: "Cucumber test",
          context: groupContext,
          traffic_percentage: 100,
          member_experiment_ids: [],
        })
      );
      this.experimentGroupId = this.lastResponse.id!;
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

When(
  "I create an experiment group including an in-progress experiment",
  async function (this: PlaywrightWorld) {
    try {
      this.lastResponse = await this.client.send(
        new CreateExperimentGroupCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          name: this.uniqueName("fail-prog"),
          description: "Should fail",
          change_reason: "Test",
          context: groupContext,
          traffic_percentage: 100,
          member_experiment_ids: [validExpId, inProgressExpId],
        })
      );
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

When(
  "I create an experiment group including an experiment with conflicting context",
  async function (this: PlaywrightWorld) {
    try {
      this.lastResponse = await this.client.send(
        new CreateExperimentGroupCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          name: this.uniqueName("fail-ctx"),
          description: "Should fail",
          change_reason: "Test",
          context: groupContext,
          traffic_percentage: 100,
          member_experiment_ids: [validExpId, conflictingContextExpId],
        })
      );
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

When(
  "I create an experiment group with traffic percentage {int}",
  async function (this: PlaywrightWorld, traffic: number) {
    try {
      this.lastResponse = await this.client.send(
        new CreateExperimentGroupCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          name: this.uniqueName("fail-traffic"),
          description: "Should fail",
          change_reason: "Test",
          context: groupContext,
          traffic_percentage: traffic,
          member_experiment_ids: [],
        })
      );
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

When(
  "I get the experiment group by its ID",
  async function (this: PlaywrightWorld) {
    try {
      this.lastResponse = await this.client.send(
        new GetExperimentGroupCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          id: this.experimentGroupId,
        })
      );
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

When(
  "I get an experiment group with ID {string}",
  async function (this: PlaywrightWorld, id: string) {
    try {
      this.lastResponse = await this.client.send(
        new GetExperimentGroupCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          id,
        })
      );
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

When(
  "I update the experiment group traffic percentage to {int}",
  async function (this: PlaywrightWorld, traffic: number) {
    try {
      this.lastResponse = await this.client.send(
        new UpdateExperimentGroupCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          id: this.experimentGroupId,
          traffic_percentage: traffic,
          change_reason: "Cucumber update",
        })
      );
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

When(
  "I update the experiment group description to {string}",
  async function (this: PlaywrightWorld, desc: string) {
    try {
      this.lastResponse = await this.client.send(
        new UpdateExperimentGroupCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          id: this.experimentGroupId,
          description: desc,
          change_reason: "Cucumber update",
        })
      );
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

When(
  "I update experiment group {string} traffic percentage to {int}",
  async function (this: PlaywrightWorld, id: string, traffic: number) {
    try {
      this.lastResponse = await this.client.send(
        new UpdateExperimentGroupCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          id,
          traffic_percentage: traffic,
          change_reason: "Cucumber update",
        })
      );
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

When(
  "I add a valid experiment to the group",
  async function (this: PlaywrightWorld) {
    try {
      this.lastResponse = await this.client.send(
        new AddMembersToGroupCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          id: this.experimentGroupId,
          member_experiment_ids: [validExp2Id],
          change_reason: "Cucumber add member",
        })
      );
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

When(
  "I remove a member from the group",
  async function (this: PlaywrightWorld) {
    try {
      this.lastResponse = await this.client.send(
        new RemoveMembersFromGroupCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          id: this.experimentGroupId,
          member_experiment_ids: [validExpId],
          change_reason: "Cucumber remove member",
        })
      );
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

When(
  "I add an in-progress experiment to the group",
  async function (this: PlaywrightWorld) {
    try {
      this.lastResponse = await this.client.send(
        new AddMembersToGroupCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          id: this.experimentGroupId,
          member_experiment_ids: [inProgressExpId],
          change_reason: "Cucumber add invalid",
        })
      );
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

When("I list experiment groups", async function (this: PlaywrightWorld) {
  try {
    this.lastResponse = await this.client.send(
      new ListExperimentGroupsCommand({
        workspace_id: this.workspaceId,
        org_id: this.orgId,
      })
    );
    this.lastError = undefined;
  } catch (e: any) {
    this.lastError = e;
    this.lastResponse = undefined;
  }
});

When(
  "I list experiment groups sorted by {string} in {string} order",
  async function (this: PlaywrightWorld, sortOn: string, sortBy: string) {
    try {
      this.lastResponse = await this.client.send(
        new ListExperimentGroupsCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          sort_on:
            sortOn === "created_at"
              ? ExperimentGroupSortOn.CREATED_AT
              : ExperimentGroupSortOn.CREATED_AT,
          sort_by: sortBy === "DESC" ? SortBy.DESC : SortBy.ASC,
          count: 5,
        })
      );
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

When(
  "I delete the experiment group",
  async function (this: PlaywrightWorld) {
    try {
      this.lastResponse = await this.client.send(
        new DeleteExperimentGroupCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          id: this.experimentGroupId,
        })
      );
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

When(
  "I delete experiment group {string}",
  async function (this: PlaywrightWorld, id: string) {
    try {
      this.lastResponse = await this.client.send(
        new DeleteExperimentGroupCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          id,
        })
      );
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

// ── Then ────────────────────────────────────────────────────────────

Then(
  "the response should contain the member experiment IDs",
  function (this: PlaywrightWorld) {
    assert.ok(this.lastResponse, "No response");
    assert.ok(
      this.lastResponse.member_experiment_ids?.length > 0,
      "No member IDs in response"
    );
  }
);

Then(
  "the response traffic percentage should be {int}",
  function (this: PlaywrightWorld, expected: number) {
    assert.ok(this.lastResponse, "No response");
    assert.strictEqual(this.lastResponse.traffic_percentage, expected);
  }
);

Then(
  "the response member list should be empty",
  function (this: PlaywrightWorld) {
    assert.ok(this.lastResponse, "No response");
    assert.deepStrictEqual(this.lastResponse.member_experiment_ids, []);
  }
);

Then(
  "the response should have a group name",
  function (this: PlaywrightWorld) {
    assert.ok(this.lastResponse, "No response");
    assert.ok(this.lastResponse.name, "No group name in response");
  }
);

Then(
  "the response should contain the added experiment ID",
  function (this: PlaywrightWorld) {
    assert.ok(this.lastResponse, "No response");
    assert.ok(
      this.lastResponse.member_experiment_ids?.includes(validExp2Id),
      "Added experiment not found in member list"
    );
  }
);

Then(
  "the response should not contain the removed experiment ID",
  function (this: PlaywrightWorld) {
    assert.ok(this.lastResponse, "No response");
    assert.ok(
      !this.lastResponse.member_experiment_ids?.includes(validExpId),
      "Removed experiment still in member list"
    );
  }
);

Then(
  "the list should contain the created group",
  function (this: PlaywrightWorld) {
    const data = this.lastResponse?.data;
    assert.ok(Array.isArray(data), "Response is not a list");
    assert.ok(data.length > 0, "List is empty");
  }
);

Then(
  "the response should be sorted by created_at descending",
  function (this: PlaywrightWorld) {
    const data = this.lastResponse?.data;
    assert.ok(Array.isArray(data), "Response is not a list");
    if (data.length > 1) {
      const d1 = new Date(data[0].created_at).getTime();
      const d2 = new Date(data[1].created_at).getTime();
      assert.ok(d1 >= d2, "Results not sorted by created_at DESC");
    }
  }
);
