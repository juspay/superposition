"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.Superposition = void 0;
const SuperpositionClient_1 = require("./SuperpositionClient");
const AddMembersToGroupCommand_1 = require("./commands/AddMembersToGroupCommand");
const ApplicableVariantsCommand_1 = require("./commands/ApplicableVariantsCommand");
const BulkOperationCommand_1 = require("./commands/BulkOperationCommand");
const ConcludeExperimentCommand_1 = require("./commands/ConcludeExperimentCommand");
const CreateContextCommand_1 = require("./commands/CreateContextCommand");
const CreateDefaultConfigCommand_1 = require("./commands/CreateDefaultConfigCommand");
const CreateDimensionCommand_1 = require("./commands/CreateDimensionCommand");
const CreateExperimentCommand_1 = require("./commands/CreateExperimentCommand");
const CreateExperimentGroupCommand_1 = require("./commands/CreateExperimentGroupCommand");
const CreateFunctionCommand_1 = require("./commands/CreateFunctionCommand");
const CreateOrganisationCommand_1 = require("./commands/CreateOrganisationCommand");
const CreateTypeTemplatesCommand_1 = require("./commands/CreateTypeTemplatesCommand");
const CreateWebhookCommand_1 = require("./commands/CreateWebhookCommand");
const CreateWorkspaceCommand_1 = require("./commands/CreateWorkspaceCommand");
const DeleteContextCommand_1 = require("./commands/DeleteContextCommand");
const DeleteDefaultConfigCommand_1 = require("./commands/DeleteDefaultConfigCommand");
const DeleteDimensionCommand_1 = require("./commands/DeleteDimensionCommand");
const DeleteExperimentGroupCommand_1 = require("./commands/DeleteExperimentGroupCommand");
const DeleteFunctionCommand_1 = require("./commands/DeleteFunctionCommand");
const DeleteTypeTemplatesCommand_1 = require("./commands/DeleteTypeTemplatesCommand");
const DiscardExperimentCommand_1 = require("./commands/DiscardExperimentCommand");
const GetConfigCommand_1 = require("./commands/GetConfigCommand");
const GetConfigFastCommand_1 = require("./commands/GetConfigFastCommand");
const GetContextCommand_1 = require("./commands/GetContextCommand");
const GetContextFromConditionCommand_1 = require("./commands/GetContextFromConditionCommand");
const GetDimensionCommand_1 = require("./commands/GetDimensionCommand");
const GetExperimentCommand_1 = require("./commands/GetExperimentCommand");
const GetExperimentGroupCommand_1 = require("./commands/GetExperimentGroupCommand");
const GetFunctionCommand_1 = require("./commands/GetFunctionCommand");
const GetOrganisationCommand_1 = require("./commands/GetOrganisationCommand");
const GetResolvedConfigCommand_1 = require("./commands/GetResolvedConfigCommand");
const GetTypeTemplatesListCommand_1 = require("./commands/GetTypeTemplatesListCommand");
const GetWebhookCommand_1 = require("./commands/GetWebhookCommand");
const ListAuditLogsCommand_1 = require("./commands/ListAuditLogsCommand");
const ListContextsCommand_1 = require("./commands/ListContextsCommand");
const ListDefaultConfigsCommand_1 = require("./commands/ListDefaultConfigsCommand");
const ListDimensionsCommand_1 = require("./commands/ListDimensionsCommand");
const ListExperimentCommand_1 = require("./commands/ListExperimentCommand");
const ListExperimentGroupsCommand_1 = require("./commands/ListExperimentGroupsCommand");
const ListFunctionCommand_1 = require("./commands/ListFunctionCommand");
const ListOrganisationCommand_1 = require("./commands/ListOrganisationCommand");
const ListVersionsCommand_1 = require("./commands/ListVersionsCommand");
const ListWebhookCommand_1 = require("./commands/ListWebhookCommand");
const ListWorkspaceCommand_1 = require("./commands/ListWorkspaceCommand");
const MoveContextCommand_1 = require("./commands/MoveContextCommand");
const PauseExperimentCommand_1 = require("./commands/PauseExperimentCommand");
const PublishCommand_1 = require("./commands/PublishCommand");
const RampExperimentCommand_1 = require("./commands/RampExperimentCommand");
const RemoveMembersFromGroupCommand_1 = require("./commands/RemoveMembersFromGroupCommand");
const ResumeExperimentCommand_1 = require("./commands/ResumeExperimentCommand");
const TestCommand_1 = require("./commands/TestCommand");
const UpdateDefaultConfigCommand_1 = require("./commands/UpdateDefaultConfigCommand");
const UpdateDimensionCommand_1 = require("./commands/UpdateDimensionCommand");
const UpdateExperimentGroupCommand_1 = require("./commands/UpdateExperimentGroupCommand");
const UpdateFunctionCommand_1 = require("./commands/UpdateFunctionCommand");
const UpdateOrganisationCommand_1 = require("./commands/UpdateOrganisationCommand");
const UpdateOverrideCommand_1 = require("./commands/UpdateOverrideCommand");
const UpdateOverridesExperimentCommand_1 = require("./commands/UpdateOverridesExperimentCommand");
const UpdateTypeTemplatesCommand_1 = require("./commands/UpdateTypeTemplatesCommand");
const UpdateWebhookCommand_1 = require("./commands/UpdateWebhookCommand");
const UpdateWorkspaceCommand_1 = require("./commands/UpdateWorkspaceCommand");
const WeightRecomputeCommand_1 = require("./commands/WeightRecomputeCommand");
const smithy_client_1 = require("@smithy/smithy-client");
const commands = {
    AddMembersToGroupCommand: AddMembersToGroupCommand_1.AddMembersToGroupCommand,
    ApplicableVariantsCommand: ApplicableVariantsCommand_1.ApplicableVariantsCommand,
    BulkOperationCommand: BulkOperationCommand_1.BulkOperationCommand,
    ConcludeExperimentCommand: ConcludeExperimentCommand_1.ConcludeExperimentCommand,
    CreateContextCommand: CreateContextCommand_1.CreateContextCommand,
    CreateDefaultConfigCommand: CreateDefaultConfigCommand_1.CreateDefaultConfigCommand,
    CreateDimensionCommand: CreateDimensionCommand_1.CreateDimensionCommand,
    CreateExperimentCommand: CreateExperimentCommand_1.CreateExperimentCommand,
    CreateExperimentGroupCommand: CreateExperimentGroupCommand_1.CreateExperimentGroupCommand,
    CreateFunctionCommand: CreateFunctionCommand_1.CreateFunctionCommand,
    CreateOrganisationCommand: CreateOrganisationCommand_1.CreateOrganisationCommand,
    CreateTypeTemplatesCommand: CreateTypeTemplatesCommand_1.CreateTypeTemplatesCommand,
    CreateWebhookCommand: CreateWebhookCommand_1.CreateWebhookCommand,
    CreateWorkspaceCommand: CreateWorkspaceCommand_1.CreateWorkspaceCommand,
    DeleteContextCommand: DeleteContextCommand_1.DeleteContextCommand,
    DeleteDefaultConfigCommand: DeleteDefaultConfigCommand_1.DeleteDefaultConfigCommand,
    DeleteDimensionCommand: DeleteDimensionCommand_1.DeleteDimensionCommand,
    DeleteExperimentGroupCommand: DeleteExperimentGroupCommand_1.DeleteExperimentGroupCommand,
    DeleteFunctionCommand: DeleteFunctionCommand_1.DeleteFunctionCommand,
    DeleteTypeTemplatesCommand: DeleteTypeTemplatesCommand_1.DeleteTypeTemplatesCommand,
    DiscardExperimentCommand: DiscardExperimentCommand_1.DiscardExperimentCommand,
    GetConfigCommand: GetConfigCommand_1.GetConfigCommand,
    GetConfigFastCommand: GetConfigFastCommand_1.GetConfigFastCommand,
    GetContextCommand: GetContextCommand_1.GetContextCommand,
    GetContextFromConditionCommand: GetContextFromConditionCommand_1.GetContextFromConditionCommand,
    GetDimensionCommand: GetDimensionCommand_1.GetDimensionCommand,
    GetExperimentCommand: GetExperimentCommand_1.GetExperimentCommand,
    GetExperimentGroupCommand: GetExperimentGroupCommand_1.GetExperimentGroupCommand,
    GetFunctionCommand: GetFunctionCommand_1.GetFunctionCommand,
    GetOrganisationCommand: GetOrganisationCommand_1.GetOrganisationCommand,
    GetResolvedConfigCommand: GetResolvedConfigCommand_1.GetResolvedConfigCommand,
    GetTypeTemplatesListCommand: GetTypeTemplatesListCommand_1.GetTypeTemplatesListCommand,
    GetWebhookCommand: GetWebhookCommand_1.GetWebhookCommand,
    ListAuditLogsCommand: ListAuditLogsCommand_1.ListAuditLogsCommand,
    ListContextsCommand: ListContextsCommand_1.ListContextsCommand,
    ListDefaultConfigsCommand: ListDefaultConfigsCommand_1.ListDefaultConfigsCommand,
    ListDimensionsCommand: ListDimensionsCommand_1.ListDimensionsCommand,
    ListExperimentCommand: ListExperimentCommand_1.ListExperimentCommand,
    ListExperimentGroupsCommand: ListExperimentGroupsCommand_1.ListExperimentGroupsCommand,
    ListFunctionCommand: ListFunctionCommand_1.ListFunctionCommand,
    ListOrganisationCommand: ListOrganisationCommand_1.ListOrganisationCommand,
    ListVersionsCommand: ListVersionsCommand_1.ListVersionsCommand,
    ListWebhookCommand: ListWebhookCommand_1.ListWebhookCommand,
    ListWorkspaceCommand: ListWorkspaceCommand_1.ListWorkspaceCommand,
    MoveContextCommand: MoveContextCommand_1.MoveContextCommand,
    PauseExperimentCommand: PauseExperimentCommand_1.PauseExperimentCommand,
    PublishCommand: PublishCommand_1.PublishCommand,
    RampExperimentCommand: RampExperimentCommand_1.RampExperimentCommand,
    RemoveMembersFromGroupCommand: RemoveMembersFromGroupCommand_1.RemoveMembersFromGroupCommand,
    ResumeExperimentCommand: ResumeExperimentCommand_1.ResumeExperimentCommand,
    TestCommand: TestCommand_1.TestCommand,
    UpdateDefaultConfigCommand: UpdateDefaultConfigCommand_1.UpdateDefaultConfigCommand,
    UpdateDimensionCommand: UpdateDimensionCommand_1.UpdateDimensionCommand,
    UpdateExperimentGroupCommand: UpdateExperimentGroupCommand_1.UpdateExperimentGroupCommand,
    UpdateFunctionCommand: UpdateFunctionCommand_1.UpdateFunctionCommand,
    UpdateOrganisationCommand: UpdateOrganisationCommand_1.UpdateOrganisationCommand,
    UpdateOverrideCommand: UpdateOverrideCommand_1.UpdateOverrideCommand,
    UpdateOverridesExperimentCommand: UpdateOverridesExperimentCommand_1.UpdateOverridesExperimentCommand,
    UpdateTypeTemplatesCommand: UpdateTypeTemplatesCommand_1.UpdateTypeTemplatesCommand,
    UpdateWebhookCommand: UpdateWebhookCommand_1.UpdateWebhookCommand,
    UpdateWorkspaceCommand: UpdateWorkspaceCommand_1.UpdateWorkspaceCommand,
    WeightRecomputeCommand: WeightRecomputeCommand_1.WeightRecomputeCommand,
};
class Superposition extends SuperpositionClient_1.SuperpositionClient {
}
exports.Superposition = Superposition;
(0, smithy_client_1.createAggregatedClient)(commands, Superposition);
