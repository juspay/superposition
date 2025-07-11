import { SuperpositionClient, } from "./SuperpositionClient";
import { AddMembersToGroupCommand, } from "./commands/AddMembersToGroupCommand";
import { ApplicableVariantsCommand, } from "./commands/ApplicableVariantsCommand";
import { BulkOperationCommand, } from "./commands/BulkOperationCommand";
import { ConcludeExperimentCommand, } from "./commands/ConcludeExperimentCommand";
import { CreateContextCommand, } from "./commands/CreateContextCommand";
import { CreateDefaultConfigCommand, } from "./commands/CreateDefaultConfigCommand";
import { CreateDimensionCommand, } from "./commands/CreateDimensionCommand";
import { CreateExperimentCommand, } from "./commands/CreateExperimentCommand";
import { CreateExperimentGroupCommand, } from "./commands/CreateExperimentGroupCommand";
import { CreateFunctionCommand, } from "./commands/CreateFunctionCommand";
import { CreateOrganisationCommand, } from "./commands/CreateOrganisationCommand";
import { CreateTypeTemplatesCommand, } from "./commands/CreateTypeTemplatesCommand";
import { CreateWebhookCommand, } from "./commands/CreateWebhookCommand";
import { CreateWorkspaceCommand, } from "./commands/CreateWorkspaceCommand";
import { DeleteContextCommand, } from "./commands/DeleteContextCommand";
import { DeleteDefaultConfigCommand, } from "./commands/DeleteDefaultConfigCommand";
import { DeleteDimensionCommand, } from "./commands/DeleteDimensionCommand";
import { DeleteExperimentGroupCommand, } from "./commands/DeleteExperimentGroupCommand";
import { DeleteFunctionCommand, } from "./commands/DeleteFunctionCommand";
import { DeleteTypeTemplatesCommand, } from "./commands/DeleteTypeTemplatesCommand";
import { DiscardExperimentCommand, } from "./commands/DiscardExperimentCommand";
import { GetConfigCommand, } from "./commands/GetConfigCommand";
import { GetConfigFastCommand, } from "./commands/GetConfigFastCommand";
import { GetContextCommand, } from "./commands/GetContextCommand";
import { GetContextFromConditionCommand, } from "./commands/GetContextFromConditionCommand";
import { GetDimensionCommand, } from "./commands/GetDimensionCommand";
import { GetExperimentCommand, } from "./commands/GetExperimentCommand";
import { GetExperimentGroupCommand, } from "./commands/GetExperimentGroupCommand";
import { GetFunctionCommand, } from "./commands/GetFunctionCommand";
import { GetOrganisationCommand, } from "./commands/GetOrganisationCommand";
import { GetResolvedConfigCommand, } from "./commands/GetResolvedConfigCommand";
import { GetTypeTemplatesListCommand, } from "./commands/GetTypeTemplatesListCommand";
import { GetWebhookCommand, } from "./commands/GetWebhookCommand";
import { ListAuditLogsCommand, } from "./commands/ListAuditLogsCommand";
import { ListContextsCommand, } from "./commands/ListContextsCommand";
import { ListDefaultConfigsCommand, } from "./commands/ListDefaultConfigsCommand";
import { ListDimensionsCommand, } from "./commands/ListDimensionsCommand";
import { ListExperimentCommand, } from "./commands/ListExperimentCommand";
import { ListExperimentGroupsCommand, } from "./commands/ListExperimentGroupsCommand";
import { ListFunctionCommand, } from "./commands/ListFunctionCommand";
import { ListOrganisationCommand, } from "./commands/ListOrganisationCommand";
import { ListVersionsCommand, } from "./commands/ListVersionsCommand";
import { ListWebhookCommand, } from "./commands/ListWebhookCommand";
import { ListWorkspaceCommand, } from "./commands/ListWorkspaceCommand";
import { MoveContextCommand, } from "./commands/MoveContextCommand";
import { PauseExperimentCommand, } from "./commands/PauseExperimentCommand";
import { PublishCommand, } from "./commands/PublishCommand";
import { RampExperimentCommand, } from "./commands/RampExperimentCommand";
import { RemoveMembersFromGroupCommand, } from "./commands/RemoveMembersFromGroupCommand";
import { ResumeExperimentCommand, } from "./commands/ResumeExperimentCommand";
import { TestCommand, } from "./commands/TestCommand";
import { UpdateDefaultConfigCommand, } from "./commands/UpdateDefaultConfigCommand";
import { UpdateDimensionCommand, } from "./commands/UpdateDimensionCommand";
import { UpdateExperimentGroupCommand, } from "./commands/UpdateExperimentGroupCommand";
import { UpdateFunctionCommand, } from "./commands/UpdateFunctionCommand";
import { UpdateOrganisationCommand, } from "./commands/UpdateOrganisationCommand";
import { UpdateOverrideCommand, } from "./commands/UpdateOverrideCommand";
import { UpdateOverridesExperimentCommand, } from "./commands/UpdateOverridesExperimentCommand";
import { UpdateTypeTemplatesCommand, } from "./commands/UpdateTypeTemplatesCommand";
import { UpdateWebhookCommand, } from "./commands/UpdateWebhookCommand";
import { UpdateWorkspaceCommand, } from "./commands/UpdateWorkspaceCommand";
import { WeightRecomputeCommand, } from "./commands/WeightRecomputeCommand";
import { createAggregatedClient } from "@smithy/smithy-client";
const commands = {
    AddMembersToGroupCommand,
    ApplicableVariantsCommand,
    BulkOperationCommand,
    ConcludeExperimentCommand,
    CreateContextCommand,
    CreateDefaultConfigCommand,
    CreateDimensionCommand,
    CreateExperimentCommand,
    CreateExperimentGroupCommand,
    CreateFunctionCommand,
    CreateOrganisationCommand,
    CreateTypeTemplatesCommand,
    CreateWebhookCommand,
    CreateWorkspaceCommand,
    DeleteContextCommand,
    DeleteDefaultConfigCommand,
    DeleteDimensionCommand,
    DeleteExperimentGroupCommand,
    DeleteFunctionCommand,
    DeleteTypeTemplatesCommand,
    DiscardExperimentCommand,
    GetConfigCommand,
    GetConfigFastCommand,
    GetContextCommand,
    GetContextFromConditionCommand,
    GetDimensionCommand,
    GetExperimentCommand,
    GetExperimentGroupCommand,
    GetFunctionCommand,
    GetOrganisationCommand,
    GetResolvedConfigCommand,
    GetTypeTemplatesListCommand,
    GetWebhookCommand,
    ListAuditLogsCommand,
    ListContextsCommand,
    ListDefaultConfigsCommand,
    ListDimensionsCommand,
    ListExperimentCommand,
    ListExperimentGroupsCommand,
    ListFunctionCommand,
    ListOrganisationCommand,
    ListVersionsCommand,
    ListWebhookCommand,
    ListWorkspaceCommand,
    MoveContextCommand,
    PauseExperimentCommand,
    PublishCommand,
    RampExperimentCommand,
    RemoveMembersFromGroupCommand,
    ResumeExperimentCommand,
    TestCommand,
    UpdateDefaultConfigCommand,
    UpdateDimensionCommand,
    UpdateExperimentGroupCommand,
    UpdateFunctionCommand,
    UpdateOrganisationCommand,
    UpdateOverrideCommand,
    UpdateOverridesExperimentCommand,
    UpdateTypeTemplatesCommand,
    UpdateWebhookCommand,
    UpdateWorkspaceCommand,
    WeightRecomputeCommand,
};
export class Superposition extends SuperpositionClient {
}
createAggregatedClient(commands, Superposition);
