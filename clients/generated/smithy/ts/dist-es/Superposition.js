import { SuperpositionClient, } from "./SuperpositionClient";
import { ApplicableVariantsCommand, } from "./commands/ApplicableVariantsCommand";
import { BulkOperationCommand, } from "./commands/BulkOperationCommand";
import { ConcludeExperimentCommand, } from "./commands/ConcludeExperimentCommand";
import { CreateContextCommand, } from "./commands/CreateContextCommand";
import { CreateDefaultConfigCommand, } from "./commands/CreateDefaultConfigCommand";
import { CreateDimensionCommand, } from "./commands/CreateDimensionCommand";
import { CreateExperimentCommand, } from "./commands/CreateExperimentCommand";
import { CreateFunctionCommand, } from "./commands/CreateFunctionCommand";
import { CreateOrganisationCommand, } from "./commands/CreateOrganisationCommand";
import { CreateTypeTemplatesCommand, } from "./commands/CreateTypeTemplatesCommand";
import { CreateWorkspaceCommand, } from "./commands/CreateWorkspaceCommand";
import { DeleteContextCommand, } from "./commands/DeleteContextCommand";
import { DeleteDefaultConfigCommand, } from "./commands/DeleteDefaultConfigCommand";
import { DeleteDimensionCommand, } from "./commands/DeleteDimensionCommand";
import { DeleteFunctionCommand, } from "./commands/DeleteFunctionCommand";
import { DeleteTypeTemplatesCommand, } from "./commands/DeleteTypeTemplatesCommand";
import { DiscardExperimentCommand, } from "./commands/DiscardExperimentCommand";
import { GetConfigCommand, } from "./commands/GetConfigCommand";
import { GetConfigFastCommand, } from "./commands/GetConfigFastCommand";
import { GetContextCommand, } from "./commands/GetContextCommand";
import { GetContextFromConditionCommand, } from "./commands/GetContextFromConditionCommand";
import { GetExperimentCommand, } from "./commands/GetExperimentCommand";
import { GetFunctionCommand, } from "./commands/GetFunctionCommand";
import { GetOrganisationCommand, } from "./commands/GetOrganisationCommand";
import { GetResolvedConfigCommand, } from "./commands/GetResolvedConfigCommand";
import { GetTypeTemplatesListCommand, } from "./commands/GetTypeTemplatesListCommand";
import { ListAuditLogsCommand, } from "./commands/ListAuditLogsCommand";
import { ListContextsCommand, } from "./commands/ListContextsCommand";
import { ListDefaultConfigsCommand, } from "./commands/ListDefaultConfigsCommand";
import { ListDimensionsCommand, } from "./commands/ListDimensionsCommand";
import { ListExperimentCommand, } from "./commands/ListExperimentCommand";
import { ListFunctionCommand, } from "./commands/ListFunctionCommand";
import { ListOrganisationCommand, } from "./commands/ListOrganisationCommand";
import { ListVersionsCommand, } from "./commands/ListVersionsCommand";
import { ListWorkspaceCommand, } from "./commands/ListWorkspaceCommand";
import { MoveContextCommand, } from "./commands/MoveContextCommand";
import { PublishCommand, } from "./commands/PublishCommand";
import { RampExperimentCommand, } from "./commands/RampExperimentCommand";
import { TestCommand, } from "./commands/TestCommand";
import { UpdateDefaultConfigCommand, } from "./commands/UpdateDefaultConfigCommand";
import { UpdateDimensionCommand, } from "./commands/UpdateDimensionCommand";
import { UpdateFunctionCommand, } from "./commands/UpdateFunctionCommand";
import { UpdateOrganisationCommand, } from "./commands/UpdateOrganisationCommand";
import { UpdateOverrideCommand, } from "./commands/UpdateOverrideCommand";
import { UpdateOverridesExperimentCommand, } from "./commands/UpdateOverridesExperimentCommand";
import { UpdateTypeTemplatesCommand, } from "./commands/UpdateTypeTemplatesCommand";
import { UpdateWorkspaceCommand, } from "./commands/UpdateWorkspaceCommand";
import { WeightRecomputeCommand, } from "./commands/WeightRecomputeCommand";
import { createAggregatedClient } from "@smithy/smithy-client";
const commands = {
    ApplicableVariantsCommand,
    BulkOperationCommand,
    ConcludeExperimentCommand,
    CreateContextCommand,
    CreateDefaultConfigCommand,
    CreateDimensionCommand,
    CreateExperimentCommand,
    CreateFunctionCommand,
    CreateOrganisationCommand,
    CreateTypeTemplatesCommand,
    CreateWorkspaceCommand,
    DeleteContextCommand,
    DeleteDefaultConfigCommand,
    DeleteDimensionCommand,
    DeleteFunctionCommand,
    DeleteTypeTemplatesCommand,
    DiscardExperimentCommand,
    GetConfigCommand,
    GetConfigFastCommand,
    GetContextCommand,
    GetContextFromConditionCommand,
    GetExperimentCommand,
    GetFunctionCommand,
    GetOrganisationCommand,
    GetResolvedConfigCommand,
    GetTypeTemplatesListCommand,
    ListAuditLogsCommand,
    ListContextsCommand,
    ListDefaultConfigsCommand,
    ListDimensionsCommand,
    ListExperimentCommand,
    ListFunctionCommand,
    ListOrganisationCommand,
    ListVersionsCommand,
    ListWorkspaceCommand,
    MoveContextCommand,
    PublishCommand,
    RampExperimentCommand,
    TestCommand,
    UpdateDefaultConfigCommand,
    UpdateDimensionCommand,
    UpdateFunctionCommand,
    UpdateOrganisationCommand,
    UpdateOverrideCommand,
    UpdateOverridesExperimentCommand,
    UpdateTypeTemplatesCommand,
    UpdateWorkspaceCommand,
    WeightRecomputeCommand,
};
export class Superposition extends SuperpositionClient {
}
createAggregatedClient(commands, Superposition);
