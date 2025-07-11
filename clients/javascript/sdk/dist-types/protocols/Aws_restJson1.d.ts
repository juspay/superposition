import { AddMembersToGroupCommandInput, AddMembersToGroupCommandOutput } from "../commands/AddMembersToGroupCommand";
import { ApplicableVariantsCommandInput, ApplicableVariantsCommandOutput } from "../commands/ApplicableVariantsCommand";
import { BulkOperationCommandInput, BulkOperationCommandOutput } from "../commands/BulkOperationCommand";
import { ConcludeExperimentCommandInput, ConcludeExperimentCommandOutput } from "../commands/ConcludeExperimentCommand";
import { CreateContextCommandInput, CreateContextCommandOutput } from "../commands/CreateContextCommand";
import { CreateDefaultConfigCommandInput, CreateDefaultConfigCommandOutput } from "../commands/CreateDefaultConfigCommand";
import { CreateDimensionCommandInput, CreateDimensionCommandOutput } from "../commands/CreateDimensionCommand";
import { CreateExperimentCommandInput, CreateExperimentCommandOutput } from "../commands/CreateExperimentCommand";
import { CreateExperimentGroupCommandInput, CreateExperimentGroupCommandOutput } from "../commands/CreateExperimentGroupCommand";
import { CreateFunctionCommandInput, CreateFunctionCommandOutput } from "../commands/CreateFunctionCommand";
import { CreateOrganisationCommandInput, CreateOrganisationCommandOutput } from "../commands/CreateOrganisationCommand";
import { CreateTypeTemplatesCommandInput, CreateTypeTemplatesCommandOutput } from "../commands/CreateTypeTemplatesCommand";
import { CreateWebhookCommandInput, CreateWebhookCommandOutput } from "../commands/CreateWebhookCommand";
import { CreateWorkspaceCommandInput, CreateWorkspaceCommandOutput } from "../commands/CreateWorkspaceCommand";
import { DeleteContextCommandInput, DeleteContextCommandOutput } from "../commands/DeleteContextCommand";
import { DeleteDefaultConfigCommandInput, DeleteDefaultConfigCommandOutput } from "../commands/DeleteDefaultConfigCommand";
import { DeleteDimensionCommandInput, DeleteDimensionCommandOutput } from "../commands/DeleteDimensionCommand";
import { DeleteExperimentGroupCommandInput, DeleteExperimentGroupCommandOutput } from "../commands/DeleteExperimentGroupCommand";
import { DeleteFunctionCommandInput, DeleteFunctionCommandOutput } from "../commands/DeleteFunctionCommand";
import { DeleteTypeTemplatesCommandInput, DeleteTypeTemplatesCommandOutput } from "../commands/DeleteTypeTemplatesCommand";
import { DiscardExperimentCommandInput, DiscardExperimentCommandOutput } from "../commands/DiscardExperimentCommand";
import { GetConfigCommandInput, GetConfigCommandOutput } from "../commands/GetConfigCommand";
import { GetConfigFastCommandInput, GetConfigFastCommandOutput } from "../commands/GetConfigFastCommand";
import { GetContextCommandInput, GetContextCommandOutput } from "../commands/GetContextCommand";
import { GetContextFromConditionCommandInput, GetContextFromConditionCommandOutput } from "../commands/GetContextFromConditionCommand";
import { GetDimensionCommandInput, GetDimensionCommandOutput } from "../commands/GetDimensionCommand";
import { GetExperimentCommandInput, GetExperimentCommandOutput } from "../commands/GetExperimentCommand";
import { GetExperimentGroupCommandInput, GetExperimentGroupCommandOutput } from "../commands/GetExperimentGroupCommand";
import { GetFunctionCommandInput, GetFunctionCommandOutput } from "../commands/GetFunctionCommand";
import { GetOrganisationCommandInput, GetOrganisationCommandOutput } from "../commands/GetOrganisationCommand";
import { GetResolvedConfigCommandInput, GetResolvedConfigCommandOutput } from "../commands/GetResolvedConfigCommand";
import { GetTypeTemplatesListCommandInput, GetTypeTemplatesListCommandOutput } from "../commands/GetTypeTemplatesListCommand";
import { GetWebhookCommandInput, GetWebhookCommandOutput } from "../commands/GetWebhookCommand";
import { ListAuditLogsCommandInput, ListAuditLogsCommandOutput } from "../commands/ListAuditLogsCommand";
import { ListContextsCommandInput, ListContextsCommandOutput } from "../commands/ListContextsCommand";
import { ListDefaultConfigsCommandInput, ListDefaultConfigsCommandOutput } from "../commands/ListDefaultConfigsCommand";
import { ListDimensionsCommandInput, ListDimensionsCommandOutput } from "../commands/ListDimensionsCommand";
import { ListExperimentCommandInput, ListExperimentCommandOutput } from "../commands/ListExperimentCommand";
import { ListExperimentGroupsCommandInput, ListExperimentGroupsCommandOutput } from "../commands/ListExperimentGroupsCommand";
import { ListFunctionCommandInput, ListFunctionCommandOutput } from "../commands/ListFunctionCommand";
import { ListOrganisationCommandInput, ListOrganisationCommandOutput } from "../commands/ListOrganisationCommand";
import { ListVersionsCommandInput, ListVersionsCommandOutput } from "../commands/ListVersionsCommand";
import { ListWebhookCommandInput, ListWebhookCommandOutput } from "../commands/ListWebhookCommand";
import { ListWorkspaceCommandInput, ListWorkspaceCommandOutput } from "../commands/ListWorkspaceCommand";
import { MoveContextCommandInput, MoveContextCommandOutput } from "../commands/MoveContextCommand";
import { PauseExperimentCommandInput, PauseExperimentCommandOutput } from "../commands/PauseExperimentCommand";
import { PublishCommandInput, PublishCommandOutput } from "../commands/PublishCommand";
import { RampExperimentCommandInput, RampExperimentCommandOutput } from "../commands/RampExperimentCommand";
import { RemoveMembersFromGroupCommandInput, RemoveMembersFromGroupCommandOutput } from "../commands/RemoveMembersFromGroupCommand";
import { ResumeExperimentCommandInput, ResumeExperimentCommandOutput } from "../commands/ResumeExperimentCommand";
import { TestCommandInput, TestCommandOutput } from "../commands/TestCommand";
import { UpdateDefaultConfigCommandInput, UpdateDefaultConfigCommandOutput } from "../commands/UpdateDefaultConfigCommand";
import { UpdateDimensionCommandInput, UpdateDimensionCommandOutput } from "../commands/UpdateDimensionCommand";
import { UpdateExperimentGroupCommandInput, UpdateExperimentGroupCommandOutput } from "../commands/UpdateExperimentGroupCommand";
import { UpdateFunctionCommandInput, UpdateFunctionCommandOutput } from "../commands/UpdateFunctionCommand";
import { UpdateOrganisationCommandInput, UpdateOrganisationCommandOutput } from "../commands/UpdateOrganisationCommand";
import { UpdateOverrideCommandInput, UpdateOverrideCommandOutput } from "../commands/UpdateOverrideCommand";
import { UpdateOverridesExperimentCommandInput, UpdateOverridesExperimentCommandOutput } from "../commands/UpdateOverridesExperimentCommand";
import { UpdateTypeTemplatesCommandInput, UpdateTypeTemplatesCommandOutput } from "../commands/UpdateTypeTemplatesCommand";
import { UpdateWebhookCommandInput, UpdateWebhookCommandOutput } from "../commands/UpdateWebhookCommand";
import { UpdateWorkspaceCommandInput, UpdateWorkspaceCommandOutput } from "../commands/UpdateWorkspaceCommand";
import { WeightRecomputeCommandInput, WeightRecomputeCommandOutput } from "../commands/WeightRecomputeCommand";
import { HttpRequest as __HttpRequest, HttpResponse as __HttpResponse } from "@smithy/protocol-http";
import { SerdeContext as __SerdeContext } from "@smithy/types";
/**
 * serializeAws_restJson1AddMembersToGroupCommand
 */
export declare const se_AddMembersToGroupCommand: (input: AddMembersToGroupCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1ApplicableVariantsCommand
 */
export declare const se_ApplicableVariantsCommand: (input: ApplicableVariantsCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1BulkOperationCommand
 */
export declare const se_BulkOperationCommand: (input: BulkOperationCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1ConcludeExperimentCommand
 */
export declare const se_ConcludeExperimentCommand: (input: ConcludeExperimentCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1CreateContextCommand
 */
export declare const se_CreateContextCommand: (input: CreateContextCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1CreateDefaultConfigCommand
 */
export declare const se_CreateDefaultConfigCommand: (input: CreateDefaultConfigCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1CreateDimensionCommand
 */
export declare const se_CreateDimensionCommand: (input: CreateDimensionCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1CreateExperimentCommand
 */
export declare const se_CreateExperimentCommand: (input: CreateExperimentCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1CreateExperimentGroupCommand
 */
export declare const se_CreateExperimentGroupCommand: (input: CreateExperimentGroupCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1CreateFunctionCommand
 */
export declare const se_CreateFunctionCommand: (input: CreateFunctionCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1CreateOrganisationCommand
 */
export declare const se_CreateOrganisationCommand: (input: CreateOrganisationCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1CreateTypeTemplatesCommand
 */
export declare const se_CreateTypeTemplatesCommand: (input: CreateTypeTemplatesCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1CreateWebhookCommand
 */
export declare const se_CreateWebhookCommand: (input: CreateWebhookCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1CreateWorkspaceCommand
 */
export declare const se_CreateWorkspaceCommand: (input: CreateWorkspaceCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1DeleteContextCommand
 */
export declare const se_DeleteContextCommand: (input: DeleteContextCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1DeleteDefaultConfigCommand
 */
export declare const se_DeleteDefaultConfigCommand: (input: DeleteDefaultConfigCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1DeleteDimensionCommand
 */
export declare const se_DeleteDimensionCommand: (input: DeleteDimensionCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1DeleteExperimentGroupCommand
 */
export declare const se_DeleteExperimentGroupCommand: (input: DeleteExperimentGroupCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1DeleteFunctionCommand
 */
export declare const se_DeleteFunctionCommand: (input: DeleteFunctionCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1DeleteTypeTemplatesCommand
 */
export declare const se_DeleteTypeTemplatesCommand: (input: DeleteTypeTemplatesCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1DiscardExperimentCommand
 */
export declare const se_DiscardExperimentCommand: (input: DiscardExperimentCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1GetConfigCommand
 */
export declare const se_GetConfigCommand: (input: GetConfigCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1GetConfigFastCommand
 */
export declare const se_GetConfigFastCommand: (input: GetConfigFastCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1GetContextCommand
 */
export declare const se_GetContextCommand: (input: GetContextCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1GetContextFromConditionCommand
 */
export declare const se_GetContextFromConditionCommand: (input: GetContextFromConditionCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1GetDimensionCommand
 */
export declare const se_GetDimensionCommand: (input: GetDimensionCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1GetExperimentCommand
 */
export declare const se_GetExperimentCommand: (input: GetExperimentCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1GetExperimentGroupCommand
 */
export declare const se_GetExperimentGroupCommand: (input: GetExperimentGroupCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1GetFunctionCommand
 */
export declare const se_GetFunctionCommand: (input: GetFunctionCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1GetOrganisationCommand
 */
export declare const se_GetOrganisationCommand: (input: GetOrganisationCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1GetResolvedConfigCommand
 */
export declare const se_GetResolvedConfigCommand: (input: GetResolvedConfigCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1GetTypeTemplatesListCommand
 */
export declare const se_GetTypeTemplatesListCommand: (input: GetTypeTemplatesListCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1GetWebhookCommand
 */
export declare const se_GetWebhookCommand: (input: GetWebhookCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1ListAuditLogsCommand
 */
export declare const se_ListAuditLogsCommand: (input: ListAuditLogsCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1ListContextsCommand
 */
export declare const se_ListContextsCommand: (input: ListContextsCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1ListDefaultConfigsCommand
 */
export declare const se_ListDefaultConfigsCommand: (input: ListDefaultConfigsCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1ListDimensionsCommand
 */
export declare const se_ListDimensionsCommand: (input: ListDimensionsCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1ListExperimentCommand
 */
export declare const se_ListExperimentCommand: (input: ListExperimentCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1ListExperimentGroupsCommand
 */
export declare const se_ListExperimentGroupsCommand: (input: ListExperimentGroupsCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1ListFunctionCommand
 */
export declare const se_ListFunctionCommand: (input: ListFunctionCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1ListOrganisationCommand
 */
export declare const se_ListOrganisationCommand: (input: ListOrganisationCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1ListVersionsCommand
 */
export declare const se_ListVersionsCommand: (input: ListVersionsCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1ListWebhookCommand
 */
export declare const se_ListWebhookCommand: (input: ListWebhookCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1ListWorkspaceCommand
 */
export declare const se_ListWorkspaceCommand: (input: ListWorkspaceCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1MoveContextCommand
 */
export declare const se_MoveContextCommand: (input: MoveContextCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1PauseExperimentCommand
 */
export declare const se_PauseExperimentCommand: (input: PauseExperimentCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1PublishCommand
 */
export declare const se_PublishCommand: (input: PublishCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1RampExperimentCommand
 */
export declare const se_RampExperimentCommand: (input: RampExperimentCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1RemoveMembersFromGroupCommand
 */
export declare const se_RemoveMembersFromGroupCommand: (input: RemoveMembersFromGroupCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1ResumeExperimentCommand
 */
export declare const se_ResumeExperimentCommand: (input: ResumeExperimentCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1TestCommand
 */
export declare const se_TestCommand: (input: TestCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1UpdateDefaultConfigCommand
 */
export declare const se_UpdateDefaultConfigCommand: (input: UpdateDefaultConfigCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1UpdateDimensionCommand
 */
export declare const se_UpdateDimensionCommand: (input: UpdateDimensionCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1UpdateExperimentGroupCommand
 */
export declare const se_UpdateExperimentGroupCommand: (input: UpdateExperimentGroupCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1UpdateFunctionCommand
 */
export declare const se_UpdateFunctionCommand: (input: UpdateFunctionCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1UpdateOrganisationCommand
 */
export declare const se_UpdateOrganisationCommand: (input: UpdateOrganisationCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1UpdateOverrideCommand
 */
export declare const se_UpdateOverrideCommand: (input: UpdateOverrideCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1UpdateOverridesExperimentCommand
 */
export declare const se_UpdateOverridesExperimentCommand: (input: UpdateOverridesExperimentCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1UpdateTypeTemplatesCommand
 */
export declare const se_UpdateTypeTemplatesCommand: (input: UpdateTypeTemplatesCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1UpdateWebhookCommand
 */
export declare const se_UpdateWebhookCommand: (input: UpdateWebhookCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1UpdateWorkspaceCommand
 */
export declare const se_UpdateWorkspaceCommand: (input: UpdateWorkspaceCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * serializeAws_restJson1WeightRecomputeCommand
 */
export declare const se_WeightRecomputeCommand: (input: WeightRecomputeCommandInput, context: __SerdeContext) => Promise<__HttpRequest>;
/**
 * deserializeAws_restJson1AddMembersToGroupCommand
 */
export declare const de_AddMembersToGroupCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<AddMembersToGroupCommandOutput>;
/**
 * deserializeAws_restJson1ApplicableVariantsCommand
 */
export declare const de_ApplicableVariantsCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<ApplicableVariantsCommandOutput>;
/**
 * deserializeAws_restJson1BulkOperationCommand
 */
export declare const de_BulkOperationCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<BulkOperationCommandOutput>;
/**
 * deserializeAws_restJson1ConcludeExperimentCommand
 */
export declare const de_ConcludeExperimentCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<ConcludeExperimentCommandOutput>;
/**
 * deserializeAws_restJson1CreateContextCommand
 */
export declare const de_CreateContextCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<CreateContextCommandOutput>;
/**
 * deserializeAws_restJson1CreateDefaultConfigCommand
 */
export declare const de_CreateDefaultConfigCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<CreateDefaultConfigCommandOutput>;
/**
 * deserializeAws_restJson1CreateDimensionCommand
 */
export declare const de_CreateDimensionCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<CreateDimensionCommandOutput>;
/**
 * deserializeAws_restJson1CreateExperimentCommand
 */
export declare const de_CreateExperimentCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<CreateExperimentCommandOutput>;
/**
 * deserializeAws_restJson1CreateExperimentGroupCommand
 */
export declare const de_CreateExperimentGroupCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<CreateExperimentGroupCommandOutput>;
/**
 * deserializeAws_restJson1CreateFunctionCommand
 */
export declare const de_CreateFunctionCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<CreateFunctionCommandOutput>;
/**
 * deserializeAws_restJson1CreateOrganisationCommand
 */
export declare const de_CreateOrganisationCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<CreateOrganisationCommandOutput>;
/**
 * deserializeAws_restJson1CreateTypeTemplatesCommand
 */
export declare const de_CreateTypeTemplatesCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<CreateTypeTemplatesCommandOutput>;
/**
 * deserializeAws_restJson1CreateWebhookCommand
 */
export declare const de_CreateWebhookCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<CreateWebhookCommandOutput>;
/**
 * deserializeAws_restJson1CreateWorkspaceCommand
 */
export declare const de_CreateWorkspaceCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<CreateWorkspaceCommandOutput>;
/**
 * deserializeAws_restJson1DeleteContextCommand
 */
export declare const de_DeleteContextCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<DeleteContextCommandOutput>;
/**
 * deserializeAws_restJson1DeleteDefaultConfigCommand
 */
export declare const de_DeleteDefaultConfigCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<DeleteDefaultConfigCommandOutput>;
/**
 * deserializeAws_restJson1DeleteDimensionCommand
 */
export declare const de_DeleteDimensionCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<DeleteDimensionCommandOutput>;
/**
 * deserializeAws_restJson1DeleteExperimentGroupCommand
 */
export declare const de_DeleteExperimentGroupCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<DeleteExperimentGroupCommandOutput>;
/**
 * deserializeAws_restJson1DeleteFunctionCommand
 */
export declare const de_DeleteFunctionCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<DeleteFunctionCommandOutput>;
/**
 * deserializeAws_restJson1DeleteTypeTemplatesCommand
 */
export declare const de_DeleteTypeTemplatesCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<DeleteTypeTemplatesCommandOutput>;
/**
 * deserializeAws_restJson1DiscardExperimentCommand
 */
export declare const de_DiscardExperimentCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<DiscardExperimentCommandOutput>;
/**
 * deserializeAws_restJson1GetConfigCommand
 */
export declare const de_GetConfigCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<GetConfigCommandOutput>;
/**
 * deserializeAws_restJson1GetConfigFastCommand
 */
export declare const de_GetConfigFastCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<GetConfigFastCommandOutput>;
/**
 * deserializeAws_restJson1GetContextCommand
 */
export declare const de_GetContextCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<GetContextCommandOutput>;
/**
 * deserializeAws_restJson1GetContextFromConditionCommand
 */
export declare const de_GetContextFromConditionCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<GetContextFromConditionCommandOutput>;
/**
 * deserializeAws_restJson1GetDimensionCommand
 */
export declare const de_GetDimensionCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<GetDimensionCommandOutput>;
/**
 * deserializeAws_restJson1GetExperimentCommand
 */
export declare const de_GetExperimentCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<GetExperimentCommandOutput>;
/**
 * deserializeAws_restJson1GetExperimentGroupCommand
 */
export declare const de_GetExperimentGroupCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<GetExperimentGroupCommandOutput>;
/**
 * deserializeAws_restJson1GetFunctionCommand
 */
export declare const de_GetFunctionCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<GetFunctionCommandOutput>;
/**
 * deserializeAws_restJson1GetOrganisationCommand
 */
export declare const de_GetOrganisationCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<GetOrganisationCommandOutput>;
/**
 * deserializeAws_restJson1GetResolvedConfigCommand
 */
export declare const de_GetResolvedConfigCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<GetResolvedConfigCommandOutput>;
/**
 * deserializeAws_restJson1GetTypeTemplatesListCommand
 */
export declare const de_GetTypeTemplatesListCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<GetTypeTemplatesListCommandOutput>;
/**
 * deserializeAws_restJson1GetWebhookCommand
 */
export declare const de_GetWebhookCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<GetWebhookCommandOutput>;
/**
 * deserializeAws_restJson1ListAuditLogsCommand
 */
export declare const de_ListAuditLogsCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<ListAuditLogsCommandOutput>;
/**
 * deserializeAws_restJson1ListContextsCommand
 */
export declare const de_ListContextsCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<ListContextsCommandOutput>;
/**
 * deserializeAws_restJson1ListDefaultConfigsCommand
 */
export declare const de_ListDefaultConfigsCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<ListDefaultConfigsCommandOutput>;
/**
 * deserializeAws_restJson1ListDimensionsCommand
 */
export declare const de_ListDimensionsCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<ListDimensionsCommandOutput>;
/**
 * deserializeAws_restJson1ListExperimentCommand
 */
export declare const de_ListExperimentCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<ListExperimentCommandOutput>;
/**
 * deserializeAws_restJson1ListExperimentGroupsCommand
 */
export declare const de_ListExperimentGroupsCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<ListExperimentGroupsCommandOutput>;
/**
 * deserializeAws_restJson1ListFunctionCommand
 */
export declare const de_ListFunctionCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<ListFunctionCommandOutput>;
/**
 * deserializeAws_restJson1ListOrganisationCommand
 */
export declare const de_ListOrganisationCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<ListOrganisationCommandOutput>;
/**
 * deserializeAws_restJson1ListVersionsCommand
 */
export declare const de_ListVersionsCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<ListVersionsCommandOutput>;
/**
 * deserializeAws_restJson1ListWebhookCommand
 */
export declare const de_ListWebhookCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<ListWebhookCommandOutput>;
/**
 * deserializeAws_restJson1ListWorkspaceCommand
 */
export declare const de_ListWorkspaceCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<ListWorkspaceCommandOutput>;
/**
 * deserializeAws_restJson1MoveContextCommand
 */
export declare const de_MoveContextCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<MoveContextCommandOutput>;
/**
 * deserializeAws_restJson1PauseExperimentCommand
 */
export declare const de_PauseExperimentCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<PauseExperimentCommandOutput>;
/**
 * deserializeAws_restJson1PublishCommand
 */
export declare const de_PublishCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<PublishCommandOutput>;
/**
 * deserializeAws_restJson1RampExperimentCommand
 */
export declare const de_RampExperimentCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<RampExperimentCommandOutput>;
/**
 * deserializeAws_restJson1RemoveMembersFromGroupCommand
 */
export declare const de_RemoveMembersFromGroupCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<RemoveMembersFromGroupCommandOutput>;
/**
 * deserializeAws_restJson1ResumeExperimentCommand
 */
export declare const de_ResumeExperimentCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<ResumeExperimentCommandOutput>;
/**
 * deserializeAws_restJson1TestCommand
 */
export declare const de_TestCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<TestCommandOutput>;
/**
 * deserializeAws_restJson1UpdateDefaultConfigCommand
 */
export declare const de_UpdateDefaultConfigCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<UpdateDefaultConfigCommandOutput>;
/**
 * deserializeAws_restJson1UpdateDimensionCommand
 */
export declare const de_UpdateDimensionCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<UpdateDimensionCommandOutput>;
/**
 * deserializeAws_restJson1UpdateExperimentGroupCommand
 */
export declare const de_UpdateExperimentGroupCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<UpdateExperimentGroupCommandOutput>;
/**
 * deserializeAws_restJson1UpdateFunctionCommand
 */
export declare const de_UpdateFunctionCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<UpdateFunctionCommandOutput>;
/**
 * deserializeAws_restJson1UpdateOrganisationCommand
 */
export declare const de_UpdateOrganisationCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<UpdateOrganisationCommandOutput>;
/**
 * deserializeAws_restJson1UpdateOverrideCommand
 */
export declare const de_UpdateOverrideCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<UpdateOverrideCommandOutput>;
/**
 * deserializeAws_restJson1UpdateOverridesExperimentCommand
 */
export declare const de_UpdateOverridesExperimentCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<UpdateOverridesExperimentCommandOutput>;
/**
 * deserializeAws_restJson1UpdateTypeTemplatesCommand
 */
export declare const de_UpdateTypeTemplatesCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<UpdateTypeTemplatesCommandOutput>;
/**
 * deserializeAws_restJson1UpdateWebhookCommand
 */
export declare const de_UpdateWebhookCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<UpdateWebhookCommandOutput>;
/**
 * deserializeAws_restJson1UpdateWorkspaceCommand
 */
export declare const de_UpdateWorkspaceCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<UpdateWorkspaceCommandOutput>;
/**
 * deserializeAws_restJson1WeightRecomputeCommand
 */
export declare const de_WeightRecomputeCommand: (output: __HttpResponse, context: __SerdeContext) => Promise<WeightRecomputeCommandOutput>;
