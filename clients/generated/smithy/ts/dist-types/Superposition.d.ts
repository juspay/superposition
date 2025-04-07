import { SuperpositionClient } from "./SuperpositionClient";
import { ApplicableVariantsCommandInput, ApplicableVariantsCommandOutput } from "./commands/ApplicableVariantsCommand";
import { BulkOperationCommandInput, BulkOperationCommandOutput } from "./commands/BulkOperationCommand";
import { ConcludeExperimentCommandInput, ConcludeExperimentCommandOutput } from "./commands/ConcludeExperimentCommand";
import { CreateContextCommandInput, CreateContextCommandOutput } from "./commands/CreateContextCommand";
import { CreateDefaultConfigCommandInput, CreateDefaultConfigCommandOutput } from "./commands/CreateDefaultConfigCommand";
import { CreateDimensionCommandInput, CreateDimensionCommandOutput } from "./commands/CreateDimensionCommand";
import { CreateExperimentCommandInput, CreateExperimentCommandOutput } from "./commands/CreateExperimentCommand";
import { CreateFunctionCommandInput, CreateFunctionCommandOutput } from "./commands/CreateFunctionCommand";
import { CreateOrganisationCommandInput, CreateOrganisationCommandOutput } from "./commands/CreateOrganisationCommand";
import { CreateTypeTemplatesCommandInput, CreateTypeTemplatesCommandOutput } from "./commands/CreateTypeTemplatesCommand";
import { CreateWorkspaceCommandInput, CreateWorkspaceCommandOutput } from "./commands/CreateWorkspaceCommand";
import { DeleteContextCommandInput, DeleteContextCommandOutput } from "./commands/DeleteContextCommand";
import { DeleteDefaultConfigCommandInput, DeleteDefaultConfigCommandOutput } from "./commands/DeleteDefaultConfigCommand";
import { DeleteDimensionCommandInput, DeleteDimensionCommandOutput } from "./commands/DeleteDimensionCommand";
import { DeleteFunctionCommandInput, DeleteFunctionCommandOutput } from "./commands/DeleteFunctionCommand";
import { DeleteTypeTemplatesCommandInput, DeleteTypeTemplatesCommandOutput } from "./commands/DeleteTypeTemplatesCommand";
import { DiscardExperimentCommandInput, DiscardExperimentCommandOutput } from "./commands/DiscardExperimentCommand";
import { GetConfigCommandInput, GetConfigCommandOutput } from "./commands/GetConfigCommand";
import { GetConfigFastCommandInput, GetConfigFastCommandOutput } from "./commands/GetConfigFastCommand";
import { GetContextCommandInput, GetContextCommandOutput } from "./commands/GetContextCommand";
import { GetContextFromConditionCommandInput, GetContextFromConditionCommandOutput } from "./commands/GetContextFromConditionCommand";
import { GetExperimentCommandInput, GetExperimentCommandOutput } from "./commands/GetExperimentCommand";
import { GetFunctionCommandInput, GetFunctionCommandOutput } from "./commands/GetFunctionCommand";
import { GetOrganisationCommandInput, GetOrganisationCommandOutput } from "./commands/GetOrganisationCommand";
import { GetResolvedConfigCommandInput, GetResolvedConfigCommandOutput } from "./commands/GetResolvedConfigCommand";
import { GetTypeTemplatesListCommandInput, GetTypeTemplatesListCommandOutput } from "./commands/GetTypeTemplatesListCommand";
import { ListAuditLogsCommandInput, ListAuditLogsCommandOutput } from "./commands/ListAuditLogsCommand";
import { ListContextsCommandInput, ListContextsCommandOutput } from "./commands/ListContextsCommand";
import { ListDefaultConfigsCommandInput, ListDefaultConfigsCommandOutput } from "./commands/ListDefaultConfigsCommand";
import { ListDimensionsCommandInput, ListDimensionsCommandOutput } from "./commands/ListDimensionsCommand";
import { ListExperimentCommandInput, ListExperimentCommandOutput } from "./commands/ListExperimentCommand";
import { ListFunctionCommandInput, ListFunctionCommandOutput } from "./commands/ListFunctionCommand";
import { ListOrganisationCommandInput, ListOrganisationCommandOutput } from "./commands/ListOrganisationCommand";
import { ListVersionsCommandInput, ListVersionsCommandOutput } from "./commands/ListVersionsCommand";
import { ListWorkspaceCommandInput, ListWorkspaceCommandOutput } from "./commands/ListWorkspaceCommand";
import { MoveContextCommandInput, MoveContextCommandOutput } from "./commands/MoveContextCommand";
import { PublishCommandInput, PublishCommandOutput } from "./commands/PublishCommand";
import { RampExperimentCommandInput, RampExperimentCommandOutput } from "./commands/RampExperimentCommand";
import { TestCommandInput, TestCommandOutput } from "./commands/TestCommand";
import { UpdateDefaultConfigCommandInput, UpdateDefaultConfigCommandOutput } from "./commands/UpdateDefaultConfigCommand";
import { UpdateDimensionCommandInput, UpdateDimensionCommandOutput } from "./commands/UpdateDimensionCommand";
import { UpdateFunctionCommandInput, UpdateFunctionCommandOutput } from "./commands/UpdateFunctionCommand";
import { UpdateOrganisationCommandInput, UpdateOrganisationCommandOutput } from "./commands/UpdateOrganisationCommand";
import { UpdateOverrideCommandInput, UpdateOverrideCommandOutput } from "./commands/UpdateOverrideCommand";
import { UpdateOverridesExperimentCommandInput, UpdateOverridesExperimentCommandOutput } from "./commands/UpdateOverridesExperimentCommand";
import { UpdateTypeTemplatesCommandInput, UpdateTypeTemplatesCommandOutput } from "./commands/UpdateTypeTemplatesCommand";
import { UpdateWorkspaceCommandInput, UpdateWorkspaceCommandOutput } from "./commands/UpdateWorkspaceCommand";
import { WeightRecomputeCommandInput, WeightRecomputeCommandOutput } from "./commands/WeightRecomputeCommand";
import { HttpHandlerOptions as __HttpHandlerOptions } from "@smithy/types";
export interface Superposition {
    /**
     * @see {@link ApplicableVariantsCommand}
     */
    applicableVariants(args: ApplicableVariantsCommandInput, options?: __HttpHandlerOptions): Promise<ApplicableVariantsCommandOutput>;
    applicableVariants(args: ApplicableVariantsCommandInput, cb: (err: any, data?: ApplicableVariantsCommandOutput) => void): void;
    applicableVariants(args: ApplicableVariantsCommandInput, options: __HttpHandlerOptions, cb: (err: any, data?: ApplicableVariantsCommandOutput) => void): void;
    /**
     * @see {@link BulkOperationCommand}
     */
    bulkOperation(args: BulkOperationCommandInput, options?: __HttpHandlerOptions): Promise<BulkOperationCommandOutput>;
    bulkOperation(args: BulkOperationCommandInput, cb: (err: any, data?: BulkOperationCommandOutput) => void): void;
    bulkOperation(args: BulkOperationCommandInput, options: __HttpHandlerOptions, cb: (err: any, data?: BulkOperationCommandOutput) => void): void;
    /**
     * @see {@link ConcludeExperimentCommand}
     */
    concludeExperiment(args: ConcludeExperimentCommandInput, options?: __HttpHandlerOptions): Promise<ConcludeExperimentCommandOutput>;
    concludeExperiment(args: ConcludeExperimentCommandInput, cb: (err: any, data?: ConcludeExperimentCommandOutput) => void): void;
    concludeExperiment(args: ConcludeExperimentCommandInput, options: __HttpHandlerOptions, cb: (err: any, data?: ConcludeExperimentCommandOutput) => void): void;
    /**
     * @see {@link CreateContextCommand}
     */
    createContext(args: CreateContextCommandInput, options?: __HttpHandlerOptions): Promise<CreateContextCommandOutput>;
    createContext(args: CreateContextCommandInput, cb: (err: any, data?: CreateContextCommandOutput) => void): void;
    createContext(args: CreateContextCommandInput, options: __HttpHandlerOptions, cb: (err: any, data?: CreateContextCommandOutput) => void): void;
    /**
     * @see {@link CreateDefaultConfigCommand}
     */
    createDefaultConfig(args: CreateDefaultConfigCommandInput, options?: __HttpHandlerOptions): Promise<CreateDefaultConfigCommandOutput>;
    createDefaultConfig(args: CreateDefaultConfigCommandInput, cb: (err: any, data?: CreateDefaultConfigCommandOutput) => void): void;
    createDefaultConfig(args: CreateDefaultConfigCommandInput, options: __HttpHandlerOptions, cb: (err: any, data?: CreateDefaultConfigCommandOutput) => void): void;
    /**
     * @see {@link CreateDimensionCommand}
     */
    createDimension(args: CreateDimensionCommandInput, options?: __HttpHandlerOptions): Promise<CreateDimensionCommandOutput>;
    createDimension(args: CreateDimensionCommandInput, cb: (err: any, data?: CreateDimensionCommandOutput) => void): void;
    createDimension(args: CreateDimensionCommandInput, options: __HttpHandlerOptions, cb: (err: any, data?: CreateDimensionCommandOutput) => void): void;
    /**
     * @see {@link CreateExperimentCommand}
     */
    createExperiment(args: CreateExperimentCommandInput, options?: __HttpHandlerOptions): Promise<CreateExperimentCommandOutput>;
    createExperiment(args: CreateExperimentCommandInput, cb: (err: any, data?: CreateExperimentCommandOutput) => void): void;
    createExperiment(args: CreateExperimentCommandInput, options: __HttpHandlerOptions, cb: (err: any, data?: CreateExperimentCommandOutput) => void): void;
    /**
     * @see {@link CreateFunctionCommand}
     */
    createFunction(args: CreateFunctionCommandInput, options?: __HttpHandlerOptions): Promise<CreateFunctionCommandOutput>;
    createFunction(args: CreateFunctionCommandInput, cb: (err: any, data?: CreateFunctionCommandOutput) => void): void;
    createFunction(args: CreateFunctionCommandInput, options: __HttpHandlerOptions, cb: (err: any, data?: CreateFunctionCommandOutput) => void): void;
    /**
     * @see {@link CreateOrganisationCommand}
     */
    createOrganisation(args: CreateOrganisationCommandInput, options?: __HttpHandlerOptions): Promise<CreateOrganisationCommandOutput>;
    createOrganisation(args: CreateOrganisationCommandInput, cb: (err: any, data?: CreateOrganisationCommandOutput) => void): void;
    createOrganisation(args: CreateOrganisationCommandInput, options: __HttpHandlerOptions, cb: (err: any, data?: CreateOrganisationCommandOutput) => void): void;
    /**
     * @see {@link CreateTypeTemplatesCommand}
     */
    createTypeTemplates(args: CreateTypeTemplatesCommandInput, options?: __HttpHandlerOptions): Promise<CreateTypeTemplatesCommandOutput>;
    createTypeTemplates(args: CreateTypeTemplatesCommandInput, cb: (err: any, data?: CreateTypeTemplatesCommandOutput) => void): void;
    createTypeTemplates(args: CreateTypeTemplatesCommandInput, options: __HttpHandlerOptions, cb: (err: any, data?: CreateTypeTemplatesCommandOutput) => void): void;
    /**
     * @see {@link CreateWorkspaceCommand}
     */
    createWorkspace(args: CreateWorkspaceCommandInput, options?: __HttpHandlerOptions): Promise<CreateWorkspaceCommandOutput>;
    createWorkspace(args: CreateWorkspaceCommandInput, cb: (err: any, data?: CreateWorkspaceCommandOutput) => void): void;
    createWorkspace(args: CreateWorkspaceCommandInput, options: __HttpHandlerOptions, cb: (err: any, data?: CreateWorkspaceCommandOutput) => void): void;
    /**
     * @see {@link DeleteContextCommand}
     */
    deleteContext(args: DeleteContextCommandInput, options?: __HttpHandlerOptions): Promise<DeleteContextCommandOutput>;
    deleteContext(args: DeleteContextCommandInput, cb: (err: any, data?: DeleteContextCommandOutput) => void): void;
    deleteContext(args: DeleteContextCommandInput, options: __HttpHandlerOptions, cb: (err: any, data?: DeleteContextCommandOutput) => void): void;
    /**
     * @see {@link DeleteDefaultConfigCommand}
     */
    deleteDefaultConfig(args: DeleteDefaultConfigCommandInput, options?: __HttpHandlerOptions): Promise<DeleteDefaultConfigCommandOutput>;
    deleteDefaultConfig(args: DeleteDefaultConfigCommandInput, cb: (err: any, data?: DeleteDefaultConfigCommandOutput) => void): void;
    deleteDefaultConfig(args: DeleteDefaultConfigCommandInput, options: __HttpHandlerOptions, cb: (err: any, data?: DeleteDefaultConfigCommandOutput) => void): void;
    /**
     * @see {@link DeleteDimensionCommand}
     */
    deleteDimension(args: DeleteDimensionCommandInput, options?: __HttpHandlerOptions): Promise<DeleteDimensionCommandOutput>;
    deleteDimension(args: DeleteDimensionCommandInput, cb: (err: any, data?: DeleteDimensionCommandOutput) => void): void;
    deleteDimension(args: DeleteDimensionCommandInput, options: __HttpHandlerOptions, cb: (err: any, data?: DeleteDimensionCommandOutput) => void): void;
    /**
     * @see {@link DeleteFunctionCommand}
     */
    deleteFunction(args: DeleteFunctionCommandInput, options?: __HttpHandlerOptions): Promise<DeleteFunctionCommandOutput>;
    deleteFunction(args: DeleteFunctionCommandInput, cb: (err: any, data?: DeleteFunctionCommandOutput) => void): void;
    deleteFunction(args: DeleteFunctionCommandInput, options: __HttpHandlerOptions, cb: (err: any, data?: DeleteFunctionCommandOutput) => void): void;
    /**
     * @see {@link DeleteTypeTemplatesCommand}
     */
    deleteTypeTemplates(args: DeleteTypeTemplatesCommandInput, options?: __HttpHandlerOptions): Promise<DeleteTypeTemplatesCommandOutput>;
    deleteTypeTemplates(args: DeleteTypeTemplatesCommandInput, cb: (err: any, data?: DeleteTypeTemplatesCommandOutput) => void): void;
    deleteTypeTemplates(args: DeleteTypeTemplatesCommandInput, options: __HttpHandlerOptions, cb: (err: any, data?: DeleteTypeTemplatesCommandOutput) => void): void;
    /**
     * @see {@link DiscardExperimentCommand}
     */
    discardExperiment(args: DiscardExperimentCommandInput, options?: __HttpHandlerOptions): Promise<DiscardExperimentCommandOutput>;
    discardExperiment(args: DiscardExperimentCommandInput, cb: (err: any, data?: DiscardExperimentCommandOutput) => void): void;
    discardExperiment(args: DiscardExperimentCommandInput, options: __HttpHandlerOptions, cb: (err: any, data?: DiscardExperimentCommandOutput) => void): void;
    /**
     * @see {@link GetConfigCommand}
     */
    getConfig(args: GetConfigCommandInput, options?: __HttpHandlerOptions): Promise<GetConfigCommandOutput>;
    getConfig(args: GetConfigCommandInput, cb: (err: any, data?: GetConfigCommandOutput) => void): void;
    getConfig(args: GetConfigCommandInput, options: __HttpHandlerOptions, cb: (err: any, data?: GetConfigCommandOutput) => void): void;
    /**
     * @see {@link GetConfigFastCommand}
     */
    getConfigFast(args: GetConfigFastCommandInput, options?: __HttpHandlerOptions): Promise<GetConfigFastCommandOutput>;
    getConfigFast(args: GetConfigFastCommandInput, cb: (err: any, data?: GetConfigFastCommandOutput) => void): void;
    getConfigFast(args: GetConfigFastCommandInput, options: __HttpHandlerOptions, cb: (err: any, data?: GetConfigFastCommandOutput) => void): void;
    /**
     * @see {@link GetContextCommand}
     */
    getContext(args: GetContextCommandInput, options?: __HttpHandlerOptions): Promise<GetContextCommandOutput>;
    getContext(args: GetContextCommandInput, cb: (err: any, data?: GetContextCommandOutput) => void): void;
    getContext(args: GetContextCommandInput, options: __HttpHandlerOptions, cb: (err: any, data?: GetContextCommandOutput) => void): void;
    /**
     * @see {@link GetContextFromConditionCommand}
     */
    getContextFromCondition(args: GetContextFromConditionCommandInput, options?: __HttpHandlerOptions): Promise<GetContextFromConditionCommandOutput>;
    getContextFromCondition(args: GetContextFromConditionCommandInput, cb: (err: any, data?: GetContextFromConditionCommandOutput) => void): void;
    getContextFromCondition(args: GetContextFromConditionCommandInput, options: __HttpHandlerOptions, cb: (err: any, data?: GetContextFromConditionCommandOutput) => void): void;
    /**
     * @see {@link GetExperimentCommand}
     */
    getExperiment(args: GetExperimentCommandInput, options?: __HttpHandlerOptions): Promise<GetExperimentCommandOutput>;
    getExperiment(args: GetExperimentCommandInput, cb: (err: any, data?: GetExperimentCommandOutput) => void): void;
    getExperiment(args: GetExperimentCommandInput, options: __HttpHandlerOptions, cb: (err: any, data?: GetExperimentCommandOutput) => void): void;
    /**
     * @see {@link GetFunctionCommand}
     */
    getFunction(args: GetFunctionCommandInput, options?: __HttpHandlerOptions): Promise<GetFunctionCommandOutput>;
    getFunction(args: GetFunctionCommandInput, cb: (err: any, data?: GetFunctionCommandOutput) => void): void;
    getFunction(args: GetFunctionCommandInput, options: __HttpHandlerOptions, cb: (err: any, data?: GetFunctionCommandOutput) => void): void;
    /**
     * @see {@link GetOrganisationCommand}
     */
    getOrganisation(args: GetOrganisationCommandInput, options?: __HttpHandlerOptions): Promise<GetOrganisationCommandOutput>;
    getOrganisation(args: GetOrganisationCommandInput, cb: (err: any, data?: GetOrganisationCommandOutput) => void): void;
    getOrganisation(args: GetOrganisationCommandInput, options: __HttpHandlerOptions, cb: (err: any, data?: GetOrganisationCommandOutput) => void): void;
    /**
     * @see {@link GetResolvedConfigCommand}
     */
    getResolvedConfig(args: GetResolvedConfigCommandInput, options?: __HttpHandlerOptions): Promise<GetResolvedConfigCommandOutput>;
    getResolvedConfig(args: GetResolvedConfigCommandInput, cb: (err: any, data?: GetResolvedConfigCommandOutput) => void): void;
    getResolvedConfig(args: GetResolvedConfigCommandInput, options: __HttpHandlerOptions, cb: (err: any, data?: GetResolvedConfigCommandOutput) => void): void;
    /**
     * @see {@link GetTypeTemplatesListCommand}
     */
    getTypeTemplatesList(args: GetTypeTemplatesListCommandInput, options?: __HttpHandlerOptions): Promise<GetTypeTemplatesListCommandOutput>;
    getTypeTemplatesList(args: GetTypeTemplatesListCommandInput, cb: (err: any, data?: GetTypeTemplatesListCommandOutput) => void): void;
    getTypeTemplatesList(args: GetTypeTemplatesListCommandInput, options: __HttpHandlerOptions, cb: (err: any, data?: GetTypeTemplatesListCommandOutput) => void): void;
    /**
     * @see {@link ListAuditLogsCommand}
     */
    listAuditLogs(args: ListAuditLogsCommandInput, options?: __HttpHandlerOptions): Promise<ListAuditLogsCommandOutput>;
    listAuditLogs(args: ListAuditLogsCommandInput, cb: (err: any, data?: ListAuditLogsCommandOutput) => void): void;
    listAuditLogs(args: ListAuditLogsCommandInput, options: __HttpHandlerOptions, cb: (err: any, data?: ListAuditLogsCommandOutput) => void): void;
    /**
     * @see {@link ListContextsCommand}
     */
    listContexts(args: ListContextsCommandInput, options?: __HttpHandlerOptions): Promise<ListContextsCommandOutput>;
    listContexts(args: ListContextsCommandInput, cb: (err: any, data?: ListContextsCommandOutput) => void): void;
    listContexts(args: ListContextsCommandInput, options: __HttpHandlerOptions, cb: (err: any, data?: ListContextsCommandOutput) => void): void;
    /**
     * @see {@link ListDefaultConfigsCommand}
     */
    listDefaultConfigs(args: ListDefaultConfigsCommandInput, options?: __HttpHandlerOptions): Promise<ListDefaultConfigsCommandOutput>;
    listDefaultConfigs(args: ListDefaultConfigsCommandInput, cb: (err: any, data?: ListDefaultConfigsCommandOutput) => void): void;
    listDefaultConfigs(args: ListDefaultConfigsCommandInput, options: __HttpHandlerOptions, cb: (err: any, data?: ListDefaultConfigsCommandOutput) => void): void;
    /**
     * @see {@link ListDimensionsCommand}
     */
    listDimensions(args: ListDimensionsCommandInput, options?: __HttpHandlerOptions): Promise<ListDimensionsCommandOutput>;
    listDimensions(args: ListDimensionsCommandInput, cb: (err: any, data?: ListDimensionsCommandOutput) => void): void;
    listDimensions(args: ListDimensionsCommandInput, options: __HttpHandlerOptions, cb: (err: any, data?: ListDimensionsCommandOutput) => void): void;
    /**
     * @see {@link ListExperimentCommand}
     */
    listExperiment(args: ListExperimentCommandInput, options?: __HttpHandlerOptions): Promise<ListExperimentCommandOutput>;
    listExperiment(args: ListExperimentCommandInput, cb: (err: any, data?: ListExperimentCommandOutput) => void): void;
    listExperiment(args: ListExperimentCommandInput, options: __HttpHandlerOptions, cb: (err: any, data?: ListExperimentCommandOutput) => void): void;
    /**
     * @see {@link ListFunctionCommand}
     */
    listFunction(args: ListFunctionCommandInput, options?: __HttpHandlerOptions): Promise<ListFunctionCommandOutput>;
    listFunction(args: ListFunctionCommandInput, cb: (err: any, data?: ListFunctionCommandOutput) => void): void;
    listFunction(args: ListFunctionCommandInput, options: __HttpHandlerOptions, cb: (err: any, data?: ListFunctionCommandOutput) => void): void;
    /**
     * @see {@link ListOrganisationCommand}
     */
    listOrganisation(): Promise<ListOrganisationCommandOutput>;
    listOrganisation(args: ListOrganisationCommandInput, options?: __HttpHandlerOptions): Promise<ListOrganisationCommandOutput>;
    listOrganisation(args: ListOrganisationCommandInput, cb: (err: any, data?: ListOrganisationCommandOutput) => void): void;
    listOrganisation(args: ListOrganisationCommandInput, options: __HttpHandlerOptions, cb: (err: any, data?: ListOrganisationCommandOutput) => void): void;
    /**
     * @see {@link ListVersionsCommand}
     */
    listVersions(args: ListVersionsCommandInput, options?: __HttpHandlerOptions): Promise<ListVersionsCommandOutput>;
    listVersions(args: ListVersionsCommandInput, cb: (err: any, data?: ListVersionsCommandOutput) => void): void;
    listVersions(args: ListVersionsCommandInput, options: __HttpHandlerOptions, cb: (err: any, data?: ListVersionsCommandOutput) => void): void;
    /**
     * @see {@link ListWorkspaceCommand}
     */
    listWorkspace(args: ListWorkspaceCommandInput, options?: __HttpHandlerOptions): Promise<ListWorkspaceCommandOutput>;
    listWorkspace(args: ListWorkspaceCommandInput, cb: (err: any, data?: ListWorkspaceCommandOutput) => void): void;
    listWorkspace(args: ListWorkspaceCommandInput, options: __HttpHandlerOptions, cb: (err: any, data?: ListWorkspaceCommandOutput) => void): void;
    /**
     * @see {@link MoveContextCommand}
     */
    moveContext(args: MoveContextCommandInput, options?: __HttpHandlerOptions): Promise<MoveContextCommandOutput>;
    moveContext(args: MoveContextCommandInput, cb: (err: any, data?: MoveContextCommandOutput) => void): void;
    moveContext(args: MoveContextCommandInput, options: __HttpHandlerOptions, cb: (err: any, data?: MoveContextCommandOutput) => void): void;
    /**
     * @see {@link PublishCommand}
     */
    publish(args: PublishCommandInput, options?: __HttpHandlerOptions): Promise<PublishCommandOutput>;
    publish(args: PublishCommandInput, cb: (err: any, data?: PublishCommandOutput) => void): void;
    publish(args: PublishCommandInput, options: __HttpHandlerOptions, cb: (err: any, data?: PublishCommandOutput) => void): void;
    /**
     * @see {@link RampExperimentCommand}
     */
    rampExperiment(args: RampExperimentCommandInput, options?: __HttpHandlerOptions): Promise<RampExperimentCommandOutput>;
    rampExperiment(args: RampExperimentCommandInput, cb: (err: any, data?: RampExperimentCommandOutput) => void): void;
    rampExperiment(args: RampExperimentCommandInput, options: __HttpHandlerOptions, cb: (err: any, data?: RampExperimentCommandOutput) => void): void;
    /**
     * @see {@link TestCommand}
     */
    test(args: TestCommandInput, options?: __HttpHandlerOptions): Promise<TestCommandOutput>;
    test(args: TestCommandInput, cb: (err: any, data?: TestCommandOutput) => void): void;
    test(args: TestCommandInput, options: __HttpHandlerOptions, cb: (err: any, data?: TestCommandOutput) => void): void;
    /**
     * @see {@link UpdateDefaultConfigCommand}
     */
    updateDefaultConfig(args: UpdateDefaultConfigCommandInput, options?: __HttpHandlerOptions): Promise<UpdateDefaultConfigCommandOutput>;
    updateDefaultConfig(args: UpdateDefaultConfigCommandInput, cb: (err: any, data?: UpdateDefaultConfigCommandOutput) => void): void;
    updateDefaultConfig(args: UpdateDefaultConfigCommandInput, options: __HttpHandlerOptions, cb: (err: any, data?: UpdateDefaultConfigCommandOutput) => void): void;
    /**
     * @see {@link UpdateDimensionCommand}
     */
    updateDimension(args: UpdateDimensionCommandInput, options?: __HttpHandlerOptions): Promise<UpdateDimensionCommandOutput>;
    updateDimension(args: UpdateDimensionCommandInput, cb: (err: any, data?: UpdateDimensionCommandOutput) => void): void;
    updateDimension(args: UpdateDimensionCommandInput, options: __HttpHandlerOptions, cb: (err: any, data?: UpdateDimensionCommandOutput) => void): void;
    /**
     * @see {@link UpdateFunctionCommand}
     */
    updateFunction(args: UpdateFunctionCommandInput, options?: __HttpHandlerOptions): Promise<UpdateFunctionCommandOutput>;
    updateFunction(args: UpdateFunctionCommandInput, cb: (err: any, data?: UpdateFunctionCommandOutput) => void): void;
    updateFunction(args: UpdateFunctionCommandInput, options: __HttpHandlerOptions, cb: (err: any, data?: UpdateFunctionCommandOutput) => void): void;
    /**
     * @see {@link UpdateOrganisationCommand}
     */
    updateOrganisation(args: UpdateOrganisationCommandInput, options?: __HttpHandlerOptions): Promise<UpdateOrganisationCommandOutput>;
    updateOrganisation(args: UpdateOrganisationCommandInput, cb: (err: any, data?: UpdateOrganisationCommandOutput) => void): void;
    updateOrganisation(args: UpdateOrganisationCommandInput, options: __HttpHandlerOptions, cb: (err: any, data?: UpdateOrganisationCommandOutput) => void): void;
    /**
     * @see {@link UpdateOverrideCommand}
     */
    updateOverride(args: UpdateOverrideCommandInput, options?: __HttpHandlerOptions): Promise<UpdateOverrideCommandOutput>;
    updateOverride(args: UpdateOverrideCommandInput, cb: (err: any, data?: UpdateOverrideCommandOutput) => void): void;
    updateOverride(args: UpdateOverrideCommandInput, options: __HttpHandlerOptions, cb: (err: any, data?: UpdateOverrideCommandOutput) => void): void;
    /**
     * @see {@link UpdateOverridesExperimentCommand}
     */
    updateOverridesExperiment(args: UpdateOverridesExperimentCommandInput, options?: __HttpHandlerOptions): Promise<UpdateOverridesExperimentCommandOutput>;
    updateOverridesExperiment(args: UpdateOverridesExperimentCommandInput, cb: (err: any, data?: UpdateOverridesExperimentCommandOutput) => void): void;
    updateOverridesExperiment(args: UpdateOverridesExperimentCommandInput, options: __HttpHandlerOptions, cb: (err: any, data?: UpdateOverridesExperimentCommandOutput) => void): void;
    /**
     * @see {@link UpdateTypeTemplatesCommand}
     */
    updateTypeTemplates(args: UpdateTypeTemplatesCommandInput, options?: __HttpHandlerOptions): Promise<UpdateTypeTemplatesCommandOutput>;
    updateTypeTemplates(args: UpdateTypeTemplatesCommandInput, cb: (err: any, data?: UpdateTypeTemplatesCommandOutput) => void): void;
    updateTypeTemplates(args: UpdateTypeTemplatesCommandInput, options: __HttpHandlerOptions, cb: (err: any, data?: UpdateTypeTemplatesCommandOutput) => void): void;
    /**
     * @see {@link UpdateWorkspaceCommand}
     */
    updateWorkspace(args: UpdateWorkspaceCommandInput, options?: __HttpHandlerOptions): Promise<UpdateWorkspaceCommandOutput>;
    updateWorkspace(args: UpdateWorkspaceCommandInput, cb: (err: any, data?: UpdateWorkspaceCommandOutput) => void): void;
    updateWorkspace(args: UpdateWorkspaceCommandInput, options: __HttpHandlerOptions, cb: (err: any, data?: UpdateWorkspaceCommandOutput) => void): void;
    /**
     * @see {@link WeightRecomputeCommand}
     */
    weightRecompute(args: WeightRecomputeCommandInput, options?: __HttpHandlerOptions): Promise<WeightRecomputeCommandOutput>;
    weightRecompute(args: WeightRecomputeCommandInput, cb: (err: any, data?: WeightRecomputeCommandOutput) => void): void;
    weightRecompute(args: WeightRecomputeCommandInput, options: __HttpHandlerOptions, cb: (err: any, data?: WeightRecomputeCommandOutput) => void): void;
}
/**
 * @public
 */
export declare class Superposition extends SuperpositionClient implements Superposition {
}
