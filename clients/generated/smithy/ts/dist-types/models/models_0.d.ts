import { SuperpositionServiceException as __BaseException } from "./SuperpositionServiceException";
import { ExceptionOptionType as __ExceptionOptionType } from "@smithy/smithy-client";
import { DocumentType as __DocumentType } from "@smithy/types";
/**
 * @public
 */
export interface ApplicableVariantsInput {
    workspace_id: string | undefined;
    org_id: string | undefined;
    context: Record<string, __DocumentType> | undefined;
    toss: number | undefined;
}
/**
 * @public
 * @enum
 */
export declare const VariantType: {
    readonly CONTROL: "CONTROL";
    readonly EXPERIMENTAL: "EXPERIMENTAL";
};
/**
 * @public
 */
export type VariantType = typeof VariantType[keyof typeof VariantType];
/**
 * @public
 */
export interface Variant {
    id: string | undefined;
    variant_type: VariantType | undefined;
    context_id?: string | undefined;
    override_id?: string | undefined;
    overrides: __DocumentType | undefined;
}
/**
 * @public
 */
export interface ApplicableVariantsOutput {
    data: (Variant)[] | undefined;
}
/**
 * @public
 */
export declare class InternalServerError extends __BaseException {
    readonly name: "InternalServerError";
    readonly $fault: "server";
    /**
     * @internal
     */
    constructor(opts: __ExceptionOptionType<InternalServerError, __BaseException>);
}
/**
 * @public
 */
export interface ListAuditLogsInput {
    workspace_id: string | undefined;
    org_id: string | undefined;
    count?: number | undefined;
    page?: number | undefined;
    from_date?: Date | undefined;
    to_date?: Date | undefined;
    /**
     * Comma serparated list of tables.
     * @public
     */
    tables?: string | undefined;
    /**
     * Comma serparated list of actions.
     * @public
     */
    action?: string | undefined;
    username?: string | undefined;
}
/**
 * @public
 */
export interface AuditLogFull {
    table_name?: string | undefined;
    user_name?: string | undefined;
    timestamp?: Date | undefined;
    action?: string | undefined;
    original_data?: __DocumentType | undefined;
    new_data?: __DocumentType | undefined;
    query?: string | undefined;
}
/**
 * @public
 */
export interface ListAuditLogsOutput {
    total_pages?: number | undefined;
    total_items?: number | undefined;
    data?: (AuditLogFull)[] | undefined;
}
/**
 * @public
 */
export interface AutocompleteFunctionRequest {
    name?: string | undefined;
    prefix?: string | undefined;
    environment?: __DocumentType | undefined;
}
/**
 * @public
 */
export interface ContextMove {
    id?: string | undefined;
    context: Record<string, __DocumentType> | undefined;
    description?: string | undefined;
    change_reason: string | undefined;
}
/**
 * @public
 */
export interface ContextPut {
    context: Record<string, __DocumentType> | undefined;
    override: Record<string, __DocumentType> | undefined;
    description?: string | undefined;
    change_reason: string | undefined;
}
/**
 * @public
 */
export type ContextAction = ContextAction.DELETEMember | ContextAction.MOVEMember | ContextAction.PUTMember | ContextAction.REPLACEMember | ContextAction.$UnknownMember;
/**
 * @public
 */
export declare namespace ContextAction {
    interface PUTMember {
        PUT: ContextPut;
        REPLACE?: never;
        DELETE?: never;
        MOVE?: never;
        $unknown?: never;
    }
    interface REPLACEMember {
        PUT?: never;
        REPLACE: ContextPut;
        DELETE?: never;
        MOVE?: never;
        $unknown?: never;
    }
    interface DELETEMember {
        PUT?: never;
        REPLACE?: never;
        DELETE: string;
        MOVE?: never;
        $unknown?: never;
    }
    interface MOVEMember {
        PUT?: never;
        REPLACE?: never;
        DELETE?: never;
        MOVE: ContextMove;
        $unknown?: never;
    }
    /**
     * @public
     */
    interface $UnknownMember {
        PUT?: never;
        REPLACE?: never;
        DELETE?: never;
        MOVE?: never;
        $unknown: [string, any];
    }
    interface Visitor<T> {
        PUT: (value: ContextPut) => T;
        REPLACE: (value: ContextPut) => T;
        DELETE: (value: string) => T;
        MOVE: (value: ContextMove) => T;
        _: (name: string, value: any) => T;
    }
    const visit: <T>(value: ContextAction, visitor: Visitor<T>) => T;
}
/**
 * @public
 */
export interface BulkOperationReq {
    operations?: (ContextAction)[] | undefined;
}
/**
 * @public
 */
export interface BulkOperationInput {
    workspace_id: string | undefined;
    org_id: string | undefined;
    config_tags?: string | undefined;
    bulk_operation: BulkOperationReq | undefined;
}
/**
 * @public
 */
export interface ContextMoveOut {
    context_id?: string | undefined;
    override_id?: string | undefined;
    weight?: string | undefined;
    description?: string | undefined;
    change_reason?: string | undefined;
}
/**
 * @public
 */
export interface ContextPutOut {
    context_id?: string | undefined;
    override_id?: string | undefined;
    weight?: string | undefined;
    description?: string | undefined;
    change_reason?: string | undefined;
}
/**
 * @public
 */
export type ContextActionOut = ContextActionOut.DELETEMember | ContextActionOut.MOVEMember | ContextActionOut.PUTMember | ContextActionOut.REPLACEMember | ContextActionOut.$UnknownMember;
/**
 * @public
 */
export declare namespace ContextActionOut {
    interface PUTMember {
        PUT: ContextPutOut;
        REPLACE?: never;
        DELETE?: never;
        MOVE?: never;
        $unknown?: never;
    }
    interface REPLACEMember {
        PUT?: never;
        REPLACE: ContextPutOut;
        DELETE?: never;
        MOVE?: never;
        $unknown?: never;
    }
    interface DELETEMember {
        PUT?: never;
        REPLACE?: never;
        DELETE: string;
        MOVE?: never;
        $unknown?: never;
    }
    interface MOVEMember {
        PUT?: never;
        REPLACE?: never;
        DELETE?: never;
        MOVE: ContextMoveOut;
        $unknown?: never;
    }
    /**
     * @public
     */
    interface $UnknownMember {
        PUT?: never;
        REPLACE?: never;
        DELETE?: never;
        MOVE?: never;
        $unknown: [string, any];
    }
    interface Visitor<T> {
        PUT: (value: ContextPutOut) => T;
        REPLACE: (value: ContextPutOut) => T;
        DELETE: (value: string) => T;
        MOVE: (value: ContextMoveOut) => T;
        _: (name: string, value: any) => T;
    }
    const visit: <T>(value: ContextActionOut, visitor: Visitor<T>) => T;
}
/**
 * @public
 */
export interface BulkOperationOut {
    output?: (ContextActionOut)[] | undefined;
}
/**
 * @public
 */
export interface BulkOperationOutput {
    bulk_operation_output?: BulkOperationOut | undefined;
}
/**
 * @public
 */
export interface ConcludeExperimentInput {
    workspace_id: string | undefined;
    org_id: string | undefined;
    id: string | undefined;
    chosen_variant: string | undefined;
    description?: string | undefined;
    change_reason: string | undefined;
}
/**
 * @public
 * @enum
 */
export declare const ExperimentStatusType: {
    readonly CONCLUDED: "CONCLUDED";
    readonly CREATED: "CREATED";
    readonly DISCARDED: "DISCARDED";
    readonly INPROGRESS: "INPROGRESS";
};
/**
 * @public
 */
export type ExperimentStatusType = typeof ExperimentStatusType[keyof typeof ExperimentStatusType];
/**
 * @public
 */
export interface ExperimentResponse {
    id: string | undefined;
    created_at: Date | undefined;
    created_by: string | undefined;
    last_modified: Date | undefined;
    name: string | undefined;
    override_keys: (string)[] | undefined;
    status: ExperimentStatusType | undefined;
    traffic_percentage: number | undefined;
    context: Record<string, __DocumentType> | undefined;
    variants: (Variant)[] | undefined;
    last_modified_by: string | undefined;
    chosen_variant?: string | undefined;
    description: string | undefined;
    change_reason: string | undefined;
}
/**
 * @public
 */
export interface GetConfigInput {
    workspace_id: string | undefined;
    org_id: string | undefined;
    prefix?: string | undefined;
    version?: string | undefined;
    /**
     * Map representing the context.
     * Keys correspond to the names of the dimensions.
     * @public
     */
    context?: Record<string, __DocumentType> | undefined;
}
/**
 * @public
 */
export interface ContextPartial {
    id?: string | undefined;
    condition?: Record<string, __DocumentType> | undefined;
    priority?: number | undefined;
    weight?: number | undefined;
    override_with_keys?: (string)[] | undefined;
}
/**
 * @public
 */
export interface GetConfigOutput {
    contexts?: (ContextPartial)[] | undefined;
    overrides?: Record<string, Record<string, __DocumentType>> | undefined;
    default_configs?: Record<string, __DocumentType> | undefined;
    version?: string | undefined;
    last_modified?: Date | undefined;
    audit_id?: string | undefined;
}
/**
 * @public
 */
export interface GetConfigFastInput {
    workspace_id: string | undefined;
    org_id: string | undefined;
}
/**
 * @public
 */
export interface GetConfigFastOutput {
    config?: __DocumentType | undefined;
    version?: string | undefined;
    last_modified?: Date | undefined;
    audit_id?: string | undefined;
}
/**
 * @public
 * @enum
 */
export declare const MergeStrategy: {
    readonly MERGE: "MERGE";
    readonly REPLACE: "REPLACE";
};
/**
 * @public
 */
export type MergeStrategy = typeof MergeStrategy[keyof typeof MergeStrategy];
/**
 * @public
 */
export interface GetResolvedConfigInput {
    workspace_id: string | undefined;
    org_id: string | undefined;
    prefix?: string | undefined;
    version?: string | undefined;
    show_reasoning?: boolean | undefined;
    merge_strategy?: MergeStrategy | undefined;
    /**
     * Map representing the context.
     * Keys correspond to the names of the dimensions.
     * @public
     */
    context?: Record<string, __DocumentType> | undefined;
}
/**
 * @public
 */
export interface GetResolvedConfigOutput {
    config?: __DocumentType | undefined;
    version?: string | undefined;
    last_modified?: Date | undefined;
    audit_id?: string | undefined;
}
/**
 * @public
 */
export interface ListVersionsInput {
    workspace_id: string | undefined;
    org_id: string | undefined;
    count?: number | undefined;
    page?: number | undefined;
}
/**
 * @public
 */
export interface ListVersionsMember {
    id: string | undefined;
    config: __DocumentType | undefined;
    config_hash: string | undefined;
    created_at: Date | undefined;
    description: string | undefined;
    tags?: (string)[] | undefined;
}
/**
 * @public
 */
export interface ListVersionsOutput {
    total_pages: number | undefined;
    total_items: number | undefined;
    data: (ListVersionsMember)[] | undefined;
}
/**
 * @public
 */
export interface ContextActionResponse {
    context_id: string | undefined;
    override_id: string | undefined;
    weight: string | undefined;
    description: string | undefined;
    change_reason: string | undefined;
}
/**
 * @public
 */
export interface CreateContextInput {
    workspace_id: string | undefined;
    org_id: string | undefined;
    context: Record<string, __DocumentType> | undefined;
    config_tags?: string | undefined;
    override: Record<string, __DocumentType> | undefined;
    description?: string | undefined;
    change_reason: string | undefined;
}
/**
 * @public
 */
export interface DeleteContextInput {
    workspace_id: string | undefined;
    org_id: string | undefined;
    id: string | undefined;
    config_tags?: string | undefined;
}
/**
 * @public
 */
export interface DeleteContextOutput {
}
/**
 * @public
 */
export declare class ResourceNotFound extends __BaseException {
    readonly name: "ResourceNotFound";
    readonly $fault: "client";
    /**
     * @internal
     */
    constructor(opts: __ExceptionOptionType<ResourceNotFound, __BaseException>);
}
/**
 * @public
 */
export interface ContextFull {
    id: string | undefined;
    value?: Record<string, __DocumentType> | undefined;
    override?: Record<string, __DocumentType> | undefined;
    override_id?: string | undefined;
    weight?: string | undefined;
    description?: string | undefined;
    change_reason?: string | undefined;
    created_at?: Date | undefined;
    created_by?: string | undefined;
    last_modified_at?: Date | undefined;
    last_modified_by?: string | undefined;
}
/**
 * @public
 */
export interface GetContextInput {
    workspace_id: string | undefined;
    org_id: string | undefined;
    id: string | undefined;
}
/**
 * @public
 */
export interface GetContextFromConditionInput {
    workspace_id: string | undefined;
    org_id: string | undefined;
    context?: __DocumentType | undefined;
}
/**
 * @public
 * @enum
 */
export declare const SortBy: {
    readonly Asc: "asc";
    readonly Desc: "desc";
};
/**
 * @public
 */
export type SortBy = typeof SortBy[keyof typeof SortBy];
/**
 * @public
 * @enum
 */
export declare const ContextFilterSortOn: {
    readonly CreatedAt: "created_at";
    readonly LastModifiedAt: "last_modified_at";
    readonly Weight: "weight";
};
/**
 * @public
 */
export type ContextFilterSortOn = typeof ContextFilterSortOn[keyof typeof ContextFilterSortOn];
/**
 * @public
 */
export interface ListContextsInput {
    workspace_id: string | undefined;
    org_id: string | undefined;
    page?: number | undefined;
    count?: number | undefined;
    prefix?: string | undefined;
    sort_on?: ContextFilterSortOn | undefined;
    sort_by?: SortBy | undefined;
    created_by?: string | undefined;
    last_modified_by?: string | undefined;
    plaintext?: string | undefined;
}
/**
 * @public
 */
export interface ListContextsOutput {
    total_pages?: number | undefined;
    total_items?: number | undefined;
    data?: (ContextFull)[] | undefined;
}
/**
 * @public
 */
export interface MoveContextInput {
    workspace_id: string | undefined;
    org_id: string | undefined;
    id: string | undefined;
    context: Record<string, __DocumentType> | undefined;
    description?: string | undefined;
    change_reason: string | undefined;
}
/**
 * @public
 */
export interface UpdateOverrideInput {
    workspace_id: string | undefined;
    org_id: string | undefined;
    context: Record<string, __DocumentType> | undefined;
    config_tags?: string | undefined;
    override: Record<string, __DocumentType> | undefined;
    description?: string | undefined;
    change_reason: string | undefined;
}
/**
 * @public
 */
export interface WeightRecomputeInput {
    workspace_id: string | undefined;
    org_id: string | undefined;
    config_tags?: string | undefined;
}
/**
 * @public
 */
export interface WeightRecomputeResponse {
    id?: string | undefined;
    condition?: Record<string, __DocumentType> | undefined;
    old_weight?: string | undefined;
    new_weight?: string | undefined;
}
/**
 * @public
 */
export interface WeightRecomputeOutput {
    data?: (WeightRecomputeResponse)[] | undefined;
}
/**
 * @public
 */
export interface CreateDefaultConfigInput {
    key: string | undefined;
    value: __DocumentType | undefined;
    schema: __DocumentType | undefined;
    description: string | undefined;
    change_reason: string | undefined;
    /**
     * Optional
     * @public
     */
    function_name?: string | undefined;
    workspace_id: string | undefined;
    org_id: string | undefined;
}
/**
 * @public
 */
export interface DefaultConfigFull {
    key: string | undefined;
    value: __DocumentType | undefined;
    schema: __DocumentType | undefined;
    description: string | undefined;
    change_reason: string | undefined;
    /**
     * Optional
     * @public
     */
    function_name?: string | undefined;
    created_at: Date | undefined;
    created_by: string | undefined;
    last_modified_at: Date | undefined;
    last_modified_by: string | undefined;
}
/**
 * @public
 */
export interface CreateDimensionInput {
    workspace_id: string | undefined;
    org_id: string | undefined;
    dimension: string | undefined;
    position: number | undefined;
    schema: __DocumentType | undefined;
    function_name?: string | undefined;
    dependencies?: (string)[] | undefined;
    description: string | undefined;
    change_reason: string | undefined;
}
/**
 * @public
 */
export interface DimensionExt {
    dimension: string | undefined;
    position: number | undefined;
    schema: __DocumentType | undefined;
    function_name?: string | undefined;
    description: string | undefined;
    change_reason: string | undefined;
    last_modified_at: Date | undefined;
    last_modified_by: string | undefined;
    created_at: Date | undefined;
    created_by: string | undefined;
    dependencies: (string)[] | undefined;
    dependents: (string)[] | undefined;
    dependency_graph: Record<string, __DocumentType> | undefined;
    mandatory?: boolean | undefined;
}
/**
 * @public
 */
export interface CreateExperimentRequest {
    workspace_id: string | undefined;
    org_id: string | undefined;
    name: string | undefined;
    context: Record<string, __DocumentType> | undefined;
    variants: (Variant)[] | undefined;
    description: string | undefined;
    change_reason: string | undefined;
}
/**
 * @public
 * @enum
 */
export declare const FunctionTypes: {
    readonly Autocomplete: "AUTOCOMPLETE";
    readonly Validation: "VALIDATION";
};
/**
 * @public
 */
export type FunctionTypes = typeof FunctionTypes[keyof typeof FunctionTypes];
/**
 * @public
 */
export interface CreateFunctionRequest {
    workspace_id: string | undefined;
    org_id: string | undefined;
    function_name: string | undefined;
    description: string | undefined;
    change_reason: string | undefined;
    function: string | undefined;
    runtime_version: string | undefined;
    function_type: FunctionTypes | undefined;
}
/**
 * @public
 */
export interface FunctionResponse {
    function_name: string | undefined;
    published_code?: string | undefined;
    draft_code: string | undefined;
    published_runtime_version?: string | undefined;
    draft_runtime_version: string | undefined;
    published_at?: Date | undefined;
    draft_edited_at: Date | undefined;
    published_by?: string | undefined;
    draft_edited_by: string | undefined;
    last_modified_at: Date | undefined;
    last_modified_by: string | undefined;
    change_reason: string | undefined;
    description: string | undefined;
    function_type: FunctionTypes | undefined;
}
/**
 * @public
 */
export interface CreateOrganisationRequest {
    country_code?: string | undefined;
    contact_email?: string | undefined;
    contact_phone?: string | undefined;
    admin_email: string | undefined;
    sector?: string | undefined;
    name: string | undefined;
}
/**
 * @public
 * @enum
 */
export declare const OrgStatus: {
    readonly Active: "Active";
    readonly Inactive: "Inactive";
    readonly PendingKyb: "PendingKyb";
};
/**
 * @public
 */
export type OrgStatus = typeof OrgStatus[keyof typeof OrgStatus];
/**
 * @public
 */
export interface OrganisationResponse {
    id: string | undefined;
    name: string | undefined;
    country_code?: string | undefined;
    contact_email?: string | undefined;
    contact_phone?: string | undefined;
    created_by: string | undefined;
    admin_email: string | undefined;
    status: OrgStatus | undefined;
    sector?: string | undefined;
    created_at: Date | undefined;
    updated_at: Date | undefined;
    updated_by: string | undefined;
}
/**
 * @public
 */
export interface CreateTypeTemplatesRequest {
    workspace_id: string | undefined;
    org_id: string | undefined;
    type_name: string | undefined;
    type_schema: __DocumentType | undefined;
    description: string | undefined;
    change_reason: string | undefined;
}
/**
 * @public
 */
export interface TypeTemplatesResponse {
    type_name: string | undefined;
    type_schema: __DocumentType | undefined;
    description: string | undefined;
    change_reason: string | undefined;
    created_by: string | undefined;
    created_at: Date | undefined;
    last_modified_at: Date | undefined;
    last_modified_by: string | undefined;
}
/**
 * @public
 * @enum
 */
export declare const WorkspaceStatus: {
    readonly DISABLED: "DISABLED";
    readonly ENABLED: "ENABLED";
};
/**
 * @public
 */
export type WorkspaceStatus = typeof WorkspaceStatus[keyof typeof WorkspaceStatus];
/**
 * @public
 */
export interface CreateWorkspaceRequest {
    org_id: string | undefined;
    workspace_admin_email: string | undefined;
    workspace_name: string | undefined;
    workspace_status?: WorkspaceStatus | undefined;
    workspace_strict_mode: boolean | undefined;
}
/**
 * @public
 */
export interface WorkspaceResponse {
    workspace_name: string | undefined;
    organisation_id: string | undefined;
    organisation_name: string | undefined;
    workspace_schema_name: string | undefined;
    workspace_status: WorkspaceStatus | undefined;
    workspace_admin_email: string | undefined;
    created_by: string | undefined;
    last_modified_by: string | undefined;
    last_modified_at: Date | undefined;
    created_at: Date | undefined;
    mandatory_dimensions?: (string)[] | undefined;
    workspace_strict_mode: boolean | undefined;
}
/**
 * @public
 */
export interface DeleteDefaultConfigInput {
    workspace_id: string | undefined;
    org_id: string | undefined;
    key: string | undefined;
}
/**
 * @public
 */
export interface DeleteDefaultConfigOutput {
}
/**
 * @public
 */
export interface ListDefaultConfigsInput {
    count?: number | undefined;
    page?: number | undefined;
    workspace_id: string | undefined;
    org_id: string | undefined;
}
/**
 * @public
 */
export interface ListDefaultConfigsOutput {
    total_pages?: number | undefined;
    total_items?: number | undefined;
    data?: (DefaultConfigFull)[] | undefined;
}
/**
 * @public
 */
export interface UpdateDefaultConfigInput {
    workspace_id: string | undefined;
    org_id: string | undefined;
    key: string | undefined;
    change_reason: string | undefined;
    value?: __DocumentType | undefined;
    schema?: __DocumentType | undefined;
    function_name?: string | undefined;
    description?: string | undefined;
}
/**
 * @public
 */
export interface DeleteDimensionInput {
    workspace_id: string | undefined;
    org_id: string | undefined;
    dimension: string | undefined;
}
/**
 * @public
 */
export interface DeleteDimensionOutput {
}
/**
 * @public
 */
export interface DeleteFunctionInput {
    workspace_id: string | undefined;
    org_id: string | undefined;
    function_name: string | undefined;
}
/**
 * @public
 */
export interface DeleteFunctionOutput {
}
/**
 * @public
 */
export declare class FunctionNotFound extends __BaseException {
    readonly name: "FunctionNotFound";
    readonly $fault: "client";
    /**
     * @internal
     */
    constructor(opts: __ExceptionOptionType<FunctionNotFound, __BaseException>);
}
/**
 * @public
 */
export interface DeleteTypeTemplatesInput {
    workspace_id: string | undefined;
    org_id: string | undefined;
    type_name: string | undefined;
}
/**
 * @public
 */
export declare class TypeTemplatesNotFound extends __BaseException {
    readonly name: "TypeTemplatesNotFound";
    readonly $fault: "client";
    /**
     * @internal
     */
    constructor(opts: __ExceptionOptionType<TypeTemplatesNotFound, __BaseException>);
}
/**
 * @public
 */
export interface ListDimensionsInput {
    count?: number | undefined;
    page?: number | undefined;
    workspace_id: string | undefined;
    org_id: string | undefined;
}
/**
 * @public
 */
export interface ListDimensionsOutput {
    total_pages?: number | undefined;
    total_items?: number | undefined;
    data?: (DimensionExt)[] | undefined;
}
/**
 * @public
 */
export interface UpdateDimensionInput {
    workspace_id: string | undefined;
    org_id: string | undefined;
    dimension: string | undefined;
    schema?: __DocumentType | undefined;
    position?: number | undefined;
    function_name?: string | undefined;
    description?: string | undefined;
    dependencies?: (string)[] | undefined;
    change_reason: string | undefined;
}
/**
 * @public
 */
export interface DiscardExperimentInput {
    workspace_id: string | undefined;
    org_id: string | undefined;
    id: string | undefined;
    change_reason: string | undefined;
}
/**
 * @public
 */
export interface ExperimentListResponse {
    total_pages: number | undefined;
    total_items: number | undefined;
    data: (ExperimentResponse)[] | undefined;
}
/**
 * @public
 */
export interface GetExperimentInput {
    workspace_id: string | undefined;
    org_id: string | undefined;
    id: string | undefined;
}
/**
 * @public
 * @enum
 */
export declare const ExperimentSortOn: {
    readonly CreatedAt: "created_at";
    readonly LastModifiedAt: "last_modified_at";
};
/**
 * @public
 */
export type ExperimentSortOn = typeof ExperimentSortOn[keyof typeof ExperimentSortOn];
/**
 * @public
 */
export interface ListExperimentInput {
    workspace_id: string | undefined;
    org_id: string | undefined;
    page?: number | undefined;
    count?: number | undefined;
    all?: boolean | undefined;
    status?: ExperimentStatusType | undefined;
    from_date?: Date | undefined;
    to_date?: Date | undefined;
    experiment_name?: string | undefined;
    experiment_ids?: string | undefined;
    created_by?: string | undefined;
    context_query?: string | undefined;
    sort_on?: ExperimentSortOn | undefined;
    sort_by?: SortBy | undefined;
}
/**
 * @public
 */
export interface RampExperimentInput {
    workspace_id: string | undefined;
    org_id: string | undefined;
    id: string | undefined;
    change_reason: string | undefined;
    traffic_percentage: number | undefined;
}
/**
 * @public
 */
export interface VariantUpdateRequest {
    id: string | undefined;
    overrides: __DocumentType | undefined;
}
/**
 * @public
 */
export interface UpdateOverrideRequest {
    workspace_id: string | undefined;
    org_id: string | undefined;
    id: string | undefined;
    variant_list: (VariantUpdateRequest)[] | undefined;
    description?: string | undefined;
    change_reason: string | undefined;
}
/**
 * @public
 */
export interface GetFunctionInput {
    workspace_id: string | undefined;
    org_id: string | undefined;
    function_name: string | undefined;
}
/**
 * @public
 */
export interface ListFunctionInput {
    count?: number | undefined;
    page?: number | undefined;
    workspace_id: string | undefined;
    org_id: string | undefined;
}
/**
 * @public
 */
export interface ListFunctionOutput {
    total_pages?: number | undefined;
    total_items?: number | undefined;
    data?: (FunctionResponse)[] | undefined;
}
/**
 * @public
 */
export interface PublishInput {
    workspace_id: string | undefined;
    org_id: string | undefined;
    function_name: string | undefined;
}
/**
 * @public
 */
export interface FunctionExecutionResponse {
    fn_output: __DocumentType | undefined;
    stdout: string | undefined;
    function_type: FunctionTypes | undefined;
}
/**
 * @public
 */
export interface ValidateFunctionRequest {
    key?: string | undefined;
    value?: __DocumentType | undefined;
}
/**
 * @public
 */
export type FunctionExecutionRequest = FunctionExecutionRequest.AutocompleteFunctionRequestMember | FunctionExecutionRequest.ValidateFunctionRequestMember | FunctionExecutionRequest.$UnknownMember;
/**
 * @public
 */
export declare namespace FunctionExecutionRequest {
    interface ValidateFunctionRequestMember {
        ValidateFunctionRequest: ValidateFunctionRequest;
        AutocompleteFunctionRequest?: never;
        $unknown?: never;
    }
    interface AutocompleteFunctionRequestMember {
        ValidateFunctionRequest?: never;
        AutocompleteFunctionRequest: AutocompleteFunctionRequest;
        $unknown?: never;
    }
    /**
     * @public
     */
    interface $UnknownMember {
        ValidateFunctionRequest?: never;
        AutocompleteFunctionRequest?: never;
        $unknown: [string, any];
    }
    interface Visitor<T> {
        ValidateFunctionRequest: (value: ValidateFunctionRequest) => T;
        AutocompleteFunctionRequest: (value: AutocompleteFunctionRequest) => T;
        _: (name: string, value: any) => T;
    }
    const visit: <T>(value: FunctionExecutionRequest, visitor: Visitor<T>) => T;
}
/**
 * @public
 * @enum
 */
export declare const Stage: {
    readonly DRAFT: "draft";
    readonly PUBLISHED: "published";
};
/**
 * @public
 */
export type Stage = typeof Stage[keyof typeof Stage];
/**
 * @public
 */
export interface TestInput {
    workspace_id: string | undefined;
    org_id: string | undefined;
    function_name: string | undefined;
    stage: Stage | undefined;
    request: FunctionExecutionRequest | undefined;
}
/**
 * @public
 */
export interface UpdateFunctionRequest {
    workspace_id: string | undefined;
    org_id: string | undefined;
    function_name: string | undefined;
    description?: string | undefined;
    change_reason: string | undefined;
    function: string | undefined;
    runtime_version: string | undefined;
    function_type?: FunctionTypes | undefined;
}
/**
 * @public
 */
export interface GetOrganisationInput {
    id: string | undefined;
}
/**
 * @public
 */
export declare class OrganisationNotFound extends __BaseException {
    readonly name: "OrganisationNotFound";
    readonly $fault: "client";
    /**
     * @internal
     */
    constructor(opts: __ExceptionOptionType<OrganisationNotFound, __BaseException>);
}
/**
 * @public
 */
export interface GetTypeTemplatesListInput {
    count?: number | undefined;
    page?: number | undefined;
    workspace_id: string | undefined;
    org_id: string | undefined;
}
/**
 * @public
 */
export interface GetTypeTemplatesListOutput {
    total_pages?: number | undefined;
    total_items?: number | undefined;
    data?: (TypeTemplatesResponse)[] | undefined;
}
/**
 * @public
 */
export interface ListOrganisationInput {
    count?: number | undefined;
    page?: number | undefined;
}
/**
 * @public
 */
export interface ListOrganisationOutput {
    total_pages?: number | undefined;
    total_items?: number | undefined;
    data?: (OrganisationResponse)[] | undefined;
}
/**
 * @public
 */
export interface ListWorkspaceInput {
    count?: number | undefined;
    page?: number | undefined;
    org_id: string | undefined;
}
/**
 * @public
 */
export interface WorkspaceListResponse {
    total_pages: number | undefined;
    total_items: number | undefined;
    data: (WorkspaceResponse)[] | undefined;
}
/**
 * @public
 */
export interface UpdateOrganisationRequest {
    country_code?: string | undefined;
    contact_email?: string | undefined;
    contact_phone?: string | undefined;
    admin_email?: string | undefined;
    sector?: string | undefined;
    id: string | undefined;
    status?: OrgStatus | undefined;
}
/**
 * @public
 */
export interface UpdateTypeTemplatesRequest {
    workspace_id: string | undefined;
    org_id: string | undefined;
    type_name: string | undefined;
    type_schema: __DocumentType | undefined;
    description?: string | undefined;
    change_reason: string | undefined;
}
/**
 * @public
 */
export interface UpdateWorkspaceRequest {
    org_id: string | undefined;
    workspace_name: string | undefined;
    workspace_admin_email: string | undefined;
    mandatory_dimensions?: (string)[] | undefined;
    workspace_status?: WorkspaceStatus | undefined;
}
/**
 * @public
 */
export declare class WorkspaceNotFound extends __BaseException {
    readonly name: "WorkspaceNotFound";
    readonly $fault: "client";
    /**
     * @internal
     */
    constructor(opts: __ExceptionOptionType<WorkspaceNotFound, __BaseException>);
}
