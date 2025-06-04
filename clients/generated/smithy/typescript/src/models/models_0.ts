// smithy-typescript generated code
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
export const VariantType = {
  CONTROL: "CONTROL",
  EXPERIMENTAL: "EXPERIMENTAL",
} as const
/**
 * @public
 */
export type VariantType = typeof VariantType[keyof typeof VariantType]

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
export class InternalServerError extends __BaseException {
  readonly name: "InternalServerError" = "InternalServerError";
  readonly $fault: "server" = "server";
  /**
   * @internal
   */
  constructor(opts: __ExceptionOptionType<InternalServerError, __BaseException>) {
    super({
      name: "InternalServerError",
      $fault: "server",
      ...opts
    });
    Object.setPrototypeOf(this, InternalServerError.prototype);
  }
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
export type ContextIdentifier =
  | ContextIdentifier.ContextMember
  | ContextIdentifier.IdMember
  | ContextIdentifier.$UnknownMember

/**
 * @public
 */
export namespace ContextIdentifier {

  export interface IdMember {
    id: string;
    context?: never;
    $unknown?: never;
  }

  export interface ContextMember {
    id?: never;
    context: Record<string, __DocumentType>;
    $unknown?: never;
  }

  /**
   * @public
   */
  export interface $UnknownMember {
    id?: never;
    context?: never;
    $unknown: [string, any];
  }

  export interface Visitor<T> {
    id: (value: string) => T;
    context: (value: Record<string, __DocumentType>) => T;
    _: (name: string, value: any) => T;
  }

  export const visit = <T>(
    value: ContextIdentifier,
    visitor: Visitor<T>
  ): T => {
    if (value.id !== undefined) return visitor.id(value.id);
    if (value.context !== undefined) return visitor.context(value.context);
    return visitor._(value.$unknown[0], value.$unknown[1]);
  }

}

/**
 * @public
 */
export interface UpdateContextOverrideRequest {
  context: ContextIdentifier | undefined;
  override: Record<string, __DocumentType> | undefined;
  description?: string | undefined;
  change_reason: string | undefined;
}

/**
 * @public
 */
export type ContextAction =
  | ContextAction.DELETEMember
  | ContextAction.MOVEMember
  | ContextAction.PUTMember
  | ContextAction.REPLACEMember
  | ContextAction.$UnknownMember

/**
 * @public
 */
export namespace ContextAction {

  export interface PUTMember {
    PUT: ContextPut;
    REPLACE?: never;
    DELETE?: never;
    MOVE?: never;
    $unknown?: never;
  }

  export interface REPLACEMember {
    PUT?: never;
    REPLACE: UpdateContextOverrideRequest;
    DELETE?: never;
    MOVE?: never;
    $unknown?: never;
  }

  export interface DELETEMember {
    PUT?: never;
    REPLACE?: never;
    DELETE: string;
    MOVE?: never;
    $unknown?: never;
  }

  export interface MOVEMember {
    PUT?: never;
    REPLACE?: never;
    DELETE?: never;
    MOVE: ContextMove;
    $unknown?: never;
  }

  /**
   * @public
   */
  export interface $UnknownMember {
    PUT?: never;
    REPLACE?: never;
    DELETE?: never;
    MOVE?: never;
    $unknown: [string, any];
  }

  export interface Visitor<T> {
    PUT: (value: ContextPut) => T;
    REPLACE: (value: UpdateContextOverrideRequest) => T;
    DELETE: (value: string) => T;
    MOVE: (value: ContextMove) => T;
    _: (name: string, value: any) => T;
  }

  export const visit = <T>(
    value: ContextAction,
    visitor: Visitor<T>
  ): T => {
    if (value.PUT !== undefined) return visitor.PUT(value.PUT);
    if (value.REPLACE !== undefined) return visitor.REPLACE(value.REPLACE);
    if (value.DELETE !== undefined) return visitor.DELETE(value.DELETE);
    if (value.MOVE !== undefined) return visitor.MOVE(value.MOVE);
    return visitor._(value.$unknown[0], value.$unknown[1]);
  }

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
export type ContextActionOut =
  | ContextActionOut.DELETEMember
  | ContextActionOut.MOVEMember
  | ContextActionOut.PUTMember
  | ContextActionOut.REPLACEMember
  | ContextActionOut.$UnknownMember

/**
 * @public
 */
export namespace ContextActionOut {

  export interface PUTMember {
    PUT: ContextPutOut;
    REPLACE?: never;
    DELETE?: never;
    MOVE?: never;
    $unknown?: never;
  }

  export interface REPLACEMember {
    PUT?: never;
    REPLACE: ContextPutOut;
    DELETE?: never;
    MOVE?: never;
    $unknown?: never;
  }

  export interface DELETEMember {
    PUT?: never;
    REPLACE?: never;
    DELETE: string;
    MOVE?: never;
    $unknown?: never;
  }

  export interface MOVEMember {
    PUT?: never;
    REPLACE?: never;
    DELETE?: never;
    MOVE: ContextMoveOut;
    $unknown?: never;
  }

  /**
   * @public
   */
  export interface $UnknownMember {
    PUT?: never;
    REPLACE?: never;
    DELETE?: never;
    MOVE?: never;
    $unknown: [string, any];
  }

  export interface Visitor<T> {
    PUT: (value: ContextPutOut) => T;
    REPLACE: (value: ContextPutOut) => T;
    DELETE: (value: string) => T;
    MOVE: (value: ContextMoveOut) => T;
    _: (name: string, value: any) => T;
  }

  export const visit = <T>(
    value: ContextActionOut,
    visitor: Visitor<T>
  ): T => {
    if (value.PUT !== undefined) return visitor.PUT(value.PUT);
    if (value.REPLACE !== undefined) return visitor.REPLACE(value.REPLACE);
    if (value.DELETE !== undefined) return visitor.DELETE(value.DELETE);
    if (value.MOVE !== undefined) return visitor.MOVE(value.MOVE);
    return visitor._(value.$unknown[0], value.$unknown[1]);
  }

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
export const ExperimentType = {
  DEFAULT: "DEFAULT",
  DELETE_OVERRIDES: "DELETE_OVERRIDES",
} as const
/**
 * @public
 */
export type ExperimentType = typeof ExperimentType[keyof typeof ExperimentType]

/**
 * @public
 * @enum
 */
export const ExperimentStatusType = {
  CONCLUDED: "CONCLUDED",
  CREATED: "CREATED",
  DISCARDED: "DISCARDED",
  INPROGRESS: "INPROGRESS",
  PAUSED: "PAUSED",
} as const
/**
 * @public
 */
export type ExperimentStatusType = typeof ExperimentStatusType[keyof typeof ExperimentStatusType]

/**
 * @public
 */
export interface ExperimentResponse {
  id: string | undefined;
  created_at: Date | undefined;
  created_by: string | undefined;
  last_modified: Date | undefined;
  name: string | undefined;
  experiment_type: ExperimentType | undefined;
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
export const MergeStrategy = {
  MERGE: "MERGE",
  REPLACE: "REPLACE",
} as const
/**
 * @public
 */
export type MergeStrategy = typeof MergeStrategy[keyof typeof MergeStrategy]

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
  context_id?: string | undefined;
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
export class ResourceNotFound extends __BaseException {
  readonly name: "ResourceNotFound" = "ResourceNotFound";
  readonly $fault: "client" = "client";
  /**
   * @internal
   */
  constructor(opts: __ExceptionOptionType<ResourceNotFound, __BaseException>) {
    super({
      name: "ResourceNotFound",
      $fault: "client",
      ...opts
    });
    Object.setPrototypeOf(this, ResourceNotFound.prototype);
  }
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
export const SortBy = {
  Asc: "asc",
  Desc: "desc",
} as const
/**
 * @public
 */
export type SortBy = typeof SortBy[keyof typeof SortBy]

/**
 * @public
 * @enum
 */
export const ContextFilterSortOn = {
  CreatedAt: "created_at",
  LastModifiedAt: "last_modified_at",
  Weight: "weight",
} as const
/**
 * @public
 */
export type ContextFilterSortOn = typeof ContextFilterSortOn[keyof typeof ContextFilterSortOn]

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
  config_tags?: string | undefined;
  request: UpdateContextOverrideRequest | undefined;
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
  experiment_type?: ExperimentType | undefined;
  context: Record<string, __DocumentType> | undefined;
  variants: (Variant)[] | undefined;
  description: string | undefined;
  change_reason: string | undefined;
}

/**
 * @public
 * @enum
 */
export const FunctionTypes = {
  Autocomplete: "AUTOCOMPLETE",
  Validation: "VALIDATION",
} as const
/**
 * @public
 */
export type FunctionTypes = typeof FunctionTypes[keyof typeof FunctionTypes]

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
export const OrgStatus = {
  Active: "Active",
  Inactive: "Inactive",
  PendingKyb: "PendingKyb",
} as const
/**
 * @public
 */
export type OrgStatus = typeof OrgStatus[keyof typeof OrgStatus]

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
export const HttpMethod = {
  DELETE: "DELETE",
  GET: "GET",
  HEAD: "HEAD",
  PATCH: "PATCH",
  POST: "POST",
  PUT: "PUT",
} as const
/**
 * @public
 */
export type HttpMethod = typeof HttpMethod[keyof typeof HttpMethod]

/**
 * @public
 * @enum
 */
export const Version = {
  V1: "V1",
} as const
/**
 * @public
 */
export type Version = typeof Version[keyof typeof Version]

/**
 * @public
 */
export interface CreateWebhookInput {
  workspace_id: string | undefined;
  org_id: string | undefined;
  name: string | undefined;
  description: string | undefined;
  enabled: boolean | undefined;
  url: string | undefined;
  method: HttpMethod | undefined;
  version?: Version | undefined;
  custom_headers?: Record<string, __DocumentType> | undefined;
  events: (string)[] | undefined;
  change_reason: string | undefined;
}

/**
 * @public
 */
export interface WebhookResponse {
  name: string | undefined;
  description: string | undefined;
  enabled: boolean | undefined;
  url: string | undefined;
  method: HttpMethod | undefined;
  version: Version | undefined;
  custom_headers?: Record<string, __DocumentType> | undefined;
  events: (string)[] | undefined;
  max_retries: number | undefined;
  last_triggered_at?: Date | undefined;
  change_reason: string | undefined;
  created_by: string | undefined;
  created_at: Date | undefined;
  last_modified_by: string | undefined;
  last_modified_at: Date | undefined;
}

/**
 * @public
 * @enum
 */
export const WorkspaceStatus = {
  DISABLED: "DISABLED",
  ENABLED: "ENABLED",
} as const
/**
 * @public
 */
export type WorkspaceStatus = typeof WorkspaceStatus[keyof typeof WorkspaceStatus]

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
  config_version?: string | undefined;
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
export class FunctionNotFound extends __BaseException {
  readonly name: "FunctionNotFound" = "FunctionNotFound";
  readonly $fault: "client" = "client";
  /**
   * @internal
   */
  constructor(opts: __ExceptionOptionType<FunctionNotFound, __BaseException>) {
    super({
      name: "FunctionNotFound",
      $fault: "client",
      ...opts
    });
    Object.setPrototypeOf(this, FunctionNotFound.prototype);
  }
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
export class TypeTemplatesNotFound extends __BaseException {
  readonly name: "TypeTemplatesNotFound" = "TypeTemplatesNotFound";
  readonly $fault: "client" = "client";
  /**
   * @internal
   */
  constructor(opts: __ExceptionOptionType<TypeTemplatesNotFound, __BaseException>) {
    super({
      name: "TypeTemplatesNotFound",
      $fault: "client",
      ...opts
    });
    Object.setPrototypeOf(this, TypeTemplatesNotFound.prototype);
  }
}

/**
 * @public
 */
export interface GetDimensionInput {
  workspace_id: string | undefined;
  org_id: string | undefined;
  dimension: string | undefined;
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
export const ExperimentSortOn = {
  CreatedAt: "created_at",
  LastModifiedAt: "last_modified_at",
} as const
/**
 * @public
 */
export type ExperimentSortOn = typeof ExperimentSortOn[keyof typeof ExperimentSortOn]

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
export interface PauseExperimentInput {
  workspace_id: string | undefined;
  org_id: string | undefined;
  id: string | undefined;
  change_reason: string | undefined;
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
export interface ResumeExperimentInput {
  workspace_id: string | undefined;
  org_id: string | undefined;
  id: string | undefined;
  change_reason: string | undefined;
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
export type FunctionExecutionRequest =
  | FunctionExecutionRequest.AutocompleteFunctionRequestMember
  | FunctionExecutionRequest.ValidateFunctionRequestMember
  | FunctionExecutionRequest.$UnknownMember

/**
 * @public
 */
export namespace FunctionExecutionRequest {

  export interface ValidateFunctionRequestMember {
    ValidateFunctionRequest: ValidateFunctionRequest;
    AutocompleteFunctionRequest?: never;
    $unknown?: never;
  }

  export interface AutocompleteFunctionRequestMember {
    ValidateFunctionRequest?: never;
    AutocompleteFunctionRequest: AutocompleteFunctionRequest;
    $unknown?: never;
  }

  /**
   * @public
   */
  export interface $UnknownMember {
    ValidateFunctionRequest?: never;
    AutocompleteFunctionRequest?: never;
    $unknown: [string, any];
  }

  export interface Visitor<T> {
    ValidateFunctionRequest: (value: ValidateFunctionRequest) => T;
    AutocompleteFunctionRequest: (value: AutocompleteFunctionRequest) => T;
    _: (name: string, value: any) => T;
  }

  export const visit = <T>(
    value: FunctionExecutionRequest,
    visitor: Visitor<T>
  ): T => {
    if (value.ValidateFunctionRequest !== undefined) return visitor.ValidateFunctionRequest(value.ValidateFunctionRequest);
    if (value.AutocompleteFunctionRequest !== undefined) return visitor.AutocompleteFunctionRequest(value.AutocompleteFunctionRequest);
    return visitor._(value.$unknown[0], value.$unknown[1]);
  }

}

/**
 * @public
 * @enum
 */
export const Stage = {
  DRAFT: "draft",
  PUBLISHED: "published",
} as const
/**
 * @public
 */
export type Stage = typeof Stage[keyof typeof Stage]

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
export class OrganisationNotFound extends __BaseException {
  readonly name: "OrganisationNotFound" = "OrganisationNotFound";
  readonly $fault: "client" = "client";
  /**
   * @internal
   */
  constructor(opts: __ExceptionOptionType<OrganisationNotFound, __BaseException>) {
    super({
      name: "OrganisationNotFound",
      $fault: "client",
      ...opts
    });
    Object.setPrototypeOf(this, OrganisationNotFound.prototype);
  }
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
export interface GetWebhookInput {
  workspace_id: string | undefined;
  org_id: string | undefined;
  name: string | undefined;
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
export interface ListWebhookInput {
  count?: number | undefined;
  page?: number | undefined;
  workspace_id: string | undefined;
  org_id: string | undefined;
}

/**
 * @public
 */
export interface WebhookListResponse {
  total_pages: number | undefined;
  total_items: number | undefined;
  data: (WebhookResponse)[] | undefined;
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
export interface UpdateWebhookInput {
  workspace_id: string | undefined;
  org_id: string | undefined;
  name: string | undefined;
  description: string | undefined;
  enabled: boolean | undefined;
  url: string | undefined;
  method: HttpMethod | undefined;
  version?: Version | undefined;
  custom_headers?: Record<string, __DocumentType> | undefined;
  events: (string)[] | undefined;
  change_reason: string | undefined;
}

/**
 * @public
 */
export class WebhookNotFound extends __BaseException {
  readonly name: "WebhookNotFound" = "WebhookNotFound";
  readonly $fault: "client" = "client";
  /**
   * @internal
   */
  constructor(opts: __ExceptionOptionType<WebhookNotFound, __BaseException>) {
    super({
      name: "WebhookNotFound",
      $fault: "client",
      ...opts
    });
    Object.setPrototypeOf(this, WebhookNotFound.prototype);
  }
}

/**
 * @public
 */
export interface UpdateWorkspaceRequest {
  org_id: string | undefined;
  workspace_name: string | undefined;
  workspace_admin_email: string | undefined;
  config_version?: string | undefined;
  mandatory_dimensions?: (string)[] | undefined;
  workspace_status?: WorkspaceStatus | undefined;
}

/**
 * @public
 */
export class WorkspaceNotFound extends __BaseException {
  readonly name: "WorkspaceNotFound" = "WorkspaceNotFound";
  readonly $fault: "client" = "client";
  /**
   * @internal
   */
  constructor(opts: __ExceptionOptionType<WorkspaceNotFound, __BaseException>) {
    super({
      name: "WorkspaceNotFound",
      $fault: "client",
      ...opts
    });
    Object.setPrototypeOf(this, WorkspaceNotFound.prototype);
  }
}
