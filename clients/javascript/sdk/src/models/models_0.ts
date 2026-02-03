// smithy-typescript generated code
import { SuperpositionServiceException as __BaseException } from "./SuperpositionServiceException";
import { ExceptionOptionType as __ExceptionOptionType } from "@smithy/smithy-client";
import { DocumentType as __DocumentType } from "@smithy/types";

/**
 * @public
 */
export interface Bucket {
  experiment_id: string | undefined;
  variant_id: string | undefined;
}

/**
 * @public
 * @enum
 */
export const GroupType = {
  SYSTEM_GENERATED: "SYSTEM_GENERATED",
  USER_CREATED: "USER_CREATED",
} as const
/**
 * @public
 */
export type GroupType = typeof GroupType[keyof typeof GroupType]

/**
 * Standard response structure for an experiment group.
 * @public
 */
export interface ExperimentGroupResponse {
  id: string | undefined;
  context_hash: string | undefined;
  name: string | undefined;
  description: string | undefined;
  change_reason: string | undefined;
  /**
   * Represents conditional criteria used for context matching. Keys define dimension names and values specify the criteria that must be met.
   * @public
   */
  context: Record<string, __DocumentType> | undefined;

  traffic_percentage: number | undefined;
  member_experiment_ids: (string)[] | undefined;
  created_at: Date | undefined;
  created_by: string | undefined;
  last_modified_at: Date | undefined;
  last_modified_by: string | undefined;
  buckets: (Bucket)[] | undefined;
  group_type: GroupType | undefined;
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
 * Input structure for adding members to an experiment group.
 * @public
 */
export interface ModifyMembersToGroupRequest {
  workspace_id: string | undefined;
  org_id: string | undefined;
  id: string | undefined;
  /**
   * Reason for adding these members.
   * @public
   */
  change_reason: string | undefined;

  /**
   * List of experiment IDs to add/remove to this group.
   * @public
   */
  member_experiment_ids: (string)[] | undefined;
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
export interface ApplicableVariantsInput {
  workspace_id: string | undefined;
  org_id: string | undefined;
  /**
   * Represents conditional criteria used for context matching. Keys define dimension names and values specify the criteria that must be met.
   * @public
   */
  context: Record<string, __DocumentType> | undefined;

  identifier: string | undefined;
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
  /**
   * Configuration overrides that replace default values when context conditions are met. Keys represent configuration keys and values are the override data.
   * @public
   */
  overrides: Record<string, __DocumentType> | undefined;
}

/**
 * @public
 */
export interface ApplicableVariantsOutput {
  data: (Variant)[] | undefined;
}

/**
 * @public
 * @enum
 */
export const AuditAction = {
  DELETE: "DELETE",
  INSERT: "INSERT",
  UPDATE: "UPDATE",
} as const
/**
 * @public
 */
export type AuditAction = typeof AuditAction[keyof typeof AuditAction]

/**
 * @public
 * @enum
 */
export const SortBy = {
  /**
   * Ascending order (A-Z, oldest first)
   */
  ASC: "asc",
  /**
   * Descending order (Z-A, newest first)
   */
  DESC: "desc",
} as const
/**
 * @public
 */
export type SortBy = typeof SortBy[keyof typeof SortBy]

/**
 * @public
 */
export interface ListAuditLogsInput {
  workspace_id: string | undefined;
  org_id: string | undefined;
  /**
   * Number of items to be returned in each page.
   * @public
   */
  count?: number | undefined;

  /**
   * Page number to retrieve, starting from 1.
   * @public
   */
  page?: number | undefined;

  /**
   * If true, returns all requested items, ignoring pagination parameters page and count.
   * @public
   */
  all?: boolean | undefined;

  from_date?: Date | undefined;
  to_date?: Date | undefined;
  tables?: (string)[] | undefined;
  action?: (AuditAction)[] | undefined;
  username?: string | undefined;
  /**
   * Sort order enumeration for list operations.
   * @public
   */
  sort_by?: SortBy | undefined;
}

/**
 * @public
 */
export interface AuditLogFull {
  id: string | undefined;
  table_name: string | undefined;
  user_name: string | undefined;
  timestamp: Date | undefined;
  action: AuditAction | undefined;
  original_data?: __DocumentType | undefined;
  new_data?: __DocumentType | undefined;
  query: string | undefined;
}

/**
 * @public
 */
export interface ListAuditLogsOutput {
  total_pages: number | undefined;
  total_items: number | undefined;
  data: (AuditLogFull)[] | undefined;
}

/**
 * @public
 */
export interface ContextMove {
  /**
   * Represents conditional criteria used for context matching. Keys define dimension names and values specify the criteria that must be met.
   * @public
   */
  context: Record<string, __DocumentType> | undefined;

  description?: string | undefined;
  change_reason: string | undefined;
}

/**
 * @public
 */
export interface ContextMoveBulkRequest {
  id: string | undefined;
  request: ContextMove | undefined;
}

/**
 * @public
 */
export interface ContextPut {
  /**
   * Represents conditional criteria used for context matching. Keys define dimension names and values specify the criteria that must be met.
   * @public
   */
  context: Record<string, __DocumentType> | undefined;

  /**
   * Configuration overrides that replace default values when context conditions are met. Keys represent configuration keys and values are the override data.
   * @public
   */
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

  /**
   * Represents conditional criteria used for context matching. Keys define dimension names and values specify the criteria that must be met.
   * @public
   */
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
  /**
   * Configuration overrides that replace default values when context conditions are met. Keys represent configuration keys and values are the override data.
   * @public
   */
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
    MOVE: ContextMoveBulkRequest;
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
    MOVE: (value: ContextMoveBulkRequest) => T;
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
export interface BulkOperationInput {
  workspace_id: string | undefined;
  org_id: string | undefined;
  config_tags?: string | undefined;
  operations: (ContextAction)[] | undefined;
}

/**
 * @public
 */
export interface ContextResponse {
  id: string | undefined;
  /**
   * Represents conditional criteria used for context matching. Keys define dimension names and values specify the criteria that must be met.
   * @public
   */
  value: Record<string, __DocumentType> | undefined;

  /**
   * Configuration overrides that replace default values when context conditions are met. Keys represent configuration keys and values are the override data.
   * @public
   */
  override: Record<string, __DocumentType> | undefined;

  override_id: string | undefined;
  /**
   * Priority weight used to determine the order of context evaluation. Higher weights take precedence during configuration resolution.
   * @public
   */
  weight: string | undefined;

  description: string | undefined;
  change_reason: string | undefined;
  created_at: Date | undefined;
  created_by: string | undefined;
  last_modified_at: Date | undefined;
  last_modified_by: string | undefined;
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
    PUT: ContextResponse;
    REPLACE?: never;
    DELETE?: never;
    MOVE?: never;
    $unknown?: never;
  }

  export interface REPLACEMember {
    PUT?: never;
    REPLACE: ContextResponse;
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
    MOVE: ContextResponse;
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
    PUT: (value: ContextResponse) => T;
    REPLACE: (value: ContextResponse) => T;
    DELETE: (value: string) => T;
    MOVE: (value: ContextResponse) => T;
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
export interface BulkOperationOutput {
  output: (ContextActionOut)[] | undefined;
}

/**
 * @public
 */
export interface ChangeReasonValidationFunctionRequest {
  change_reason: string | undefined;
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
  /**
   * Represents conditional criteria used for context matching. Keys define dimension names and values specify the criteria that must be met.
   * @public
   */
  context: Record<string, __DocumentType> | undefined;

  variants: (Variant)[] | undefined;
  last_modified_by: string | undefined;
  chosen_variant?: string | undefined;
  description: string | undefined;
  change_reason: string | undefined;
  started_at?: Date | undefined;
  started_by?: string | undefined;
  metrics_url?: string | undefined;
  metrics?: __DocumentType | undefined;
  experiment_group_id?: string | undefined;
}

/**
 * @public
 */
export interface GetConfigInput {
  workspace_id: string | undefined;
  org_id: string | undefined;
  prefix?: (string)[] | undefined;
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
  id: string | undefined;
  /**
   * Represents conditional criteria used for context matching. Keys define dimension names and values specify the criteria that must be met.
   * @public
   */
  condition: Record<string, __DocumentType> | undefined;

  priority: number | undefined;
  weight: number | undefined;
  override_with_keys: (string)[] | undefined;
}

/**
 * @public
 */
export interface Unit {
}

/**
 * @public
 */
export type DimensionType =
  | DimensionType.LOCAL_COHORTMember
  | DimensionType.REGULARMember
  | DimensionType.REMOTE_COHORTMember
  | DimensionType.$UnknownMember

/**
 * @public
 */
export namespace DimensionType {

  export interface REGULARMember {
    REGULAR: Unit;
    LOCAL_COHORT?: never;
    REMOTE_COHORT?: never;
    $unknown?: never;
  }

  export interface LOCAL_COHORTMember {
    REGULAR?: never;
    LOCAL_COHORT: string;
    REMOTE_COHORT?: never;
    $unknown?: never;
  }

  export interface REMOTE_COHORTMember {
    REGULAR?: never;
    LOCAL_COHORT?: never;
    REMOTE_COHORT: string;
    $unknown?: never;
  }

  /**
   * @public
   */
  export interface $UnknownMember {
    REGULAR?: never;
    LOCAL_COHORT?: never;
    REMOTE_COHORT?: never;
    $unknown: [string, any];
  }

  export interface Visitor<T> {
    REGULAR: (value: Unit) => T;
    LOCAL_COHORT: (value: string) => T;
    REMOTE_COHORT: (value: string) => T;
    _: (name: string, value: any) => T;
  }

  export const visit = <T>(
    value: DimensionType,
    visitor: Visitor<T>
  ): T => {
    if (value.REGULAR !== undefined) return visitor.REGULAR(value.REGULAR);
    if (value.LOCAL_COHORT !== undefined) return visitor.LOCAL_COHORT(value.LOCAL_COHORT);
    if (value.REMOTE_COHORT !== undefined) return visitor.REMOTE_COHORT(value.REMOTE_COHORT);
    return visitor._(value.$unknown[0], value.$unknown[1]);
  }

}

/**
 * @public
 */
export interface DimensionInfo {
  /**
   * Generic key-value object structure used for flexible data representation throughout the API.
   * @public
   */
  schema: Record<string, __DocumentType> | undefined;

  position: number | undefined;
  dimension_type: DimensionType | undefined;
  dependency_graph: Record<string, (string)[]> | undefined;
  value_compute_function_name?: string | undefined;
}

/**
 * @public
 */
export interface GetConfigOutput {
  contexts: (ContextPartial)[] | undefined;
  overrides: Record<string, Record<string, __DocumentType>> | undefined;
  /**
   * Generic key-value object structure used for flexible data representation throughout the API.
   * @public
   */
  default_configs: Record<string, __DocumentType> | undefined;

  dimensions: Record<string, DimensionInfo> | undefined;
  version: string | undefined;
  last_modified: Date | undefined;
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
  prefix?: (string)[] | undefined;
  version?: string | undefined;
  show_reasoning?: boolean | undefined;
  merge_strategy?: MergeStrategy | undefined;
  context_id?: string | undefined;
  /**
   * Intended for control resolution. If true, evaluates and includes remote cohort-based contexts during config resolution.
   * @public
   */
  resolve_remote?: boolean | undefined;

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
  config: __DocumentType | undefined;
  version: string | undefined;
  last_modified: Date | undefined;
  audit_id?: string | undefined;
}

/**
 * @public
 */
export interface GetResolvedConfigWithIdentifierInput {
  workspace_id: string | undefined;
  org_id: string | undefined;
  prefix?: (string)[] | undefined;
  version?: string | undefined;
  show_reasoning?: boolean | undefined;
  merge_strategy?: MergeStrategy | undefined;
  context_id?: string | undefined;
  /**
   * Intended for control resolution. If true, evaluates and includes remote cohort-based contexts during config resolution.
   * @public
   */
  resolve_remote?: boolean | undefined;

  /**
   * Map representing the context.
   * Keys correspond to the names of the dimensions.
   * @public
   */
  context?: Record<string, __DocumentType> | undefined;

  identifier?: string | undefined;
}

/**
 * @public
 */
export interface GetResolvedConfigWithIdentifierOutput {
  config: __DocumentType | undefined;
  version: string | undefined;
  last_modified: Date | undefined;
  audit_id?: string | undefined;
}

/**
 * @public
 */
export interface GetVersionInput {
  workspace_id: string | undefined;
  org_id: string | undefined;
  id: string | undefined;
}

/**
 * @public
 */
export interface GetVersionResponse {
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
export interface ListVersionsInput {
  workspace_id: string | undefined;
  org_id: string | undefined;
  /**
   * Number of items to be returned in each page.
   * @public
   */
  count?: number | undefined;

  /**
   * Page number to retrieve, starting from 1.
   * @public
   */
  page?: number | undefined;
}

/**
 * @public
 */
export interface ListVersionsMember {
  id: string | undefined;
  config: __DocumentType | undefined;
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
export interface CreateContextInput {
  workspace_id: string | undefined;
  org_id: string | undefined;
  config_tags?: string | undefined;
  request: ContextPut | undefined;
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
export const DimensionMatchStrategy = {
  /**
   * Match the overrides which have the exact context
   */
  EXACT: "exact",
  /**
   * Match the overrides which have the given context as subset
   */
  SUBSET: "subset",
} as const
/**
 * @public
 */
export type DimensionMatchStrategy = typeof DimensionMatchStrategy[keyof typeof DimensionMatchStrategy]

/**
 * @public
 * @enum
 */
export const ContextFilterSortOn = {
  CREATED_AT: "created_at",
  LAST_MODIFIED_AT: "last_modified_at",
  WEIGHT: "weight",
} as const
/**
 * @public
 */
export type ContextFilterSortOn = typeof ContextFilterSortOn[keyof typeof ContextFilterSortOn]

/**
 * @public
 */
export interface ListContextsInput {
  /**
   * Number of items to be returned in each page.
   * @public
   */
  count?: number | undefined;

  /**
   * Page number to retrieve, starting from 1.
   * @public
   */
  page?: number | undefined;

  /**
   * If true, returns all requested items, ignoring pagination parameters page and count.
   * @public
   */
  all?: boolean | undefined;

  workspace_id: string | undefined;
  org_id: string | undefined;
  prefix?: (string)[] | undefined;
  sort_on?: ContextFilterSortOn | undefined;
  /**
   * Sort order enumeration for list operations.
   * @public
   */
  sort_by?: SortBy | undefined;

  created_by?: (string)[] | undefined;
  last_modified_by?: (string)[] | undefined;
  plaintext?: string | undefined;
  /**
   * Strategy to follow while filter items based on the context
   * @public
   */
  dimension_match_strategy?: DimensionMatchStrategy | undefined;
}

/**
 * @public
 */
export interface ListContextsOutput {
  total_pages: number | undefined;
  total_items: number | undefined;
  data: (ContextResponse)[] | undefined;
}

/**
 * @public
 */
export interface MoveContextInput {
  workspace_id: string | undefined;
  org_id: string | undefined;
  id: string | undefined;
  request: ContextMove | undefined;
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
export interface ValidateContextInput {
  workspace_id: string | undefined;
  org_id: string | undefined;
  /**
   * Represents conditional criteria used for context matching. Keys define dimension names and values specify the criteria that must be met.
   * @public
   */
  context: Record<string, __DocumentType> | undefined;
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
  id: string | undefined;
  /**
   * Represents conditional criteria used for context matching. Keys define dimension names and values specify the criteria that must be met.
   * @public
   */
  condition: Record<string, __DocumentType> | undefined;

  /**
   * Priority weight used to determine the order of context evaluation. Higher weights take precedence during configuration resolution.
   * @public
   */
  old_weight: string | undefined;

  /**
   * Priority weight used to determine the order of context evaluation. Higher weights take precedence during configuration resolution.
   * @public
   */
  new_weight: string | undefined;
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
export interface ContextValidationFunctionRequest {
  environment: __DocumentType | undefined;
}

/**
 * @public
 */
export interface CreateDefaultConfigInput {
  key: string | undefined;
  value: __DocumentType | undefined;
  /**
   * Generic key-value object structure used for flexible data representation throughout the API.
   * @public
   */
  schema: Record<string, __DocumentType> | undefined;

  description: string | undefined;
  change_reason: string | undefined;
  value_validation_function_name?: string | undefined;
  value_compute_function_name?: string | undefined;
  workspace_id: string | undefined;
  org_id: string | undefined;
}

/**
 * @public
 */
export interface DefaultConfigResponse {
  key: string | undefined;
  value: __DocumentType | undefined;
  /**
   * Generic key-value object structure used for flexible data representation throughout the API.
   * @public
   */
  schema: Record<string, __DocumentType> | undefined;

  description: string | undefined;
  change_reason: string | undefined;
  value_validation_function_name?: string | undefined;
  value_compute_function_name?: string | undefined;
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
  /**
   * Generic key-value object structure used for flexible data representation throughout the API.
   * @public
   */
  schema: Record<string, __DocumentType> | undefined;

  value_validation_function_name?: string | undefined;
  description: string | undefined;
  change_reason: string | undefined;
  dimension_type?: DimensionType | undefined;
  value_compute_function_name?: string | undefined;
}

/**
 * @public
 */
export interface DimensionResponse {
  dimension: string | undefined;
  position: number | undefined;
  /**
   * Generic key-value object structure used for flexible data representation throughout the API.
   * @public
   */
  schema: Record<string, __DocumentType> | undefined;

  value_validation_function_name?: string | undefined;
  description: string | undefined;
  change_reason: string | undefined;
  last_modified_at: Date | undefined;
  last_modified_by: string | undefined;
  created_at: Date | undefined;
  created_by: string | undefined;
  dependency_graph: Record<string, (string)[]> | undefined;
  dimension_type: DimensionType | undefined;
  value_compute_function_name?: string | undefined;
  mandatory: boolean | undefined;
}

/**
 * @public
 */
export interface CreateExperimentRequest {
  workspace_id: string | undefined;
  org_id: string | undefined;
  name: string | undefined;
  experiment_type?: ExperimentType | undefined;
  /**
   * Represents conditional criteria used for context matching. Keys define dimension names and values specify the criteria that must be met.
   * @public
   */
  context: Record<string, __DocumentType> | undefined;

  variants: (Variant)[] | undefined;
  description: string | undefined;
  change_reason: string | undefined;
  metrics?: __DocumentType | undefined;
  experiment_group_id?: string | undefined;
}

/**
 * Input structure for creating a new experiment group.
 * @public
 */
export interface CreateExperimentGroupRequest {
  workspace_id: string | undefined;
  org_id: string | undefined;
  name: string | undefined;
  description: string | undefined;
  /**
   * Reason for creating this experiment group.
   * @public
   */
  change_reason: string | undefined;

  /**
   * Represents conditional criteria used for context matching. Keys define dimension names and values specify the criteria that must be met.
   * @public
   */
  context: Record<string, __DocumentType> | undefined;

  traffic_percentage: number | undefined;
  /**
   * List of experiment IDs that are members of this group.
   * @public
   */
  member_experiment_ids?: (string)[] | undefined;
}

/**
 * @public
 * @enum
 */
export const FunctionTypes = {
  CHANGE_REASON_VALIDATION: "CHANGE_REASON_VALIDATION",
  CONTEXT_VALIDATION: "CONTEXT_VALIDATION",
  VALUE_COMPUTE: "VALUE_COMPUTE",
  VALUE_VALIDATION: "VALUE_VALIDATION",
} as const
/**
 * @public
 */
export type FunctionTypes = typeof FunctionTypes[keyof typeof FunctionTypes]

/**
 * @public
 * @enum
 */
export const FunctionRuntimeVersion = {
  V1: "1.0",
} as const
/**
 * @public
 */
export type FunctionRuntimeVersion = typeof FunctionRuntimeVersion[keyof typeof FunctionRuntimeVersion]

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
  runtime_version: FunctionRuntimeVersion | undefined;
  function_type: FunctionTypes | undefined;
}

/**
 * @public
 */
export interface FunctionResponse {
  function_name: string | undefined;
  published_code?: string | undefined;
  draft_code: string | undefined;
  published_runtime_version?: FunctionRuntimeVersion | undefined;
  draft_runtime_version: FunctionRuntimeVersion | undefined;
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
  ACTIVE: "Active",
  INACTIVE: "Inactive",
  PENDING_KYB: "PendingKyb",
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
export interface CreateSecretInput {
  workspace_id: string | undefined;
  org_id: string | undefined;
  name: string | undefined;
  /**
   * Plaintext value to be encrypted and stored.
   * @public
   */
  value: string | undefined;

  description: string | undefined;
  change_reason: string | undefined;
}

/**
 * Response structure for secret operations. Secret values are never returned for security.
 * @public
 */
export interface SecretResponse {
  name: string | undefined;
  description: string | undefined;
  change_reason: string | undefined;
  created_by: string | undefined;
  created_at: Date | undefined;
  last_modified_by: string | undefined;
  last_modified_at: Date | undefined;
}

/**
 * @public
 */
export interface CreateTypeTemplatesRequest {
  workspace_id: string | undefined;
  org_id: string | undefined;
  type_name: string | undefined;
  /**
   * Generic key-value object structure used for flexible data representation throughout the API.
   * @public
   */
  type_schema: Record<string, __DocumentType> | undefined;

  description: string | undefined;
  change_reason: string | undefined;
}

/**
 * @public
 */
export interface TypeTemplatesResponse {
  type_name: string | undefined;
  /**
   * Generic key-value object structure used for flexible data representation throughout the API.
   * @public
   */
  type_schema: Record<string, __DocumentType> | undefined;

  description: string | undefined;
  change_reason: string | undefined;
  created_by: string | undefined;
  created_at: Date | undefined;
  last_modified_at: Date | undefined;
  last_modified_by: string | undefined;
}

/**
 * @public
 */
export interface CreateVariableInput {
  workspace_id: string | undefined;
  org_id: string | undefined;
  name: string | undefined;
  value: string | undefined;
  description: string | undefined;
  change_reason: string | undefined;
}

/**
 * @public
 */
export interface VariableResponse {
  name: string | undefined;
  value: string | undefined;
  description: string | undefined;
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
  /**
   * Generic key-value object structure used for flexible data representation throughout the API.
   * @public
   */
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
  /**
   * Generic key-value object structure used for flexible data representation throughout the API.
   * @public
   */
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
  metrics?: __DocumentType | undefined;
  allow_experiment_self_approval?: boolean | undefined;
  auto_populate_control?: boolean | undefined;
  enable_context_validation?: boolean | undefined;
  enable_change_reason_validation?: boolean | undefined;
  change_reason: string | undefined;
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
  metrics: __DocumentType | undefined;
  allow_experiment_self_approval: boolean | undefined;
  auto_populate_control: boolean | undefined;
  enable_context_validation: boolean | undefined;
  enable_change_reason_validation: boolean | undefined;
  change_reason: string | undefined;
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
export interface GetDefaultConfigInput {
  workspace_id: string | undefined;
  org_id: string | undefined;
  key: string | undefined;
}

/**
 * @public
 */
export interface ListDefaultConfigsInput {
  workspace_id: string | undefined;
  org_id: string | undefined;
  /**
   * Number of items to be returned in each page.
   * @public
   */
  count?: number | undefined;

  /**
   * Page number to retrieve, starting from 1.
   * @public
   */
  page?: number | undefined;

  /**
   * If true, returns all requested items, ignoring pagination parameters page and count.
   * @public
   */
  all?: boolean | undefined;

  name?: string | undefined;
}

/**
 * @public
 */
export interface ListDefaultConfigsOutput {
  total_pages: number | undefined;
  total_items: number | undefined;
  data: (DefaultConfigResponse)[] | undefined;
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
  /**
   * Generic key-value object structure used for flexible data representation throughout the API.
   * @public
   */
  schema?: Record<string, __DocumentType> | undefined;

  /**
   * To unset the function name, pass "null" string.
   * @public
   */
  value_validation_function_name?: string | undefined;

  description?: string | undefined;
  /**
   * To unset the function name, pass "null" string.
   * @public
   */
  value_compute_function_name?: string | undefined;
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
export interface DeleteExperimentGroupInput {
  workspace_id: string | undefined;
  org_id: string | undefined;
  id: string | undefined;
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
export interface DeleteSecretInput {
  workspace_id: string | undefined;
  org_id: string | undefined;
  name: string | undefined;
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
export interface DeleteVariableInput {
  workspace_id: string | undefined;
  org_id: string | undefined;
  name: string | undefined;
}

/**
 * @public
 */
export interface DeleteWebhookInput {
  workspace_id: string | undefined;
  org_id: string | undefined;
  name: string | undefined;
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
  /**
   * Number of items to be returned in each page.
   * @public
   */
  count?: number | undefined;

  /**
   * Page number to retrieve, starting from 1.
   * @public
   */
  page?: number | undefined;

  /**
   * If true, returns all requested items, ignoring pagination parameters page and count.
   * @public
   */
  all?: boolean | undefined;

  workspace_id: string | undefined;
  org_id: string | undefined;
}

/**
 * @public
 */
export interface ListDimensionsOutput {
  total_pages: number | undefined;
  total_items: number | undefined;
  data: (DimensionResponse)[] | undefined;
}

/**
 * @public
 */
export interface UpdateDimensionInput {
  workspace_id: string | undefined;
  org_id: string | undefined;
  dimension: string | undefined;
  /**
   * Generic key-value object structure used for flexible data representation throughout the API.
   * @public
   */
  schema?: Record<string, __DocumentType> | undefined;

  position?: number | undefined;
  /**
   * To unset the function name, pass "null" string.
   * @public
   */
  value_validation_function_name?: string | undefined;

  description?: string | undefined;
  change_reason: string | undefined;
  /**
   * To unset the function name, pass "null" string.
   * @public
   */
  value_compute_function_name?: string | undefined;
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
export interface GetExperimentGroupInput {
  workspace_id: string | undefined;
  org_id: string | undefined;
  id: string | undefined;
}

/**
 * @public
 * @enum
 */
export const ExperimentGroupSortOn = {
  /**
   * Sort by creation timestamp.
   */
  CREATED_AT: "created_at",
  /**
   * Sort by last modification timestamp.
   */
  LAST_MODIFIED_AT: "last_modified_at",
  /**
   * Sort by name.
   */
  NAME: "name",
} as const
/**
 * @public
 */
export type ExperimentGroupSortOn = typeof ExperimentGroupSortOn[keyof typeof ExperimentGroupSortOn]

/**
 * @public
 */
export interface ListExperimentGroupsInput {
  /**
   * Number of items to be returned in each page.
   * @public
   */
  count?: number | undefined;

  /**
   * Page number to retrieve, starting from 1.
   * @public
   */
  page?: number | undefined;

  /**
   * If true, returns all requested items, ignoring pagination parameters page and count.
   * @public
   */
  all?: boolean | undefined;

  workspace_id: string | undefined;
  org_id: string | undefined;
  /**
   * Filter by experiment group name (exact match or substring, depending on backend implementation).
   * @public
   */
  name?: string | undefined;

  /**
   * Filter by the user who created the experiment group.
   * @public
   */
  created_by?: string | undefined;

  /**
   * Filter by the user who last modified the experiment group.
   * @public
   */
  last_modified_by?: string | undefined;

  /**
   * Field to sort the results by.
   * @public
   */
  sort_on?: ExperimentGroupSortOn | undefined;

  /**
   * Sort order (ascending or descending).
   * @public
   */
  sort_by?: SortBy | undefined;

  /**
   * Filter by the type of group (USER_CREATED or SYSTEM_GENERATED).
   * @public
   */
  group_type?: (GroupType)[] | undefined;
}

/**
 * @public
 */
export interface ListExperimentGroupsOutput {
  total_pages: number | undefined;
  total_items: number | undefined;
  /**
   * A list of experiment group responses.
   * @public
   */
  data: (ExperimentGroupResponse)[] | undefined;
}

/**
 * Input structure for updating an existing experiment group.
 * @public
 */
export interface UpdateExperimentGroupRequest {
  workspace_id: string | undefined;
  org_id: string | undefined;
  id: string | undefined;
  /**
   * Reason for this update.
   * @public
   */
  change_reason: string | undefined;

  /**
   * Optional new description for the group.
   * @public
   */
  description?: string | undefined;

  /**
   * Optional new traffic percentage for the group.
   * @public
   */
  traffic_percentage?: number | undefined;
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
  CREATED_AT: "created_at",
  LAST_MODIFIED_AT: "last_modified_at",
} as const
/**
 * @public
 */
export type ExperimentSortOn = typeof ExperimentSortOn[keyof typeof ExperimentSortOn]

/**
 * @public
 */
export interface ListExperimentInput {
  /**
   * Number of items to be returned in each page.
   * @public
   */
  count?: number | undefined;

  /**
   * Page number to retrieve, starting from 1.
   * @public
   */
  page?: number | undefined;

  /**
   * If true, returns all requested items, ignoring pagination parameters page and count.
   * @public
   */
  all?: boolean | undefined;

  workspace_id: string | undefined;
  org_id: string | undefined;
  status?: (ExperimentStatusType)[] | undefined;
  from_date?: Date | undefined;
  to_date?: Date | undefined;
  experiment_name?: string | undefined;
  experiment_ids?: (string)[] | undefined;
  experiment_group_ids?: (string)[] | undefined;
  created_by?: (string)[] | undefined;
  sort_on?: ExperimentSortOn | undefined;
  /**
   * Sort order enumeration for list operations.
   * @public
   */
  sort_by?: SortBy | undefined;

  global_experiments_only?: boolean | undefined;
  /**
   * Strategy to follow while filter items based on the context
   * @public
   */
  dimension_match_strategy?: DimensionMatchStrategy | undefined;
}

/**
 * @public
 */
export interface ListExperimentOutput {
  total_pages: number | undefined;
  total_items: number | undefined;
  data: (ExperimentResponse)[] | undefined;
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
  /**
   * Configuration overrides that replace default values when context conditions are met. Keys represent configuration keys and values are the override data.
   * @public
   */
  overrides: Record<string, __DocumentType> | undefined;
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
  metrics?: __DocumentType | undefined;
  /**
   * To unset experiment group, pass "null" string.
   * @public
   */
  experiment_group_id?: string | undefined;
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
  /**
   * Number of items to be returned in each page.
   * @public
   */
  count?: number | undefined;

  /**
   * Page number to retrieve, starting from 1.
   * @public
   */
  page?: number | undefined;

  /**
   * If true, returns all requested items, ignoring pagination parameters page and count.
   * @public
   */
  all?: boolean | undefined;

  workspace_id: string | undefined;
  org_id: string | undefined;
  function_type?: (FunctionTypes)[] | undefined;
}

/**
 * @public
 */
export interface ListFunctionOutput {
  total_pages: number | undefined;
  total_items: number | undefined;
  data: (FunctionResponse)[] | undefined;
}

/**
 * @public
 */
export interface PublishInput {
  workspace_id: string | undefined;
  org_id: string | undefined;
  function_name: string | undefined;
  change_reason: string | undefined;
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
export interface ValueComputeFunctionRequest {
  name: string | undefined;
  prefix: string | undefined;
  type: string | undefined;
  environment: __DocumentType | undefined;
}

/**
 * @public
 */
export interface ValueValidationFunctionRequest {
  key: string | undefined;
  value: __DocumentType | undefined;
  type: string | undefined;
  environment: __DocumentType | undefined;
}

/**
 * @public
 */
export type FunctionExecutionRequest =
  | FunctionExecutionRequest.Change_reason_validateMember
  | FunctionExecutionRequest.Context_validateMember
  | FunctionExecutionRequest.Value_computeMember
  | FunctionExecutionRequest.Value_validateMember
  | FunctionExecutionRequest.$UnknownMember

/**
 * @public
 */
export namespace FunctionExecutionRequest {

  export interface Value_validateMember {
    value_validate: ValueValidationFunctionRequest;
    value_compute?: never;
    context_validate?: never;
    change_reason_validate?: never;
    $unknown?: never;
  }

  export interface Value_computeMember {
    value_validate?: never;
    value_compute: ValueComputeFunctionRequest;
    context_validate?: never;
    change_reason_validate?: never;
    $unknown?: never;
  }

  export interface Context_validateMember {
    value_validate?: never;
    value_compute?: never;
    context_validate: ContextValidationFunctionRequest;
    change_reason_validate?: never;
    $unknown?: never;
  }

  export interface Change_reason_validateMember {
    value_validate?: never;
    value_compute?: never;
    context_validate?: never;
    change_reason_validate: ChangeReasonValidationFunctionRequest;
    $unknown?: never;
  }

  /**
   * @public
   */
  export interface $UnknownMember {
    value_validate?: never;
    value_compute?: never;
    context_validate?: never;
    change_reason_validate?: never;
    $unknown: [string, any];
  }

  export interface Visitor<T> {
    value_validate: (value: ValueValidationFunctionRequest) => T;
    value_compute: (value: ValueComputeFunctionRequest) => T;
    context_validate: (value: ContextValidationFunctionRequest) => T;
    change_reason_validate: (value: ChangeReasonValidationFunctionRequest) => T;
    _: (name: string, value: any) => T;
  }

  export const visit = <T>(
    value: FunctionExecutionRequest,
    visitor: Visitor<T>
  ): T => {
    if (value.value_validate !== undefined) return visitor.value_validate(value.value_validate);
    if (value.value_compute !== undefined) return visitor.value_compute(value.value_compute);
    if (value.context_validate !== undefined) return visitor.context_validate(value.context_validate);
    if (value.change_reason_validate !== undefined) return visitor.change_reason_validate(value.change_reason_validate);
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
  function?: string | undefined;
  runtime_version?: FunctionRuntimeVersion | undefined;
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
export interface GetSecretInput {
  workspace_id: string | undefined;
  org_id: string | undefined;
  name: string | undefined;
}

/**
 * @public
 */
export interface GetTypeTemplateInput {
  workspace_id: string | undefined;
  org_id: string | undefined;
  type_name: string | undefined;
}

/**
 * @public
 */
export interface GetTypeTemplatesListInput {
  /**
   * Number of items to be returned in each page.
   * @public
   */
  count?: number | undefined;

  /**
   * Page number to retrieve, starting from 1.
   * @public
   */
  page?: number | undefined;

  /**
   * If true, returns all requested items, ignoring pagination parameters page and count.
   * @public
   */
  all?: boolean | undefined;

  workspace_id: string | undefined;
  org_id: string | undefined;
}

/**
 * @public
 */
export interface GetTypeTemplatesListOutput {
  total_pages: number | undefined;
  total_items: number | undefined;
  data: (TypeTemplatesResponse)[] | undefined;
}

/**
 * @public
 */
export interface GetVariableInput {
  workspace_id: string | undefined;
  org_id: string | undefined;
  name: string | undefined;
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
export interface GetWebhookByEventInput {
  workspace_id: string | undefined;
  org_id: string | undefined;
  event: string | undefined;
}

/**
 * @public
 */
export interface GetWorkspaceInput {
  org_id: string | undefined;
  workspace_name: string | undefined;
}

/**
 * @public
 */
export interface ListOrganisationInput {
  /**
   * Number of items to be returned in each page.
   * @public
   */
  count?: number | undefined;

  /**
   * Page number to retrieve, starting from 1.
   * @public
   */
  page?: number | undefined;

  /**
   * If true, returns all requested items, ignoring pagination parameters page and count.
   * @public
   */
  all?: boolean | undefined;
}

/**
 * @public
 */
export interface ListOrganisationOutput {
  total_pages: number | undefined;
  total_items: number | undefined;
  data: (OrganisationResponse)[] | undefined;
}

/**
 * @public
 * @enum
 */
export const SecretSortOn = {
  CREATED_AT: "created_at",
  LAST_MODIFIED_AT: "last_modified_at",
  NAME: "name",
} as const
/**
 * @public
 */
export type SecretSortOn = typeof SecretSortOn[keyof typeof SecretSortOn]

/**
 * @public
 */
export interface ListSecretsInput {
  /**
   * Number of items to be returned in each page.
   * @public
   */
  count?: number | undefined;

  /**
   * Page number to retrieve, starting from 1.
   * @public
   */
  page?: number | undefined;

  /**
   * If true, returns all requested items, ignoring pagination parameters page and count.
   * @public
   */
  all?: boolean | undefined;

  workspace_id: string | undefined;
  org_id: string | undefined;
  /**
   * Filter by secret name.
   * @public
   */
  name?: string | undefined;

  /**
   * Filter by the user who created the secret.
   * @public
   */
  created_by?: string | undefined;

  /**
   * Filter by the user who last modified the secret.
   * @public
   */
  last_modified_by?: string | undefined;

  /**
   * Field to sort the results by.
   * @public
   */
  sort_on?: SecretSortOn | undefined;

  /**
   * Sort order (ascending or descending).
   * @public
   */
  sort_by?: SortBy | undefined;
}

/**
 * @public
 */
export interface ListSecretsOutput {
  total_pages: number | undefined;
  total_items: number | undefined;
  data: (SecretResponse)[] | undefined;
}

/**
 * @public
 * @enum
 */
export const VariableSortOn = {
  CREATED_AT: "created_at",
  LAST_MODIFIED_AT: "last_modified_at",
  NAME: "name",
} as const
/**
 * @public
 */
export type VariableSortOn = typeof VariableSortOn[keyof typeof VariableSortOn]

/**
 * @public
 */
export interface ListVariablesInput {
  /**
   * Number of items to be returned in each page.
   * @public
   */
  count?: number | undefined;

  /**
   * Page number to retrieve, starting from 1.
   * @public
   */
  page?: number | undefined;

  /**
   * If true, returns all requested items, ignoring pagination parameters page and count.
   * @public
   */
  all?: boolean | undefined;

  workspace_id: string | undefined;
  org_id: string | undefined;
  /**
   * Filter by variable name (exact match or substring, depending on backend implementation).
   * @public
   */
  name?: string | undefined;

  /**
   * Filter by the user who created the variable
   * @public
   */
  created_by?: string | undefined;

  /**
   * Filter by the user who last modified the variable
   * @public
   */
  last_modified_by?: string | undefined;

  /**
   * Field to sort the results by.
   * @public
   */
  sort_on?: VariableSortOn | undefined;

  /**
   * Sort order (ascending or descending).
   * @public
   */
  sort_by?: SortBy | undefined;
}

/**
 * @public
 */
export interface ListVariablesOutput {
  total_pages: number | undefined;
  total_items: number | undefined;
  data: (VariableResponse)[] | undefined;
}

/**
 * @public
 */
export interface ListWebhookInput {
  /**
   * Number of items to be returned in each page.
   * @public
   */
  count?: number | undefined;

  /**
   * Page number to retrieve, starting from 1.
   * @public
   */
  page?: number | undefined;

  /**
   * If true, returns all requested items, ignoring pagination parameters page and count.
   * @public
   */
  all?: boolean | undefined;

  workspace_id: string | undefined;
  org_id: string | undefined;
}

/**
 * @public
 */
export interface ListWebhookOutput {
  total_pages: number | undefined;
  total_items: number | undefined;
  data: (WebhookResponse)[] | undefined;
}

/**
 * @public
 */
export interface ListWorkspaceInput {
  /**
   * Number of items to be returned in each page.
   * @public
   */
  count?: number | undefined;

  /**
   * Page number to retrieve, starting from 1.
   * @public
   */
  page?: number | undefined;

  /**
   * If true, returns all requested items, ignoring pagination parameters page and count.
   * @public
   */
  all?: boolean | undefined;

  org_id: string | undefined;
}

/**
 * @public
 */
export interface ListWorkspaceOutput {
  total_pages: number | undefined;
  total_items: number | undefined;
  data: (WorkspaceResponse)[] | undefined;
}

/**
 * @public
 */
export interface RotateMasterEncryptionKeyOutput {
  workspaces_rotated: number | undefined;
  total_secrets_re_encrypted: number | undefined;
}

/**
 * @public
 */
export interface WorkspaceSelectorRequest {
  org_id: string | undefined;
  workspace_name: string | undefined;
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
export interface RotateWorkspaceEncryptionKeyOutput {
  /**
   * Number of secrets that were re-encrypted with the new key.
   * @public
   */
  total_secrets_re_encrypted: number | undefined;
}

/**
 * @public
 */
export interface UpdateSecretInput {
  workspace_id: string | undefined;
  org_id: string | undefined;
  name: string | undefined;
  /**
   * New plaintext value to encrypt and store. If provided, will be encrypted with current key.
   * @public
   */
  value?: string | undefined;

  description?: string | undefined;
  change_reason: string | undefined;
}

/**
 * @public
 */
export interface UpdateTypeTemplatesRequest {
  workspace_id: string | undefined;
  org_id: string | undefined;
  type_name: string | undefined;
  /**
   * Generic key-value object structure used for flexible data representation throughout the API.
   * @public
   */
  type_schema: Record<string, __DocumentType> | undefined;

  description?: string | undefined;
  change_reason: string | undefined;
}

/**
 * @public
 */
export interface UpdateVariableInput {
  workspace_id: string | undefined;
  org_id: string | undefined;
  name: string | undefined;
  value?: string | undefined;
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
  description?: string | undefined;
  enabled?: boolean | undefined;
  url?: string | undefined;
  method?: HttpMethod | undefined;
  version?: Version | undefined;
  /**
   * Generic key-value object structure used for flexible data representation throughout the API.
   * @public
   */
  custom_headers?: Record<string, __DocumentType> | undefined;

  events?: (string)[] | undefined;
  change_reason: string | undefined;
}

/**
 * @public
 */
export interface UpdateWorkspaceRequest {
  org_id: string | undefined;
  workspace_name: string | undefined;
  workspace_admin_email: string | undefined;
  /**
   * To unset config version, pass "null" string.
   * @public
   */
  config_version?: string | undefined;

  mandatory_dimensions?: (string)[] | undefined;
  workspace_status?: WorkspaceStatus | undefined;
  metrics?: __DocumentType | undefined;
  allow_experiment_self_approval?: boolean | undefined;
  auto_populate_control?: boolean | undefined;
  enable_context_validation?: boolean | undefined;
  enable_change_reason_validation?: boolean | undefined;
  change_reason?: string | undefined;
}
