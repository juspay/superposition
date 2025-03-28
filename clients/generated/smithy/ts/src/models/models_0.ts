// smithy-typescript generated code
import { SuperpositionServiceException as __BaseException } from "./SuperpositionServiceException";
import { ExceptionOptionType as __ExceptionOptionType } from "@smithy/smithy-client";
import { DocumentType as __DocumentType } from "@smithy/types";

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
export interface ContextMove {
  id?: string | undefined;
  condition: Record<string, __DocumentType> | undefined;
  description?: string | undefined;
  change_reason: string | undefined;
}

/**
 * @public
 */
export interface ContextPut {
  condition: Record<string, __DocumentType> | undefined;
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
    REPLACE: ContextPut;
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
    REPLACE: (value: ContextPut) => T;
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
  weight?: number | undefined;
  description?: string | undefined;
  change_reason?: string | undefined;
}

/**
 * @public
 */
export interface ContextPutOut {
  context_id?: string | undefined;
  override_id?: string | undefined;
  weight?: number | undefined;
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
export interface GetConfigInput {
  workspace_id: string | undefined;
  org_id: string | undefined;
  prefix?: string | undefined;
  version?: string | undefined;
  context?: __DocumentType | undefined;
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
  context?: __DocumentType | undefined;
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
export interface ListVersionsOutput {
  total_pages?: number | undefined;
  total_items?: number | undefined;
  data?: (string)[] | undefined;
}

/**
 * @public
 */
export interface ContextActionResponse {
  context_id: string | undefined;
  override_id: string | undefined;
  weight: number | undefined;
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
  weight?: number | undefined;
  override_with_keys?: (string)[] | undefined;
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
  Asc: "Asc",
  Desc: "Desc",
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
  CreatedAt: "CreatedAt",
  Weight: "Weight",
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
  size?: number | undefined;
  prefix?: string | undefined;
  sort_on?: ContextFilterSortOn | undefined;
  sort_by?: SortBy | undefined;
  created_by?: string | undefined;
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
export interface WeightRecomputeOutput {
  id?: string | undefined;
  condition?: Record<string, __DocumentType> | undefined;
  old_weight?: number | undefined;
  new_weight?: number | undefined;
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
  mandatory?: boolean | undefined;
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
  function_name?: string | undefined;
  description?: string | undefined;
  change_reason: string | undefined;
}
