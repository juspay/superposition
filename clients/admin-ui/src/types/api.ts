// ── Shared primitives ──────────────────────────────────────────────

export type JsonValue =
  | string
  | number
  | boolean
  | null
  | JsonValue[]
  | { [key: string]: JsonValue };

export type Condition = Record<string, JsonValue>;
export type Overrides = Record<string, JsonValue>;
export type DependencyGraph = Record<string, string[]>;

export type SortBy = "asc" | "desc";

// ── Pagination ─────────────────────────────────────────────────────

export interface PaginationParams {
  page?: number;
  count?: number;
  all?: boolean;
}

export interface PaginatedResponse<T> {
  total_pages: number;
  total_items: number;
  data: T[];
}

// ── Dimension ──────────────────────────────────────────────────────

export type DimensionType =
  | "REGULAR"
  | { LOCAL_COHORT: string }
  | { REMOTE_COHORT: string };

export interface Dimension {
  dimension: string;
  position: number;
  created_at: string;
  created_by: string;
  schema: Record<string, JsonValue>;
  value_validation_function_name: string | null;
  last_modified_at: string;
  last_modified_by: string;
  mandatory: boolean;
  dependency_graph: DependencyGraph;
  description: string;
  change_reason: string;
  value_compute_function_name: string | null;
  dimension_type: DimensionType;
}

export interface CreateDimensionRequest {
  dimension: string;
  position: number;
  schema: Record<string, JsonValue>;
  function_name?: string;
  autocomplete_function_name?: string;
  description: string;
  change_reason: string;
  dimension_type?: DimensionType;
}

export interface UpdateDimensionRequest {
  position?: number;
  schema?: Record<string, JsonValue>;
  function_name?: string | null;
  autocomplete_function_name?: string | null;
  description?: string;
  change_reason: string;
}

// ── Default Config ─────────────────────────────────────────────────

export interface DefaultConfig {
  key: string;
  value: JsonValue;
  created_at: string;
  created_by: string;
  schema: Record<string, JsonValue>;
  value_validation_function_name: string | null;
  last_modified_at: string;
  last_modified_by: string;
  description: string;
  change_reason: string;
  value_compute_function_name: string | null;
}

export interface CreateDefaultConfigRequest {
  key: string;
  value: JsonValue;
  schema: Record<string, JsonValue>;
  function_name?: string;
  autocomplete_function_name?: string;
  description: string;
  change_reason: string;
}

export interface UpdateDefaultConfigRequest {
  value?: JsonValue;
  schema?: Record<string, JsonValue>;
  function_name?: string | null;
  autocomplete_function_name?: string | null;
  description?: string;
  change_reason: string;
}

export interface DefaultConfigFilters {
  name?: string;
}

// ── Context / Override ─────────────────────────────────────────────

export interface ContextOverride {
  id: string;
  value: Condition;
  override_id: string;
  created_at: string;
  created_by: string;
  override_: Overrides;
  last_modified_at: string;
  last_modified_by: string;
  weight: string;
  description: string;
  change_reason: string;
}

export interface PutContextRequest {
  context: Condition;
  override: Overrides;
  description?: string;
  change_reason: string;
}

export interface ContextListFilters {
  prefix?: string[];
  sort_on?: "weight" | "created_at" | "last_modified_at";
  sort_by?: SortBy;
  created_by?: string[];
  last_modified_by?: string[];
  plaintext?: string;
}

export type ContextAction =
  | { type: "PUT"; data: PutContextRequest }
  | {
      type: "REPLACE";
      data: {
        context: Condition;
        override: Overrides;
        description?: string;
        change_reason: string;
      };
    }
  | { type: "DELETE"; id: string }
  | {
      type: "MOVE";
      id: string;
      data: { context: Condition; description?: string; change_reason: string };
    };

// ── Experiment ─────────────────────────────────────────────────────

export type ExperimentStatus =
  | "CREATED"
  | "INPROGRESS"
  | "PAUSED"
  | "CONCLUDED"
  | "DISCARDED";

export type ExperimentType = "DEFAULT" | "DELETE_OVERRIDES";

export type VariantType = "CONTROL" | "EXPERIMENTAL";

export interface Variant {
  id: string;
  variant_type: VariantType;
  context_id?: string;
  override_id?: string;
  overrides: Overrides;
}

export interface Experiment {
  id: string;
  created_at: string;
  created_by: string;
  last_modified: string;
  name: string;
  experiment_type: ExperimentType;
  override_keys: string[];
  status: ExperimentStatus;
  traffic_percentage: number;
  started_at: string | null;
  started_by: string | null;
  context: Condition;
  variants: Variant[];
  last_modified_by: string;
  chosen_variant: string | null;
  description: string;
  change_reason: string;
  metrics: Record<string, JsonValue>;
  metrics_url?: string;
  experiment_group_id: string | null;
}

export interface CreateExperimentRequest {
  name: string;
  context: Condition;
  variants: Array<{
    id: string;
    variant_type: VariantType;
    overrides: Overrides;
  }>;
  metrics?: Record<string, JsonValue>;
  experiment_type?: ExperimentType;
  description: string;
  change_reason: string;
  experiment_group_id?: string;
}

export interface RampExperimentRequest {
  traffic_percentage: number;
  change_reason: string;
}

export interface ConcludeExperimentRequest {
  chosen_variant: string;
  description?: string;
  change_reason: string;
}

export interface ExperimentListFilters {
  status?: ExperimentStatus[];
  from_date?: string;
  to_date?: string;
  experiment_name?: string;
  experiment_ids?: string[];
  experiment_group_ids?: string[];
  created_by?: string[];
  sort_on?: "last_modified_at" | "created_at";
  sort_by?: SortBy;
}

// ── Config Resolution ──────────────────────────────────────────────

export interface ResolveConfigQuery {
  context: Record<string, JsonValue>;
  merge_strategy?: "DEEP" | "SHALLOW";
}

export interface Config {
  contexts: ContextOverride[];
  overrides: Record<string, Record<string, JsonValue>>;
  default_configs: Record<string, DefaultConfig>;
}

// ── Function ───────────────────────────────────────────────────────

export type FunctionType =
  | "VALUE_VALIDATION"
  | "VALUE_COMPUTE"
  | "CONTEXT_VALIDATION"
  | "CHANGE_REASON_VALIDATION";

export interface FunctionDef {
  function_name: string;
  published_code: string | null;
  draft_code: string;
  description: string;
  published_runtime_version: string | null;
  draft_runtime_version: string;
  published_at: string | null;
  draft_edited_at: string;
  published_by: string | null;
  draft_edited_by: string;
  last_modified_at: string;
  last_modified_by: string;
  change_reason: string;
  function_type: FunctionType;
  created_by: string;
  created_at: string;
}

// ── Type Template ──────────────────────────────────────────────────

export interface TypeTemplate {
  type_name: string;
  type_schema: Record<string, JsonValue>;
  created_by: string;
  created_at: string;
  last_modified_at: string;
  last_modified_by: string;
  description: string;
  change_reason: string;
}

// ── Audit Log ──────────────────────────────────────────────────────

export type EventAction = "INSERT" | "UPDATE" | "DELETE";

export interface AuditLogEntry {
  id: string;
  table_name: string;
  user_name: string;
  timestamp: string;
  action: EventAction;
  original_data: JsonValue | null;
  new_data: JsonValue | null;
  query: string;
}

export interface AuditLogFilters {
  from_date?: string;
  to_date?: string;
  table?: string[];
  action?: EventAction[];
  username?: string;
  sort_by?: SortBy;
}

// ── Config Version ─────────────────────────────────────────────────

export interface ConfigVersionListItem {
  id: string;
  config_hash: string;
  tags: string[] | null;
  created_at: string;
  description: string;
}

export interface ConfigVersion {
  id: string;
  config: JsonValue;
  config_hash: string;
  tags: string[] | null;
  created_at: string;
  description: string;
}
