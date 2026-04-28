import type {
  ContextFilterSortOn,
  ContextPut,
  ContextResponse,
  CreateDefaultConfigInput,
  CreateDimensionInput,
  DefaultConfigResponse,
  DimensionMatchStrategy,
  DimensionResponse,
  DimensionType,
  GetResolvedConfigOutput,
  ListContextsInput,
  ListContextsOutput,
  ListDefaultConfigsInput,
  SortBy,
  UpdateDefaultConfigInput,
  UpdateDimensionInput,
} from "superposition-sdk";

type ServiceContextKeys = "workspace_id" | "org_id";

type RequestBody<T, ExtraKeys extends keyof T = never> = Omit<
  T,
  Extract<ServiceContextKeys | ExtraKeys, keyof T>
>;

type Defined<T> = Exclude<T, undefined>;
type ApiTimestamp = Date | string;
type OptionalFunctionKeys =
  | "value_validation_function_name"
  | "value_compute_function_name";

type RawResponse<
  T,
  OptionalKeys extends keyof T = never,
> = {
  [Key in Exclude<keyof T, OptionalKeys>]-?: Key extends
    | "created_at"
    | "last_modified_at"
    ? ApiTimestamp
    : Defined<T[Key]>;
} & {
  [Key in OptionalKeys]?: Defined<T[Key]> | null;
};

// ── Shared primitives ──────────────────────────────────────────────

export type JsonValue = Defined<CreateDefaultConfigInput["value"]>;
export type Condition = Defined<ContextPut["context"]>;
export type Overrides = Defined<ContextPut["override"]>;
export type DependencyGraph = Defined<DimensionResponse["dependency_graph"]>;

export type { SortBy };

// ── Pagination ─────────────────────────────────────────────────────

export type PaginationParams = Pick<ListContextsInput, "page" | "count" | "all">;

export interface PaginatedResponse<T> {
  total_pages: Defined<ListContextsOutput["total_pages"]>;
  total_items: Defined<ListContextsOutput["total_items"]>;
  data: T[];
}

// ── Dimension ──────────────────────────────────────────────────────

export type { DimensionType };
export type Dimension = RawResponse<
  DimensionResponse,
  Extract<OptionalFunctionKeys, keyof DimensionResponse>
>;
export type CreateDimensionRequest = RequestBody<CreateDimensionInput>;
export type UpdateDimensionRequest = RequestBody<UpdateDimensionInput, "dimension">;

// ── Default Config ─────────────────────────────────────────────────

export type DefaultConfig = RawResponse<
  DefaultConfigResponse,
  Extract<OptionalFunctionKeys, keyof DefaultConfigResponse>
>;
export type CreateDefaultConfigRequest = RequestBody<CreateDefaultConfigInput>;
export type UpdateDefaultConfigRequest = RequestBody<UpdateDefaultConfigInput, "key">;
export type DefaultConfigFilters = Pick<ListDefaultConfigsInput, "name"> & {
  prefix?: string[];
};

// ── Context / Override ─────────────────────────────────────────────

export type ContextOverride = Omit<RawResponse<ContextResponse>, "override"> & {
  override_: Overrides;
  override?: ContextResponse["override"];
};

export type PutContextRequest = ContextPut;

export type ContextListFilters = Pick<
  ListContextsInput,
  "prefix" | "sort_by" | "created_by" | "last_modified_by" | "plaintext"
> & {
  dimension?: Condition;
  dimension_match_strategy?: DimensionMatchStrategy;
  sort_on?: ContextFilterSortOn;
};

// ── Config Resolution ──────────────────────────────────────────────

export interface Config {
  contexts: ContextOverride[];
  overrides: Record<string, Record<string, JsonValue>>;
  default_configs: Record<string, DefaultConfig>;
}

export type ResolvedConfigResponse = GetResolvedConfigOutput;
