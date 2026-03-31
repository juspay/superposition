// ── API Client ──────────────────────────────────────────────────────
export { SuperpositionClient, SuperpositionApiError } from "./api/client";
export type { ClientConfig } from "./api/client";

// ── Providers ───────────────────────────────────────────────────────
export { SuperpositionProvider, useSuperposition } from "./providers";
export { AlertProvider, useAlerts } from "./providers";

// ── Pages (embeddable feature components) ───────────────────────────
export {
  SuperpositionAdmin,
  ConfigManager,
  DimensionManager,
  OverrideManager,
  ExperimentManager,
  ResolveView,
  AuditLog,
} from "./pages";

// ── Core components ─────────────────────────────────────────────────
export { Table, Pagination, Modal, FormField, AlertBar } from "./components";
export { ConditionBadges, StatusBadge, JsonViewer } from "./components";

// ── Hooks ───────────────────────────────────────────────────────────
export { useApi, useMutation } from "./hooks";

// ── Utils ───────────────────────────────────────────────────────────
export {
  contextMatchesScope,
  filterOverridesByScope,
  filterExperimentsByScope,
  mergeScopedContext,
  getLockedDimensions,
} from "./utils";

// ── Types ───────────────────────────────────────────────────────────
export type { SuperpositionConfig } from "./types";
export type {
  Dimension,
  DefaultConfig,
  ContextOverride,
  Experiment,
  ExperimentStatus,
  Variant,
  Condition,
  Overrides,
  PaginatedResponse,
  PaginationParams,
  JsonValue,
  AuditLogEntry,
  Config,
  FunctionDef,
  TypeTemplate,
  ConfigVersion,
} from "./types";
