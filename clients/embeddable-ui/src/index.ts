import "./styles.css";

// ── API Client ──────────────────────────────────────────────────────
export { SuperpositionApiError, SuperpositionClient } from "./api/client";
export type { ClientConfig } from "./api/client";

// ── Providers ───────────────────────────────────────────────────────
export {
  AlertProvider,
  SuperpositionProvider,
  ThemeProvider,
  useAlerts,
  useSuperposition,
  useSuperpositionTheme,
} from "./providers";
export type { SuperpositionThemeValue, ThemeProviderProps } from "./providers";

// ── Pages (embeddable feature components) ───────────────────────────
export {
  AuditLog,
  ConfigManager,
  DefaultConfigManager,
  DimensionManager,
  DimensionsManager,
  ExperimentManager,
  OverrideManager,
  OverridesManager,
  ResolveView,
  SuperpositionAdmin,
} from "./pages";

// ── Core components ─────────────────────────────────────────────────
export {
  AlertBar,
  ConditionBadges,
  FormField,
  JsonViewer,
  Modal,
  Pagination,
  StatusBadge,
  Table,
} from "./components";

// ── Hooks ───────────────────────────────────────────────────────────
export { useApi, useMutation } from "./hooks";

// ── Public constants ────────────────────────────────────────────────
export { SUPERPOSITION_FEATURE_LABELS, SUPERPOSITION_FEATURES } from "./types";

// ── Utils ───────────────────────────────────────────────────────────
export {
  contextMatchesScope,
  filterExperimentsByScope,
  filterOverridesByScope,
  getLockedDimensions,
  mergeScopedContext,
} from "./utils";

// ── Types ───────────────────────────────────────────────────────────
export type {
  AuditLogEntry,
  AuthMode,
  Condition,
  Config,
  ContextOverride,
  DefaultConfig,
  Dimension,
  Experiment,
  ExperimentStatus,
  JsonValue,
  Overrides,
  PaginatedResponse,
  PaginationParams,
  RouteMode,
  SuperpositionAuthConfig,
  SuperpositionEmbeddableConfig,
  SuperpositionFeature,
  SuperpositionNetworkHooks,
  SuperpositionRequestContext,
  SuperpositionResponseContext,
  SuperpositionFilterConfig,
  SuperpositionRoutingConfig,
  SuperpositionScopeConfig,
  SuperpositionThemeConfig,
  SuperpositionThemeMode,
  SuperpositionThemeTokens,
  SuperpositionUiAdapters,
  Variant,
} from "./types";
