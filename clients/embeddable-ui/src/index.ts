// Minimal convenience entrypoint.
// Prefer explicit subpaths such as ./admin or ./config-manager for real usage.
export { SuperpositionUIProvider } from "./providers/SuperpositionUIProvider";
export type { SuperpositionUIProviderProps } from "./providers/SuperpositionUIProvider";
export { AlertBar, AlertProvider, useAlerts } from "./providers";
export { SuperpositionAdmin } from "./pages/SuperpositionAdmin";
export type { SuperpositionAdminProps } from "./pages/SuperpositionAdmin";
export type {
  SuperpositionEmbeddableConfig,
} from "./types";
