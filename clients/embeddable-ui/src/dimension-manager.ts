export type { ClientConfig } from "./api/client";
export { DimensionManager } from "./pages/DimensionManager";
export type { DimensionManagerProps } from "./pages/DimensionManager";
export {
    SuperpositionUIProvider,
    useOptionalSuperposition,
    useSuperposition,
    useSuperpositionTheme,
} from "./providers/SuperpositionUIProvider";
export type {
    SuperpositionThemeValue,
} from "./providers/theme-context";
export type { SuperpositionUIProviderProps } from "./providers/SuperpositionUIProvider";
export type {
    Dimension,
    JsonValue,
    SuperpositionEmbeddableConfig,
    SuperpositionFeatureCapabilities,
    SuperpositionFilterConfig,
    SuperpositionScopeConfig,
    SuperpositionThemeConfig,
    SuperpositionThemeMode,
    SuperpositionThemeTokens,
} from "./types";
