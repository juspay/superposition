export * from "./api";
import type { JsonValue } from "./api";

export const SUPERPOSITION_FEATURES = [
  "config",
  "overrides",
  "dimensions",
  "experiments",
  "resolve",
  "audit",
] as const;

export type SuperpositionFeature = (typeof SUPERPOSITION_FEATURES)[number];

export const SUPERPOSITION_FEATURE_LABELS: Record<SuperpositionFeature, string> = {
  config: "Configs",
  overrides: "Overrides",
  dimensions: "Dimensions",
  experiments: "Experiments",
  resolve: "Resolve",
  audit: "Audit Log",
};

export type RouteMode = "internal" | "external";
type InternalTransportMode = "same-origin" | "cross-origin" | "host-proxy";
export type AuthMode = "cookie" | "bearer" | "custom";
export type SuperpositionThemeMode = "light" | "dark" | "system";

export interface SuperpositionRequestContext {
  url: string;
  init: RequestInit;
}

export interface SuperpositionResponseContext {
  request: SuperpositionRequestContext;
  response: Response;
}

export interface SuperpositionAuthConfig {
  mode: AuthMode;
  token?: string;
  headers?: Record<string, string>;
}

interface InternalTransportConfig {
  mode: InternalTransportMode;
  baseUrl: string;
  apiBasePath?: string;
  credentials?: RequestCredentials;
  workspaceHeaderName?: "x-workspace" | "x-tenant";
}

export interface SuperpositionRoutingConfig {
  mode: RouteMode;
  initialFeature?: SuperpositionFeature;
  currentFeature?: SuperpositionFeature;
  onNavigate?: (feature: SuperpositionFeature) => void;
  getFeatureHref?: (feature: SuperpositionFeature) => string;
}

export interface SuperpositionThemeTokens {
  colorBg?: string;
  colorPanel?: string;
  colorText?: string;
  colorMuted?: string;
  colorBorder?: string;
  colorPrimary?: string;
  colorSuccess?: string;
  colorWarning?: string;
  colorDanger?: string;
  fontFamily?: string;
  fontSize?: string;
  radiusSm?: string;
  radiusMd?: string;
  radiusLg?: string;
  spaceXs?: string;
  spaceSm?: string;
  spaceMd?: string;
  spaceLg?: string;
  shadowSm?: string;
  shadowMd?: string;
}

export interface SuperpositionThemeConfig extends SuperpositionThemeTokens {
  mode?: SuperpositionThemeMode;
}

export interface SuperpositionScopeConfig {
  context?: Record<string, JsonValue>;
  locked?: boolean;
}

export interface SuperpositionFilterConfig {
  defaultConfigPrefix?: string | string[];
}

export interface SuperpositionUiAdapters {
  confirm?: (input: {
    title: string;
    description?: string;
    confirmLabel?: string;
    cancelLabel?: string;
  }) => Promise<boolean>;
}

export interface SuperpositionNetworkHooks {
  interceptRequest?: (
    context: SuperpositionRequestContext,
  ) => Promise<SuperpositionRequestContext> | SuperpositionRequestContext;
  interceptResponse?: (
    context: SuperpositionResponseContext,
  ) => Promise<Response> | Response;
  onUnauthorized?: (response: Response) => void;
  onForbidden?: (response: Response) => void;
  onApiError?: (error: unknown) => void;
}

export interface SuperpositionEmbeddableConfig {
  apiBaseUrl: string;
  apiBasePath?: string;
  credentials?: RequestCredentials;
  workspaceHeaderName?: "x-workspace" | "x-tenant";
  orgId: string;
  workspace: string;
  auth?: SuperpositionAuthConfig;
  network?: SuperpositionNetworkHooks;
  scope?: SuperpositionScopeConfig;
  filters?: SuperpositionFilterConfig;
  readOnly?: boolean;
  features?: SuperpositionFeature[];
  routing?: SuperpositionRoutingConfig;
  theme?: SuperpositionThemeConfig;
  ui?: SuperpositionUiAdapters;
}

export interface NormalizedSuperpositionConfig extends Omit<
  SuperpositionEmbeddableConfig,
  "apiBaseUrl" | "apiBasePath" | "credentials" | "workspaceHeaderName" | "scope"
> {
  transport: InternalTransportConfig;
  context?: Record<string, JsonValue>;
  lockScopedDimensions: boolean;
}
